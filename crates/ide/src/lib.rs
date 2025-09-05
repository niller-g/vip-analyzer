//! ide crate provides "ide-centric" APIs for the rust-analyzer. That is,
//! it generally operates with files and text ranges, and returns results as
//! Strings, suitable for displaying to the human.
//!
//! What powers this API are the `RootDatabase` struct, which defines a `salsa`
//! database, and the `hir` crate, where majority of the analysis happens.
//! However, IDE specific bits of the analysis (most notably completion) happen
//! in this crate.

#[cfg(test)]
mod fixture;

mod goto_declaration;
mod goto_definition;
mod navigation_target;
mod references;
mod view_syntax_tree;

use ide_db::{
    LineIndexDatabase,
    assists::AssistResolveStrategy,
    base_db::{RootQueryDb, SourceDatabase},
    prime_caches,
    search::SearchScope,
};
use syntax::SourceFile;
use triomphe::Arc;

pub use crate::navigation_target::{NavigationTarget, TryToNav};
use hir::Semantics;
pub use ide_db::{
    RootDatabase,
    base_db::{FileChange, FileId, FilePosition, FileRange, salsa::Cancelled},
    label::Label,
    line_index::{LineCol, LineIndex},
    prime_caches::ParallelPrimeCachesProgress,
    source_change::{FileSystemEdit, SnippetEdit, SourceChange},
    text_edit::{Indel, TextEdit},
};
pub use ide_diagnostics::{Diagnostic, DiagnosticCode};
use references::ReferenceSearchResult;
use std::panic::AssertUnwindSafe;
pub use syntax::{TextRange, TextSize};

pub type Cancellable<T> = Result<T, Cancelled>;

/// Info associated with a text range.
#[derive(Debug)]
pub struct RangeInfo<T> {
    pub range: TextRange,
    pub info: T,
}

impl<T> RangeInfo<T> {
    pub fn new(range: TextRange, info: T) -> RangeInfo<T> {
        RangeInfo { range, info }
    }
}

/// `AnalysisHost` stores the current state of the world.
#[derive(Debug)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    pub fn new(lru_capacity: Option<u16>) -> AnalysisHost {
        AnalysisHost { db: RootDatabase::new(lru_capacity) }
    }

    pub fn with_database(db: RootDatabase) -> AnalysisHost {
        AnalysisHost { db }
    }

    pub fn update_lru_capacity(&mut self, lru_capacity: Option<u16>) {
        self.db.update_base_query_lru_capacities(lru_capacity);
    }

    // pub fn update_lru_capacities(&mut self, lru_capacities: &FxHashMap<Box<str>, usize>) {
    //     self.db.update_lru_capacities(lru_capacities);
    // }

    /// Returns a snapshot of the current state, which you can query for
    /// semantic information.
    pub fn analysis(&self) -> Analysis {
        Analysis { db: self.db.clone() }
    }

    /// Applies changes to the current state of the world. If there are
    /// outstanding snapshots, they will be canceled.
    pub fn apply_change(&mut self, change: FileChange) {
        self.db.apply_change(change);
    }

    /// NB: this clears the database
    // pub fn per_query_memory_usage(&mut self) -> Vec<(String, profile::Bytes, usize)> {
    //     self.db.per_query_memory_usage()
    // }
    pub fn request_cancellation(&mut self) {
        self.db.request_cancellation();
    }
    pub fn raw_database(&self) -> &RootDatabase {
        &self.db
    }
    pub fn raw_database_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }
}

impl Default for AnalysisHost {
    fn default() -> AnalysisHost {
        AnalysisHost::new(None)
    }
}

/// Analysis is a snapshot of a world state at a moment in time. It is the main
/// entry point for asking semantic information about the world. When the world
/// state is advanced using `AnalysisHost::apply_change` method, all existing
/// `Analysis` are canceled (most method return `Err(Canceled)`).
#[derive(Debug)]
pub struct Analysis {
    db: RootDatabase,
}

// As a general design guideline, `Analysis` API are intended to be independent
// from the language server protocol. That is, when exposing some functionality
// we should think in terms of "what API makes most sense" and not in terms of
// "what types LSP uses". Although currently LSP is the only consumer of the
// API, the API should in theory be usable as a library, or via a different
// protocol.
impl Analysis {
    pub fn parallel_prime_caches<F>(&self, num_worker_threads: usize, cb: F) -> Cancellable<()>
    where
        F: Fn(ParallelPrimeCachesProgress) + Sync + std::panic::UnwindSafe,
    {
        self.with_db(move |db| prime_caches::parallel_prime_caches(db, num_worker_threads, &cb))
    }

    /// Gets the text of the source file.
    pub fn file_text(&self, file_id: FileId) -> Cancellable<Arc<str>> {
        self.with_db(|db| SourceDatabase::file_text(db, file_id).text(db))
    }

    /// Gets the syntax tree of the file.
    pub fn parse(&self, file_id: FileId) -> Cancellable<SourceFile> {
        self.with_db(|db| db.parse(file_id).tree())
    }

    /// Gets the file's `LineIndex`: data structure to convert between absolute
    /// offsets and line/column representation.
    pub fn file_line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.with_db(|db| db.line_index(file_id))
    }

    pub fn view_syntax_tree(&self, file_id: FileId) -> Cancellable<String> {
        self.with_db(|db| view_syntax_tree::view_syntax_tree(db, file_id))
    }

    /// Returns the definitions from the symbol at `position`.
    pub fn goto_definition(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<RangeInfo<Vec<NavigationTarget>>>> {
        self.with_db(|db| goto_definition::goto_definition(db, position))
    }

    /// Returns the declaration from the symbol at `position`.
    pub fn goto_declaration(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<RangeInfo<Vec<NavigationTarget>>>> {
        self.with_db(|db| goto_declaration::goto_declaration(db, position))
    }

    /// Finds all usages of the reference at point.
    pub fn find_all_refs(
        &self,
        position: FilePosition,
        search_scope: Option<SearchScope>,
    ) -> Cancellable<Option<Vec<ReferenceSearchResult>>> {
        let search_scope = AssertUnwindSafe(search_scope);
        self.with_db(|db| {
            let _ = &search_scope;
            references::find_all_refs(&Semantics::new(db), position, search_scope.0)
        })
    }

    /// Computes the set of parser level diagnostics for the given file.
    pub fn syntax_diagnostics(&self, file_id: FileId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| ide_diagnostics::syntax_diagnostics(db, file_id))
    }

    /// Computes the set of semantic diagnostics for the given file.
    pub fn semantic_diagnostics(
        &self,
        resolve: AssistResolveStrategy,
        file_id: FileId,
    ) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| ide_diagnostics::semantic_diagnostics(db, &resolve, file_id))
    }

    /// Computes the set of both syntax and semantic diagnostics for the given file.
    pub fn full_diagnostics(
        &self,
        resolve: AssistResolveStrategy,
        file_id: FileId,
    ) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| ide_diagnostics::full_diagnostics(db, &resolve, file_id))
    }

    /// Performs an operation on the database that may be canceled.
    ///
    /// rust-analyzer needs to be able to answer semantic questions about the
    /// code while the code is being modified. A common problem is that a
    /// long-running query is being calculated when a new change arrives.
    ///
    /// We can't just apply the change immediately: this will cause the pending
    /// query to see inconsistent state (it will observe an absence of
    /// repeatable read). So what we do is we **cancel** all pending queries
    /// before applying the change.
    ///
    /// Salsa implements cancellation by unwinding with a special value and
    /// catching it on the API boundary.
    fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        let snap = self.db.clone();
        Cancelled::catch(|| f(&snap))
    }
}

#[test]
fn analysis_is_send() {
    fn is_send<T: Send>() {}
    is_send::<Analysis>();
}
