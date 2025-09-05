//! This crate defines the core data structure representing IDE state -- `RootDatabase`.
//!
//! It is mainly a `HirDatabase` for semantic analysis, plus a `SymbolsDatabase`, for fuzzy search.

mod apply_change;
pub mod assists;
pub mod classify;
pub mod documentation;
pub mod helpers;
pub mod label;
pub mod prime_caches;
pub mod search;
pub mod source_change;
pub mod symbol_index;
pub mod text_edit;

pub mod syntax_helpers {
    pub mod tree_diff;

    pub use parser::LexedStr;
}

use {
    base_db::{FileText, Files, ProjectGraphBuilder, ProjectsMap, RootQueryDb, query_group},
    std::{fmt, mem::ManuallyDrop},
};

use base_db::SourceDatabase;
use salsa::Durability;
use triomphe::Arc;

use crate::line_index::LineIndex;
pub use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
pub use vfs::FileId;

pub use ::line_index;

/// `base_db` is normally also needed in places where `ide_db` is used, so this re-export is for convenience.
pub use base_db;

pub type FxIndexSet<T> = indexmap::IndexSet<T, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;
pub type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

// pub type FilePosition = FilePositionWrapper<FileId>;
// pub type FileRange = FileRangeWrapper<FileId>;

#[salsa_macros::db]
pub struct RootDatabase {
    // FIXME: Revisit this commit now that we migrated to the new salsa, given we store arcs in this
    // db directly now
    // We use `ManuallyDrop` here because every codegen unit that contains a
    // `&RootDatabase -> &dyn OtherDatabase` cast will instantiate its drop glue in the vtable,
    // which duplicates `Weak::drop` and `Arc::drop` tens of thousands of times, which makes
    // compile times of all `ide_*` and downstream crates suffer greatly.
    storage: ManuallyDrop<salsa::Storage<Self>>,
    files: Arc<Files>,
    projects_map: Arc<ProjectsMap>,
}

impl std::panic::RefUnwindSafe for RootDatabase {}

#[salsa_macros::db]
impl salsa::Database for RootDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

impl Drop for RootDatabase {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.storage) };
    }
}

impl Clone for RootDatabase {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            files: self.files.clone(),
            projects_map: self.projects_map.clone(),
        }
    }
}

impl fmt::Debug for RootDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RootDatabase").finish()
    }
}

#[salsa_macros::db]
impl SourceDatabase for RootDatabase {
    fn file_text(&self, file_id: vfs::FileId) -> FileText {
        self.files.file_text(file_id)
    }

    fn set_file_text(&mut self, file_id: vfs::FileId, path: vfs::VfsPath, text: &str) {
        let files = Arc::clone(&self.files);
        files.set_file_text(self, file_id, path, text);
    }

    fn set_file_text_with_durability(
        &mut self,
        file_id: vfs::FileId,
        path: vfs::VfsPath,
        text: &str,
        durability: Durability,
    ) {
        let files = Arc::clone(&self.files);
        files.set_file_text_with_durability(self, file_id, path, text, durability);
    }

    fn file_for_path(&self, path: base_db::VfsPath) -> Option<base_db::FileId> {
        self.files.file_for_path(path)
    }

    fn projects_map(&self) -> Arc<ProjectsMap> {
        self.projects_map.clone()
    }
}

impl Default for RootDatabase {
    fn default() -> RootDatabase {
        RootDatabase::new(None)
    }
}

impl RootDatabase {
    pub fn new(lru_capacity: Option<u16>) -> RootDatabase {
        let mut db = RootDatabase {
            storage: ManuallyDrop::new(salsa::Storage::default()),
            files: Default::default(),
            projects_map: Default::default(),
        };
        // This needs to be here otherwise `ProjectGraphBuilder` will panic.
        db.set_all_projects(Arc::new(Box::new([])));
        ProjectGraphBuilder::default().set_in_db(&mut db);
        db.update_base_query_lru_capacities(lru_capacity);
        db
    }

    pub fn update_base_query_lru_capacities(&mut self, _lru_capacity: Option<u16>) {
        // let lru_capacity = lru_capacity.unwrap_or(base_db::DEFAULT_PARSE_LRU_CAP);
        // base_db::FileTextQuery.in_db_mut(self).set_lru_capacity(DEFAULT_FILE_TEXT_LRU_CAP);
        // base_db::ParseQuery.in_db_mut(self).set_lru_capacity(lru_capacity);
    }

    pub fn update_lru_capacities(&mut self, _lru_capacities: &FxHashMap<Box<str>, u16>) {
        // base_db::FileTextQuery.in_db_mut(self).set_lru_capacity(DEFAULT_FILE_TEXT_LRU_CAP);
        // base_db::ParseQuery.in_db_mut(self).set_lru_capacity(
        //     lru_capacities
        //         .get(stringify!(ParseQuery))
        //         .copied()
        //         .unwrap_or(base_db::DEFAULT_PARSE_LRU_CAP),
        // );
    }
}

#[query_group::query_group]
pub trait LineIndexDatabase: base_db::RootQueryDb {
    #[salsa::invoke_interned(line_index)]
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;
}

fn line_index(db: &dyn LineIndexDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_text(file_id).text(db);
    Arc::new(LineIndex::new(&text))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SnippetCap {
    _private: (),
}

impl SnippetCap {
    pub const fn new(allow_snippets: bool) -> Option<SnippetCap> {
        if allow_snippets { Some(SnippetCap { _private: () }) } else { None }
    }
}
