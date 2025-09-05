use {
    crate::{RootDatabase, classify::Declaration},
    hir::Semantics,
    rustc_hash::FxHashMap,
    span::{FileRange, TextRange},
    syntax::ast,
    vfs::FileId,
};

#[derive(Debug, Default, Clone)]
pub struct UsageSearchResult {
    pub references: FxHashMap<FileId, Vec<FileReference>>,
}

impl UsageSearchResult {
    pub fn is_empty(&self) -> bool {
        self.references.is_empty()
    }

    pub fn len(&self) -> usize {
        self.references.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (FileId, &[FileReference])> + '_ {
        self.references.iter().map(|(&file_id, refs)| (file_id, &**refs))
    }

    pub fn file_ranges(&self) -> impl Iterator<Item = FileRange> + '_ {
        self.references.iter().flat_map(|(&file_id, refs)| {
            refs.iter().map(move |&FileReference { range, .. }| FileRange { file_id, range })
        })
    }
}

impl IntoIterator for UsageSearchResult {
    type Item = (FileId, Vec<FileReference>);
    type IntoIter = <FxHashMap<FileId, Vec<FileReference>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.references.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct FileReference {
    /// The range of the reference in the original file
    pub range: TextRange,
    /// The node of the reference in the (macro-)file
    pub name: FileReferenceNode,
    pub category: ReferenceCategory,
}

#[derive(Debug, Clone)]
pub enum FileReferenceNode {
    Name(ast::Ident),
    // TODO: add more node types as needed
}

bitflags::bitflags! {
    #[derive(Copy, Clone, Default, PartialEq, Eq, Hash, Debug)]
    pub struct ReferenceCategory: u8 {
        const WRITE = 1 << 0;
        const READ = 1 << 1;
    }
}

/// Generally, `search_scope` returns files that might contain references for the element.
///
/// In some cases, the location of the references is known to within a `TextRange`,
/// e.g. for things like local variables.
#[derive(Clone, Debug)]
pub struct SearchScope {
    #[expect(dead_code)]
    entries: FxHashMap<FileId, Option<TextRange>>,
}
impl SearchScope {
    pub fn new(entries: FxHashMap<FileId, Option<TextRange>>) -> Self {
        Self { entries }
    }
}

impl Declaration {
    pub fn usages<'a>(self, sema: &'a Semantics<'_, RootDatabase>) -> FindUsages<'a> {
        FindUsages { decl: self, sema, scope: None }
    }
}

#[derive(Clone)]
pub struct FindUsages<'a> {
    #[expect(dead_code)]
    decl: Declaration,
    #[expect(dead_code)]
    sema: &'a Semantics<'a, RootDatabase>,
    scope: Option<&'a SearchScope>,
}
impl<'a> FindUsages<'a> {
    /// Limit the search to a given [`SearchScope`].
    pub fn set_scope(mut self, scope: Option<&'a SearchScope>) -> Self {
        assert!(self.scope.is_none());
        self.scope = scope;
        self
    }

    pub fn all(self) -> UsageSearchResult {
        let mut res = UsageSearchResult::default();
        self.search(&mut |file_id, reference| {
            res.references.entry(file_id).or_default().push(reference);
            false
        });
        res
    }

    pub fn search(&self, sink: &mut dyn FnMut(FileId, FileReference) -> bool) {
        let _p = tracing::info_span!("FindUsages::search").entered();

        _ = sink;
        unimplemented!()
    }
}
