use crate::{FilePosition, NavigationTarget, TryToNav};
use hir::Semantics;
use ide_db::{
    FileId, RootDatabase,
    classify::Declaration,
    search::{ReferenceCategory, SearchScope},
};
use itertools::Itertools;
use nohash_hasher::IntMap;
use syntax::{AstNode, SyntaxNode, TextRange, TextSize};

#[derive(Debug, Clone)]
pub struct ReferenceSearchResult {
    pub declaration: Option<NavigationTarget>,
    pub references: IntMap<FileId, Vec<(TextRange, ReferenceCategory)>>,
}

// Feature: Find All References
//
// Shows all references of the item at the cursor location
//
// | Editor  | Shortcut |
// |---------|----------|
// | VS Code | <kbd>Shift+Alt+F12</kbd> |
//
// ![Find All References](https://user-images.githubusercontent.com/48062697/113020670-b7c34f00-917a-11eb-8003-370ac5f2b3cb.gif)
pub(crate) fn find_all_refs(
    sema: &Semantics<'_, RootDatabase>,
    position: FilePosition,
    search_scope: Option<SearchScope>,
) -> Option<Vec<ReferenceSearchResult>> {
    let _p = tracing::info_span!("find_all_refs").entered();
    let syntax = sema.parse(position.file_id).syntax().clone();

    let search = |decl: Declaration| {
        let usages = decl.usages(sema).set_scope(search_scope.as_ref()).all();

        let references: IntMap<FileId, Vec<(TextRange, ReferenceCategory)>> = usages
            .into_iter()
            .map(|(file_id, refs)| {
                (
                    file_id,
                    refs.into_iter()
                        .map(|file_ref| (file_ref.range, file_ref.category))
                        .unique()
                        .collect(),
                )
            })
            .collect();

        ReferenceSearchResult { declaration: decl.try_to_nav(sema.db), references }
    };

    Some(find_decls(sema, &syntax, position.offset)?.into_iter().map(search).collect())
}

pub(crate) fn find_decls(
    sema: &Semantics<'_, RootDatabase>,
    syntax: &SyntaxNode,
    offset: TextSize,
) -> Option<Vec<Declaration>> {
    _ = sema;
    _ = syntax;
    _ = offset;
    unimplemented!()
}
