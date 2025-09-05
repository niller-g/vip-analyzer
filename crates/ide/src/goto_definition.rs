use crate::goto_declaration::goto_declaration;
use crate::navigation_target::TryToNav;
use crate::{NavigationTarget, RangeInfo};
use hir::Semantics;
use ide_db::base_db::RootQueryDb;
use ide_db::classify::{Declaration, IdentClass};
use ide_db::{
    FileId, RootDatabase,
    base_db::{AnchoredPath, FilePosition, SourceDatabase},
    helpers::pick_best_token,
};
use itertools::Itertools;
use syntax::{
    AstNode, AstToken,
    SyntaxKind::*,
    SyntaxToken, T, TextRange,
    ast::{self},
};

// Feature: Go to Definition
//
// Navigates to the definition of an identifier.
//
// For outline modules, this will navigate to the source file of the module.
//
// | Editor  | Shortcut |
// |---------|----------|
// | VS Code | <kbd>F12</kbd> |
//
// ![Go to Definition](https://user-images.githubusercontent.com/48062697/113065563-025fbe00-91b1-11eb-83e4-a5a703610b23.gif)
pub(crate) fn goto_definition(
    db: &RootDatabase,
    position @ FilePosition { file_id, offset }: FilePosition,
) -> Option<RangeInfo<Vec<NavigationTarget>>> {
    let sema = &Semantics::new(db);

    let file = sema.parse(file_id).syntax().clone();
    let token = pick_best_token(file.token_at_offset(offset), |kind| match kind {
        IDENT | VAR | INT_NUMBER | COMMENT => 4,
        // Prefix operators
        T![+] | T![-] | T![~~] => 3,
        kind if kind.is_keyword() => 2,
        T!['('] | T![')'] => 2,
        kind if kind.is_trivia() => 0,
        _ => 1,
    })?;

    let navs: Vec<NavigationTarget> =
        lookup_defs(sema, &token, file_id).into_iter().flatten().unique().collect();

    if navs.is_empty() {
        goto_declaration(db, position)
    } else {
        Some(RangeInfo::new(token.text_range(), navs))
    }
}

fn lookup_defs(
    sema: &Semantics<'_, RootDatabase>,
    token: &SyntaxToken,
    file_id: FileId,
) -> Option<Vec<NavigationTarget>> {
    if let Some(token) = ast::String::cast(token.clone()) {
        let navs = lookup_include_path(sema, token, file_id);
        if !navs.is_empty() {
            return Some(navs);
        }
    }
    let node = IdentClass::classify_token(sema, token)?;

    let declaration = match node {
        IdentClass::Definition(_) => return None, // <-- When already at the definition, instead goto declaration
        IdentClass::Declaration(decl) => match decl {
            Declaration::ClassItem(class) => {
                let def = class.definition_source(sema.db)?;
                let implements = sema.matching_implements(def.as_ref());
                let navs = implements.into_iter().filter_map(|impl_| impl_.try_to_nav(sema.db));

                return Some(navs.collect());
            }
            Declaration::Predicate(decl) => decl.into(),
            Declaration::ClassPredicate(decl) => decl.into(),
            Declaration::Constructor(decl) => decl.into(),
            // These might have a definition
            Declaration::FactFunctor(decl) => decl.into(),
            Declaration::ClassFactFunctor(decl) => decl.into(),
            Declaration::Property(decl) => decl.into(),
            Declaration::ClassProperty(decl) => decl.into(),
            // Use goto declaration for these
            Declaration::InterfaceItem(_)
            | Declaration::Constant(_)
            | Declaration::Domain(_)
            | Declaration::Functor(_)
            | Declaration::FactVar(_)
            | Declaration::ClassFactVar(_) => return None,
        },
    };

    let nav = match sema.find_definition(&token.parent()?, &declaration)? {
        hir::MemberDefinition::ClauseFamily(clauses) => {
            clauses.first().and_then(|c| c.try_to_nav(sema.db))?
        }
    };

    Some(vec![nav])
}

fn lookup_include_path(
    sema: &Semantics<'_, RootDatabase>,
    token: ast::String,
    file_id: FileId,
) -> Vec<NavigationTarget> {
    let string_seq = match token.string_seq() {
        Some(it) => it,
        None => return Vec::new(),
    };
    if !string_seq.is_path_directive() {
        return Vec::new();
    }
    let path = match string_seq.value() {
        Some(it) => it,
        None => return Vec::new(),
    };

    sema.db
        .resolve_paths(AnchoredPath { anchor: file_id, path: &path })
        .into_iter()
        .filter_map(|file_id| {
            let size = sema.db.file_text(file_id).text(sema.db).len().try_into().ok()?;
            Some(NavigationTarget {
                file_id,
                full_range: TextRange::new(0.into(), size),
                name: path.clone().into(),
                alias: None,
                focus_range: None,
                container_name: None,
                description: None,
                docs: None,
            })
        })
        .collect()
}
