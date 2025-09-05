use {
    hir::{
        ClassFactFunctor, ClassPredicate, ClassProperty, Constructor, FactFunctor,
        MemberDeclaration, Predicate, Property, Semantics,
    },
    syntax::SyntaxToken,
};
use {
    ide_db::classify::Definition,
    syntax::{AstNode, SyntaxKind::*, T},
};
use {
    ide_db::{RootDatabase, classify::IdentClass, helpers::pick_best_token},
    itertools::Itertools,
};

use crate::{FilePosition, NavigationTarget, RangeInfo, TryToNav};

// Feature: Go to Declaration
//
// Navigates to the declaration of an identifier.
//
pub(crate) fn goto_declaration(
    db: &RootDatabase,
    FilePosition { file_id, offset }: FilePosition,
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

    let navs = lookup_decls(sema, &token).into_iter().flatten().unique();

    Some(RangeInfo::new(token.text_range(), navs.collect()))
}

fn lookup_decls(
    sema: &Semantics<'_, RootDatabase>,
    token: &SyntaxToken,
) -> Option<Vec<NavigationTarget>> {
    let node = IdentClass::classify_token(sema, token)?;

    let navs = match node {
        IdentClass::Definition(def) => match def {
            Definition::ImplementItem(implement) => {
                let decl = implement.declaration_source(sema.db)?;
                let classes = sema.matching_classes(decl.as_ref());
                let navs = classes.into_iter().filter_map(|class| class.try_to_nav(sema.db));
                return Some(navs.collect());
            }
            Definition::Clause(clause) => match sema.find_clause_declaration(clause)? {
                MemberDeclaration::Predicate(id) => Predicate::from(id).try_to_nav(sema.db),
                MemberDeclaration::ClassPredicate(id) => {
                    ClassPredicate::from(id).try_to_nav(sema.db)
                }
                MemberDeclaration::Constructor(id) => Constructor::from(id).try_to_nav(sema.db),
                MemberDeclaration::FactFunctor(id) => FactFunctor::from(id).try_to_nav(sema.db),
                MemberDeclaration::ClassFactFunctor(id) => {
                    ClassFactFunctor::from(id).try_to_nav(sema.db)
                }
                MemberDeclaration::Property(id) => Property::from(id).try_to_nav(sema.db),
                MemberDeclaration::ClassProperty(id) => ClassProperty::from(id).try_to_nav(sema.db),
                // Not a declaration of a clause
                MemberDeclaration::Constant(_)
                | MemberDeclaration::FactVar(_)
                | MemberDeclaration::ClassFactVar(_)
                | MemberDeclaration::Functor(_) => None,
            },
        },
        IdentClass::Declaration(decl) => decl.try_to_nav(sema.db),
    }?;
    Some(vec![navs])
}

#[cfg(test)]
mod tests {
    use crate::FileRange;
    use crate::fixture;
    use itertools::Itertools;

    fn check(fixture: &str) {
        let (analysis, position, expected) = fixture::annotations(fixture);
        let navs = analysis
            .goto_declaration(position)
            .unwrap()
            .expect("no declaration or definition found")
            .info;
        if navs.is_empty() {
            panic!("unresolved reference")
        }

        let cmp = |&FileRange { file_id, range }: &_| (file_id, range.start());
        let navs = navs
            .into_iter()
            .map(|nav| FileRange { file_id: nav.file_id, range: nav.focus_or_full_range() })
            .sorted_by_key(cmp)
            .collect::<Vec<_>>();
        let expected = expected
            .into_iter()
            .map(|(FileRange { file_id, range }, _)| FileRange { file_id, range })
            .sorted_by_key(cmp)
            .collect::<Vec<_>>();
        assert_eq!(expected, navs);
    }

    #[test]
    fn goto_decl_smoke_test() {
        check(
            r"
//- /main.i
interface foo
        % ^^^
predicates
    bar : (foo$0 Foo).

end interface
",
        )
    }
}
