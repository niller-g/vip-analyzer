pub(crate) mod expressions;
pub(crate) mod statements;

use self::items::scope_items;
use super::*;
use crate::{
    parser::{CompletedMarker, Marker},
    test_ok,
};
use namespaces::at_namespace;

pub(crate) fn at_ref_term(p: &Parser<'_>) -> bool {
    match p.current() {
        IDENT => true,
        T![:] => p.nth_at(1, T![:]) && p.nth_at(2, IDENT),
        _ => at_namespace(p),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum RefKind {
    Name { generics: bool },
    Member { generics: bool },
    NamespacedScope,
    NamespacedMember { generics: bool },
}

test_ok!(|p| ref_term(p, true), "bar", simple);
test_ok!(|p| ref_term(p, true), "::foo", qualified);
test_ok!(|p| ref_term(p, true), "\\foo", single_backslash);
test_ok!(|p| ref_term(p, true), "\\foo::bar", single_backslash_qualified);
pub(crate) fn ref_term(
    p: &mut Parser<'_>,
    allow_generics: bool,
) -> Option<(CompletedMarker, RefKind)> {
    let m = p.start();
    let scope_m = p.start();
    if namespaces::namespace_path(p).is_some() {
        scope_items::scope_name_ref(p);
        scope_m.complete(p, SCOPE_REF);
        if p.eat(T![::]) {
            let generics = names::name(p, true)
                .is_some_and(|generics_m| complete_generics(p, generics_m, allow_generics));

            Some((m.complete(p, REF_TERM), RefKind::NamespacedMember { generics }))
        } else {
            Some((m.complete(p, REF_TERM), RefKind::NamespacedScope))
        }
    } else {
        let first_name = names::name(p, false);
        if p.at(T![::]) {
            if let Some(generics) = first_name {
                complete_generics(p, generics, true);
                scope_m.complete(p, SCOPE_REF);
            } else {
                scope_m.abandon(p);
            };
            p.bump(T![::]);

            let generics = names::name(p, true)
                .is_some_and(|generics_m| complete_generics(p, generics_m, allow_generics));
            Some((m.complete(p, REF_TERM), RefKind::Member { generics }))
        } else {
            scope_m.abandon(p);
            match first_name {
                Some(generics) => {
                    let has_generics = generics.is_some();
                    complete_generics(p, generics, allow_generics);
                    Some((m.complete(p, REF_TERM), RefKind::Name { generics: has_generics }))
                }
                None => {
                    m.abandon(p);
                    None
                }
            }
        }
    }
}

fn complete_generics(p: &mut Parser<'_>, generics: Option<Marker>, allow_generics: bool) -> bool {
    if let Some(generics) = generics {
        if allow_generics {
            generics.complete(p, GENERICS);
            true
        } else {
            generics.complete(p, ERROR);
            p.error("generics are not allowed here");
            false
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_err, parse_ok};
    use expect_test::expect;

    #[test]
    fn named_term_ok_1() {
        parse_ok!(
            |p| ref_term(p, true),
            "a",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    IDENT "a"
            "#]]
        );
    }

    #[test]
    fn named_term_ok_2() {
        parse_ok!(
            |p| ref_term(p, true),
            "::a",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    COLONCOLON "::"
                    IDENT "a"
            "#]]
        );
    }

    #[test]
    fn named_term_ok_3() {
        parse_ok!(
            |p| ref_term(p, true),
            "a::b",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    SCOPE_REF
                      IDENT "a"
                    COLONCOLON "::"
                    IDENT "b"
            "#]]
        );
    }

    #[test]
    fn named_term_ok_4() {
        parse_ok!(
            |p| ref_term(p, true),
            "a\\b::c",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    SCOPE_REF
                      NAMESPACE_PATH
                        PATH_SEGMENT
                          IDENT "a"
                          BACKSLASH "\\"
                      IDENT "b"
                    COLONCOLON "::"
                    IDENT "c"
            "#]]
        );
    }

    #[test]
    fn named_term_ok_5() {
        parse_ok!(
            |p| ref_term(p, true),
            "a\\b\\c",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    SCOPE_REF
                      NAMESPACE_PATH
                        PATH_SEGMENT
                          IDENT "a"
                          BACKSLASH "\\"
                        PATH_SEGMENT
                          IDENT "b"
                          BACKSLASH "\\"
                      IDENT "c"
            "#]]
        );
    }

    #[test]
    fn named_term_err_1() {
        parse_err!(
            |p| ref_term(p, true),
            "::",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    COLONCOLON "::"
                error 2: expected IDENT
            "#]]
        );
    }

    #[test]
    fn named_term_err_2() {
        parse_err!(
            |p| ref_term(p, true),
            "a::",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    SCOPE_REF
                      IDENT "a"
                    COLONCOLON "::"
                error 3: expected IDENT
            "#]]
        );
    }

    #[test]
    fn named_term_err_3() {
        parse_err!(
            |p| ref_term(p, true),
            "a\\::b",
            expect![[r#"
                SOURCE_FILE
                  REF_TERM
                    SCOPE_REF
                      NAMESPACE_PATH
                        PATH_SEGMENT
                          IDENT "a"
                          BACKSLASH "\\"
                    COLONCOLON "::"
                    IDENT "b"
                error 2: expected IDENT
            "#]]
        );
    }
}
