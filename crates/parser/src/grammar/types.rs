use self::{
    items::scope_items::{self, scope_type},
    terms::at_ref_term,
};
use super::{
    terms::{RefKind, ref_term},
    *,
};
use crate::parser::CompletedMarker;

pub(crate) fn at_type(p: &Parser<'_>) -> bool {
    match p.current() {
        IDENT => true,
        VAR => true,
        T![@] => p.nth_at(1, VAR),
        T![:] => p.nth_at(1, T![:]) && p.nth_at(2, IDENT),
        T!['\\'] => p.nth_at(1, IDENT),
        _ => false,
    }
}

pub(crate) enum TypeKind {
    Var,
    RefTerm(RefKind),
    ScopeType,
    List,
}

pub(crate) fn type_ref(p: &mut Parser<'_>) -> Result<TypeKind, &'static str> {
    let m = p.start();
    let (lhs, kind) = match p.current() {
        VAR => (var_type(p), TypeKind::Var),
        T![@] => (scope_type(p)?, TypeKind::ScopeType),
        _ if at_ref_term(p) => match ref_term(p, true) {
            Some((m, kind)) => (m, TypeKind::RefTerm(kind)),
            None => {
                m.abandon(p);
                return Err("expected type reference");
            }
        },
        _ => {
            m.abandon(p);
            return Err("expected type reference");
        }
    };
    let kind = postfix_list_type(p, lhs).unwrap_or(kind);

    // This extra node is created because a valid `TypeKind` is a `RefTerm`. Since `TypeKind` is an enum, it does not create a new node.
    // Without the extra node, a `RefTerm` could be a child node of some parent node.
    // However, that node could be be using `RefTerm` for something else and it would not be possible to distinguish between them.
    m.complete(p, TYPE);

    Ok(kind)
}

fn postfix_list_type(p: &mut Parser<'_>, mut lhs: CompletedMarker) -> Option<TypeKind> {
    let mut any = false;
    while p.at(T![*]) {
        any = true;
        lhs = lhs.precede(p).complete(p, TYPE);
        let m = lhs.precede(p);
        p.bump(T![*]);
        lhs = m.complete(p, LIST_TYPE);
    }
    if any { Some(TypeKind::List) } else { None }
}

fn var_type(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(VAR);
    m.complete(p, VAR_TYPE)
}

pub(crate) fn type_arg<'a>(p: &mut Parser<'a>) -> Result<CompletedMarker, &'a str> {
    let m = p.start();
    if let Err(msg) = type_ref(p) {
        m.abandon(p);
        return Err(msg);
    }
    p.eat(VAR);
    Ok(m.complete(p, TYPE_ARG))
}

pub(crate) fn type_args(p: &mut Parser<'_>) {
    while !p.at(EOF) && !p.at(T!['}']) {
        if let Err(msg) = type_arg(p) {
            p.error_until(msg, |p| p.at_ts([T![,], T!['}']]))
        }
        if p.at(T![,]) {
            if p.nth(1) == T!['}'] {
                // test_err name_refs_trailing_comma
                // class a
                //    open a{,}, b{c, d,}
                // end class
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
}

#[inline]
pub(crate) fn at_type_bounds(p: &Parser<'_>) -> bool {
    p.at(T![where])
}

pub(crate) fn type_bounds(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !at_type_bounds(p) {
        return None;
    }
    let m = p.start();
    while at_type_bounds(p) {
        // test type_bounds
        // class a
        //     where V supports @T
        //     where @V supports T
        // end class
        type_bound(p);
    }
    Some(m.complete(p, TYPE_BOUNDS))
}

fn type_bound(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![where]);
    match p.current() {
        VAR => {
            // test type_bound_var
            // class a
            //     where V supports T
            // end class
            p.bump(VAR);
        }
        T![@] => {
            // test type_bound_scope_param
            // class a
            //     where @V supports T
            // end class
            if let Err(msg) = scope_items::scope_type(p) {
                p.error(msg);
            }
        }
        t => {
            // test_err type_bound_missing
            // class a
            //     where
            // end class
            p.error(format!(
                "expected variable or scope-param as subject in type bound, found {:?}",
                t
            ));
        }
    }
    p.expect(T![supports]);
    if let Err(msg) = types::type_ref(p) {
        p.error(msg);
    }
    m.complete(p, TYPE_BOUND)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_ok;
    use expect_test::expect;

    #[test]
    fn type_ref_ok_0() {
        parse_ok!(
            type_ref,
            "V",
            expect![[r#"
                SOURCE_FILE
                  TYPE
                    VAR_TYPE
                      VAR "V"
            "#]]
        );
    }

    #[test]
    fn type_ref_ok_1() {
        parse_ok!(
            type_ref,
            "_",
            expect![[r#"
                SOURCE_FILE
                  TYPE
                    VAR_TYPE
                      VAR "_"
            "#]]
        );
    }

    #[test]
    fn type_ref_ok_2() {
        parse_ok!(
            type_ref,
            "@TYPE*",
            expect![[r#"
                SOURCE_FILE
                  TYPE
                    LIST_TYPE
                      TYPE
                        SCOPE_TYPE
                          AT "@"
                          VAR "TYPE"
                      STAR "*"
            "#]]
        );
    }

    #[test]
    fn type_ref_ok_3() {
        parse_ok!(
            type_ref,
            "a",
            expect![[r#"
                SOURCE_FILE
                  TYPE
                    REF_TERM
                      IDENT "a"
            "#]]
        );
    }

    #[test]
    fn type_ref_ok_4() {
        parse_ok!(
            type_ref,
            "a**",
            expect![[r#"
                SOURCE_FILE
                  TYPE
                    LIST_TYPE
                      TYPE
                        LIST_TYPE
                          TYPE
                            REF_TERM
                              IDENT "a"
                          STAR "*"
                      STAR "*"
            "#]]
        );
    }

    #[test]
    fn type_ref_ok_5() {
        parse_ok!(
            type_ref,
            "",
            expect![[r#"
                SOURCE_FILE
            "#]]
        );
    }
}
