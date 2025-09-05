use super::*;
use crate::grammar::{
    attributes::attribute_list,
    params::formal_arg_list,
    terms::RefKind,
    types::{TypeKind, type_ref},
};
use items::{attributes::attributes_opt, terms::expressions};
use types::at_type;

// test domains
// class a domains end class
// class a
//     domains
//         dom = something.
//         dom2 = core::string.
//         dom3 = bar().
//         dom{A} = bar(); foo(A); baz.
// end class
pub(crate) fn domains_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![domains]);
    while p.at(IDENT) {
        domain(p);
    }
    m.complete(p, DOMAINS_SECTION)
}

// test_err ranged_var_domain
// class a
// domains
//     dom008{T} = T [1..2].
// end class
fn domain(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    if let Some(generics) = names::name(p, true) {
        generics.map(|g| g.complete(p, GENERICS));
    }
    p.expect(T![=]);
    domain_def(p);
    if !p.at(T![.]) && !p.at(EOF) {
        p.error_until("expected `.` to end domain definition", |p| {
            p.at(T![.]) || at_section(p) || at_item(p)
        });
    }
    p.expect(T![.]);
    m.complete(p, DOMAIN)
}

fn domain_def(p: &mut Parser<'_>) {
    match p.current() {
        _ if at_type(p) => {
            let m = p.start();
            let type_ref = match type_ref(p) {
                Ok(t) => t,
                Err(_) => {
                    m.abandon(p);
                    return;
                }
            };
            match type_ref {
                TypeKind::RefTerm(RefKind::Name { generics: false }) => match p.current() {
                    T![bitsize] => {
                        bitsize_domain(p);
                        m.complete(p, NUMERIC_DOMAIN);
                    }
                    T![digits] => {
                        digits_domain(p);
                        m.complete(p, NUMERIC_DOMAIN);
                    }
                    T!['['] => {
                        let range_dom = p.start();
                        match range_or_attributes(p) {
                            Some(RangeOrAttribute::RangeExpr) => {
                                range_domain_continued(p);
                                range_dom.complete(p, RANGE_DOMAIN);
                                m.complete(p, NUMERIC_DOMAIN);
                            }
                            Some(RangeOrAttribute::Attributes) => {
                                range_dom.abandon(p);
                                if p.at(T![;]) {
                                    let m = m.complete(p, FUNCTOR).precede(p);
                                    functors_tail(p);
                                    m.complete(p, FUNCTOR_DOMAIN);
                                } else {
                                    m.complete(p, ALIAS_DOMAIN);
                                }
                            }
                            None => {
                                range_dom.abandon(p);
                                m.complete(p, ALIAS_DOMAIN);
                            }
                        }
                    }
                    T!['('] => {
                        paren_formal_arg_list(p);
                        attributes::attributes_opt(p);
                        let m = m.complete(p, FUNCTOR).precede(p);
                        functors_tail(p);
                        m.complete(p, FUNCTOR_DOMAIN);
                    }
                    _ => {
                        if p.at(T![;]) {
                            let m = m.complete(p, FUNCTOR).precede(p);
                            functors_tail(p);
                            m.complete(p, FUNCTOR_DOMAIN);
                        } else {
                            m.complete(p, ALIAS_DOMAIN);
                        }
                    }
                },
                TypeKind::RefTerm(RefKind::Member { generics: false })
                | TypeKind::RefTerm(RefKind::NamespacedMember { generics: false }) => {
                    match p.current() {
                        T![bitsize] => {
                            bitsize_domain(p);
                            m.complete(p, NUMERIC_DOMAIN);
                        }
                        T![digits] => {
                            digits_domain(p);
                            m.complete(p, NUMERIC_DOMAIN);
                        }
                        T!['['] => {
                            let range_dom = p.start();
                            match range_or_attributes(p) {
                                Some(RangeOrAttribute::RangeExpr) => {
                                    range_domain_continued(p);
                                    range_dom.complete(p, RANGE_DOMAIN);
                                    m.complete(p, NUMERIC_DOMAIN);
                                }
                                Some(RangeOrAttribute::Attributes) | None => {
                                    range_dom.abandon(p);
                                    m.complete(p, ALIAS_DOMAIN);
                                }
                            }
                        }
                        _ => {
                            m.complete(p, ALIAS_DOMAIN);
                        }
                    }
                }
                TypeKind::List
                | TypeKind::ScopeType
                | TypeKind::Var
                | TypeKind::RefTerm(RefKind::Name { generics: true })
                | TypeKind::RefTerm(RefKind::Member { generics: true })
                | TypeKind::RefTerm(RefKind::NamespacedMember { generics: true })
                | TypeKind::RefTerm(RefKind::NamespacedScope) => match p.current() {
                    T![bitsize] => {
                        let error_m = p.start();
                        bitsize_domain(p);
                        error_m.complete(p, ERROR);
                        m.complete(p, ALIAS_DOMAIN);
                    }
                    T![digits] => {
                        let error_m = p.start();
                        digits_domain(p);
                        error_m.complete(p, ERROR);
                        m.complete(p, ALIAS_DOMAIN);
                    }
                    T!['['] => {
                        attribute_list(p);
                        m.complete(p, ALIAS_DOMAIN);
                    }
                    _ => {
                        m.complete(p, ALIAS_DOMAIN);
                    }
                },
            }
        }
        T![align] => {
            // test domain_def_align
            // class a
            //     domains
            //         b = align 1 bar() [used].
            // end class
            let m = p.start();
            p.bump(T![align]);
            if expressions::expr(p).is_none() {
                p.error("expected expression after `align`");
            }
            if !functors(p) {
                p.error("expected functor variants after `align`");
            }
            m.complete(p, FUNCTOR_DOMAIN);
        }
        T![bitsize] => {
            let m = p.start();
            bitsize_domain(p);
            m.complete(p, NUMERIC_DOMAIN);
        }
        T![digits] => {
            let m = p.start();
            digits_domain(p);
            m.complete(p, NUMERIC_DOMAIN);
        }
        T!['['] => {
            let m = p.start();
            range_domain(p);
            m.complete(p, NUMERIC_DOMAIN);
        }
        T!['('] => {
            // test domain_def_fn
            // class a
            //     domains
            //         b = (string _).
            //         c = () -> string suspending.
            // end class
            let m = p.start();
            params::formal_param_list(p);
            predicates::return_param_opt(p);
            p.eat(T![suspending]);
            predicates::mode_opt(p);
            predicates::flow_pattern_list(p);
            predicates::calling_convention_opt(p);
            attributes::attributes_opt(p);
            m.complete(p, PREDICATE_DOMAIN);
        }
        kind => {
            p.error(format!("expected domain definition, found {:?}", kind));
        }
    }
}

fn functors_tail(p: &mut Parser<'_>) {
    if p.eat(T![;]) {
        (!functors(p)).then(|| p.error("expected functor variants after `;`"));
    }
}

fn functors(p: &mut Parser<'_>) -> bool {
    fn functor(p: &mut Parser<'_>) -> bool {
        let m = p.start();
        if type_ref(p).is_err() {
            m.abandon(p);
            return false;
        }
        if p.at(T!['(']) {
            paren_formal_arg_list(p);
        }
        attributes::attributes_opt(p);
        m.complete(p, FUNCTOR);
        true
    }

    let mut any = false;
    while p.at(IDENT) {
        functor(p);
        any = true;
        if !p.eat(T![;]) {
            break;
        }
    }
    any
}

fn paren_formal_arg_list(p: &mut Parser<'_>) {
    p.bump(T!['(']);
    formal_arg_list(p, |p| p.at(T![')']));
    p.expect(T![')']);
}

enum RangeOrAttribute {
    RangeExpr,
    Attributes,
}
fn range_or_attributes(p: &mut Parser<'_>) -> Option<RangeOrAttribute> {
    let m = p.start();
    p.bump(T!['[']);
    match p.current() {
        T![..] => {
            p.bump(T![..]);
            if expressions::expr(p).is_none() {
                p.error("expected expression after `..`");
            }
            p.expect(T![']']);
            m.complete(p, RANGE_EXPR);
            Some(RangeOrAttribute::RangeExpr)
        }
        T![']'] => {
            p.error("expected range or attributes");
            p.bump(T![']']);
            m.complete(p, ERROR);
            None
        }
        _ => {
            let start_attr = p.start();
            if expressions::expr(p).is_some() {
                if p.at(T![..]) {
                    start_attr.abandon(p);
                    p.bump(T![..]);
                    expressions::expr(p);
                    p.expect(T![']']);
                    m.complete(p, RANGE_EXPR);
                    Some(RangeOrAttribute::RangeExpr)
                } else {
                    expressions::exprs_raw(p, T![']'].into());
                    start_attr.complete(p, EXPRS);
                    if p.at(T![']']) {
                        p.bump(T![']']);
                    } else {
                        p.error_until("expected ']' to end attributes", |p| {
                            p.at_ts([T![']'], T![.]]) || at_section(p) || at_item(p)
                        });
                        p.eat(T![']']);
                    }
                    m.complete(p, ATTRIBUTE_LIST);
                    Some(RangeOrAttribute::Attributes)
                }
            } else {
                start_attr.abandon(p);
                m.abandon(p);
                p.err_and_bump("expected expression or term after `[`");
                None
            }
        }
    }
}

fn range_domain(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    range_expr(p);
    range_domain_continued(p);
    m.complete(p, RANGE_DOMAIN)
}
fn range_domain_continued(p: &mut Parser<'_>) {
    match p.current() {
        T![bitsize] => bitsize_expr(p),
        T![digits] => digits_expr(p),
        _ => {}
    }
    attributes_opt(p);
}

fn range_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['[']);
    expressions::expr(p);
    p.expect(T![..]);
    expressions::expr(p);
    p.expect(T![']']);
    m.complete(p, RANGE_EXPR)
}

fn bitsize_domain(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    bitsize_expr(p);
    range_or_attributes_opt(p);
    m.complete(p, BITSIZE_DOMAIN)
}
fn bitsize_expr(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![bitsize]);
    if expressions::expr(p).is_none() {
        p.error("expected expression after `bitsize`");
    }
    m.complete(p, BITSIZE_EXPR);
}

fn range_or_attributes_opt(p: &mut Parser<'_>) {
    if p.at(T!['[']) {
        match range_or_attributes(p) {
            Some(RangeOrAttribute::Attributes) => {}
            Some(RangeOrAttribute::RangeExpr) | None => {
                attributes_opt(p);
            }
        }
    }
}

fn digits_domain(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    digits_expr(p);
    range_or_attributes_opt(p);
    m.complete(p, DIGITS_DOMAIN)
}
fn digits_expr(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![digits]);
    if expressions::expr(p).is_none() {
        p.error("expected expression after `digits`");
    }
    m.complete(p, DIGITS_EXPR);
}
