use super::*;
use crate::{parser::CompletedMarker, test_ok, test_panic};
use namespaces::nth_at_namespace;
use types::at_type_bounds;

// test class_item
// class a end class
// monitor class a end class
// monitor class a end monitor class
pub(crate) fn class_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.eat(T![monitor]);
    p.bump(T![class]);
    scope_name_decl(p);
    if p.eat(T![:]) {
        // test class_item_constructor
        // class a : obj
        // end class

        if interface_ref(p).is_none() {
            let m = p.start();
            let mut any = false;
            while !at_type_bounds(p)
                && !p.at_ts(SCOPE_QUALIFICATION_START)
                && !at_item(p)
                && !p.at(EOF)
            {
                p.bump_any();
                any = true;
            }
            m.complete_if(p, ERROR, any);
        }
    }
    types::type_bounds(p);
    qualifications(p);
    attributes::attributes_opt(p);
    sections::class_sections(p);
    // test_err class_item_no_end
    // class a

    if !end_class_item(p) {
        p.error_until(format!("expected `end class`, found {:?}", p.current()), at_item);
        end_class_item(p);
    }
    m.complete(p, CLASS_ITEM);
}

fn end_class_item(p: &mut Parser<'_>) -> bool {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.eat(T![monitor]);
        if p.eat(T![class]) {
            end_scope_name_opt(p);
        } else {
            p.error_until(format!("expected `end class`, found {:?}", p.current()), at_item);
        }
        m.complete(p, END_CLASS_ITEM);
        true
    } else {
        false
    }
}

fn end_scope_name_opt(p: &mut Parser<'_>) {
    if p.at(IDENT) {
        // test class_item_tailing_name
        // class a end class a

        // test implement_item_tailing_name
        // implement a end implement a

        // test interface_item_tailing_name
        // interface a end interface a
        scope_name_decl(p);
    }
}

// test implement_item
// implement a end implement
pub(crate) fn implement_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![implement]);
    scope_name_decl(p);
    types::type_bounds(p);
    qualifications(p);
    attributes::attributes_opt(p);
    sections::implement_sections(p);

    if !end_implement_item(p) {
        p.error_until(format!("expected `end implement`, found {:?}", p.current()), at_item);
        end_implement_item(p);
    }
    m.complete(p, IMPLEMENT_ITEM);
}

fn end_implement_item(p: &mut Parser<'_>) -> bool {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        if p.eat(T![implement]) {
            end_scope_name_opt(p);
        } else {
            p.error_until(format!("expected `end implement`, found {:?}", p.current()), at_item);
        }
        m.complete(p, END_IMPLEMENT_ITEM);
        true
    } else {
        false
    }
}

// test interface_item
// interface a end interface
// monitor interface a end interface
// monitor interface a end monitor interface
pub(crate) fn interface_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.eat(T![monitor]);
    p.bump(T![interface]);
    scope_name_decl(p);
    types::type_bounds(p);
    qualifications(p);
    attributes::attributes_opt(p);
    sections::interface_sections(p);

    if !end_interface_item(p) {
        p.error_until(format!("expected `end interface`, found {:?}", p.current()), at_item);
        end_interface_item(p);
    }
    m.complete(p, INTERFACE_ITEM);
}

fn end_interface_item(p: &mut Parser<'_>) -> bool {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.eat(T![monitor]);
        if p.eat(T![interface]) {
            end_scope_name_opt(p);
        } else {
            p.error_until(format!("expected `end interface`, found {:?}", p.current()), at_item);
        }
        m.complete(p, END_INTERFACE_ITEM);
        true
    } else {
        false
    }
}

test_panic!(scope_name_decl, "a{T}", scope_name3);
test_panic!(scope_name_decl, "a{,}", scope_name4);
fn scope_name_decl(p: &mut Parser<'_>) -> CompletedMarker {
    // test scope_name
    // implement a end implement
    // implement a{} end implement
    // implement a{@T} end implement
    // implement a{@ABE, @Fisk} end implement
    let m = p.start();
    p.expect(IDENT);
    if p.at(T!['{']) {
        p.bump(T!['{']);
        let m = p.start();
        while !p.at(EOF) && !p.at(T!['}']) {
            if let Err(msg) = scope_type(p) {
                // test_err invalid_scope_param
                // implement a{V, string} end implement
                // implement a{,} end implement
                p.err_and_bump(msg);
            }
            if p.at(T![,]) {
                if p.nth(1) == T!['}'] {
                    // test_err scope_name_tailing_comma
                    // implement a{@T,} end implement
                    p.err_and_bump("unexpected trailing comma");
                    break;
                }
                p.bump(T![,]);
            }
        }
        m.complete(p, SCOPE_PARAMS);
        p.expect(T!['}']);
    }
    m.complete(p, SCOPE_NAME_DECL)
}

pub(crate) fn scope_name_ref(p: &mut Parser<'_>) -> bool {
    scope_name_ref_opt(p, true)
}

pub(crate) fn scope_name_ref_opt(p: &mut Parser<'_>, expect_something: bool) -> bool {
    if let Some(generics) = names::name(p, expect_something) {
        generics.map(|g| g.complete(p, GENERICS));
        true
    } else {
        false
    }
}

pub(crate) fn scope_type<'a>(p: &mut Parser<'_>) -> Result<CompletedMarker, &'a str> {
    let m = p.start();
    if p.eat(T![@]) {
        if !p.eat(VAR) {
            p.error("expected a variable name after '@'");
        }
        Ok(m.complete(p, SCOPE_TYPE))
    } else {
        m.abandon(p);
        Err("expected a scope-type")
    }
}

#[inline]
pub(crate) fn at_scope_ref(p: &Parser<'_>) -> bool {
    nth_at_scope_ref(p, 0)
}
pub(crate) fn nth_at_scope_ref(p: &Parser<'_>, n: usize) -> bool {
    match p.nth(n) {
        IDENT => true,
        _ => nth_at_namespace(p, n),
    }
}

test_ok!(interface_ref, "a", interface_ref1);
test_ok!(interface_ref, "a\\b", interface_ref2);
test_panic!(interface_ref, "a\\b::c", interface_ref3);
fn scope_ref(p: &mut Parser<'_>, subject: &str) -> Option<CompletedMarker> {
    // test scope_ref
    // class a : a\scopeRef end class

    // test_err scope_ref_qualified
    // class a : a\scopeRef::qual end class
    let m = p.start();
    let namespace = namespaces::namespace_path(p);
    let scope_name = scope_name_ref(p);
    if namespace.is_some() || scope_name {
        Some(m.complete(p, SCOPE_REF))
    } else {
        // test_err missing_scope_ref
        // class a :
        // end class
        m.abandon(p);
        p.error(format!("expected {} reference", subject));
        None
    }
}
#[inline]
pub(crate) fn interface_ref(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    scope_ref(p, "interface")
}
#[inline]
pub(crate) fn class_ref(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    scope_ref(p, "class")
}

pub(crate) const SCOPE_QUALIFICATION_START: TokenSet =
    TokenSet::new(&[T![supports], T![open], T![inherits]]);

pub(crate) fn qualifications(p: &mut Parser<'_>) {
    match p.current() {
        T![supports] | T![open] | T![inherits] => {
            while matches!(p.current(), T![supports] | T![open] | T![inherits]) {
                qualification(p);
            }
        }
        _ => {}
    }
}

fn qualification(p: &mut Parser<'_>) {
    debug_assert!(matches!(p.current(), T![supports] | T![open] | T![inherits]));
    match p.current() {
        T![supports] => supports_qualification(p),
        T![open] => open_qualification(p),
        T![inherits] => inherits_qualification(p),
        _ => unreachable!("{:?}", p.current()),
    }
}

fn supports_qualification(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![supports]);
    while at_scope_ref(p) {
        interface_ref(p);
        if p.at(T![,]) {
            if !nth_at_scope_ref(p, 1) {
                p.err_and_bump("expected a scope reference after ','");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete(p, SUPPORTS_QUALIFICATIONS);
}

#[inline]
fn at_opening(p: &Parser<'_>) -> bool {
    nth_at_opening(p, 0)
}
fn nth_at_opening(p: &Parser<'_>, n: usize) -> bool {
    nth_at_scope_ref(p, n) || nth_at_namespace(p, n)
}

fn open_qualification(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![open]);
    while at_opening(p) {
        scope_or_namespace_ref(p);
        if p.at(T![,]) {
            if !nth_at_opening(p, 1) {
                p.err_and_bump("expected a scope reference after ','");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete(p, OPEN_QUALIFICATIONS);
}

fn inherits_qualification(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![inherits]);
    while at_scope_ref(p) {
        class_ref(p);
        if p.at(T![,]) {
            if !nth_at_scope_ref(p, 1) {
                p.err_and_bump("expected a scope reference after ','");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete(p, INHERITS_QUALIFICATIONS);
}

// test openings
// interface a open core, pfc\log\, \ end interface
fn scope_or_namespace_ref(p: &mut Parser<'_>) {
    let m = p.start();
    match (namespaces::namespace_path(p), scope_name_ref_opt(p, false)) {
        (Some(_), true) | (None, true) => {
            m.complete(p, SCOPE_REF);
        }
        (Some(_), false) => {
            m.abandon(p);
        }
        (None, false) => {
            m.abandon(p);
            p.error("expected a scope reference or namespace");
        }
    }
}
