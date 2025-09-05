use self::predicates::arity;
use super::*;
use crate::{grammar::terms::ref_term, test_ok};
use items::terms::expressions;

// test resolve
// implement baz
//     resolve
//         bar from baz,
//         baz externally,
//         baz externally from foo,
//         interface bar from hoo
// end implement
pub(crate) fn resolve_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![resolve]);
    resolutions(p);
    m.complete(p, RESOLVE_SECTION)
}

const RESOLVE_START: TokenSet = TokenSet::new(&[IDENT, T![interface]]);

fn resolutions(p: &mut Parser<'_>) {
    let mut any = false;
    let m = p.start();
    while p.at_ts(RESOLVE_START) {
        any = true;
        resolution(p);
        if p.at(T![,]) {
            if !RESOLVE_START.contains(p.nth(1)) {
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete_if(p, RESOLUTIONS, any);
}

test_ok!(resolution, "size from outputStreamSegmentedSupport{binary}::size", pred_rename);
fn resolution(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at_ts(RESOLVE_START));
    match p.current() {
        IDENT => {
            let m = p.start();
            arity(p);
            match p.current() {
                T![from] => {
                    p.bump(T![from]);
                    if ref_term(p, true).is_none() {
                        p.error("expected a scope or predicate name");
                    }
                    m.complete(p, PREDICATE_RESOLUTION)
                }
                T![externally] => {
                    p.bump(T![externally]);
                    if p.eat(T![from]) {
                        expressions::string_or_const(p);
                    }
                    m.complete(p, EXTERNAL_PREDICATE_RESOLUTION)
                }
                _ => {
                    p.error("expected 'from' or 'externally'");
                    while !p.at(EOF) && !p.at(T![,]) && !at_section(p) {
                        p.bump_any();
                    }
                    m.complete(p, ERROR)
                }
            }
        }
        T![interface] => {
            let m = p.start();
            p.bump(T![interface]);
            let interface_m = p.start();
            match scope_items::interface_ref(p) {
                Some(_) => {
                    interface_m.complete(p, INTERFACE_REF);
                }
                None => {
                    interface_m.abandon(p);
                    p.error("expected an interface name");
                }
            }
            p.expect(T![from]);
            if scope_items::class_ref(p).is_none() {
                p.error("expected a class name");
            }
            m.complete(p, INTERFACE_RESOLUTION)
        }
        _ => unreachable!("{:?}", p.current()),
    }
}
