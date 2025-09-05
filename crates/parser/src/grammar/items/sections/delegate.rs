use super::*;
use crate::parser::Marker;

// test delegate
// implement bar
//     delegate
//         foo/1 to baz,
//         interface bar\foo{T} to baz
// end implement
pub(crate) fn delegate_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![delegate]);
    delegates(p, &m);
    m.complete(p, DELEGATE_SECTION)
}

const DELEGATE_START: TokenSet = TokenSet::new(&[IDENT, T![interface]]);

fn delegates(p: &mut Parser<'_>, _m: &Marker) {
    let mut any = false;
    let m = p.start();
    while p.at_ts(DELEGATE_START) {
        any = true;
        delegate(p);
        if p.at(T![,]) {
            if !DELEGATE_START.contains(p.nth(1)) {
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete_if(p, DELEGATES, any);
}

fn delegate(p: &mut Parser<'_>) -> CompletedMarker {
    assert!(p.at_ts(DELEGATE_START));
    match p.current() {
        IDENT => {
            let m = p.start();
            predicates::arity(p);
            to_fact(p);
            m.complete(p, PREDICATE_DELEGATE)
        }
        T![interface] => {
            let m = p.start();
            p.bump(T![interface]);
            if scope_items::interface_ref(p).is_none() {
                p.error("expected a interface name");
            }
            to_fact(p);
            m.complete(p, INTERFACE_DELEGATE)
        }
        _ => unreachable!("{:?}", p.current()),
    }
}

fn to_fact(p: &mut Parser<'_>) {
    if !p.eat_contextual_kw(T![to]) {
        p.error("expected 'to'");
    }
    if !p.eat(IDENT) {
        p.error("expected a fact name");
    }
}
