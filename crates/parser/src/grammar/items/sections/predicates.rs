use self::items::terms::expressions;
use super::*;
use crate::{grammar::terms::ref_term, parser::Marker};
use terms::at_ref_term;

pub(crate) fn constructors_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![constructors]);
    predicate_list(p, CONSTRUCTOR);
    m.complete(p, CONSTRUCTORS_SECTION)
}

// test class_predicates
// implement fisk class predicates end implement
pub(crate) fn class_predicates_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![class]);
    p.bump(T![predicates]);
    predicate_list(p, CLASS_PREDICATE);
    m.complete(p, CLASS_PREDICATES_SECTION)
}

// test predicates
// implement fisk predicates end implement
// implement fisk
//     predicates
//         tryGet : (string Name) -> val Value determ.
//         onShow : window::showListener.
// end implement
pub(crate) fn predicates_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![predicates]);
    predicate_list(p, PREDICATE);
    m.complete(p, PREDICATES_SECTION)
}

fn predicate_list(p: &mut Parser<'_>, kind: SyntaxKind) {
    while p.at(IDENT) {
        predicate(p, kind);
    }
}

fn predicate(p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    p.expect(T![:]);
    pred_params(p);
    calling_convention_opt(p);
    link_name_opt(p);
    types::type_bounds(p);
    attributes::attributes_opt(p);
    if !p.eat(T![.]) {
        p.error_until(format!("expected `.`, found {:?}", p.current()), |p| {
            p.at(T![.]) || at_section(p) || at_item(p)
        });
        p.eat(T![.]);
    }
    m.complete(p, kind)
}

fn pred_params(p: &mut Parser<'_>) {
    match p.current() {
        T!['('] => {
            let m = p.start();
            p.bump(T!['(']);
            params::formal_arg_list(p, |p| p.at(T![')']) || p.at(T![...]));
            p.eat(T![...]);
            p.expect(T![')']);

            return_param_opt(p);
            pred_properties_opt(p);
            m.complete(p, INLINE_PARAMS);
        }
        _ if at_ref_term(p) => {
            if ref_term(p, true).is_none() {
                p.error("expected predicate declaration type");
            }
        }
        _ => {
            p.error("expected predicate declaration type");
            let m = p.start();
            while !p.at(EOF) && !p.at(T![.]) && at_section(p) {
                p.bump_any();
            }
            m.complete(p, ERROR);
        }
    }
}

const PRED_PROPERTY_START: TokenSet =
    TokenSet::new(&[T![suspending]]).union(MODE_TOKEN_SET).union(FLOW_PATTERN_START);

fn pred_properties_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let list_m = p.start();
    let mut any = false;
    while p.at_ts(PRED_PROPERTY_START) {
        let m = p.start();
        p.eat(T![suspending]);
        mode_opt(p);
        flow_pattern_list(p);
        m.complete(p, PRED_PROPERTY);
        any = true;
    }
    list_m.complete_if(p, PRED_PROPERTY_LIST, any)
}

pub(crate) fn return_param_opt(p: &mut Parser<'_>) {
    if p.at(T![->]) {
        let m = p.start();
        p.bump(T![->]);
        _ = types::type_arg(p).map_err(|e| p.error(e));
        m.complete(p, RETURN_PARAM);
    }
}

const MODE_TOKEN_SET: TokenSet = TokenSet::new(&[
    T![procedure],
    T![erroneous],
    T![failure],
    T![determ],
    T![nondeterm],
    T![multi],
]);

// test domain_pred_modes
// class a
//     domains
//         b = () procedure.
//         b = () erroneous.
//         b = () failure.
//         b = () nondeterm.
//         b = () multi.
// end class
pub(crate) fn mode_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.at_ts(MODE_TOKEN_SET) {
        let m = p.start();
        p.bump_any();
        Some(m.complete(p, MODE))
    } else {
        None
    }
}

pub(crate) fn flow_pattern_list(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();
    let mut any = false;
    while p.at_ts(FLOW_PATTERN_START) {
        flow_pattern(p);
        any = true;
    }

    m.complete_if(p, FLOW_PATTERN_LIST, any)
}

pub(crate) const FLOW_PATTERN_START: TokenSet = TokenSet::new(&[T!['('], T![anyflow]]);

pub(crate) fn flow_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.at_ts(FLOW_PATTERN_START) { Some(flow(p)) } else { None }
}

fn flow_pattern(p: &mut Parser<'_>) {
    assert!(p.at_ts(FLOW_PATTERN_START));
    match p.current() {
        T!['('] => {
            flow(p);
        }
        T![anyflow] => {
            let m = p.start();
            p.bump(T![anyflow]);
            m.complete(p, ANY_FLOW);
        }
        _ => unreachable!("{:?}", p.current()),
    }
}

fn flow(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['(']);
    flow_arg_list(p, true);
    p.expect(T![')']);
    m.complete(p, FLOW)
}

const FLOW_ARG_START: TokenSet = TokenSet::new(&[IDENT, T!['[']]);

fn flow_arg_list(p: &mut Parser<'_>, allow_ellipsis: bool) {
    #[inline]
    fn at_flow_arg_start(p: &mut Parser<'_>, allow_ellipsis: bool) -> bool {
        p.at_ts(FLOW_ARG_START) || (allow_ellipsis && p.at(T![...]))
    }

    let m = p.start();
    let mut any = false;
    while at_flow_arg_start(p, allow_ellipsis) {
        any = true;
        flow_arg(p, allow_ellipsis);
        if p.eat(T![,]) {
            if p.at(T![')']) {
                // test_err flow_args_trailing_comma
                // class a domains b = () (d,). end class
                p.error("unexpected trailing comma");
                break;
            }

            if !at_flow_arg_start(p, allow_ellipsis) {
                // test_err flow_args_invalid_arg
                // class a domains b = () (d, ..). end class
                p.error(format!("expected flow argument, found {:?}", p.current()));
                let error_m = p.start();
                while !at_flow_arg_start(p, allow_ellipsis)
                    && !p.at_ts(TokenSet::new(&[T![,], T![')'], EOF]))
                {
                    p.bump_any();
                }
                error_m.complete(p, ERROR);
            }
        } else {
            break;
        }
    }

    m.complete_if(p, FLOW_ARG_LIST, any);
}

fn flow_arg(p: &mut Parser<'_>, allow_ellipsis: bool) {
    match p.current() {
        IDENT => {
            let m = p.start();
            if p.at_contextual_kw(T![i]) && !p.nth_at(1, T!['(']) {
                p.eat_contextual_kw(T![i]);
                m.complete(p, FLOW_DIRECTION);
            } else if p.at_contextual_kw(T![o]) && !p.nth_at(1, T!['(']) {
                p.eat_contextual_kw(T![o]);
                m.complete(p, FLOW_DIRECTION);
            } else {
                p.bump(IDENT);
                if p.at(T!['(']) {
                    flow(p);
                } else {
                    p.error("missing flow arguments of functor");
                }
                m.complete(p, FUNCTOR_FLOW);
            }
        }
        T!['['] => list_flow(p),
        T![..] if p.at(T![...]) && allow_ellipsis => {
            let m = p.start();
            p.bump(T![...]);
            m.complete(p, ELLIPSIS);
        }
        _ => unreachable!("{:?}", p.current()),
    }
}

fn list_flow(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T!['[']);
    flow_arg_list(p, false);
    if p.eat(T![|]) {
        match p.current() {
            IDENT => {
                p.bump(IDENT);
            }
            T!['['] => {
                list_flow(p);
            }
            _ => {
                p.error("expected list flow tail");
            }
        }
    }
    p.expect(T![']']);
    m.complete(p, LIST_FLOW);
}

// test domain_pred_calling_convention
// class a
//     domains
//         b = () language c.
// end class
pub(crate) fn calling_convention_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !p.at_contextual_kw(T![language]) {
        return None;
    }
    let m = p.start();
    p.bump_remap(T![language]);
    p.expect(IDENT);
    Some(m.complete(p, CALLING_CONVENTION))
}

fn link_name_opt(p: &mut Parser<'_>) {
    if p.at(T![as]) {
        let m = p.start();
        p.bump(T![as]);
        expressions::string_or_const(p);
        p.eat(IDENT);
        m.complete(p, LINK_NAME);
    }
}

// test predicates_from_empty
// interface bar
//     class predicates from lib
// end interface

// test predicates_from
// interface bar
//     predicates from lib
//         a, b/, c/1, d/1..., e/1->
// end interface
pub(crate) fn predicates_from_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.eat(T![class]);
    p.bump(T![predicates]);
    arity_list(p, &m);
    m.complete(p, PREDICATES_FROM_SECTION)
}

pub(crate) fn arity_list(p: &mut Parser<'_>, _m: &Marker) {
    p.bump(T![from]);
    scope_items::interface_ref(p);
    let mut any = false;
    let m = p.start();
    while p.at(IDENT) {
        any = true;
        arity(p);
        if p.at(T![,]) {
            if !p.nth_at(1, IDENT) {
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete_if(p, ARITY_LIST, any);
}

pub(crate) fn arity(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    if p.eat(T![/]) {
        p.eat(INT_NUMBER);
        p.eat(T![...]);
        p.eat(T![->]);
    }
    m.complete(p, ARITY)
}
