use super::*;
use scope_items::{at_scope_ref, nth_at_scope_ref};

pub(crate) fn requires(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![#requires]);
    if expressions::string_opt(p).is_none() {
        p.error("expected a string");
    }
    or_requires(p);
    m.complete(p, REQUIRES_DIRECTIVE)
}

fn or_requires(p: &mut Parser<'_>) {
    while p.at(T![#orrequires]) {
        let m = p.start();
        p.bump(T![#orrequires]);
        if expressions::string_opt(p).is_none() {
            p.error("expected a string");
        }
        m.complete(p, OR_REQUIRES_DIRECTIVE);
    }
}

pub(crate) fn message(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![#message]);
    if expressions::string_opt(p).is_none() {
        p.error("expected a string");
    }
    m.complete(p, MESSAGE_DIRECTIVE)
}

pub(crate) fn error(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![#error]);
    if expressions::string_opt(p).is_none() {
        p.error("expected a string");
    }
    m.complete(p, ERROR_DIRECTIVE)
}

pub(crate) fn export_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![#export]);
    let mut any = false;
    while at_scope_ref(p) {
        any = true;
        scope_items::class_ref(p).map_or_else(|| p.error("expected a scope name"), |_| {});
        if p.at(T![,]) {
            if !nth_at_scope_ref(p, 1) {
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
    (!any).then(|| p.error("expected a scope name"));
    m.complete(p, EXPORT_ITEM);
}

pub(crate) fn externally_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![#externally]);
    while at_scope_ref(p) {
        let m = p.start();
        if scope_items::class_ref(p).is_none() {
            p.error("expected a scope name");
        }
        if p.eat(T![#from]) {
            expressions::string_or_const(p);
        }
        m.complete(p, EXTERNALLY_SUBJECT);
        if p.at(T![,]) {
            if !nth_at_scope_ref(p, 1) {
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
    m.complete(p, EXTERNALLY_ITEM);
}

pub(crate) fn options_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![#options]);
    if expressions::string_opt(p).is_none() {
        p.error("expected a string");
    }
    m.complete(p, OPTIONS_ITEM);
}

// test conditional_item
// #if 32 = platform_bits #then
// #endif
pub(crate) fn if_directive(
    p: &mut Parser<'_>,
    condition: fn(&mut Parser<'_>),
    body: fn(&mut Parser<'_>),
    kind: SyntaxKind,
    else_if_kind: SyntaxKind,
    if_kind: SyntaxKind,
) -> CompletedMarker {
    let m = p.start();
    p.bump(T![#if]);
    condition(p);
    p.expect(T![#then]);
    body(p);
    else_if_directive(p, condition, body, else_if_kind);
    if p.at(T![#else]) {
        let m = p.start();
        p.bump(T![#else]);
        body(p);
        m.complete(p, if_kind);
    }
    p.expect(T![#endif]);
    m.complete(p, kind)
}

fn else_if_directive(
    p: &mut Parser<'_>,
    condition: fn(&mut Parser<'_>),
    body: fn(&mut Parser<'_>),
    kind: SyntaxKind,
) {
    while p.at(T![#elseif]) {
        let m = p.start();
        p.bump(T![#elseif]);
        condition(p);
        p.expect(T![#then]);
        body(p);
        m.complete(p, kind);
    }
}
