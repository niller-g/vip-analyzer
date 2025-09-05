use super::*;
use crate::parser::CompletedMarker;
use {
    self::terms::expressions,
    super::items::{at_item, sections::at_section},
};

pub(crate) fn attributes_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !p.at(T!['[']) {
        return None;
    }
    Some(attribute_list(p))
}

// test implement_attributes
// implement a [] end implement
// implement b [c] end implement
// implement d [e\f::g, h("")] end implement
pub(crate) fn attribute_list(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['[']);
    expressions::exprs(p, T![']'].into());
    if p.at(T![']']) {
        p.bump(T![']']);
    } else {
        p.error_until("expected ']' to end attributes", |p| {
            p.at_ts([T![']'], T![.]]) || at_section(p) || at_item(p)
        });
        p.eat(T![']']);
    }
    m.complete(p, ATTRIBUTE_LIST)
}
