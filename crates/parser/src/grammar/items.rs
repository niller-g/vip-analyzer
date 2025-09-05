mod directives;
pub(crate) mod scope_items;
pub(crate) mod sections;

use self::terms::statements;
use super::*;
use crate::parser::CompletedMarker;
use items::terms::expressions;

pub(crate) fn at_item_start(p: &Parser<'_>) -> bool {
    const ITEM_START: TokenSet = TokenSet::new(&[
        T![class],
        T![implement],
        T![interface],
        T![namespace],
        T![#if],
        T![#include],
        T![#requires],
        T![#message],
        T![#error],
        T![#export],
        T![#externally],
        T![#options],
        T![goal],
    ]);

    if p.at_ts(ITEM_START) {
        true
    } else if p.at(T![monitor]) {
        matches!(p.nth(1), T![class] | T![interface])
    } else {
        false
    }
}

pub(crate) fn at_item_end(p: &Parser<'_>) -> bool {
    if !p.at(T![end]) {
        return false;
    }
    match p.nth(1) {
        T![class] | T![implement] | T![interface] => true,
        T![monitor] => matches!(p.nth(2), T![class] | T![interface]),
        _ => false,
    }
}

pub(crate) fn at_item(p: &Parser<'_>) -> bool {
    at_item_start(p) || at_item_end(p)
}

pub(crate) fn items_opt(p: &mut Parser<'_>) {
    while at_item_start(p) {
        item(p);
    }
}

fn item(p: &mut Parser<'_>) {
    debug_assert!(at_item_start(p));
    match p.current() {
        T![class] => scope_items::class_item(p),
        T![implement] => scope_items::implement_item(p),
        T![interface] => scope_items::interface_item(p),
        T![monitor] => match p.nth(1) {
            T![class] => scope_items::class_item(p),
            T![interface] => scope_items::interface_item(p),
            _ => {
                p.err_and_bump("expected a class or interface");
            }
        },
        T![namespace] => namespace_item(p),
        T![#if] => {
            directives::if_directive(p, item_cond, items_opt, COND_ITEM, ELSE_IF_ITEM, ELSE_ITEM);
        }
        T![#include] => include_item(p),
        T![#requires] => {
            directives::requires(p);
        }
        T![#message] => {
            directives::message(p);
        }
        T![#error] => {
            directives::error(p);
        }
        T![#export] => directives::export_item(p),
        T![#externally] => directives::externally_item(p),
        T![#options] => directives::options_item(p),
        T![goal] => goal_item(p),
        _ => unreachable!("{:?}", p.current()),
    }
}

fn include_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![#include]);
    if expressions::string_opt(p).is_none() {
        p.error("expected a string");
    }
    m.complete(p, INCLUDE_ITEM);
}

// test namespaces
// namespace \
// namespace a
// namespace \a
// namespace a\b
// namespace \a\b
// namespace a\b\c
// namespace \a\b\c
fn namespace_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.expect(T![namespace]);
    namespaces::namespace(p);
    m.complete(p, NAMESPACE_ITEM);
}

fn item_cond(p: &mut Parser<'_>) {
    match p.current() {
        T![#export] => {
            // test cond_item_export
            // #if #export someClass #then
            // #endif
            directives::export_item(p);
        }
        _ => {
            // #if true = test #then
            // #endif
            if statements::stmt(p).is_none() {
                p.error("expected a conditional statement");
            }
        }
    }
}

// test goal
// goal mainExe::run(main::run).
fn goal_item(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(T![goal]);
    statements::stmt(p);
    p.expect(T![.]);
    m.complete(p, GOAL_ITEM);
}
