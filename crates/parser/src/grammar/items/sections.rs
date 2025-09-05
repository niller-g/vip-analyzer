pub(crate) mod clauses;
mod constants;
mod delegate;
mod domains;
mod facts;
mod predicates;
mod properties;
mod resolve;

use super::*;
use items::terms::statements;

pub(crate) fn at_section(p: &Parser<'_>) -> bool {
    match p.current() {
        T![constants] => true,
        T![domains] => true,
        T![class] => matches!(p.nth(1), T![predicates] | T![properties] | T![facts]),
        T![clauses] => true,
        T![constructors] => true,
        T![facts] => true,
        T![predicates] => true,
        T![properties] => true,
        T![resolve] => true,
        T![delegate] => true,
        T![#if] => true,
        T![#error] => true,
        T![#message] => true,
        T![#requires] => true,
        _ => false,
    }
}

pub(crate) fn class_sections(p: &mut Parser<'_>) {
    sections(p, class_section, CLASS_SECTIONS);
}

pub(crate) fn implement_sections(p: &mut Parser<'_>) {
    sections(p, implement_section, IMPLEMENT_SECTIONS);
}

pub(crate) fn interface_sections(p: &mut Parser<'_>) {
    sections(p, interface_section, INTERFACE_SECTIONS);
}

fn sections(
    p: &mut Parser<'_>,
    section: fn(&mut Parser<'_>) -> Option<CompletedMarker>,
    section_kind: SyntaxKind,
) {
    let m = p.start();
    let mut any = false;
    while at_section(p) {
        if section(p).is_some() {
            any = true;
        }
    }
    m.complete_if(p, section_kind, any);
}

macro_rules! sections {
    (
        $p:expr,
        $class_preds:expr,
        $class_props:expr,
        $class_facts:expr,
        $clauses:expr,
        $constructors:expr,
        $facts:expr,
        $preds_from:expr,
        $preds:expr,
        $props_from:expr,
        $props:expr,
        $resolve:expr,
        $delegate:expr,
        $if:expr,
    ) => {
        assert!(at_section($p));
        match $p.current() {
            T![constants] => Some(constants::constants_section($p)),
            T![domains] => Some(domains::domains_section($p)),
            T![class] => match $p.nth(1) {
                T![predicates] => match $p.nth(2) {
                    T![from] => $preds_from,
                    _ => $class_preds,
                },
                T![properties] => match $p.nth(2) {
                    T![from] => $props_from,
                    _ => $class_props,
                },
                T![facts] => $class_facts,
                _ => {
                    $p.err_and_bump("expected a class section");
                    None
                }
            },
            T![clauses] => $clauses,
            T![constructors] => $constructors,
            T![facts] => $facts,
            T![predicates] => match $p.nth(1) {
                T![from] => $preds_from,
                _ => $preds,
            },
            T![properties] => match $p.nth(1) {
                T![from] => $props_from,
                _ => $props,
            },
            T![resolve] => $resolve,
            T![delegate] => $delegate,
            T![#if] => $if,
            T![#error] => Some(directives::error($p)),
            T![#message] => Some(directives::message($p)),
            T![#requires] => Some(directives::requires($p)),
            _ => unreachable!("{:?}", $p.current()),
        }
    };
}

fn class_section(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    sections! {p,
        disallow(p, predicates::class_predicates_section),
        disallow(p, properties::class_properties_section),
        disallow(p, facts::class_facts),
        disallow(p, clauses::clauses_section),
        Some(predicates::constructors_section(p)),
        disallow(p, facts::facts_section),
        disallow(p, predicates::predicates_from_section),
        Some(predicates::predicates_section(p)),
        disallow(p, properties::properties_from_section),
        Some(properties::properties_section(p)),
        disallow(p, resolve::resolve_section),
        disallow(p, delegate::delegate_section),
        Some(directives::if_directive(
            p,
            section_cond,
            class_sections,
            COND_CLASS_SECTION,
            ELSE_IF_CLASS_SECTION,
            ELSE_CLASS_SECTION,
        )),
    }
}

fn implement_section(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    sections! { p,
        Some(predicates::class_predicates_section(p)),
        Some(properties::class_properties_section(p)),
        Some(facts::class_facts(p)),
        Some(clauses::clauses_section(p)),
        Some(predicates::constructors_section(p)),
        Some(facts::facts_section(p)),
        disallow(p, predicates::predicates_from_section),
        Some(predicates::predicates_section(p)),
        disallow(p, properties::properties_from_section),
        Some(properties::properties_section(p)),
        Some(resolve::resolve_section(p)),
        Some(delegate::delegate_section(p)),
        Some(directives::if_directive(
            p,
            section_cond,
            implement_sections,
            COND_IMPLEMENT_SECTION,
            ELSE_IF_IMPLEMENT_SECTION,
            ELSE_IMPLEMENT_SECTION,
        )),
    }
}

fn interface_section(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    sections! {p,
        disallow(p, predicates::class_predicates_section),
        disallow(p, properties::class_properties_section),
        disallow(p, facts::class_facts),
        disallow(p, clauses::clauses_section),
        disallow(p, predicates::constructors_section),
        disallow(p, facts::facts_section),
        Some(predicates::predicates_from_section(p)),
        Some(predicates::predicates_section(p)),
        Some(properties::properties_from_section(p)),
        Some(properties::properties_section(p)),
        disallow(p, resolve::resolve_section),
        disallow(p, delegate::delegate_section),
        Some(directives::if_directive(
            p,
            section_cond,
            interface_sections,
            COND_INTERFACE_SECTION,
            ELSE_IF_INTERFACE_SECTION,
            ELSE_INTERFACE_SECTION,
        )),
    }
}

fn disallow(
    p: &mut Parser<'_>,
    section: fn(&mut Parser<'_>) -> CompletedMarker,
) -> Option<CompletedMarker> {
    p.error(format!("section not allowed here, found {:?}", p.current()));
    let m = p.start();
    section(p);
    m.complete(p, ERROR);
    None
}

fn section_cond(p: &mut Parser<'_>) {
    if statements::stmt(p).is_none() {
        p.error("expected a condition");
    }
}
