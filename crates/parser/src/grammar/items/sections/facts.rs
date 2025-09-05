use super::*;
use crate::test_panic;
use items::terms::expressions;
use types::at_type;

pub(crate) fn class_facts(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![class]);
    p.bump(T![facts]);
    fact_db_name(p);
    fact_list(p, true);
    m.complete(p, CLASS_FACTS_SECTION)
}

// test facts
// implement hej
//     facts
//         a : integer.
//         b : string := "hej".
//         c : abe := erroneous.
//         d : (ticket Ticket) nondeterm.
// end implement
pub(crate) fn facts_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![facts]);
    fact_db_name(p);
    fact_list(p, false);
    m.complete(p, FACTS_SECTION)
}

fn fact_db_name(p: &mut Parser<'_>) {
    if p.eat(T![-]) && !p.eat(IDENT) {
        p.error("expected a fact db name");
    }
}

fn fact_list(p: &mut Parser<'_>, class: bool) {
    while p.at(IDENT) {
        fact(p, class);
    }
}

// test_err missing_fact_type
// implement a facts b : . end implement
test_panic!(|p| fact(p, false), "a :: b.", fact_var1);
test_panic!(|p| fact(p, false), "error1 :: boolean := false.", fact_var2);
fn fact(p: &mut Parser<'_>, class: bool) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    p.expect(T![:]);
    if let Some(kind) = fact_kind(p, class) {
        attributes::attributes_opt(p);
        p.expect(T![.]);
        m.complete(p, kind)
    } else {
        p.expect(T![.]);
        m.complete(p, if class { CLASS_FACT_VAR } else { FACT_VAR })
    }
}

fn fact_kind(p: &mut Parser<'_>, class: bool) -> Option<SyntaxKind> {
    if p.at(T!['(']) {
        params::formal_param_list(p);
        if matches!(p.current(), T![single] | T![determ] | T![nondeterm]) {
            p.bump_any()
        }
        Some(if class { CLASS_FACT_FUNCTOR } else { FACT_FUNCTOR })
    } else if at_type(p) {
        if types::type_ref(p).is_err() {
            p.error_until("expected a type fact-var type", |p| {
                p.at(T![.]) || p.at(T![:=]) || at_section(p) || at_item(p)
            });
        }
        if p.eat(T![:=]) && !p.eat(T![erroneous]) {
            expressions::expr(p);
        }
        Some(if class { CLASS_FACT_VAR } else { FACT_VAR })
    } else {
        p.error_until("expected a type or functor as fact type", |p| {
            p.at(T![.]) || at_section(p) || at_item(p)
        });

        None
    }
}
