use super::*;
use expressions::exprs;

// test clauses
// implement hello
//     clauses
//         hello().
//         world() = true.
//         stalker() :- fail.
//         look() = true :- succeed.
// end implement
pub(crate) fn clauses_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![clauses]);
    clauses(p);
    m.complete(p, CLAUSES_SECTION)
}

pub(crate) fn clauses(p: &mut Parser<'_>) {
    while p.at(IDENT) {
        clause(p);
    }
}

fn clause(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    if p.eat(T!['(']) {
        exprs(p, T![')'].into());
        p.expect(T![')']);
    } else if p.eat(T![guard]) {
        // test clause_guard
        // implement ohai
        //     clauses
        //         hello guard {  :- count = 0 }.
        // end implement
        expressions::expr(p);
        p.expect(T![.]);
        return m.complete(p, GUARD_CLAUSE);
    }
    if p.eat(T![=]) {
        expressions::expr(p);
    }
    clause_body(p);
    if !p.eat(T![.]) {
        p.error_until(format!("unexpected clause ending, found {:?}", p.current()), |p| {
            p.at(T![.]) || at_section(p) || at_item(p)
        });
        p.eat(T![.]);
    }
    m.complete(p, CLAUSE)
}

pub(crate) fn clause_body(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.at(T![:-]) {
        let m = p.start();
        p.bump(T![:-]);
        statements::stmt(p);
        Some(m.complete(p, CLAUSE_BODY))
    } else {
        None
    }
}
