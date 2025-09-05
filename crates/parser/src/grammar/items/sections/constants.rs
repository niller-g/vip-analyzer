use super::*;
use items::terms::expressions;

pub(crate) fn constants_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![constants]);
    while p.at(IDENT) {
        constant(p);
    }
    m.complete(p, CONSTANTS_SECTION)
}

fn constant(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    // test constant_implicit_type
    // class a constants a = 1. end class
    match p.current() {
        T![:] => {
            p.bump(T![:]);
            // test constant_with_type
            // class a constants a : integer = 1. end class
            if let Err(msg) = types::type_ref(p) {
                // test_err constant_without_type
                // class a constants a :  = 1. end class
                p.error(msg);
            }
            if p.eat(T![=]) {
                if expressions::expr(p).is_none() {
                    p.error("expected an expression");
                }
                attributes::attributes_opt(p);
            } else {
                p.error_until(format!("expected a constant value, found {:?}", p.current()), |p| {
                    p.at(T![.]) || at_section(p) || at_item(p)
                });
            }
        }
        T![=] => {
            p.bump(T![=]);
            if expressions::expr(p).is_none() {
                p.error("expected an expression");
            }
            attributes::attributes_opt(p);
        }
        kind => {
            p.error_until(format!("expected type or value, found {:?}", kind), |p| {
                p.at_ts([T![=], T![.]]) || at_section(p) || at_item(p)
            });
        }
    }

    p.expect(T![.]);
    m.complete(p, CONSTANT)
}
