use super::*;
use crate::parser::Marker;

// test name_refs
// class a
//    open a, b{}, c{d A}, d{@T, f B}
// end class
#[must_use]
pub(crate) fn name(p: &mut Parser<'_>, expect_something: bool) -> Option<Option<Marker>> {
    let any = if expect_something { p.expect(IDENT) } else { p.eat(IDENT) };
    let mut generics = None;
    if any && p.at(T!['{']) {
        generics = Some({
            let m = p.start();
            p.bump(T!['{']);
            types::type_args(p);
            p.expect(T!['}']);
            m
        });
    }
    if any { Some(generics) } else { None }
}
