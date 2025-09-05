use super::*;

pub(crate) fn class_properties_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![class]);
    p.bump(T![properties]);
    property_list(p, CLASS_PROPERTY);
    m.complete(p, CLASS_PROPERTIES_SECTION)
}

// test property
// implement prop
//     properties
//         height : integer.
//         width : integer (i).
//         name : string (o).
// end implement
pub(crate) fn properties_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![properties]);
    property_list(p, PROPERTY);
    m.complete(p, PROPERTIES_SECTION)
}

pub(crate) fn properties_from_section(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.eat(T![class]);
    p.bump(T![properties]);
    predicates::arity_list(p, &m);
    m.complete(p, PROPERTIES_FROM_SECTION)
}

fn property_list(p: &mut Parser<'_>, kind: SyntaxKind) {
    while p.at(IDENT) {
        property(p, kind);
        if p.at(T![,]) {
            if !p.nth_at(1, IDENT) {
                p.err_and_bump("unexpected trailing comma");
                break;
            }
            p.bump(T![,]);
        }
    }
}

fn property(p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    p.expect(T![:]);
    if let Err(msg) = types::type_ref(p) {
        p.error(msg);
    }
    predicates::flow_opt(p);
    attributes::attributes_opt(p);
    p.expect(T![.]);
    m.complete(p, kind)
}
