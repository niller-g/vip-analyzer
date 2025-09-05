use super::*;
use crate::{parser::CompletedMarker, test_ok, test_panic};

#[inline]
pub(crate) fn at_namespace(p: &Parser<'_>) -> bool {
    nth_at_namespace(p, 0)
}

pub(crate) fn nth_at_namespace(p: &Parser<'_>, n: usize) -> bool {
    match p.nth(n) {
        IDENT => p.nth_at(n + 1, T!['\\']),
        T!['\\'] => true,
        _ => false,
    }
}

test_panic!(namespace, "foo\\", trailing_backslash_1);
test_panic!(namespace, "bar\\foo\\", trailing_backslash_2);
test_ok!(namespace, "\\", single_backslash);
test_ok!(namespace, "\\bar", backslash_ident);
test_ok!(namespace, "foo", normal_namespace_1);
test_ok!(namespace, "foo\\bar", normal_namespace_2);
pub(crate) fn namespace(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();

    if let Some((_, kind)) = namespace_path(p) {
        match kind {
            NamespacePathKind::Backslash => {
                p.eat(IDENT);
                Some(m.complete(p, NAMESPACE))
            }
            NamespacePathKind::Normal => {
                p.expect(IDENT);
                Some(m.complete(p, NAMESPACE))
            }
        }
    } else if p.eat(IDENT) {
        Some(m.complete(p, NAMESPACE))
    } else {
        // test_err no_namespace
        // namespace
        m.abandon(p);
        p.error("expected a namespace");
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NamespacePathKind {
    /// A namespace path only consisting of a single backslash.
    Backslash,
    /// A normal namespace path.
    Normal,
}

test_panic!(namespace_path, "foo", ns_paths_1);
test_panic!(namespace_path, "foo\\bar", ns_paths_2);
test_ok!(namespace_path, "foo\\bar\\", ns_paths_3);
pub(crate) fn namespace_path(p: &mut Parser<'_>) -> Option<(CompletedMarker, NamespacePathKind)> {
    let m = p.start();
    let mut any = p.eat(T!['\\']);
    let mut kind = if any { NamespacePathKind::Backslash } else { NamespacePathKind::Normal };
    while path_segment(p).is_some() {
        any = true;
        kind = NamespacePathKind::Normal;
    }
    if any {
        Some((m.complete(p, NAMESPACE_PATH), kind))
    } else {
        m.abandon(p);
        None
    }
}

fn path_segment(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.at(IDENT) && p.nth_at(1, T!['\\']) {
        let m = p.start();
        p.bump(IDENT);
        p.bump(T!['\\']);
        Some(m.complete(p, PATH_SEGMENT))
    } else {
        None
    }
}
