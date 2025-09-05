use super::{AstNode, HasItems, HasScopeNameDecl};
use crate::{SourceFile, ast};

#[track_caller]
fn ast_from_text<N: AstNode>(text: &str) -> N {
    let parse = SourceFile::parse(text);
    let node = match parse.tree().syntax().descendants().find_map(N::cast) {
        Some(it) => it,
        None => {
            let node = std::any::type_name::<N>();
            panic!("Failed to make ast node `{node}` from text {text}")
        }
    };
    let node = node.clone_subtree();
    assert_eq!(node.syntax().text_range().start(), 0.into());
    node
}

pub fn scope_name_decl_in_namespace(
    namespace: &str,
    name: &str,
) -> (ast::Namespace, ast::ScopeNameDecl) {
    let source_file: ast::SourceFile =
        ast_from_text(&format!("namespace {namespace} class {} end class", name));

    let namespace = source_file
        .items_recursively()
        .find_map(|item| match item {
            ast::Item::NamespaceItem(namespace_item) => Some(namespace_item.namespace().unwrap()),
            _ => None,
        })
        .unwrap();
    let scope_name_decl = source_file
        .items_recursively()
        .find_map(|item| match item {
            ast::Item::ClassItem(class_item) => class_item.scope_name_decl(),
            _ => None,
        })
        .unwrap();
    (namespace, scope_name_decl)
}

pub fn scope_name_decl(name: &str) -> ast::ScopeNameDecl {
    ast_from_text(&format!("class {} end class", name))
}

/// Create a [`ast::StringSeq`] from a string.
/// The string should be enclosed in valid string delimiters (e.g. quotes).
/// There should not be any leading or trailing whitespace.
pub fn string_seq(text: &str) -> ast::StringSeq {
    assert_eq!(text.trim(), text);
    assert!(matches!(&text[..1], "\"" | "'" | "@"));
    ast_from_text(&format!("#include {text}"))
}

/// Create a [`ast::IncludeItem`] with a specified include path.
/// The path should not be enclosed in string delimiters (e.g. quotes).
pub fn include_item(path: &str) -> ast::IncludeItem {
    ast_from_text(&format!("#include @\"{path}\""))
}

/// Create a [`ast::RequiresDirective`] with a specified include path.
/// The path should not be enclosed in string delimiters (e.g. quotes).
pub fn requires_directive(path: &str) -> ast::RequiresDirective {
    ast_from_text(&format!("#requires @\"{path}\""))
}

/// Create a [`ast::BinInclude`] with a specified include path.
/// The path should not be enclosed in string delimiters (e.g. quotes).
pub fn bin_include(path: &str) -> ast::BinInclude {
    ast_from_text(&format!("class a constants b = #bininclude(@\"{path}\"). end class"))
}

/// Create a [`ast::StringInclude`] with a specified include path.
/// The path should not be enclosed in string delimiters (e.g. quotes).
pub fn string_include(path: &str) -> ast::StringInclude {
    ast_from_text(&format!("class a constants b = #stringinclude(@\"{path}\"). end class"))
}

#[test]
fn string_seq_smoke() {
    let seq = string_seq("\"foo\"");
    assert_eq!(seq.syntax.kind(), parser::SyntaxKind::STRING_SEQ);
    assert_eq!(seq.syntax.to_string(), "\"foo\"".to_owned());
}
