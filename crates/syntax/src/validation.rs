//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

use crate::{
    SyntaxError, SyntaxNode,
    ast::{self, AstNode},
    match_ast,
};
use parser::T;
use rowan::{Direction, TextRange};

pub(crate) fn validate(root: &SyntaxNode, errors: &mut Vec<SyntaxError>) {
    let _p = tracing::info_span!("parser::validate").entered();

    for node in root.descendants() {
        match_ast! {
            match node {
                ast::Constructor(it) => validate_constructor(it, errors),
                ast::Property(it) => validate_property(it, errors),
                ast::PredPropertyList(it) => validate_pred_property_list(it, errors),
                ast::PredicateDomain(it) => validate_predicate_domain(it, errors),
                ast::FormalArgList(it) => validate_formal_arg_list(it, errors),
                _ => {},
            }
        }
    }
}

fn validate_constructor(constructor: ast::Constructor, errors: &mut Vec<SyntaxError>) {
    if let Some(ref_term) = constructor.syntax.children().find_map(ast::RefTerm::cast) {
        errors.push(SyntaxError::new(
            "constructor parameters must be explicitly typed",
            ref_term.syntax().text_range(),
        ));
    }
}

fn validate_property(prop: ast::Property, errors: &mut Vec<SyntaxError>) {
    if let Some(flow) = prop.flow() {
        if let Some(flow_arg_list) = flow.flow_arg_list() {
            for flow_arg in flow_arg_list.flow_args() {
                if !matches!(flow_arg, ast::FlowArg::FlowDirection(_)) {
                    errors.push(SyntaxError::new(
                        "flow of properties must be `i` or `o`",
                        flow_arg.syntax().text_range(),
                    ));
                }
            }
        }
    }
}

fn validate_predicate_domain(
    predicate_domain: ast::PredicateDomain,
    errors: &mut Vec<SyntaxError>,
) {
    if predicate_domain.mode().is_none() {
        flow_pattern_mode(predicate_domain.syntax(), predicate_domain.flow_pattern_list(), errors);
    }
}

fn validate_pred_property_list(
    pred_property_list: ast::PredPropertyList,
    errors: &mut Vec<SyntaxError>,
) {
    let pred_props: Vec<_> = pred_property_list.pred_properties().collect();
    if pred_props.len() > 1 {
        for pred_prop in pred_props.into_iter().filter(|it| it.mode().is_none()) {
            flow_pattern_mode(pred_prop.syntax(), pred_prop.flow_pattern_list(), errors);
        }
    }
}

fn flow_pattern_mode(
    node: &SyntaxNode,
    flow_pattern_list: Option<ast::FlowPatternList>,
    errors: &mut Vec<SyntaxError>,
) {
    if let Some(flow_pat_list) = flow_pattern_list {
        if flow_pat_list.flow_patterns().any(|it| matches!(it, ast::FlowPattern::Flow(_))) {
            errors.push(SyntaxError::new(
                "flow patterns must be preceded by a mode",
                node.text_range(),
            ));
        }
    }
}

fn validate_formal_arg_list(formal_arg_list: ast::FormalArgList, errors: &mut Vec<SyntaxError>) {
    let syntax = formal_arg_list.syntax();
    let trailing_comma = syntax.last_token().filter(|last_token| last_token.kind() == T![,]);
    let ellipsis = syntax.siblings_with_tokens(Direction::Next).find_map(|it| match it {
        rowan::NodeOrToken::Token(tok) => (tok.kind() == T![...]).then_some(tok),
        _ => None,
    });
    match (trailing_comma, ellipsis) {
        (Some(tok), None) => {
            errors.push(SyntaxError::new("unexpected trailing ','", tok.text_range()));
        }
        (None, Some(ellipsis)) => {
            errors.push(SyntaxError::new(
                "expected ','",
                TextRange::new(syntax.text_range().end(), ellipsis.text_range().start()),
            ));
        }
        _ => {}
    }
}
