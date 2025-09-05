mod ok {
    #[cfg(test)]
    use crate::tests::*;
    #[test]
    fn class_item() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_item.pro");
    }
    #[test]
    fn class_item_constructor() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_item_constructor.pro");
    }
    #[test]
    fn class_item_tailing_name() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_item_tailing_name.pro");
    }
    #[test]
    fn class_predicates() {
        run_and_expect_no_errors("test_data/parser/inline/ok/class_predicates.pro");
    }
    #[test]
    fn clause_guard() {
        run_and_expect_no_errors("test_data/parser/inline/ok/clause_guard.pro");
    }
    #[test]
    fn clauses() {
        run_and_expect_no_errors("test_data/parser/inline/ok/clauses.pro");
    }
    #[test]
    fn cond_item_export() {
        run_and_expect_no_errors("test_data/parser/inline/ok/cond_item_export.pro");
    }
    #[test]
    fn conditional_item() {
        run_and_expect_no_errors("test_data/parser/inline/ok/conditional_item.pro");
    }
    #[test]
    fn cons_expr() {
        run_and_expect_no_errors("test_data/parser/inline/ok/cons_expr.pro");
    }
    #[test]
    fn constant_implicit_type() {
        run_and_expect_no_errors("test_data/parser/inline/ok/constant_implicit_type.pro");
    }
    #[test]
    fn constant_with_type() {
        run_and_expect_no_errors("test_data/parser/inline/ok/constant_with_type.pro");
    }
    #[test]
    fn delegate() {
        run_and_expect_no_errors("test_data/parser/inline/ok/delegate.pro");
    }
    #[test]
    fn domain_def_align() {
        run_and_expect_no_errors("test_data/parser/inline/ok/domain_def_align.pro");
    }
    #[test]
    fn domain_def_fn() {
        run_and_expect_no_errors("test_data/parser/inline/ok/domain_def_fn.pro");
    }
    #[test]
    fn domain_pred_calling_convention() {
        run_and_expect_no_errors("test_data/parser/inline/ok/domain_pred_calling_convention.pro");
    }
    #[test]
    fn domain_pred_modes() {
        run_and_expect_no_errors("test_data/parser/inline/ok/domain_pred_modes.pro");
    }
    #[test]
    fn domains() {
        run_and_expect_no_errors("test_data/parser/inline/ok/domains.pro");
    }
    #[test]
    fn facts() {
        run_and_expect_no_errors("test_data/parser/inline/ok/facts.pro");
    }
    #[test]
    fn functor_original() {
        run_and_expect_no_errors("test_data/parser/inline/ok/functor_original.pro");
    }
    #[test]
    fn goal() {
        run_and_expect_no_errors("test_data/parser/inline/ok/goal.pro");
    }
    #[test]
    fn implement_attributes() {
        run_and_expect_no_errors("test_data/parser/inline/ok/implement_attributes.pro");
    }
    #[test]
    fn implement_item() {
        run_and_expect_no_errors("test_data/parser/inline/ok/implement_item.pro");
    }
    #[test]
    fn implement_item_tailing_name() {
        run_and_expect_no_errors("test_data/parser/inline/ok/implement_item_tailing_name.pro");
    }
    #[test]
    fn interface_item() {
        run_and_expect_no_errors("test_data/parser/inline/ok/interface_item.pro");
    }
    #[test]
    fn interface_item_tailing_name() {
        run_and_expect_no_errors("test_data/parser/inline/ok/interface_item_tailing_name.pro");
    }
    #[test]
    fn keyword_arg() {
        run_and_expect_no_errors("test_data/parser/inline/ok/keyword_arg.pro");
    }
    #[test]
    fn list_expr() {
        run_and_expect_no_errors("test_data/parser/inline/ok/list_expr.pro");
    }
    #[test]
    fn list_expr_empty() {
        run_and_expect_no_errors("test_data/parser/inline/ok/list_expr_empty.pro");
    }
    #[test]
    fn name_refs() {
        run_and_expect_no_errors("test_data/parser/inline/ok/name_refs.pro");
    }
    #[test]
    fn namespaces() {
        run_and_expect_no_errors("test_data/parser/inline/ok/namespaces.pro");
    }
    #[test]
    fn openings() {
        run_and_expect_no_errors("test_data/parser/inline/ok/openings.pro");
    }
    #[test]
    fn predicates() {
        run_and_expect_no_errors("test_data/parser/inline/ok/predicates.pro");
    }
    #[test]
    fn predicates_from() {
        run_and_expect_no_errors("test_data/parser/inline/ok/predicates_from.pro");
    }
    #[test]
    fn predicates_from_empty() {
        run_and_expect_no_errors("test_data/parser/inline/ok/predicates_from_empty.pro");
    }
    #[test]
    fn property() {
        run_and_expect_no_errors("test_data/parser/inline/ok/property.pro");
    }
    #[test]
    fn resolve() {
        run_and_expect_no_errors("test_data/parser/inline/ok/resolve.pro");
    }
    #[test]
    fn scope_name() {
        run_and_expect_no_errors("test_data/parser/inline/ok/scope_name.pro");
    }
    #[test]
    fn scope_ref() {
        run_and_expect_no_errors("test_data/parser/inline/ok/scope_ref.pro");
    }
    #[test]
    fn type_bound_scope_param() {
        run_and_expect_no_errors("test_data/parser/inline/ok/type_bound_scope_param.pro");
    }
    #[test]
    fn type_bound_var() {
        run_and_expect_no_errors("test_data/parser/inline/ok/type_bound_var.pro");
    }
    #[test]
    fn type_bounds() {
        run_and_expect_no_errors("test_data/parser/inline/ok/type_bounds.pro");
    }
    #[test]
    fn value_like_apply() {
        run_and_expect_no_errors("test_data/parser/inline/ok/value_like_apply.pro");
    }
}
mod err {
    #[cfg(test)]
    use crate::tests::*;
    #[test]
    fn class_item_no_end() {
        run_and_expect_errors("test_data/parser/inline/err/class_item_no_end.pro");
    }
    #[test]
    fn cons_missing_expr() {
        run_and_expect_errors("test_data/parser/inline/err/cons_missing_expr.pro");
    }
    #[test]
    fn cons_missing_expr_after() {
        run_and_expect_errors("test_data/parser/inline/err/cons_missing_expr_after.pro");
    }
    #[test]
    fn cons_missing_expr_before() {
        run_and_expect_errors("test_data/parser/inline/err/cons_missing_expr_before.pro");
    }
    #[test]
    fn constant_without_type() {
        run_and_expect_errors("test_data/parser/inline/err/constant_without_type.pro");
    }
    #[test]
    fn findall_missing_expr_after() {
        run_and_expect_errors("test_data/parser/inline/err/findall_missing_expr_after.pro");
    }
    #[test]
    fn findall_missing_expr_before() {
        run_and_expect_errors("test_data/parser/inline/err/findall_missing_expr_before.pro");
    }
    #[test]
    fn flow_args_invalid_arg() {
        run_and_expect_errors("test_data/parser/inline/err/flow_args_invalid_arg.pro");
    }
    #[test]
    fn flow_args_trailing_comma() {
        run_and_expect_errors("test_data/parser/inline/err/flow_args_trailing_comma.pro");
    }
    #[test]
    fn formal_params_invalid() {
        run_and_expect_errors("test_data/parser/inline/err/formal_params_invalid.pro");
    }
    #[test]
    fn invalid_scope_param() {
        run_and_expect_errors("test_data/parser/inline/err/invalid_scope_param.pro");
    }
    #[test]
    fn missing_fact_type() {
        run_and_expect_errors("test_data/parser/inline/err/missing_fact_type.pro");
    }
    #[test]
    fn missing_scope_ref() {
        run_and_expect_errors("test_data/parser/inline/err/missing_scope_ref.pro");
    }
    #[test]
    fn name_refs_trailing_comma() {
        run_and_expect_errors("test_data/parser/inline/err/name_refs_trailing_comma.pro");
    }
    #[test]
    fn no_namespace() {
        run_and_expect_errors("test_data/parser/inline/err/no_namespace.pro");
    }
    #[test]
    fn ranged_var_domain() {
        run_and_expect_errors("test_data/parser/inline/err/ranged_var_domain.pro");
    }
    #[test]
    fn scope_name_tailing_comma() {
        run_and_expect_errors("test_data/parser/inline/err/scope_name_tailing_comma.pro");
    }
    #[test]
    fn scope_ref_qualified() {
        run_and_expect_errors("test_data/parser/inline/err/scope_ref_qualified.pro");
    }
    #[test]
    fn type_bound_missing() {
        run_and_expect_errors("test_data/parser/inline/err/type_bound_missing.pro");
    }
    #[test]
    fn value_like_no_apply() {
        run_and_expect_errors("test_data/parser/inline/err/value_like_no_apply.pro");
    }
}
