use super::{
    AstNode, AstToken,
    operators::{ExprOp, PrefixOp, RelationOp, StmtOp},
    support,
};
use crate::{SyntaxToken, ast};
use parser::T;

impl ast::ImplementItem {
    pub fn inherits_qualifications(&self) -> impl Iterator<Item = ast::ScopeRef> + '_ {
        self.qualifications()
            .filter_map(|qual| match qual {
                ast::ImplementQualification::InheritsQualifications(qual) => Some(qual),
                ast::ImplementQualification::SupportsQualifications(_)
                | ast::ImplementQualification::OpenQualifications(_) => None,
            })
            .flat_map(|qual| qual.scope_refs())
    }
}

impl ast::Clause {
    pub fn clause_args(&self) -> impl Iterator<Item = ast::Expr> + use<> {
        self.exprs().into_iter().flat_map(|e| e.exprs())
    }
    pub fn body_stmt(&self) -> Option<ast::Stmt> {
        self.clause_body().and_then(|b| b.stmt())
    }
}

impl ast::ApplyTerm {
    pub fn args(&self) -> impl Iterator<Item = ast::FunctorArg> {
        self.functor_args().into_iter().flat_map(|a| a.functor_args())
    }
}

impl ast::ForeachStmt {
    pub fn iteration_stmt(&self) -> Option<ast::Stmt> {
        self.iter_stmt().and_then(|i| i.stmt())
    }
}

impl ast::IfTerm {
    pub fn cond_stmt(&self) -> Option<ast::Stmt> {
        self.condition().and_then(|c| c.stmt())
    }
}
impl ast::ElseIfTerm {
    pub fn cond_stmt(&self) -> Option<ast::Stmt> {
        self.condition().and_then(|c| c.stmt())
    }
}

impl ast::MatchStmt {
    pub fn args(&self) -> impl Iterator<Item = ast::ApplyArg> {
        self.apply_args().into_iter().flat_map(|a| a.apply_args())
    }
}

impl ast::ConsExpr {
    pub fn head_exprs(&self) -> impl Iterator<Item = ast::Expr> {
        self.exprs().into_iter().flat_map(|e| e.exprs())
    }
}

impl ast::LambdaExpr {
    pub fn args(&self) -> impl Iterator<Item = ast::Expr> {
        self.exprs().into_iter().flat_map(|e| e.exprs())
    }
}

impl ast::ListExpr {
    pub fn elements(&self) -> impl Iterator<Item = ast::Expr> {
        self.exprs().into_iter().flat_map(|e| e.exprs())
    }
}

impl ast::Namespace {
    pub fn namespace_value(&self) -> Option<String> {
        let path = self.namespace_path().map(|p| p.to_string());
        let ident = self.ident_token();
        match (path, ident) {
            (None, None) => None,
            (Some(path), None) => Some(path),
            (None, Some(ident)) => Some(ident.to_string()),
            (Some(path), Some(ident)) => Some(path + &ident.to_string()),
        }
    }
}
impl ast::ScopeParams {
    /// Returns the number of scope parameters.
    pub fn count(&self) -> usize {
        self.scope_types().count()
    }
}

impl ast::StringSeq {
    pub fn is_path_directive(&self) -> bool {
        let Some(parent) = self.syntax().parent() else {
            return false;
        };
        let kind = parent.kind();

        ast::IncludeItem::can_cast(kind)
            || ast::RequiresDirective::can_cast(kind)
            || ast::BinInclude::can_cast(kind)
            || ast::StringInclude::can_cast(kind)
    }

    pub fn value(&self) -> Option<String> {
        let mut res = String::new();
        for s in self.string_parts().filter_map(|s| s.string_token()).filter_map(ast::String::cast)
        {
            if let Some(s) = s.value() {
                res.push_str(&s);
            } else {
                return None;
            }
        }
        Some(res)
    }
}

impl ast::RangeExpr {
    #[inline]
    pub fn start(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }
    #[inline]
    pub fn end(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

pub enum AssignRhs {
    Expr(ast::Expr),
    Erroneous(ast::SyntaxToken),
}

impl ast::AssignStmt {
    #[inline]
    pub fn lhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }
    #[inline]
    pub fn rhs(&self) -> Option<AssignRhs> {
        self.erroneous_token().map(AssignRhs::Erroneous).or_else(|| {
            self.syntax().children().filter_map(ast::Expr::cast).nth(1).map(AssignRhs::Expr)
        })
    }
    #[inline]
    fn erroneous_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![erroneous])
    }
}

impl ast::RelationStmt {
    #[inline]
    pub fn lhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }
    #[inline]
    pub fn rhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn op(&self) -> Option<RelationOp> {
        self.syntax.children_with_tokens().filter_map(|it| it.into_token()).find_map(|token| {
            match token.kind() {
                T![<] => Some(RelationOp::Lt),
                T![<=] => Some(RelationOp::Le),
                T![>] => Some(RelationOp::Gt),
                T![>=] => Some(RelationOp::Ge),
                T![<>] => Some(RelationOp::Ne),
                T![><] => Some(RelationOp::NeAlt),
                T![==] => Some(RelationOp::MustUnify),
                T![=] => Some(RelationOp::Eq),
                _ => None,
            }
        })
    }
}

impl ast::InStmt {
    #[inline]
    pub fn lhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }
    #[inline]
    pub fn rhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

impl ast::BinExpr {
    #[inline]
    pub fn lhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).next()
    }
    #[inline]
    pub fn rhs(&self) -> Option<ast::Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn op(&self) -> Option<ExprOp> {
        self.syntax.children_with_tokens().filter_map(|it| it.into_token()).find_map(|token| {
            match token.kind() {
                T![^^] => Some(ExprOp::BitXor),
                T![**] => Some(ExprOp::BitAnd),
                T![++] => Some(ExprOp::BitOr),
                T![--] => Some(ExprOp::BitClear),
                T![^] => Some(ExprOp::Pow),
                T![*] => Some(ExprOp::Mul),
                T![/] => Some(ExprOp::DivReal),
                T![div] => Some(ExprOp::DivInt),
                T![mod] => Some(ExprOp::Mod),
                T![rem] => Some(ExprOp::Rem),
                T![quot] => Some(ExprOp::Quot),
                T![+] => Some(ExprOp::Add),
                T![-] => Some(ExprOp::Sub),
                T![<<] => Some(ExprOp::Shl),
                T![>>] => Some(ExprOp::Shr),
                T![otherwise] => Some(ExprOp::Otherwise),
                _ => None,
            }
        })
    }
}

impl ast::BinStmt {
    #[inline]
    pub fn lhs(&self) -> Option<ast::Stmt> {
        support::children(self.syntax()).next()
    }
    #[inline]
    pub fn rhs(&self) -> Option<ast::Stmt> {
        support::children(self.syntax()).nth(1)
    }

    pub fn op(&self) -> Option<StmtOp> {
        self.syntax.children_with_tokens().filter_map(|it| it.into_token()).find_map(|token| {
            match token.kind() {
                T![,] => Some(StmtOp::Comma),
                T![and] => Some(StmtOp::And),
                T![;] => Some(StmtOp::Semicolon),
                T![or] => Some(StmtOp::Or),
                T![orelse] => Some(StmtOp::OrElse),
                _ => None,
            }
        })
    }
}

impl ast::PrefixExpr {
    pub fn op(&self) -> Option<PrefixOp> {
        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|token| {
            match token.kind() {
                T![+] => Some(PrefixOp::Plus),
                T![-] => Some(PrefixOp::Minus),
                T![~~] => Some(PrefixOp::BitNot),
                _ => None,
            }
        })
    }
}

impl ast::InlineParams {
    /// Returns an iterator over the normal arguments and a boolean
    /// indicating whether there is an ellipsis at the end.
    pub fn args(&self) -> (impl Iterator<Item = ast::FormalArg> + '_, bool) {
        (
            self.formal_arg_list().into_iter().flat_map(|p| p.formal_args()),
            self.dotdotdot_token().is_some(),
        )
    }

    /// Returns an iterator over annotations where each item is (suspending, mode, flow_patterns)
    pub fn annotations(
        &self,
    ) -> impl Iterator<Item = (bool, Option<ast::Mode>, Option<Vec<ast::FlowPattern>>)> {
        self.pred_property_list().into_iter().flat_map(|it| it.pred_properties()).map(|it| {
            let suspending = it.suspending_token().is_some();
            let flow_patterns = it.flow_pattern_list().map(|it| it.flow_patterns().collect());

            (suspending, it.mode(), flow_patterns)
        })
    }
}

impl ast::PredicateDomain {
    /// Returns the annotation where the values are (suspending, mode, flow_patterns)
    pub fn annotation(&self) -> (bool, Option<ast::Mode>, Option<Vec<ast::FlowPattern>>) {
        (
            self.suspending_token().is_some(),
            self.mode(),
            self.flow_pattern_list().map(|it| it.flow_patterns().collect()),
        )
    }
}

impl ast::Domain {
    pub fn generic_args(&self) -> impl Iterator<Item = ast::TypeArg> {
        self.generics().into_iter().flat_map(|g| g.type_args())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::{make, support};

    fn check_lit<'a>(lit: &str, expected: impl Into<Option<&'a str>>) {
        assert_eq!(make::string_seq(lit).value().as_deref(), expected.into());
    }

    #[test]
    fn concat_sequences() {
        check_lit(r#""foo" "bar""#, "foobar");
        check_lit(r#""foo" @|bar|  'baz'"#, "foobarbaz");
        check_lit(r#""foo" @|bar| 'baz' @|quux| "quuz""#, "foobarbazquuxquuz");
        check_lit(r#""foo" "b\ear""#, None);
        check_lit(r#""foo" "b\"ear""#, "foob\"ear");
        check_lit(r#""foo" "b\\ear""#, "foob\\ear");
        check_lit(r#"@[foo\] "n""#, "foo\\n");
    }

    #[test]
    fn path_directive() {
        assert!(
            support::child::<ast::StringSeq>(make::include_item("pfc\\core.ph").syntax())
                .unwrap()
                .is_path_directive()
        );
        assert!(
            support::child::<ast::StringSeq>(make::requires_directive("pfc\\core.ph").syntax())
                .unwrap()
                .is_path_directive()
        );
        assert!(
            support::child::<ast::StringSeq>(make::bin_include("pfc\\core.ph").syntax())
                .unwrap()
                .is_path_directive()
        );
        assert!(
            support::child::<ast::StringSeq>(make::string_include("pfc\\core.ph").syntax())
                .unwrap()
                .is_path_directive()
        );
    }
}
