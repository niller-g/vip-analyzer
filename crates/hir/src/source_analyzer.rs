//! Lookup hir elements using positions in the source code. This is a lossy
//! transformation: in general, a single source might correspond to several
//! modules, functions, etc, due to macros, cfgs and `#[path=]` attributes on
//! modules.
//!
//! So, this modules should not be used during hir construction, it exists
//! purely for "IDE needs".

use hir_def::{
    ClassId, DefWithBodyId, DomainId, InterfaceId,
    db::DefDatabase,
    expr_store::{
        Body, BodySourceMap, ExpressionStore, ExpressionStoreSourceMap, lower::lower_ref_term,
    },
    hir::{ExprId, MemberDeclaration, MemberDefinition, StmtId, TermType},
    resolver::{HasResolver, Resolver},
    signatures::ClauseSignature,
    src::InFile,
};
use hir_ty::{InferenceResult, db::HirDatabase};
use itertools::{Either, EitherOrBoth};
use span::{FileId, TextSize};
use std::ops::RangeInclusive;
use syntax::{
    SyntaxNode,
    ast::{self},
};
use triomphe::Arc;

/// `SourceAnalyzer` is a convenience wrapper which exposes HIR API in terms of
/// original source files. It should not be used inside the HIR itself.
#[derive(Debug)]
pub(crate) struct SourceAnalyzer<'db> {
    pub(crate) file_id: FileId,
    pub(crate) resolver: Resolver<'db>,
    def: Option<(DefWithBodyId, Arc<Body>, Arc<BodySourceMap>)>,
    infer: Option<Arc<InferenceResult>>,
}

impl<'db> SourceAnalyzer<'db> {
    pub(crate) fn new_for_body(
        db: &'db dyn HirDatabase,
        def: DefWithBodyId,
        node @ InFile { file_id, .. }: InFile<&SyntaxNode>,
        offset: Option<TextSize>,
    ) -> SourceAnalyzer<'db> {
        _ = node;
        _ = offset;

        let resolver = def.resolver(db);
        let (body, source_map) = db.body_with_source_map(def);
        SourceAnalyzer {
            resolver,
            def: Some((def, body, source_map)),
            file_id,
            infer: Some(db.infer(def)),
        }
    }

    pub(crate) fn new_for_resolver(
        resolver: Resolver<'db>,
        node: InFile<&SyntaxNode>,
    ) -> SourceAnalyzer<'db> {
        SourceAnalyzer { file_id: node.file_id, resolver, def: None, infer: None }
    }

    fn body_(&self) -> Option<(DefWithBodyId, &Body, &BodySourceMap, Option<&InferenceResult>)> {
        let (def, body, source_map) = self.def.as_ref()?;
        let infer = self.infer.as_ref();
        Some((*def, body, source_map, infer.map(|v| &**v)))
    }

    fn infer(&self) -> Option<&InferenceResult> {
        self.infer.as_deref()
    }

    fn body(&self) -> Option<&Body> {
        self.def.as_ref().map(|(_, body, _)| &**body)
    }

    #[expect(dead_code)]
    pub(crate) fn store(&self) -> Option<&ExpressionStore> {
        self.body().map(|body| &body.store)
    }

    pub(crate) fn store_sm(&self) -> Option<&ExpressionStoreSourceMap> {
        self.body_().map(|(_, _, sm, _)| &**sm)
    }

    fn expr_id(&self, expr: &ast::Expr) -> Option<ExprId> {
        let src = InFile::new(self.file_id, expr);
        self.store_sm()?.node_expr(src)
    }

    #[expect(unused)]
    fn stmt_id(&self, db: &dyn HirDatabase, stmt: &ast::Stmt) -> Option<StmtId> {
        let src = InFile::new(self.file_id, stmt);
        self.store_sm()?.node_stmt(src)
    }

    #[expect(dead_code, unused_variables)]
    pub(crate) fn type_of_expr(&self, db: &'db dyn HirDatabase, expr: &ast::Expr) -> Option<()> {
        let expr_id = self.expr_id(expr)?;
        let infer = self.infer()?;
        #[expect(clippy::let_unit_value)]
        let ty = infer[expr_id];
        Some(ty)
    }

    pub(crate) fn find_type(
        &self,
        db: &dyn DefDatabase,
        ty: &ast::RefTerm,
    ) -> Option<Either<DomainId, InterfaceId>> {
        let ref_term = lower_ref_term(db, InFile::new(self.file_id, ty.clone())).2?;

        self.resolver.lookup_type_ref(db, &ref_term)
    }

    pub(crate) fn find_scope(
        &self,
        db: &dyn DefDatabase,
        ast: &ast::ScopeRef,
    ) -> Option<EitherOrBoth<ClassId, InterfaceId>> {
        let scope_ref = hir_def::nameres::ScopeRef::from_ast(ast)?;
        self.resolver.lookup_scope(db, scope_ref)
    }

    pub(crate) fn find_member(
        &self,
        db: &dyn DefDatabase,
        member: &ast::RefTerm,
        arity: Option<RangeInclusive<u8>>,
    ) -> Option<MemberDeclaration> {
        let ref_term = lower_ref_term(db, InFile::new(self.file_id, member.clone())).2?;
        self.resolver.lookup_member_ref(db, &ref_term, arity, TermType::ExprStmt).first()
    }

    pub(crate) fn find_definition(
        &self,
        db: &dyn DefDatabase,
        declaration: &MemberDeclaration,
    ) -> Option<MemberDefinition> {
        self.resolver.lookup_definition(db, declaration)
    }

    pub(crate) fn find_clause_declaration(
        &self,
        db: &dyn DefDatabase,
        sig: &ClauseSignature,
    ) -> Option<MemberDeclaration> {
        self.resolver.lookup_clause(db, sig).first()
    }
}
