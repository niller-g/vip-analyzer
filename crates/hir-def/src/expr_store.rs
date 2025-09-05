//! Defines `ExpressionStore`: a lowered representation of functions, statics and
//! consts.
pub mod body;
pub mod lower;

pub use self::body::{Body, BodySourceMap};
use crate::{
    hir::{
        Expr, ExprId, Stmt, StmtId,
        type_ref::{TypeRef, TypeRefId},
    },
    src::InFile,
};
use la_arena::{Arena, ArenaMap};
use rustc_hash::FxHashMap;
use std::ops::Index;
use syntax::{
    AstPtr,
    ast::{self},
};

pub type StmtPtr = AstPtr<ast::Stmt>;
pub type StmtSource = InFile<StmtPtr>;

pub type ExprPtr = AstPtr<ast::Expr>;
pub type ExprSource = InFile<ExprPtr>;

pub type TypePtr = AstPtr<ast::TypeKind>;
pub type TypeSource = InFile<TypePtr>;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct ExpressionStore {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<TypeRef>,
}
impl Index<TypeRefId> for ExpressionStore {
    type Output = TypeRef;

    fn index(&self, b: TypeRefId) -> &TypeRef {
        &self.types[b]
    }
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct ExpressionStoreBuilder {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<TypeRef>,
}
impl ExpressionStoreBuilder {
    pub fn finish(self) -> ExpressionStore {
        let Self { mut stmts, mut exprs, mut types } = self;
        stmts.shrink_to_fit();
        exprs.shrink_to_fit();
        types.shrink_to_fit();

        ExpressionStore { stmts, exprs, types }
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct ExpressionStoreSourceMap {
    stmt_map: FxHashMap<StmtSource, StmtId>,
    stmt_map_back: ArenaMap<StmtId, StmtSource>,

    expr_map: FxHashMap<ExprSource, ExprId>,
    expr_map_back: ArenaMap<ExprId, ExprSource>,

    types_map_back: ArenaMap<TypeRefId, TypeSource>,
    types_map: FxHashMap<TypeSource, TypeRefId>,
}
impl ExpressionStoreSourceMap {
    fn shrink_to_fit(&mut self) {
        let Self { stmt_map, stmt_map_back, expr_map, expr_map_back, types_map, types_map_back } =
            self;
        stmt_map.shrink_to_fit();
        stmt_map_back.shrink_to_fit();
        expr_map.shrink_to_fit();
        expr_map_back.shrink_to_fit();
        types_map.shrink_to_fit();
        types_map_back.shrink_to_fit();
    }

    pub fn node_expr(&self, node: InFile<&ast::Expr>) -> Option<ExprId> {
        let src = node.map(AstPtr::new);
        self.expr_map.get(&src).cloned()
    }

    pub fn node_stmt(&self, node: InFile<&ast::Stmt>) -> Option<StmtId> {
        let src = node.map(AstPtr::new);
        self.stmt_map.get(&src).cloned()
    }
}
