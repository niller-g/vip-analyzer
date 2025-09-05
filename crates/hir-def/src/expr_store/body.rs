use crate::{
    DefWithBodyId,
    db::DefDatabase,
    expr_store::{ExpressionStore, ExpressionStoreSourceMap, lower::lower_body},
    hir::{ExprId, StmtId},
};
use std::ops;
use triomphe::Arc;

/// The body of an item (function, const etc.).
#[derive(Debug, Eq, PartialEq)]
pub struct Body {
    pub store: ExpressionStore,
    pub args: Box<[ExprId]>,
    pub body_stmt: Option<StmtId>,
    pub return_expr: Option<ExprId>,
}

impl ops::Deref for Body {
    type Target = ExpressionStore;

    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    pub store: ExpressionStoreSourceMap,
}

impl ops::Deref for BodySourceMap {
    type Target = ExpressionStoreSourceMap;

    fn deref(&self) -> &Self::Target {
        &self.store
    }
}

impl Body {
    pub(crate) fn body_with_source_map_query(
        db: &dyn DefDatabase,
        def: DefWithBodyId,
    ) -> (Arc<Body>, Arc<BodySourceMap>) {
        let _p = tracing::info_span!("body_with_source_map_query").entered();

        let (body, mut source_map) = lower_body(db, def);
        source_map.store.shrink_to_fit();

        (Arc::new(body), Arc::new(source_map))
    }

    pub(crate) fn body_query(db: &dyn DefDatabase, def: DefWithBodyId) -> Arc<Body> {
        db.body_with_source_map(def).0
    }
}
