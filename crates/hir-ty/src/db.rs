//! The home of `HirDatabase`, which is the Salsa database containing all the
//! type inference-related queries.

use crate::infer::InferenceResult;
use hir_def::{DefWithBodyId, db::DefDatabase};
use triomphe::Arc;

#[query_group::query_group]
pub trait HirDatabase: DefDatabase + std::fmt::Debug {
    #[salsa::invoke(crate::infer::infer_query)]
    fn infer(&self, def: DefWithBodyId) -> Arc<InferenceResult>;
}

#[test]
fn hir_database_is_dyn_compatible() {
    fn _assert_dyn_compatible(_: &dyn HirDatabase) {}
}
