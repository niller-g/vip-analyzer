//! This file contains the [`Diagnostics`] type used during inference,

use {
    super::{InferenceDiagnostic, InferenceTyDiagnosticSource},
    crate::{TyLoweringDiagnostic, db::HirDatabase, lower::TyLoweringContext},
    hir_def::{expr_store::ExpressionStore, resolver::Resolver},
    std::{
        cell::RefCell,
        ops::{Deref, DerefMut},
    },
};

#[derive(Debug, Default, Clone)]
pub(super) struct Diagnostics(RefCell<Vec<InferenceDiagnostic>>);

impl Diagnostics {
    pub(super) fn push(&self, diagnostic: InferenceDiagnostic) {
        self.0.borrow_mut().push(diagnostic);
    }

    fn push_ty_diagnostics(
        &self,
        source: InferenceTyDiagnosticSource,
        diagnostics: Vec<TyLoweringDiagnostic>,
    ) {
        self.0.borrow_mut().extend(
            diagnostics.into_iter().map(|diag| InferenceDiagnostic::TyDiagnostic { source, diag }),
        );
    }

    pub(super) fn finish(self) -> Vec<InferenceDiagnostic> {
        self.0.into_inner()
    }
}

pub(super) struct InferenceTyLoweringContext<'a> {
    ctx: TyLoweringContext<'a>,
    diagnostics: &'a Diagnostics,
    source: InferenceTyDiagnosticSource,
}
impl<'a> InferenceTyLoweringContext<'a> {
    #[inline]
    pub(super) fn new(
        db: &'a dyn HirDatabase,
        resolver: &'a Resolver<'_>,
        store: &'a ExpressionStore,
        diagnostics: &'a Diagnostics,
        source: InferenceTyDiagnosticSource,
    ) -> Self {
        Self { ctx: TyLoweringContext::new(db, resolver, store), diagnostics, source }
    }
}

impl<'a> Deref for InferenceTyLoweringContext<'a> {
    type Target = TyLoweringContext<'a>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl DerefMut for InferenceTyLoweringContext<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl Drop for InferenceTyLoweringContext<'_> {
    #[inline]
    fn drop(&mut self) {
        self.diagnostics
            .push_ty_diagnostics(self.source, std::mem::take(&mut self.ctx.diagnostics));
    }
}
