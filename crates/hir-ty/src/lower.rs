pub(crate) mod diagnostics;

use {
    crate::{Ty, db::HirDatabase},
    diagnostics::TyLoweringDiagnostic,
    hir_def::{
        expr_store::ExpressionStore,
        hir::type_ref::{TypeRef, TypeRefId},
        resolver::Resolver,
    },
};

#[expect(dead_code)]
#[derive(Debug)]
pub struct TyLoweringContext<'db> {
    pub db: &'db dyn HirDatabase,
    resolver: &'db Resolver<'db>,
    store: &'db ExpressionStore,
    pub type_param_mode: ParamLoweringMode,
    pub(crate) diagnostics: Vec<TyLoweringDiagnostic>,
}
impl<'db> TyLoweringContext<'db> {
    pub fn new(
        db: &'db dyn HirDatabase,
        resolver: &'db Resolver<'db>,
        store: &'db ExpressionStore,
    ) -> Self {
        let type_param_mode = ParamLoweringMode::Placeholder;
        Self {
            db,
            resolver,
            // def,
            // generics: Default::default(),
            store,
            type_param_mode,
            diagnostics: Vec::new(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParamLoweringMode {
    Placeholder,
    Variable,
}

impl TyLoweringContext<'_> {
    #[expect(clippy::todo)]
    pub fn lower_ty(&mut self, type_ref: TypeRefId) -> Ty {
        let type_ref = &self.store[type_ref];

        match type_ref {
            TypeRef::Error => Ty::Error,
            TypeRef::List(type_ref) => {
                let inner_ty = self.lower_ty(*type_ref);
                Ty::List(Box::new(inner_ty))
            }
            TypeRef::ScopeType(var_name) => Ty::AssociatedType(var_name.clone()),
            TypeRef::VarType(var_name) => match self.type_param_mode {
                ParamLoweringMode::Placeholder => Ty::TypeParam(var_name.clone()),
                ParamLoweringMode::Variable => Ty::BoundVar(var_name.clone()),
            },
            TypeRef::Wildcard => Ty::Error,
            TypeRef::Name(_ref_term) => todo!(),
            TypeRef::Fn(_signature) => todo!(),
            TypeRef::Compound(_functors) => todo!(),
            TypeRef::Numeric => Ty::Numeric,
        }
    }
}
