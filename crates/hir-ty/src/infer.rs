pub(crate) mod diagnostics;

use crate::{TyLoweringDiagnostic, db::HirDatabase};
use diagnostics::{Diagnostics, InferenceTyLoweringContext};
use hir_def::{
    DefWithBodyId,
    expr_store::{Body, ExpressionStore},
    hir::{Bindings, ExprId, MemberDeclaration},
    resolver::{HasResolver, Resolver},
    signatures::{
        ClassPredicateSignature, ClassPropertySignature, ClauseSignature, ConstructorSignature,
        PredicateSignature, PropertySignature,
    },
};
use la_arena::ArenaMap;
use std::ops::Index;
use triomphe::Arc;

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct InferenceResult {
    pub diagnostics: Vec<InferenceDiagnostic>,
    pub type_of_expr: ArenaMap<ExprId, ()>,
}
impl Index<ExprId> for InferenceResult {
    type Output = ();

    fn index(&self, expr: ExprId) -> &Self::Output {
        self.type_of_expr
            .get(expr)
            .expect("expr not found in inference result. Maybe return a `unknown` type instead?")
    }
}

/// The entry point of type inference.
pub(crate) fn infer_query(db: &dyn HirDatabase, def: DefWithBodyId) -> Arc<InferenceResult> {
    let _p = tracing::info_span!("infer_query").entered();
    let resolver = def.resolver(db);
    let body = db.body(def);
    let mut ctx = InferenceContext::new(db, def, &body, resolver);

    match def {
        DefWithBodyId::ClauseId(clause_id) => ctx.collect_clause(clause_id),
    }

    ctx.infer_body();

    Arc::new(ctx.finish())
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InferenceTyDiagnosticSource {
    /// Diagnostics that come from types in the body.
    Body,
    /// Diagnostics that come from types in fn parameters/return type, or static & const types.
    Signature,
}

/// The inference context contains all information needed during type inference.
#[derive(Clone, Debug)]
#[expect(unused)]
pub(crate) struct InferenceContext<'db> {
    pub(crate) db: &'db dyn HirDatabase,
    pub(crate) owner: DefWithBodyId,
    pub(crate) body: &'db Body,
    /// Generally you should not resolve things via this resolver. Instead create a TyLoweringContext
    /// and resolve the path via its methods. This will ensure proper error reporting.
    pub(crate) resolver: Resolver<'db>,

    pub(crate) result: InferenceResult,

    diagnostics: Diagnostics,
}
impl<'db> InferenceContext<'db> {
    fn new(
        db: &'db dyn HirDatabase,
        owner: DefWithBodyId,
        body: &'db Body,
        resolver: Resolver<'db>,
    ) -> Self {
        InferenceContext {
            db,
            owner,
            body,
            resolver,
            result: InferenceResult::default(),
            diagnostics: Diagnostics::default(),
        }
    }

    pub(crate) fn finish(self) -> InferenceResult {
        let InferenceContext { db: _, owner: _, body: _, resolver: _, mut result, diagnostics } =
            self;
        let InferenceResult { type_of_expr, diagnostics: _ } = &mut result;
        type_of_expr.shrink_to_fit();

        let mut diagnostics = diagnostics.finish();
        diagnostics.shrink_to_fit();
        result.diagnostics = diagnostics;

        result
    }

    fn collect_clause(&mut self, clause_id: hir_def::ClauseId) {
        let clause = self.db.clause_signature(clause_id);
        let declarations = self.clause_declarations(&clause);

        for bindings in declarations.iter().flat_map(ClauseDeclaration::binding_flow) {
            self.bind_analyze(&bindings, self.body);
        }
    }

    fn bind_analyze(&mut self, bindings: &Bindings, body: &Body) {
        _ = bindings;
        _ = body;
    }

    fn clause_declarations(&self, clause: &ClauseSignature) -> Vec<ClauseDeclaration> {
        let declarations = self.resolver.lookup_clause(self.db, clause);

        declarations
            .iter()
            .filter_map(|member| {
                let member = match member {
                    MemberDeclaration::Predicate(id) => {
                        ClauseDeclaration::Predicate(self.db.predicate_signature(id))
                    }
                    MemberDeclaration::ClassPredicate(id) => {
                        ClauseDeclaration::ClassPredicate(self.db.class_predicate_signature(id))
                    }
                    MemberDeclaration::Constructor(id) => {
                        ClauseDeclaration::Constructor(self.db.constructor_signature(id))
                    }
                    MemberDeclaration::Property(id) => ClauseDeclaration::Property(
                        self.db.property_signature(id),
                        PropertyInstance::from_returns(clause.returns),
                    ),
                    MemberDeclaration::ClassProperty(id) => ClauseDeclaration::ClassProperty(
                        self.db.class_property_signature(id),
                        PropertyInstance::from_returns(clause.returns),
                    ),
                    _ => return None,
                };
                Some(member)
            })
            .collect()
    }

    fn infer_body(&mut self) {
        _ = self.body;
    }

    #[expect(dead_code)]
    fn push_diagnostic(&self, diagnostic: InferenceDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    #[expect(dead_code)]
    fn with_ty_lowering<R>(
        &mut self,
        store: &ExpressionStore,
        types_source: InferenceTyDiagnosticSource,
        f: impl FnOnce(&mut InferenceTyLoweringContext<'_>) -> R,
    ) -> R {
        let mut ctx = InferenceTyLoweringContext::new(
            self.db,
            &self.resolver,
            store,
            &self.diagnostics,
            types_source,
            // self.generic_def,
        );
        f(&mut ctx)
    }
}

enum PropertyInstance {
    In,
    Out,
}
impl PropertyInstance {
    fn from_returns(returns: bool) -> Self {
        if returns { PropertyInstance::Out } else { PropertyInstance::In }
    }
}

enum ClauseDeclaration {
    /// When found in class, it is a static member. When found in interface or implement, it is an object member.
    Predicate(Arc<PredicateSignature>),
    /// Static member
    ClassPredicate(Arc<ClassPredicateSignature>),
    /// Static member
    Constructor(Arc<ConstructorSignature>),
    /// When found in class, it is a static member. When found in interface or implement, it is an object member.
    Property(Arc<PropertySignature>, PropertyInstance),
    /// Static member
    ClassProperty(Arc<ClassPropertySignature>, PropertyInstance),
    /*
    TODO: Not yet implemented

    FactFunctor(Arc<FactFunctorSignature>),
    ClassFactFunctor(Arc<ClassFactFunctorSignature>),
    */
}
impl ClauseDeclaration {
    #[expect(unreachable_code, unused_mut, clippy::todo)]
    fn binding_flow(&self) -> Vec<Bindings> {
        let mut bindings = Vec::new();
        return bindings;

        match self {
            ClauseDeclaration::Predicate(_sig) => todo!(),
            ClauseDeclaration::ClassPredicate(_sig) => todo!(),
            ClauseDeclaration::Constructor(sig) => {
                bindings.extend(sig.binding_flow());
            }
            ClauseDeclaration::Property(sig, kind) => {
                if let Some(binding) = match kind {
                    PropertyInstance::In => sig.flow_pattern.in_binding_flow(),
                    PropertyInstance::Out => sig.flow_pattern.out_binding_flow(),
                } {
                    bindings.push(Box::new([binding]));
                }
            }
            ClauseDeclaration::ClassProperty(sig, kind) => {
                if let Some(binding) = match kind {
                    PropertyInstance::In => sig.flow_pattern.in_binding_flow(),
                    PropertyInstance::Out => sig.flow_pattern.out_binding_flow(),
                } {
                    bindings.push(Box::new([binding]));
                }
            }
        };

        bindings
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferenceDiagnostic {
    TyDiagnostic { source: InferenceTyDiagnosticSource, diag: TyLoweringDiagnostic },
}
