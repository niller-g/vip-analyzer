use crate::{
    ClassFactFunctorId, ClassFactVarId, ClassPredicateId, ClassPropertyId, ClauseId, ConstantId,
    ConstructorId, DomainId, FactFunctorId, FactVarId, FunctorId, Lookup, PredicateId, PropertyId,
    db::{self},
    expr_store::{
        ExpressionStore, ExpressionStoreSourceMap,
        lower::{
            lower_constructor_params, lower_domain, lower_formal_params, lower_functor,
            lower_pred_params, lower_type_ref, maybe_lower_type_ref,
        },
    },
    hir::{
        Annotation, Bindings, FlowArg, FlowPattern, FormalArg, Name, Params, PropertyFlowPattern,
        Signature, TermType, type_ref::TypeRefId,
    },
    src::HasSource,
};
use std::ops::RangeInclusive;
use syntax::ast::{self, HasFormalParamList, HasPropertyFlow, HasType};
use triomphe::Arc;

const _: () = assert!(
    std::mem::size_of::<ClauseSignature>() == 16,
    "Consider using bitflags when this no longer holds"
);
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClauseSignature {
    pub name: Name,
    pub arg_count: u8,
    pub ellipsis: bool,
    pub returns: bool,
}
impl ClauseSignature {
    pub(crate) fn query(db: &dyn db::DefDatabase, id: ClauseId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);
        let args: Vec<_> = source.value.clause_args().collect();
        let arg_count = args.len() as u8;
        let ellipsis = args.last().is_some_and(|last| matches!(last, ast::Expr::Ellipsis(_)));
        let returns = source.value.eq_token().is_some();

        Arc::new(ClauseSignature { name: item.name.clone(), arg_count, ellipsis, returns })
    }

    pub fn arity_range(&self) -> RangeInclusive<u8> {
        let min = self.arg_count;
        let max = if self.ellipsis { u8::MAX } else { self.arg_count };
        min..=max
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassPredicateSignature {
    pub name: Name,
    pub params: Params,
    pub store: Arc<ExpressionStore>,
}
impl ClassPredicateSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: ClassPredicateId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let pred = &item[loc.id.value];
        let source = loc.source(db);

        let (store, source_map, params) = lower_pred_params(db, source.file_id, source.value);

        (
            Arc::new(ClassPredicateSignature {
                name: pred.name.clone(),
                params,
                store: Arc::new(store),
            }),
            Arc::new(source_map),
        )
    }

    pub fn arity_range(&self) -> RangeInclusive<u8> {
        match &self.params {
            Params::RefTerm(_ref_term) => {
                // TODO: This should is wrong. It depends on the type of the predicate in type lowering.
                0..=u8::MAX
            }
            Params::Inline(signature) => arity_range(&signature.params, &signature.annotations),
        }
    }

    pub fn term_type(&self) -> TermType {
        match &self.params {
            Params::RefTerm(_ref_term) => {
                // TODO: This should is wrong. It depends on the type of the predicate in type lowering.
                TermType::ExprStmt
            }
            Params::Inline(signature) => TermType::from_bool(signature.ret_type.is_some()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PredicateSignature {
    pub name: Name,
    pub params: Params,
    pub store: Arc<ExpressionStore>,
}
impl PredicateSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: PredicateId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let pred = &item[loc.id.value];
        let source = loc.source(db);
        let (store, source_map, params) = lower_pred_params(db, source.file_id, source.value);

        (
            Arc::new(PredicateSignature {
                name: pred.name.clone(),
                params,
                store: Arc::new(store),
            }),
            Arc::new(source_map),
        )
    }

    pub fn arity_range(&self) -> RangeInclusive<u8> {
        match &self.params {
            Params::RefTerm(_ref_term) => {
                // TODO: This should is wrong. It depends on the type of the predicate in type lowering.
                0..=u8::MAX
            }
            Params::Inline(signature) => arity_range(&signature.params, &signature.annotations),
        }
    }

    pub fn term_type(&self) -> TermType {
        match &self.params {
            Params::RefTerm(_ref_term) => {
                // TODO: This should is wrong. It depends on the type of the predicate in type lowering.
                TermType::ExprStmt
            }
            Params::Inline(signature) => TermType::from_bool(signature.ret_type.is_some()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorSignature {
    pub name: Name,
    pub params: Signature,
    pub store: Arc<ExpressionStore>,
}
impl ConstructorSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: ConstructorId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let constructor = &item[loc.id.value];
        let source = loc.source(db);

        let (store, source_map, params) =
            lower_constructor_params(db, source.file_id, source.value);

        (
            Arc::new(ConstructorSignature {
                name: constructor.name.clone(),
                params,
                store: Arc::new(store),
            }),
            Arc::new(source_map),
        )
    }

    pub fn binding_flow(&self) -> impl Iterator<Item = Bindings> {
        self.params.annotations.iter().filter_map(Annotation::binding_flow)
    }

    pub fn arity_range(&self) -> RangeInclusive<u8> {
        arity_range(&self.params.params, &self.params.annotations)
    }
}

fn arity_range(params: &[FormalArg], annotations: &[Annotation]) -> RangeInclusive<u8> {
    let default_count = params.iter().filter(|a| a.has_default()).count();
    let mut min_arity = params.len() - default_count;
    let mut max_arity = params.len();

    debug_assert!(!annotations.is_empty());
    for annotation in annotations {
        match &annotation.flow_pattern {
            FlowPattern::AnyFlow => {}
            FlowPattern::FlowPattern { flow_args, ellipsis } => {
                debug_assert_eq!(flow_args.len(), params.len());

                let in_args = flow_args.iter().filter(|arg| **arg == FlowArg::In).count();
                #[cfg(not(debug_assertions))]
                let min = in_args.saturating_sub(default_count);
                #[cfg(debug_assertions)]
                let min = in_args.checked_sub(default_count).unwrap();

                let max = if *ellipsis { u8::MAX as usize } else { flow_args.len() };
                min_arity = min_arity.min(min);
                max_arity = max_arity.max(max);
            }
        }
    }

    min_arity as u8..=max_arity as u8
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DomainSignature {
    pub name: Name,
    pub generics: Box<[TypeRefId]>,
    pub domain_def: TypeRefId,
    pub store: Arc<ExpressionStore>,
}
impl DomainSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: DomainId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let domain = &item[loc.id.value];
        let source = loc.source(db);

        let (store, source_map, generics, domain_ty) = lower_domain(db, source);

        (
            Arc::new(DomainSignature {
                name: domain.name.clone(),
                generics,
                domain_def: domain_ty,
                store: Arc::new(store),
            }),
            Arc::new(source_map),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctorSignature {
    pub name: Name,
    pub params: Box<[FormalArg]>,
    pub store: Arc<ExpressionStore>,
}
impl FunctorSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: FunctorId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);
        let (store, source_map, functor) = lower_functor(db, source);
        let data = match functor {
            Some(functor) => FunctorSignature {
                name: item.name.clone(),
                params: functor.params.clone(),
                store: Arc::new(store),
            },
            None => FunctorSignature {
                name: item.name.clone(),
                params: Box::new([]),
                store: Arc::new(store),
            },
        };

        (Arc::new(data), Arc::new(source_map))
    }

    pub fn arg_count(&self) -> RangeInclusive<u8> {
        let default_count = self.params.iter().filter(|a| a.has_default()).count();
        let min_arity = self.params.len() - default_count;
        let max_arity = self.params.len();

        min_arity as u8..=max_arity as u8
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum FactFunctorMode {
    Determ,
    #[default]
    Nondeterm,
    Single,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FactFunctorSignature {
    pub name: Name,
    pub params: Box<[TypeRefId]>,
    pub ellipsis: bool,
    pub mode: FactFunctorMode,
    pub store: Arc<ExpressionStore>,
}
impl FactFunctorSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: FactFunctorId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);
        let (formal_params, ellipsis) = source.value.params();
        let (store, source_map, params) = lower_formal_params(db, source.file_id, formal_params);
        (
            Arc::new(FactFunctorSignature {
                name: item.name.clone(),
                params,
                ellipsis,
                mode: fact_functor_mode(&source.value),
                store: Arc::new(store),
            }),
            Arc::new(source_map),
        )
    }

    pub fn arg_count(&self) -> RangeInclusive<u8> {
        let start = self.params.len() as u8;
        let end = if self.ellipsis { u8::MAX } else { start };
        start..=end
    }
}

macro_rules! impl_fact_functor_mode {
    ($fn_name:ident(ast: $type:ty)) => {
        fn $fn_name(ast: &$type) -> FactFunctorMode {
            ast.determ_token()
                .is_some()
                .then_some(FactFunctorMode::Determ)
                .or_else(|| ast.single_token().is_some().then_some(FactFunctorMode::Single))
                .unwrap_or_default()
        }
    };
}

impl_fact_functor_mode!(fact_functor_mode(ast: ast::FactFunctor));
impl_fact_functor_mode!(class_fact_functor_mode(ast: ast::ClassFactFunctor));

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FactVarSignature {
    pub name: Name,
    pub ty: TypeRefId,
    pub store: Arc<ExpressionStore>,
}
impl FactVarSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: FactVarId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);
        let ty_kind = source.map(|value| value.ty());
        let (store, source_map, ty) = lower_type_ref(db, ty_kind);
        (
            Arc::new(FactVarSignature { name: item.name.clone(), ty, store: Arc::new(store) }),
            Arc::new(source_map),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassFactFunctorSignature {
    pub name: Name,
    pub params: Box<[TypeRefId]>,
    pub ellipsis: bool,
    pub mode: FactFunctorMode,
    pub store: Arc<ExpressionStore>,
}
impl ClassFactFunctorSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: ClassFactFunctorId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);
        let (formal_params, ellipsis) = source.value.params();
        let (store, source_map, params) = lower_formal_params(db, source.file_id, formal_params);
        (
            Arc::new(ClassFactFunctorSignature {
                name: item.name.clone(),
                params,
                ellipsis,
                mode: class_fact_functor_mode(&source.value),
                store: Arc::new(store),
            }),
            Arc::new(source_map),
        )
    }

    pub fn arg_count(&self) -> RangeInclusive<u8> {
        let start = self.params.len() as u8;
        let end = if self.ellipsis { u8::MAX } else { start };
        start..=end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassFactVarSignature {
    pub name: Name,
    pub ty: TypeRefId,
    pub store: Arc<ExpressionStore>,
}
impl ClassFactVarSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: ClassFactVarId,
    ) -> (Arc<Self>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);
        let ty_kind = source.map(|value| value.ty());
        let (store, source_map, ty) = lower_type_ref(db, ty_kind);
        (
            Arc::new(ClassFactVarSignature { name: item.name.clone(), ty, store: Arc::new(store) }),
            Arc::new(source_map),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantSignature {
    pub name: Name,
    pub ty: Option<TypeRefId>,
    pub store: Arc<ExpressionStore>,
}
impl ConstantSignature {
    pub(crate) fn query(
        db: &dyn db::DefDatabase,
        id: ConstantId,
    ) -> (Arc<ConstantSignature>, Arc<ExpressionStoreSourceMap>) {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let constant = &item[loc.id.value];
        let source = loc.source(db);
        let ty_kind = source.map(|value| value.ty());
        let (store, source_map, ty) = maybe_lower_type_ref(db, ty_kind);

        (
            Arc::new(ConstantSignature { name: constant.name.clone(), ty, store: Arc::new(store) }),
            Arc::new(source_map),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertySignature {
    pub name: Name,
    pub flow_pattern: PropertyFlowPattern,
}
impl PropertySignature {
    pub(crate) fn query(db: &dyn db::DefDatabase, id: PropertyId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);

        let flow_pattern = match source.value.in_out_flow() {
            (true, false) => PropertyFlowPattern::In,
            (false, true) => PropertyFlowPattern::Out,
            (true, true) | (false, false) => PropertyFlowPattern::InOut,
        };

        Arc::new(PropertySignature { name: item.name.clone(), flow_pattern })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassPropertySignature {
    pub name: Name,
    pub flow_pattern: PropertyFlowPattern,
}
impl ClassPropertySignature {
    pub(crate) fn query(db: &dyn db::DefDatabase, id: ClassPropertyId) -> Arc<Self> {
        let loc = id.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let item = &item[loc.id.value];
        let source = loc.source(db);

        let flow_pattern = match source.value.in_out_flow() {
            (true, false) => PropertyFlowPattern::In,
            (false, true) => PropertyFlowPattern::Out,
            (true, true) | (false, false) => PropertyFlowPattern::InOut,
        };

        Arc::new(ClassPropertySignature { name: item.name.clone(), flow_pattern })
    }
}
