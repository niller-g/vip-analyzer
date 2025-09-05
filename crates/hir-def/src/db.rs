//! Defines database & queries for name resolution.
use crate::{
    ClassFactDbId, ClassFactDbLoc, ClassFactFunctorId, ClassFactFunctorLoc, ClassFactVarId,
    ClassFactVarLoc, ClassPredicateId, ClassPredicateLoc, ClassPropertyId, ClassPropertyLoc,
    ClauseId, ClauseLoc, ConstantId, ConstantLoc, ConstructorId, ConstructorLoc, DefWithBodyId,
    DomainId, DomainLoc, ExternalPredicateResolutionId, ExternalPredicateResolutionLoc, FactDbId,
    FactDbLoc, FactFunctorId, FactFunctorLoc, FactVarId, FactVarLoc, FunctorId, FunctorLoc,
    GuardClauseId, GuardClauseLoc, InterfaceDelegateId, InterfaceDelegateLoc,
    InterfaceResolutionId, InterfaceResolutionLoc, PredicateDelegateId, PredicateDelegateLoc,
    PredicateFromId, PredicateFromLoc, PredicateId, PredicateLoc, PredicateResolutionId,
    PredicateResolutionLoc, PropertyFromId, PropertyFromLoc, PropertyId, PropertyLoc,
    expr_store::{Body, BodySourceMap, ExpressionStoreSourceMap},
    item_scope::{AvailableScopes, ItemScope},
    item_tree::ItemTree,
    signatures::{
        ClassFactFunctorSignature, ClassFactVarSignature, ClassPredicateSignature,
        ClassPropertySignature, ClauseSignature, ConstantSignature, ConstructorSignature,
        DomainSignature, FactFunctorSignature, FactVarSignature, FunctorSignature,
        PredicateSignature, PropertySignature,
    },
};
use base_db::{RootQueryDb, SourceDatabase};
use la_arena::Arena;
use salsa::plumbing::AsId;
use span::{AstIdMap, FileId};
use triomphe::Arc;

#[query_group::query_group(InternDatabaseStorage)]
pub trait InternDatabase: RootQueryDb {
    #[salsa::interned]
    fn intern_predicate(&self, loc: PredicateLoc) -> PredicateId;

    #[salsa::interned]
    fn intern_class_predicate(&self, loc: ClassPredicateLoc) -> ClassPredicateId;

    #[salsa::interned]
    fn intern_constructor(&self, loc: ConstructorLoc) -> ConstructorId;

    #[salsa::interned]
    fn intern_property(&self, loc: PropertyLoc) -> PropertyId;

    #[salsa::interned]
    fn intern_class_property(&self, loc: ClassPropertyLoc) -> ClassPropertyId;

    #[salsa::interned]
    fn intern_fact_db(&self, loc: FactDbLoc) -> FactDbId;

    #[salsa::interned]
    fn intern_class_fact_db(&self, loc: ClassFactDbLoc) -> ClassFactDbId;

    #[salsa::interned]
    fn intern_fact_functor(&self, loc: FactFunctorLoc) -> FactFunctorId;

    #[salsa::interned]
    fn intern_fact_var(&self, loc: FactVarLoc) -> FactVarId;

    #[salsa::interned]
    fn intern_class_fact_functor(&self, loc: ClassFactFunctorLoc) -> ClassFactFunctorId;

    #[salsa::interned]
    fn intern_class_fact_var(&self, loc: ClassFactVarLoc) -> ClassFactVarId;

    #[salsa::interned]
    fn intern_constant(&self, loc: ConstantLoc) -> ConstantId;

    #[salsa::interned]
    fn intern_domain(&self, loc: DomainLoc) -> DomainId;

    #[salsa::interned]
    fn intern_functor(&self, loc: FunctorLoc) -> FunctorId;

    #[salsa::interned]
    fn intern_clause(&self, loc: ClauseLoc) -> ClauseId;

    #[salsa::interned]
    fn intern_guard_clause(&self, loc: GuardClauseLoc) -> GuardClauseId;

    #[salsa::interned]
    fn intern_predicate_delegate(&self, loc: PredicateDelegateLoc) -> PredicateDelegateId;

    #[salsa::interned]
    fn intern_interface_delegate(&self, loc: InterfaceDelegateLoc) -> InterfaceDelegateId;

    #[salsa::interned]
    fn intern_interface_resolution(&self, loc: InterfaceResolutionLoc) -> InterfaceResolutionId;

    #[salsa::interned]
    fn intern_predicate_resolution(&self, loc: PredicateResolutionLoc) -> PredicateResolutionId;

    #[salsa::interned]
    fn intern_external_predicate_resolution(
        &self,
        loc: ExternalPredicateResolutionLoc,
    ) -> ExternalPredicateResolutionId;

    #[salsa::interned]
    fn intern_predicate_from(&self, loc: PredicateFromLoc) -> PredicateFromId;

    #[salsa::interned]
    fn intern_property_from(&self, loc: PropertyFromLoc) -> PropertyFromId;
}

#[query_group::query_group]
pub trait DefDatabase: InternDatabase + SourceDatabase {
    /// Computes an [`ItemTree`] for the given file.
    #[salsa::invoke_interned(ItemTree::file_item_trees_query)]
    fn file_item_trees(&self, file_id: FileId) -> Arc<Arena<ItemTree>>;

    #[salsa::invoke_interned(ast_id_map)]
    fn ast_id_map(&self, file_id: FileId) -> Arc<AstIdMap>;

    #[salsa::invoke_interned(ItemScope::available_scopes_query)]
    fn available_scopes(&self, scope: ItemScope) -> Arc<AvailableScopes>;

    #[salsa::invoke(ClauseSignature::query)]
    fn clause_signature(&self, clause: ClauseId) -> Arc<ClauseSignature>;

    #[salsa::tracked]
    fn constant_signature(&self, constant: ConstantId) -> Arc<ConstantSignature> {
        self.constant_signature_with_source_map(constant).0
    }

    #[salsa::tracked]
    fn class_predicate_signature(
        &self,
        predicate: ClassPredicateId,
    ) -> Arc<ClassPredicateSignature> {
        self.class_predicate_signature_with_source_map(predicate).0
    }

    #[salsa::tracked]
    fn predicate_signature(&self, predicate: PredicateId) -> Arc<PredicateSignature> {
        self.predicate_signature_with_source_map(predicate).0
    }

    #[salsa::tracked]
    fn constructor_signature(&self, constructor: ConstructorId) -> Arc<ConstructorSignature> {
        self.constructor_signature_with_source_map(constructor).0
    }

    #[salsa::tracked]
    fn domain_signature(&self, domain: DomainId) -> Arc<DomainSignature> {
        self.domain_signature_with_source_map(domain).0
    }

    #[salsa::tracked]
    fn functor_signature(&self, functor: FunctorId) -> Arc<FunctorSignature> {
        self.functor_signature_with_source_map(functor).0
    }

    #[salsa::tracked]
    fn fact_functor_signature(&self, fact: FactFunctorId) -> Arc<FactFunctorSignature> {
        self.fact_functor_signature_with_source_map(fact).0
    }

    #[salsa::tracked]
    fn fact_var_signature(&self, fact: FactVarId) -> Arc<FactVarSignature> {
        self.fact_var_signature_with_source_map(fact).0
    }

    #[salsa::tracked]
    fn class_fact_functor_signature(
        &self,
        fact: ClassFactFunctorId,
    ) -> Arc<ClassFactFunctorSignature> {
        self.class_fact_functor_signature_with_source_map(fact).0
    }

    #[salsa::tracked]
    fn class_fact_var_signature(&self, fact: ClassFactVarId) -> Arc<ClassFactVarSignature> {
        self.class_fact_var_signature_with_source_map(fact).0
    }

    #[salsa::invoke(ConstantSignature::query)]
    fn constant_signature_with_source_map(
        &self,
        constant: ConstantId,
    ) -> (Arc<ConstantSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(ClassPredicateSignature::query)]
    fn class_predicate_signature_with_source_map(
        &self,
        predicate: ClassPredicateId,
    ) -> (Arc<ClassPredicateSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(PredicateSignature::query)]
    fn predicate_signature_with_source_map(
        &self,
        predicate: PredicateId,
    ) -> (Arc<PredicateSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(ConstructorSignature::query)]
    fn constructor_signature_with_source_map(
        &self,
        constructor: ConstructorId,
    ) -> (Arc<ConstructorSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(DomainSignature::query)]
    fn domain_signature_with_source_map(
        &self,
        domain: DomainId,
    ) -> (Arc<DomainSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(FunctorSignature::query)]
    fn functor_signature_with_source_map(
        &self,
        functor: FunctorId,
    ) -> (Arc<FunctorSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(FactFunctorSignature::query)]
    fn fact_functor_signature_with_source_map(
        &self,
        fact: FactFunctorId,
    ) -> (Arc<FactFunctorSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(FactVarSignature::query)]
    fn fact_var_signature_with_source_map(
        &self,
        fact: FactVarId,
    ) -> (Arc<FactVarSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(ClassFactFunctorSignature::query)]
    fn class_fact_functor_signature_with_source_map(
        &self,
        fact: ClassFactFunctorId,
    ) -> (Arc<ClassFactFunctorSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(ClassFactVarSignature::query)]
    fn class_fact_var_signature_with_source_map(
        &self,
        fact: ClassFactVarId,
    ) -> (Arc<ClassFactVarSignature>, Arc<ExpressionStoreSourceMap>);

    #[salsa::invoke(PropertySignature::query)]
    fn property_signature(&self, property: PropertyId) -> Arc<PropertySignature>;

    #[salsa::invoke(ClassPropertySignature::query)]
    fn class_property_signature(&self, property: ClassPropertyId) -> Arc<ClassPropertySignature>;

    #[salsa::invoke(Body::body_with_source_map_query)]
    #[salsa::lru(512)]
    fn body_with_source_map(&self, def: DefWithBodyId) -> (Arc<Body>, Arc<BodySourceMap>);

    #[salsa::invoke(Body::body_query)]
    fn body(&self, def: DefWithBodyId) -> Arc<Body>;
}

fn ast_id_map(db: &dyn DefDatabase, file_id: span::FileId) -> Arc<AstIdMap> {
    Arc::new(AstIdMap::from_source(&db.parse(file_id).syntax_node()))
}
