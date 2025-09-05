//! When *constructing* `hir`, we start at some parent syntax node and recursively
//! lower the children.
//!
//! This module allows one to go in the opposite direction: start with a syntax
//! node for a *child*, and get its hir.

use hir_def::{
    ClassId, DefWithBodyId, ImplementId, InterfaceId, ItemDefId, ItemTreeLoc, Lookup,
    db::DefDatabase,
    dyn_map::{
        DynMap,
        keys::{self, Key},
    },
    item_tree::ItemTreeNode,
    src::HasSource,
};
use span::FileId;

pub(crate) trait ChildBySource {
    fn child_by_source(&self, db: &dyn DefDatabase, file_id: FileId) -> DynMap {
        let mut res = DynMap::default();
        self.child_by_source_to(db, &mut res, file_id);
        res
    }
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId);
}

impl ChildBySource for ImplementId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        let def_map = self.def_map(db);
        let data = &def_map[self];
        data.declarations().iter().copied().for_each(|item| add_module_def(db, map, file_id, item))
    }
}
impl ChildBySource for ClassId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        let def_map = self.def_map(db);
        let data = &def_map[self];
        data.declarations().iter().copied().for_each(|item| add_module_def(db, map, file_id, item))
    }
}
impl ChildBySource for InterfaceId {
    fn child_by_source_to(&self, db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId) {
        let def_map = self.def_map(db);
        let data = &def_map[self];
        data.declarations().iter().copied().for_each(|item| add_module_def(db, map, file_id, item))
    }
}

fn add_module_def(db: &dyn DefDatabase, map: &mut DynMap, file_id: FileId, item: ItemDefId) {
    match item {
        ItemDefId::PredicateId(id) => insert_item_loc(db, map, file_id, id, keys::PREDICATE),
        ItemDefId::ClassPredicateId(id) => {
            insert_item_loc(db, map, file_id, id, keys::CLASS_PREDICATE)
        }
        ItemDefId::ConstructorId(id) => insert_item_loc(db, map, file_id, id, keys::CONSTRUCTOR),
        ItemDefId::FactDbId(id) => insert_item_loc(db, map, file_id, id, keys::FACT_DB),
        ItemDefId::ClassFactDbId(id) => insert_item_loc(db, map, file_id, id, keys::CLASS_FACT_DB),
        ItemDefId::FactFunctorId(id) => insert_item_loc(db, map, file_id, id, keys::FACT_FUNCTOR),
        ItemDefId::FactVarId(id) => insert_item_loc(db, map, file_id, id, keys::FACT_VAR),
        ItemDefId::ClassFactFunctorId(id) => {
            insert_item_loc(db, map, file_id, id, keys::CLASS_FACT_FUNCTOR)
        }
        ItemDefId::ClassFactVarId(id) => {
            insert_item_loc(db, map, file_id, id, keys::CLASS_FACT_VAR)
        }
        ItemDefId::PropertyId(id) => insert_item_loc(db, map, file_id, id, keys::PROPERTY),
        ItemDefId::ClassPropertyId(id) => {
            insert_item_loc(db, map, file_id, id, keys::CLASS_PROPERTY)
        }
        ItemDefId::ConstantId(id) => insert_item_loc(db, map, file_id, id, keys::CONSTANT),
        ItemDefId::DomainId(id) => insert_item_loc(db, map, file_id, id, keys::DOMAIN),
        ItemDefId::FunctorId(id) => insert_item_loc(db, map, file_id, id, keys::FUNCTOR),
        ItemDefId::ClauseId(id) => insert_item_loc(db, map, file_id, id, keys::CLAUSE),
        ItemDefId::GuardClauseId(id) => insert_item_loc(db, map, file_id, id, keys::GUARD_CLAUSE),
        ItemDefId::PredicateDelegateId(id) => {
            insert_item_loc(db, map, file_id, id, keys::PREDICATE_DELEGATE)
        }
        ItemDefId::InterfaceDelegateId(id) => {
            insert_item_loc(db, map, file_id, id, keys::INTERFACE_DELEGATE)
        }
        ItemDefId::PredicateResolutionId(id) => {
            insert_item_loc(db, map, file_id, id, keys::PREDICATE_RESOLUTION)
        }
        ItemDefId::InterfaceResolutionId(id) => {
            insert_item_loc(db, map, file_id, id, keys::INTERFACE_RESOLUTION)
        }
        ItemDefId::ExternalPredicateResolutionId(id) => {
            insert_item_loc(db, map, file_id, id, keys::EXTERNAL_PREDICATE_RESOLUTION)
        }
        ItemDefId::PredicateFromId(id) => {
            insert_item_loc(db, map, file_id, id, keys::PREDICATE_FROM)
        }
        ItemDefId::PropertyFromId(id) => insert_item_loc(db, map, file_id, id, keys::PROPERTY_FROM),
    }
}

fn insert_item_loc<ID, N, Data>(
    db: &dyn DefDatabase,
    res: &mut DynMap,
    file_id: FileId,
    id: ID,
    key: Key<N::Source, ID>,
) where
    ID: Lookup<Database = dyn DefDatabase, Data = Data> + 'static,
    Data: ItemTreeLoc<Id = N>,
    N: ItemTreeNode,
    N::Source: 'static,
{
    let loc = id.lookup(db);
    if loc.item_tree_id().file_id() == file_id {
        res[key].insert(loc.ast_ptr(db).value, id)
    }
}

impl ChildBySource for DefWithBodyId {
    fn child_by_source_to(&self, _db: &dyn DefDatabase, _map: &mut DynMap, _file_id: FileId) {
        match self {
            DefWithBodyId::ClauseId(_clause_id) => {}
        }
    }
}
