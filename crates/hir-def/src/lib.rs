pub mod db;
pub mod dyn_map;
pub mod expr_store;
pub mod hir;
pub mod item_scope;
pub mod item_tree;
pub mod nameres;
pub mod resolver;
pub mod signatures;
pub mod src;

#[cfg(test)]
mod test_db;

use la_arena::Idx;
use nameres::DefMap;
use span::FileId;
use std::hash::{Hash, Hasher};
use stdx::impl_from;
use {
    base_db::{Project, impl_intern_key},
    item_tree::FactFunctor,
};
use {
    db::DefDatabase,
    item_tree::{ClassFactFunctor, ClassFactVar, FactVar},
};
use {
    item_tree::{
        ClassFactDb, ClassPredicate, ClassProperty, Clause, Constant, Constructor, Domain,
        ExternalPredicateResolution, FactDb, FileItemTreeId, Functor, GuardClause,
        InterfaceDelegate, InterfaceResolution, ItemTree, ItemTreeId, ItemTreeNode, Predicate,
        PredicateDelegate, PredicateFrom, PredicateResolution, Property, PropertyFrom,
    },
    nameres::project_def_map,
};

/// The defs which can be visible inside an item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemDefId {
    PredicateId(PredicateId),
    ClassPredicateId(ClassPredicateId),
    ConstructorId(ConstructorId),
    FactDbId(FactDbId),
    ClassFactDbId(ClassFactDbId),
    FactFunctorId(FactFunctorId),
    FactVarId(FactVarId),
    ClassFactFunctorId(ClassFactFunctorId),
    ClassFactVarId(ClassFactVarId),
    PropertyId(PropertyId),
    ClassPropertyId(ClassPropertyId),
    ConstantId(ConstantId),
    DomainId(DomainId),
    FunctorId(FunctorId),
    ClauseId(ClauseId),
    GuardClauseId(GuardClauseId),
    PredicateDelegateId(PredicateDelegateId),
    InterfaceDelegateId(InterfaceDelegateId),
    PredicateResolutionId(PredicateResolutionId),
    InterfaceResolutionId(InterfaceResolutionId),
    ExternalPredicateResolutionId(ExternalPredicateResolutionId),
    PredicateFromId(PredicateFromId),
    PropertyFromId(PropertyFromId),
}
impl_from!(
    PredicateId,
    ClassPredicateId,
    ConstructorId,
    FactDbId,
    ClassFactDbId,
    FactFunctorId,
    FactVarId,
    ClassFactFunctorId,
    ClassFactVarId,
    PropertyId,
    ClassPropertyId,
    ConstantId,
    DomainId,
    FunctorId,
    ClauseId,
    GuardClauseId,
    PredicateDelegateId,
    InterfaceDelegateId,
    PredicateResolutionId,
    InterfaceResolutionId,
    ExternalPredicateResolutionId,
    PredicateFromId,
    PropertyFromId
    for ItemDefId
);
impl ItemDefId {
    pub fn as_domain(&self) -> Option<DomainId> {
        if let ItemDefId::DomainId(id) = self { Some(*id) } else { None }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemContainerId {
    ImplementId(ImplementId),
    ClassId(ClassId),
    InterfaceId(InterfaceId),
}
impl_from!(ImplementId, ClassId, InterfaceId for ItemContainerId);
impl From<StaticContainerId> for ItemContainerId {
    fn from(id: StaticContainerId) -> Self {
        match id {
            StaticContainerId::ImplementId(id) => ItemContainerId::ImplementId(id),
            StaticContainerId::ClassId(id) => ItemContainerId::ClassId(id),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticContainerId {
    ImplementId(ImplementId),
    ClassId(ClassId),
}
impl_from!(ImplementId, ClassId for StaticContainerId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImplementId {
    pub project: Project,
    pub idx: Idx<nameres::ImplementData>,
}
impl ImplementId {
    #[inline]
    pub fn new(project: Project, idx: Idx<nameres::ImplementData>) -> Self {
        Self { project, idx }
    }

    pub fn def_map<'db>(&self, db: &'db dyn DefDatabase) -> &'db DefMap {
        project_def_map(db, self.project)
    }

    pub fn file_id(&self, db: &dyn DefDatabase) -> FileId {
        self.def_map(db)[self].file_id()
    }

    pub fn find_class_declaration(&self, def_map: &DefMap) -> Option<ClassId> {
        let data = &def_map[self];
        def_map.lookup_class(data.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ClassId {
    pub project: Project,
    pub idx: Idx<nameres::ClassData>,
}
impl ClassId {
    #[inline]
    pub fn new(project: Project, idx: Idx<nameres::ClassData>) -> Self {
        Self { project, idx }
    }

    pub fn def_map<'db>(&self, db: &'db dyn DefDatabase) -> &'db DefMap {
        project_def_map(db, self.project)
    }

    pub fn file_id(&self, db: &dyn DefDatabase) -> FileId {
        self.def_map(db)[self].file_id()
    }

    pub fn find_class_implementation(&self, def_map: &DefMap) -> Option<ImplementId> {
        let data = &def_map[self];
        def_map.lookup_implement(data.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InterfaceId {
    pub project: Project,
    pub idx: Idx<nameres::InterfaceData>,
}
impl InterfaceId {
    #[inline]
    pub fn new(project: Project, idx: Idx<nameres::InterfaceData>) -> Self {
        Self { project, idx }
    }

    pub fn def_map<'db>(&self, db: &'db dyn DefDatabase) -> &'db DefMap {
        project_def_map(db, self.project)
    }

    pub fn file_id(&self, db: &dyn DefDatabase) -> FileId {
        self.def_map(db)[self].file_id()
    }
}

macro_rules! define_item_loc {
    ($name:ident, $container_type:ty) => {
        #[derive(Debug)]
        pub struct $name<N: ItemTreeNode> {
            pub container: $container_type,
            pub id: ItemTreeId<N>,
        }

        impl<N: ItemTreeNode> $name<N> {
            #[inline]
            pub fn new<C: Into<$container_type>>(
                container: C,
                file_id: FileId,
                tree_id: Idx<ItemTree>,
                id: FileItemTreeId<N>,
            ) -> Self {
                Self { container: container.into(), id: ItemTreeId::new(file_id, tree_id, id) }
            }
        }

        impl<N: ItemTreeNode> Clone for $name<N> {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<N: ItemTreeNode> Copy for $name<N> {}

        impl<N: ItemTreeNode> PartialEq for $name<N> {
            fn eq(&self, other: &Self) -> bool {
                self.container == other.container && self.id == other.id
            }
        }

        impl<N: ItemTreeNode> Eq for $name<N> {}

        impl<N: ItemTreeNode> Hash for $name<N> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.container.hash(state);
                self.id.hash(state);
            }
        }
    };
}

define_item_loc!(ImplItemLoc, ImplementId);
define_item_loc!(InterfaceItemLoc, InterfaceId);
define_item_loc!(StaticItemLoc, StaticContainerId);
define_item_loc!(AssocItemLoc, ItemContainerId);

pub trait ItemTreeLoc {
    type Container;
    type Id;
    fn item_tree_id(&self) -> ItemTreeId<Self::Id>;
    fn container(&self) -> Self::Container;
}

macro_rules! impl_intern {
    ($id:ident, $loc:ident, $intern:ident, $lookup:ident) => {
        impl_intern_key!($id, $loc);
        impl_intern_lookup!(DefDatabase, $id, $loc, $intern, $lookup);
    };
}

#[macro_export]
macro_rules! impl_intern_lookup {
    ($db:ident, $id:ident, $loc:ident, $intern:ident, $lookup:ident) => {
        impl $crate::Intern for $loc {
            type Database = dyn $db;
            type ID = $id;
            fn intern(self, db: &Self::Database) -> Self::ID {
                db.$intern(self)
            }
        }

        impl $crate::Lookup for $id {
            type Database = dyn $db;
            type Data = $loc;
            fn lookup(&self, db: &Self::Database) -> Self::Data {
                db.$lookup(*self)
            }
        }
    };
}

pub trait Intern {
    type Database: ?Sized;
    type ID;
    fn intern(self, db: &Self::Database) -> Self::ID;
}
pub trait Lookup {
    type Database: ?Sized;
    type Data;
    fn lookup(&self, db: &Self::Database) -> Self::Data;
}

macro_rules! impl_loc {
    ($loc:ident, $id:ident: $id_ty:ident, $container:ident: $container_type:ident) => {
        impl ItemTreeLoc for $loc {
            type Container = $container_type;
            type Id = $id_ty;
            fn item_tree_id(&self) -> ItemTreeId<Self::Id> {
                self.$id
            }
            fn container(&self) -> Self::Container {
                self.$container
            }
        }
    };
}

macro_rules! intern_id_loc_container {
    ($($item_loc:ident<$item_ty:ident> in $container:ident, $id_ty:ident, $loc:ident, $intern:ident, $lookup:ident),+) => {
        $(
            pub type $loc = $item_loc<$item_ty>;
            impl_intern!($id_ty, $loc, $intern, $lookup);
            impl_loc!($loc, id: $item_ty, container: $container);
        )+
    };
}

intern_id_loc_container!(AssocItemLoc<Predicate> in ItemContainerId, PredicateId, PredicateLoc, intern_predicate, lookup_intern_predicate);
intern_id_loc_container!(ImplItemLoc<ClassPredicate> in ImplementId, ClassPredicateId, ClassPredicateLoc, intern_class_predicate, lookup_intern_class_predicate);
intern_id_loc_container!(StaticItemLoc<Constructor> in StaticContainerId, ConstructorId, ConstructorLoc, intern_constructor, lookup_intern_constructor);
intern_id_loc_container!(AssocItemLoc<Property> in ItemContainerId, PropertyId, PropertyLoc, intern_property, lookup_intern_property);
intern_id_loc_container!(ImplItemLoc<ClassProperty> in ImplementId, ClassPropertyId, ClassPropertyLoc, intern_class_property, lookup_intern_class_property);
intern_id_loc_container!(ImplItemLoc<FactDb> in ImplementId, FactDbId, FactDbLoc, intern_fact_db, lookup_intern_fact_db);
intern_id_loc_container!(ImplItemLoc<ClassFactDb> in ImplementId, ClassFactDbId, ClassFactDbLoc, intern_class_fact_db, lookup_intern_class_fact_db);
intern_id_loc_container!(ImplItemLoc<FactFunctor> in ImplementId, FactFunctorId, FactFunctorLoc, intern_fact_functor, lookup_intern_fact_functor);
intern_id_loc_container!(ImplItemLoc<FactVar> in ImplementId, FactVarId, FactVarLoc, intern_fact_var, lookup_intern_fact_var);
intern_id_loc_container!(ImplItemLoc<ClassFactFunctor> in ImplementId, ClassFactFunctorId, ClassFactFunctorLoc, intern_class_fact_functor, lookup_intern_class_fact_functor);
intern_id_loc_container!(ImplItemLoc<ClassFactVar> in ImplementId, ClassFactVarId, ClassFactVarLoc, intern_class_fact_var, lookup_intern_class_fact_var);
intern_id_loc_container!(AssocItemLoc<Constant> in ItemContainerId, ConstantId, ConstantLoc, intern_constant, lookup_intern_constant);
intern_id_loc_container!(AssocItemLoc<Domain> in ItemContainerId, DomainId, DomainLoc, intern_domain, lookup_intern_domain);
intern_id_loc_container!(AssocItemLoc<Functor> in ItemContainerId, FunctorId, FunctorLoc, intern_functor, lookup_intern_functor);
intern_id_loc_container!(ImplItemLoc<Clause> in ImplementId, ClauseId, ClauseLoc, intern_clause, lookup_intern_clause);
intern_id_loc_container!(ImplItemLoc<GuardClause> in ImplementId, GuardClauseId, GuardClauseLoc, intern_guard_clause, lookup_intern_guard_clause);
intern_id_loc_container!(ImplItemLoc<PredicateDelegate> in ImplementId, PredicateDelegateId, PredicateDelegateLoc, intern_predicate_delegate, lookup_intern_predicate_delegate);
intern_id_loc_container!(ImplItemLoc<InterfaceDelegate> in ImplementId, InterfaceDelegateId, InterfaceDelegateLoc, intern_interface_delegate, lookup_intern_interface_delegate);
intern_id_loc_container!(ImplItemLoc<InterfaceResolution> in ImplementId, InterfaceResolutionId, InterfaceResolutionLoc, intern_interface_resolution, lookup_intern_interface_resolution);
intern_id_loc_container!(ImplItemLoc<PredicateResolution> in ImplementId, PredicateResolutionId, PredicateResolutionLoc, intern_predicate_resolution, lookup_intern_predicate_resolution);
intern_id_loc_container!(ImplItemLoc<ExternalPredicateResolution> in ImplementId, ExternalPredicateResolutionId, ExternalPredicateResolutionLoc, intern_external_predicate_resolution, lookup_intern_external_predicate_resolution);
intern_id_loc_container!(InterfaceItemLoc<PredicateFrom> in InterfaceId, PredicateFromId, PredicateFromLoc, intern_predicate_from, lookup_intern_predicate_from);
intern_id_loc_container!(InterfaceItemLoc<PropertyFrom> in InterfaceId, PropertyFromId, PropertyFromLoc, intern_property_from, lookup_intern_property_from);

/// The defs which have a body.
#[derive(Debug, PartialOrd, Ord, Clone, Copy, PartialEq, Eq, Hash, salsa_macros::Supertype)]
pub enum DefWithBodyId {
    ClauseId(ClauseId),
}
impl DefWithBodyId {
    pub fn file_id(&self, db: &dyn DefDatabase) -> FileId {
        match self {
            DefWithBodyId::ClauseId(id) => id.lookup(db).id.file_id(),
        }
    }
}

#[derive(Default, Debug, Eq, PartialEq, Clone, Copy)]
pub struct SyntheticSyntax;
