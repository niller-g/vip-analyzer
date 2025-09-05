//! Utility module for converting between hir_def ids and code_model wrappers.
//!
//! It's unclear if we need this long-term, but it's definitely useful while we
//! are splitting the hir.

macro_rules! from_id {
    ($(($id:path, $ty:path)),* $(,)?) => {$(
        impl From<$id> for $ty {
            fn from(id: $id) -> $ty {
                $ty { id }
            }
        }
        impl From<$ty> for $id {
            fn from(ty: $ty) -> $id {
                ty.id
            }
        }
    )*}
}

from_id![
    (hir_def::ImplementId, crate::ImplementItem),
    (hir_def::ClassId, crate::ClassItem),
    (hir_def::InterfaceId, crate::InterfaceItem),
    (hir_def::DomainId, crate::Domain),
    (hir_def::ConstantId, crate::Constant),
    (hir_def::PredicateId, crate::Predicate),
    (hir_def::ClassPredicateId, crate::ClassPredicate),
    (hir_def::ConstructorId, crate::Constructor),
    (hir_def::FactFunctorId, crate::FactFunctor),
    (hir_def::FactVarId, crate::FactVar),
    (hir_def::ClassFactFunctorId, crate::ClassFactFunctor),
    (hir_def::ClassFactVarId, crate::ClassFactVar),
    (hir_def::FunctorId, crate::Functor),
    (hir_def::PropertyId, crate::Property),
    (hir_def::ClassPropertyId, crate::ClassProperty),
    (hir_def::FactDbId, crate::FactDb),
    (hir_def::ClassFactDbId, crate::ClassFactDb),
    (hir_def::ClauseId, crate::Clause),
    (hir_def::GuardClauseId, crate::GuardClause),
];
