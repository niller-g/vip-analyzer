//! Name resolution facade.

mod member_declaration;
mod member_definition;
mod type_name_resolution;

use crate::{
    ClassFactFunctorId, ClassFactVarId, ClassId, ClassPredicateId, ClauseId, ConstructorId,
    DefWithBodyId, DomainId, FactFunctorId, FactVarId, ImplementId, InterfaceId, ItemContainerId,
    ItemTreeLoc, Lookup, PredicateId, StaticContainerId,
    db::DefDatabase,
    hir::{MemberDeclaration, MemberDefinition, RefTerm, TermType},
    item_scope::ItemScope,
    nameres::{DefMap, ScopeRef},
    signatures::ClauseSignature,
};
use member_declaration::{DeclarationFinder, MemberDeclarations};
use member_definition::find_definition;
use std::ops::RangeInclusive;
use stdx::itertools::{Either, EitherOrBoth};
use type_name_resolution::ScopeTypeNames;

#[derive(Debug, Clone)]
pub struct Resolver<'db> {
    def_map: &'db DefMap,
    /// The current item's scope
    scope: ItemScope,
}
impl<'db> Resolver<'db> {
    pub fn lookup_scope(
        &self,
        db: &dyn DefDatabase,
        scope_ref: ScopeRef,
    ) -> Option<EitherOrBoth<ClassId, InterfaceId>> {
        let finder = ScopeTypeNames::new(db, self.def_map, &self.scope);
        finder.scope(scope_ref)
    }

    /// TODO: This function simply takes the first one found. It is not correct to just to find the first one. There should be some priority
    pub fn lookup_type_ref(
        &self,
        db: &dyn DefDatabase,
        look_for: &RefTerm,
    ) -> Option<Either<DomainId, InterfaceId>> {
        let finder = ScopeTypeNames::new(db, self.def_map, &self.scope);

        match look_for {
            RefTerm::Scope(scope_ref) => finder.interface(scope_ref.clone()).map(Either::Right),
            RefTerm::Member { scope, name, name_generics } => {
                finder.domain(scope.clone(), name, name_generics.len()).map(Either::Left)
            }
            RefTerm::Name { name, name_generics } => finder.name(name.clone(), name_generics.len()),
        }
    }

    pub fn lookup_member_ref(
        &'db self,
        db: &'db dyn DefDatabase,
        look_for: &RefTerm,
        arity: Option<RangeInclusive<u8>>,
        term_type: TermType,
    ) -> MemberDeclarations<'db> {
        let finder = match look_for {
            RefTerm::Scope(_) => {
                return MemberDeclarations::empty(db, self.def_map);
            }
            RefTerm::Member { scope, name, name_generics: _ } => {
                DeclarationFinder::new(db, self.def_map, &self.scope)
                    .with_members_from(scope.clone())
                    .filter_name(name.clone())
            }
            RefTerm::Name { name, name_generics: _ } => {
                DeclarationFinder::new(db, self.def_map, &self.scope)
                    .with_self_scope()
                    .filter_name(name.clone())
            }
        };
        let finder = if let Some(arity) = arity { finder.filter_arity(arity) } else { finder };

        finder.filter_term_type(term_type).build()
    }

    pub fn lookup_clause(
        &'db self,
        db: &'db dyn DefDatabase,
        signature: &ClauseSignature,
    ) -> MemberDeclarations<'db> {
        let name = signature.name.clone();
        let arity = signature.arity_range();
        let term_type = TermType::from_bool(signature.returns);

        DeclarationFinder::new(db, self.def_map, &self.scope)
            .with_self_scope()
            .filter_name(name)
            .filter_arity(arity)
            .filter_term_type(term_type)
            .build()
    }

    pub fn lookup_definition(
        &self,
        db: &dyn DefDatabase,
        declaration: &MemberDeclaration,
    ) -> Option<MemberDefinition> {
        find_definition(db, self.scope, declaration)
    }
}

impl HasResolver for ImplementId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
        Resolver { def_map: self.def_map(db), scope: ItemScope::Implement(self) }
    }
}
impl HasResolver for ClassId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
        Resolver { def_map: self.def_map(db), scope: ItemScope::Class(self) }
    }
}
impl HasResolver for InterfaceId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
        Resolver { def_map: self.def_map(db), scope: ItemScope::Interface(self) }
    }
}
impl HasResolver for ItemContainerId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
        match self {
            ItemContainerId::ImplementId(it) => it.resolver(db),
            ItemContainerId::ClassId(it) => it.resolver(db),
            ItemContainerId::InterfaceId(it) => it.resolver(db),
        }
    }
}
impl HasResolver for StaticContainerId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
        match self {
            StaticContainerId::ImplementId(it) => it.resolver(db),
            StaticContainerId::ClassId(it) => it.resolver(db),
        }
    }
}
impl HasResolver for DefWithBodyId {
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
        match self {
            DefWithBodyId::ClauseId(c) => c.resolver(db),
        }
    }
}
macro_rules! impl_has_resolver {
    ($($type:ty),* $(,)?) => {
        $(
            impl HasResolver for $type {
                fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_> {
                    lookup_resolver(db, self)
                }
            }
        )*
    };
}

impl_has_resolver! {
    ClassPredicateId,
    PredicateId,
    ConstructorId,
    ClauseId,
    FactFunctorId,
    FactVarId,
    ClassFactFunctorId,
    ClassFactVarId,
    DomainId,
}

pub trait HasResolver: Copy {
    /// Builds a resolver for type references inside this def.
    fn resolver(self, db: &dyn DefDatabase) -> Resolver<'_>;
}

fn lookup_resolver(
    db: &dyn DefDatabase,
    lookup: impl Lookup<
        Database = dyn DefDatabase,
        Data = impl ItemTreeLoc<Container = impl HasResolver>,
    >,
) -> Resolver<'_> {
    lookup.lookup(db).container().resolver(db)
}
