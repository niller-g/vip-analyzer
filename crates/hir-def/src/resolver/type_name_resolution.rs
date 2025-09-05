use crate::{
    ClassId, DomainId, InterfaceId, ItemDefId,
    db::DefDatabase,
    hir::{Name, namespaces::NsPath},
    item_scope::ItemScope,
    nameres::{DefMap, ScopeRef},
};
use stdx::itertools::{Either, EitherOrBoth};

pub(crate) struct ScopeTypeNames<'a> {
    db: &'a dyn DefDatabase,
    def_map: &'a DefMap,
    self_scope: &'a ItemScope,
}
impl ScopeTypeNames<'_> {
    pub(crate) fn new<'a>(
        db: &'a dyn DefDatabase,
        def_map: &'a DefMap,
        self_scope: &'a ItemScope,
    ) -> ScopeTypeNames<'a> {
        ScopeTypeNames { db, def_map, self_scope }
    }

    pub(crate) fn interface(&self, filter: ScopeRef) -> Option<InterfaceId> {
        self.self_scope.absolutize(self.def_map, filter)?.right()
    }

    pub(crate) fn scope(&self, filter: ScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>> {
        self.self_scope.absolutize(self.def_map, filter)
    }

    pub(crate) fn domain(
        &self,
        scope_filter: ScopeRef,
        name_filter: &Name,
        generics_filter: usize,
    ) -> Option<DomainId> {
        let (class, interface) =
            self.self_scope.absolutize(self.def_map, scope_filter)?.left_and_right();

        class
            .map(ItemScope::Class)
            .into_iter()
            .chain(interface.map(ItemScope::Interface))
            .flat_map(|scope| collect_local_domains(&scope, self.def_map))
            .find(|domain_id| {
                let sig = self.db.domain_signature(*domain_id);
                &sig.name == name_filter && sig.generics.len() == generics_filter
            })
    }

    pub(crate) fn name(
        &self,
        name_filter: Name,
        generics_filter: usize,
    ) -> Option<Either<DomainId, InterfaceId>> {
        self.db
            .available_scopes(*self.self_scope)
            .iter()
            .flat_map(|scope| collect_local_domains(&scope, self.def_map))
            .find(|domain_id| {
                let sig = self.db.domain_signature(*domain_id);
                sig.name == name_filter
            })
            .map(Either::Left)
            .or_else(|| {
                self.self_scope
                    .absolutize(
                        self.def_map,
                        ScopeRef::new(NsPath::EMPTY, name_filter, generics_filter as u8),
                    )?
                    .right()
                    .map(Either::Right)
            })
    }
}

fn collect_local_domains<'a>(
    scope: &ItemScope,
    def_map: &'a DefMap,
) -> impl Iterator<Item = DomainId> + use<'a> {
    let declarations = match scope {
        ItemScope::Implement(id) => def_map[id].declarations(),
        ItemScope::Class(id) => def_map[id].declarations(),
        ItemScope::Interface(id) => def_map[id].declarations(),
    };

    declarations.iter().filter_map(|def| match def {
        ItemDefId::DomainId(id) => Some(*id),
        _ => None,
    })
}
