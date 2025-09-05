use crate::{
    ClassId, ImplementId, InterfaceId, InterfaceResolutionId, Lookup,
    db::DefDatabase,
    hir::Arity,
    item_tree,
    nameres::{DefMap, ScopeRef},
};
use rustc_hash::FxHashSet;
use std::{collections::VecDeque, iter};
use stdx::itertools::EitherOrBoth;
use triomphe::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemScope {
    Implement(ImplementId),
    Class(ClassId),
    Interface(InterfaceId),
}
impl From<ImplementId> for ItemScope {
    fn from(id: ImplementId) -> Self {
        ItemScope::Implement(id)
    }
}
impl From<&ImplementId> for ItemScope {
    fn from(id: &ImplementId) -> Self {
        ItemScope::Implement(*id)
    }
}
impl From<ClassId> for ItemScope {
    fn from(id: ClassId) -> Self {
        ItemScope::Class(id)
    }
}
impl From<&ClassId> for ItemScope {
    fn from(id: &ClassId) -> Self {
        ItemScope::Class(*id)
    }
}
impl From<InterfaceId> for ItemScope {
    fn from(id: InterfaceId) -> Self {
        ItemScope::Interface(id)
    }
}
impl From<&InterfaceId> for ItemScope {
    fn from(id: &InterfaceId) -> Self {
        ItemScope::Interface(*id)
    }
}
impl ItemScope {
    pub fn is_object_ctx(&self, def_map: &DefMap) -> bool {
        match self {
            ItemScope::Implement(implement) => implement
                .find_class_declaration(def_map)
                .is_some_and(|cl| def_map[&cl].constructs_object()),
            ItemScope::Class(_) => false,
            ItemScope::Interface(_) => true,
        }
    }

    /// The interfaces that are declared to be supported.
    /// This is a shallow list i.e. it does not recursively expand the scopes.
    fn supports(&self, def_map: &DefMap) -> Vec<InterfaceId> {
        match self {
            ItemScope::Implement(id) => def_map[id].supports().to_vec(),
            ItemScope::Class(id) => def_map[id].supports().to_vec(),
            ItemScope::Interface(id) => def_map[id].supports().to_vec(),
        }
    }

    /// The classes that are declared to be opened.
    /// This is a shallow list i.e. it does not recursively expand the scopes.
    pub fn opened_classes(&self, def_map: &DefMap) -> Vec<ClassId> {
        match self {
            ItemScope::Implement(id) => def_map[id].opened_classes().to_vec(),
            ItemScope::Class(id) => def_map[id].opened_classes().to_vec(),
            ItemScope::Interface(id) => def_map[id].opened_classes().to_vec(),
        }
    }

    /// The interface that are declared to be opened.
    /// This is a shallow list i.e. it does not recursively expand the scopes.
    fn opened_interfaces(&self, def_map: &DefMap) -> Vec<InterfaceId> {
        match self {
            ItemScope::Implement(id) => def_map[id].opened_interfaces().to_vec(),
            ItemScope::Class(id) => def_map[id].opened_interfaces().to_vec(),
            ItemScope::Interface(id) => def_map[id].opened_interfaces().to_vec(),
        }
    }

    pub fn def_map<'db>(&self, db: &'db dyn DefDatabase) -> &'db DefMap {
        match self {
            ItemScope::Implement(id) => id.def_map(db),
            ItemScope::Class(id) => id.def_map(db),
            ItemScope::Interface(id) => id.def_map(db),
        }
    }

    pub fn absolutize(
        &self,
        def_map: &DefMap,
        relative: ScopeRef,
    ) -> Option<EitherOrBoth<ClassId, InterfaceId>> {
        let candidates = match self {
            ItemScope::Implement(id) => def_map[id].absolutize(relative),
            ItemScope::Class(id) => def_map[id].absolutize(relative),
            ItemScope::Interface(id) => def_map[id].absolutize(relative),
        };

        candidates.into_iter().find_map(|candidate| {
            let class = def_map.lookup_class(&candidate);
            let interface = def_map.lookup_interface(&candidate);

            match (class, interface) {
                (None, None) => None,
                (Some(class), None) => Some(EitherOrBoth::Left(class)),
                (None, Some(interface)) => Some(EitherOrBoth::Right(interface)),
                (Some(class), Some(interface)) => Some(EitherOrBoth::Both(class, interface)),
            }
        })
    }

    pub fn absolutize_interface(
        &self,
        def_map: &DefMap,
        relative: ScopeRef,
    ) -> Option<InterfaceId> {
        let candidates = match self {
            ItemScope::Implement(id) => def_map[id].absolutize(relative),
            ItemScope::Class(id) => def_map[id].absolutize(relative),
            ItemScope::Interface(id) => def_map[id].absolutize(relative),
        };
        candidates.into_iter().find_map(|candidate| def_map.lookup_interface(&candidate))
    }

    pub fn absolutize_class(&self, def_map: &DefMap, relative: ScopeRef) -> Option<ClassId> {
        let candidates = match self {
            ItemScope::Implement(id) => def_map[id].absolutize(relative),
            ItemScope::Class(id) => def_map[id].absolutize(relative),
            ItemScope::Interface(id) => def_map[id].absolutize(relative),
        };
        candidates.into_iter().find_map(|candidate| def_map.lookup_class(&candidate))
    }

    pub fn predicate_resolutions(&self, db: &dyn DefDatabase) -> Vec<PredicateResolution> {
        match self {
            ItemScope::Implement(id) => {
                let def_map = id.def_map(db);

                def_map[id]
                    .predicate_resolutions()
                    .filter_map(|resolution| {
                        let loc = resolution.lookup(db);
                        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
                        let resolution = &item[loc.id.value];

                        let from = match &resolution.from {
                            item_tree::PredicateFromSource::Class(scope_ref) => {
                                PredicateFrom::Class(
                                    self.absolutize_class(def_map, scope_ref.clone())?,
                                )
                            }
                            item_tree::PredicateFromSource::ClassAndRename(scope_ref, name) => {
                                PredicateFrom::ClassAndRename {
                                    class: self.absolutize_class(def_map, scope_ref.clone())?,
                                    new_name: name.clone(),
                                }
                            }
                        };

                        Some(PredicateResolution { arity: resolution.pred_arity.clone(), from })
                    })
                    .collect()
            }
            ItemScope::Class(_) | ItemScope::Interface(_) => Vec::new(),
        }
    }

    fn absolutize_interface_resolution(
        &self,
        db: &dyn DefDatabase,
        def_map: &DefMap,
        resolution: &InterfaceResolutionId,
    ) -> Option<(InterfaceId, ClassId)> {
        let loc = resolution.lookup(db);
        let item = &db.file_item_trees(loc.id.file_id())[loc.id.tree_id()];
        let resolution = &item[loc.id.value];
        let resolve_interface =
            self.absolutize_interface(def_map, resolution.resolve_interface.clone())?;
        let from_class = self.absolutize_class(def_map, resolution.from.clone())?;

        Some((resolve_interface, from_class))
    }

    /// Get the available (opened/supported/inherited) scopes in this scope including the self scope.
    pub fn available_scopes_query(
        db: &dyn DefDatabase,
        self_scope: ItemScope,
    ) -> Arc<AvailableScopes> {
        fn collect_interface(
            interface: InterfaceId,
            def_map: &DefMap,
            visited: &mut FxHashSet<InterfaceId>,
        ) -> Option<SupportedInterface> {
            if visited.insert(interface) {
                let interface_supports = def_map[&interface].supports();
                let mut next = Vec::with_capacity(interface_supports.len());
                let next_supports = interface_supports
                    .iter()
                    .filter_map(|sup| collect_interface(*sup, def_map, visited));
                next.extend(next_supports);
                Some(SupportedInterface { interface, supports: next })
            } else {
                None
            }
        }
        fn collect_class(
            class: ClassId,
            def_map: &DefMap,
            explicit_resolves: &[(InterfaceId, ClassId)],
            visited_cls: &mut FxHashSet<ClassId>,
        ) -> Option<(ClassId, Vec<SupportedInterface>)> {
            if visited_cls.insert(class) {
                let supports = def_map[&class].supports();
                let mut class_supports = Vec::with_capacity(supports.len());
                let next_supports = supports.iter().filter_map(|sup| {
                    if explicit_resolves.iter().any(|(i, c)| *i == *sup && *c != class) {
                        return None;
                    }
                    collect_interface(*sup, def_map, &mut FxHashSet::default())
                });
                class_supports.extend(next_supports);
                Some((class, class_supports))
            } else {
                None
            }
        }

        let def_map = &self_scope.def_map(db);
        let mut res = AvailableScopes::new(self_scope);
        let mut visited_interfaces = FxHashSet::default();
        let mut visited_classes = FxHashSet::default();

        match &self_scope {
            ItemScope::Class(id) => {
                visited_classes.insert(*id);
            }
            ItemScope::Interface(id) => {
                visited_interfaces.insert(*id);
            }
            ItemScope::Implement(id) => {
                let explicit_resolves: Vec<_> = def_map[id]
                    .interface_resolutions()
                    .filter_map(|resolution| {
                        self_scope.absolutize_interface_resolution(db, def_map, resolution)
                    })
                    .collect();

                for inherit in def_map[id].inherits() {
                    if let Some(inherit) =
                        collect_class(*inherit, def_map, &explicit_resolves, &mut visited_classes)
                    {
                        res.inherited.push(inherit);
                    }
                }
            }
        }
        for id in self_scope.supports(def_map) {
            if let Some(support) = collect_interface(id, def_map, &mut visited_interfaces) {
                res.supports.push(support)
            }
        }
        for open in self_scope.opened_classes(def_map) {
            res.opened_classes.push(open)
        }
        for open in self_scope.opened_interfaces(def_map) {
            res.opened_interfaces.push(open)
        }

        Arc::new(res)
    }
}

/// A supported interface is an interface that is supported by another interface or class.
///
/// Invariant: There are no cycles in the supported interfaces (only keep first entries on duplicates).
#[derive(Debug, PartialEq, Eq)]
pub struct SupportedInterface {
    /// The interface itself.
    interface: InterfaceId,
    /// The interfaces that this interface supports.
    supports: Vec<SupportedInterface>,
}
impl SupportedInterface {
    /// Check if this interface supports the given interface anywhere in the hierarchy.
    pub fn supports(&self, other: &InterfaceId) -> bool {
        if self.interface == *other {
            return true;
        }
        for sup in &self.supports {
            if sup.interface == *other || sup.supports(other) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AvailableScopes {
    /// The scope itself.
    pub self_scope: ItemScope,
    /// A list of inherited classes and hierarchy of supported interfaces.
    pub inherited: Vec<(ClassId, Vec<SupportedInterface>)>,
    /// Hierarchy of supported interfaces.
    pub supports: Vec<SupportedInterface>,
    /// A list of opened classes and their supported interfaces.
    pub opened_classes: Vec<ClassId>,
    /// A list of opened interfaces.
    pub opened_interfaces: Vec<InterfaceId>,
}
impl AvailableScopes {
    pub fn new(self_scope: ItemScope) -> Self {
        Self {
            self_scope,
            inherited: Default::default(),
            supports: Default::default(),
            opened_classes: Default::default(),
            opened_interfaces: Default::default(),
        }
    }

    /// Get the available scopes in this scope including the self scope.
    ///
    /// Order:
    /// 1. self scope
    /// 2. All inherited classes with their supported interfaces right after
    /// 3. All supported interfaces
    /// 4. All opened classes with their supported interfaces right after
    /// 5. All opened interfaces
    ///
    /// Note: There might be (expected) duplicates between the supported interfaces that stem from the inherited and opened classes.
    pub fn iter(&self) -> impl Iterator<Item = ItemScope> + use<'_> {
        iter::once(self.self_scope)
            .chain(self.all_inherits())
            .chain(self.all_supports().map(ItemScope::from))
            .chain(self.opened_classes.iter().map(ItemScope::from))
            .chain(self.opened_interfaces.iter().map(ItemScope::from))
    }

    pub fn all_supports(&self) -> impl Iterator<Item = InterfaceId> + use<'_> {
        Self::bfs_iter(self.supports.iter())
    }

    pub fn all_inherits(&self) -> impl Iterator<Item = ItemScope> {
        self.inherited.iter().map(|(class, _)| class.into()).chain(
            Self::bfs_iter(self.inherited.iter().flat_map(|(_, interfaces)| interfaces))
                .map(ItemScope::from),
        )
    }

    pub fn bfs_iter<'a, It>(supports: It) -> impl Iterator<Item = InterfaceId>
    where
        It: Iterator<Item = &'a SupportedInterface>,
    {
        let mut queue: VecDeque<&SupportedInterface> = VecDeque::new();
        queue.extend(supports);

        std::iter::from_fn(move || {
            queue.pop_front().map(|node| {
                for child in &node.supports {
                    queue.push_back(child);
                }
                node.interface
            })
        })
    }
}

pub struct PredicateResolution {
    /// The predicate which is being resolved
    pub arity: Arity,
    pub from: PredicateFrom,
}

pub enum PredicateFrom {
    Class(ClassId),
    ClassAndRename { class: ClassId, new_name: crate::hir::Name },
}
