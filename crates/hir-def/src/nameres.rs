mod collector;

#[cfg(test)]
mod tests;

use crate::{
    ClassId, ImplementId, InterfaceId, InterfaceResolutionId, ItemDefId, PredicateResolutionId,
    db::DefDatabase,
    hir::{
        Name,
        namespaces::{NsPath, RootNs, RootNsPath},
    },
    item_tree::{
        Class, ClassSection, Implement, ImplementSection, Interface, InterfaceSection,
        OpenQualification,
    },
};
use base_db::Project;
use la_arena::{Arena, Idx};
use rustc_hash::{FxHashMap, FxHashSet};
use span::FileId;
use std::{iter, mem};
use stdx::itertools::EitherOrBoth;
use syntax::{AstNode, Direction, ast};

/// Common data shared by all scope types (implement, class, interface)
#[derive(Debug, PartialEq, Eq)]
pub struct BaseScopeData {
    /// The file this item is defined in.
    pub file_id: FileId,
    /// The name of the scope.
    pub name: RootScopeRef,

    /// The open qualifications declared in this scope.
    unresolved_opens: Box<[OpenQualification]>,
    /// The opened namespace paths after partitioning the unresolved opens.
    opened_ns_paths: Box<[NsPath]>,
    /// The opened root namespaces after partitioning the unresolved opens.
    opened_root_nss: Box<[RootNs]>,
    /// The opened classes after resolving the opens.
    /// This includes the related class if this is an implement.
    opened_classes: Box<[ClassId]>,
    /// The opened interfaces after resolving the opens.
    opened_interfaces: Box<[InterfaceId]>,

    /// The defs declared in this scope.
    pub declarations: Vec<ItemDefId>,
}

impl BaseScopeData {
    fn new(
        file_id: FileId,
        name: RootScopeRef,
        unresolved_opens: Box<[OpenQualification]>,
    ) -> Self {
        Self {
            file_id,
            name,
            unresolved_opens,
            opened_ns_paths: Default::default(),
            opened_root_nss: Default::default(),
            opened_classes: Default::default(),
            opened_interfaces: Default::default(),
            declarations: Vec::default(),
        }
    }

    /// Declare a def in this scope.
    fn declare(&mut self, def: ItemDefId) {
        self.declarations.push(def)
    }

    #[inline]
    fn partition_openings(
        &mut self,
        find_scope: impl Fn(&RootScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>>,
        namespaces: &FxHashSet<RootNs>,
    ) {
        self.partition_openings_with(find_scope, namespaces, None)
    }

    fn partition_openings_with(
        &mut self,
        find_scope: impl Fn(&RootScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>>,
        namespaces: &FxHashSet<RootNs>,
        related_class: Option<ClassId>,
    ) {
        let (opened_ns_paths, opened_root_nss, opened_scopes) = partition_openings(
            mem::take(&mut self.unresolved_opens),
            |s| find_scope(s).is_some(),
            namespaces,
        );

        self.opened_ns_paths = opened_ns_paths.into();
        self.opened_root_nss = opened_root_nss.into();

        let mut opened_classes = Vec::new();
        let mut opened_interfaces = Vec::new();
        for opened_scope in opened_scopes {
            self.absolutize(opened_scope).into_iter().find_map(|candidate| {
                find_scope(&candidate).map(|resolved| match resolved {
                    EitherOrBoth::Both(class, interface) => {
                        opened_classes.push(class);
                        opened_interfaces.push(interface);
                    }
                    EitherOrBoth::Left(class) => {
                        opened_classes.push(class);
                    }
                    EitherOrBoth::Right(interface) => {
                        opened_interfaces.push(interface);
                    }
                })
            });
        }

        if let Some(class) = related_class {
            opened_classes.push(class);
        }
        self.opened_classes = opened_classes.into();
        self.opened_interfaces = opened_interfaces.into();
    }

    fn absolutize(&self, relative: ScopeRef) -> Vec<RootScopeRef> {
        match &relative.ns_path {
            NsPath::Root(_) => {
                vec![relative.into()]
            }
            NsPath::Relative(path) => {
                let mut candidates = Vec::new();
                let mut seen = FxHashSet::default();
                let mut insert = |elem: RootScopeRef| {
                    if seen.insert(elem.clone()) {
                        candidates.push(elem);
                    }
                };

                for open in &self.opened_ns_paths {
                    let root = match open {
                        NsPath::Root(open) => path.prefix(open.clone()),
                        NsPath::Relative(open) => path.prefix(open.into()),
                    };
                    insert(relative.set_root(root));
                }

                for root in self
                    .opened_root_nss
                    .iter()
                    .map(RootNsPath::from)
                    .chain(iter::once(self.name.ns_path.clone()))
                {
                    if root.ends_with(path) {
                        insert(relative.set_root(root));
                    } else {
                        insert(relative.set_root(path.prefix(root)));
                    }
                }
                insert(relative.into());

                candidates
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImplementData {
    /// Common scope data
    base: BaseScopeData,

    /// The classes this implement inherits from.
    /// These are yet to be resolved in the name resolution phase.
    unresolved_inherits: Box<[ScopeRef]>,
    /// The classes this implement inherits from.
    /// These are the result of the name resolution phase.
    inherits: Box<[ClassId]>,
    /// The interfaces this implement supports.
    /// These are yet to be resolved in the name resolution phase.
    unresolved_supports: Box<[ScopeRef]>,
    /// The interfaces this implement supports.
    /// These are the result of the name resolution phase.
    /// The first element is the construction type of the related class if that class constructs objects.
    supports: Box<[InterfaceId]>,
    /// The sections of the item.
    sections: Box<[ImplementSection]>,
}
impl ImplementData {
    pub fn from(file_id: FileId, implement: Implement) -> Self {
        Self {
            base: BaseScopeData::new(file_id, implement.name, implement.opens),
            unresolved_inherits: implement.inherits,
            inherits: Default::default(),
            unresolved_supports: implement.supports,
            supports: Default::default(),
            sections: implement.sections,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.base.file_id
    }

    pub fn name(&self) -> &RootScopeRef {
        &self.base.name
    }

    /// Declare a def in this scope.
    fn declare(&mut self, def: ItemDefId) {
        self.base.declare(def)
    }

    /// Get the defs declared in this scope.
    pub fn declarations(&self) -> &Vec<ItemDefId> {
        &self.base.declarations
    }

    pub fn inherits(&self) -> &[ClassId] {
        &self.inherits
    }

    pub fn interface_resolutions(&self) -> impl Iterator<Item = &InterfaceResolutionId> {
        self.base.declarations.iter().filter_map(|def| {
            if let ItemDefId::InterfaceResolutionId(resolution) = def {
                Some(resolution)
            } else {
                None
            }
        })
    }

    pub fn predicate_resolutions(&self) -> impl Iterator<Item = &PredicateResolutionId> {
        self.base.declarations.iter().filter_map(|def| {
            if let ItemDefId::PredicateResolutionId(resolution) = def {
                Some(resolution)
            } else {
                None
            }
        })
    }

    pub fn opened_classes(&self) -> &[ClassId] {
        &self.base.opened_classes
    }

    pub fn opened_interfaces(&self) -> &[InterfaceId] {
        &self.base.opened_interfaces
    }

    pub(crate) fn resolve_inherits(&mut self, resolver: impl Fn(&RootScopeRef) -> Option<ClassId>) {
        let mut inherits = Vec::with_capacity(self.unresolved_inherits.len());
        for inherit in std::mem::take(&mut self.unresolved_inherits) {
            self.base
                .absolutize(inherit)
                .into_iter()
                .find_map(|candidate| resolver(&candidate).map(|resolved| inherits.push(resolved)));
        }
        self.inherits = inherits.into();
    }

    pub fn supports(&self) -> &[InterfaceId] {
        &self.supports
    }

    pub(crate) fn resolve_supports(
        &mut self,
        resolve: impl Fn(&RootScopeRef) -> Option<InterfaceId>,
        object_type: Option<InterfaceId>,
    ) {
        let mut supports;
        match object_type {
            Some(interface) => {
                supports = Vec::with_capacity(self.unresolved_supports.len() + 1);
                supports.push(interface)
            }
            None => {
                supports = Vec::with_capacity(self.unresolved_supports.len());
            }
        }
        for support in std::mem::take(&mut self.unresolved_supports) {
            self.base
                .absolutize(support)
                .into_iter()
                .find_map(|candidate| resolve(&candidate).map(|resolved| supports.push(resolved)));
        }
        self.supports = supports.into();
    }

    pub fn partition_openings(
        &mut self,
        find_scope: impl Fn(&RootScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>>,
        namespace_lookup: &FxHashSet<RootNs>,
        related_class: Option<ClassId>,
    ) {
        self.base.partition_openings_with(find_scope, namespace_lookup, related_class);
    }

    pub fn absolutize(&self, relative: ScopeRef) -> Vec<RootScopeRef> {
        self.base.absolutize(relative)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassData {
    /// Common scope data
    base: BaseScopeData,

    /// The interface of the object this class constructs.
    /// This is yet to be resolved in the name resolution phase.
    unresolved_construction_interface: Option<ScopeRef>,
    /// The interface of the object this class constructs.
    /// This is the result of the name resolution phase.
    construction_interface: Option<InterfaceId>,
    /// The sections of the item.
    sections: Box<[ClassSection]>,
}
impl ClassData {
    pub fn from(file_id: FileId, class: Class) -> Self {
        Self {
            base: BaseScopeData::new(file_id, class.name, class.opens),
            unresolved_construction_interface: class.construction_ty,
            construction_interface: None,
            sections: class.sections,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.base.file_id
    }

    pub fn name(&self) -> &RootScopeRef {
        &self.base.name
    }

    /// Declare a def in this scope.
    fn declare(&mut self, def: ItemDefId) {
        self.base.declare(def)
    }

    /// Get the defs declared in this scope.
    pub fn declarations(&self) -> &Vec<ItemDefId> {
        &self.base.declarations
    }

    pub fn opened_classes(&self) -> &[ClassId] {
        &self.base.opened_classes
    }

    pub fn opened_interfaces(&self) -> &[InterfaceId] {
        &self.base.opened_interfaces
    }

    /// Whether this class constructs an object.
    pub fn constructs_object(&self) -> bool {
        self.construction_interface.is_some()
    }

    pub fn construction_interface(&self) -> Option<InterfaceId> {
        self.construction_interface
    }

    /// The supported interfaces of this class.
    /// This is exactly the same as the construction interface wrapped in a slice.
    pub fn supports(&self) -> &[InterfaceId] {
        match &self.construction_interface {
            Some(interface) => std::slice::from_ref(interface),
            None => &[],
        }
    }

    pub fn resolve_construction_interface(
        &mut self,
        resolver: impl Fn(&RootScopeRef) -> Option<InterfaceId>,
    ) {
        if let Some(interface) = self.unresolved_construction_interface.take() {
            self.base.absolutize(interface).into_iter().find_map(|candidate| {
                resolver(&candidate).map(|resolved| self.construction_interface = Some(resolved))
            });
        }
    }

    pub fn partition_openings(
        &mut self,
        resolve_scope: impl Fn(&RootScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>>,
        namespace_lookup: &FxHashSet<RootNs>,
    ) {
        self.base.partition_openings(resolve_scope, namespace_lookup);
    }

    pub fn absolutize(&self, relative: ScopeRef) -> Vec<RootScopeRef> {
        self.base.absolutize(relative)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InterfaceData {
    /// Common scope data
    base: BaseScopeData,

    /// The interfaces this interface supports.
    /// These are yet to be resolved in the name resolution phase.
    unresolved_supports: Box<[ScopeRef]>,
    /// The interfaces this interface supports.
    /// These are the result of the name resolution phase.
    supports: Box<[InterfaceId]>,
    /// The sections of the item.
    sections: Box<[InterfaceSection]>,
}

impl InterfaceData {
    pub fn from(file_id: FileId, interface: Interface) -> Self {
        Self {
            base: BaseScopeData::new(file_id, interface.name, interface.opens),
            unresolved_supports: interface.supports,
            supports: Default::default(),
            sections: interface.sections,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.base.file_id
    }

    pub fn name(&self) -> &RootScopeRef {
        &self.base.name
    }

    /// Declare a def in this scope.
    fn declare(&mut self, def: ItemDefId) {
        self.base.declare(def)
    }

    /// Get the defs declared in this scope.
    pub fn declarations(&self) -> &Vec<ItemDefId> {
        &self.base.declarations
    }

    pub fn opened_classes(&self) -> &[ClassId] {
        &self.base.opened_classes
    }

    pub fn opened_interfaces(&self) -> &[InterfaceId] {
        &self.base.opened_interfaces
    }

    pub fn supports(&self) -> &[InterfaceId] {
        &self.supports
    }

    pub fn resolve_supports(&mut self, resolver: impl Fn(&RootScopeRef) -> Option<InterfaceId>) {
        let mut supports = Vec::with_capacity(self.unresolved_supports.len());
        for support in std::mem::take(&mut self.unresolved_supports) {
            self.base
                .absolutize(support)
                .into_iter()
                .find_map(|candidate| resolver(&candidate).map(|resolved| supports.push(resolved)));
        }
        self.supports = supports.into();
    }

    pub fn partition_openings(
        &mut self,
        resolve_scope: impl Fn(&RootScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>>,
        namespace_lookup: &FxHashSet<RootNs>,
    ) {
        self.base.partition_openings(resolve_scope, namespace_lookup);
    }

    pub fn absolutize(&self, relative: ScopeRef) -> Vec<RootScopeRef> {
        self.base.absolutize(relative)
    }
}

fn partition_openings(
    opens: Box<[OpenQualification]>,
    scope_exists: impl Fn(&RootScopeRef) -> bool,
    namespaces: &FxHashSet<RootNs>,
) -> (Vec<NsPath>, Vec<RootNs>, Vec<ScopeRef>) {
    let mut opened_ns_paths = Vec::new();
    let mut opened_root_nss = Vec::new();
    let mut opened_scopes = Vec::new();

    for opening in opens {
        match opening {
            OpenQualification::Root => {}
            OpenQualification::NamespacePath(ns_path) => {
                opened_ns_paths.push(ns_path.clone());
                let root_ns = ns_path.into();
                if namespaces.contains(&root_ns) {
                    opened_root_nss.push(root_ns);
                }
            }
            OpenQualification::Ambiguous(scope_ref) => {
                if scope_exists(&scope_ref.clone().into()) {
                    opened_scopes.push(scope_ref);
                } else {
                    match RootNs::try_from_scope(&scope_ref) {
                        Some(root_ns) => {
                            if namespaces.contains(&root_ns) {
                                opened_root_nss.push(root_ns);
                            } else {
                                opened_ns_paths.extend(NsPath::try_from_scope(&scope_ref));
                                opened_scopes.push(scope_ref);
                            }
                        }
                        None => {
                            opened_scopes.push(scope_ref);
                        }
                    }
                }
            }
        }
    }

    (opened_ns_paths, opened_root_nss, opened_scopes)
}

/// Contains the results of (early) name resolution.
///
/// A `DefMap` stores the package tree and the definitions that are in scope in every package item
///
/// Every project has a primary `DefMap` whose root is the crate's main file (`main.rs`/`lib.rs`),
/// computed by the `crate_def_map` query. Additionally, every block expression introduces the
/// opportunity to write arbitrary item and module hierarchies, and thus gets its own `DefMap` that
/// is computed by the `block_def_map` query.
#[derive(Debug, PartialEq, Eq)]
pub struct DefMap {
    /// The project this [`DefMap`] belongs to.
    project: Project,
    /// The `implement` items and their data declared in this project.
    implementations: Arena<ImplementData>,
    implementation_lookup: FxHashMap<RootScopeRef, Idx<ImplementData>>,
    /// The `class` items and their data declared in this project.
    classes: Arena<ClassData>,
    class_lookup: FxHashMap<RootScopeRef, Idx<ClassData>>,
    /// The `interface` items and their data declared in this project.
    interfaces: Arena<InterfaceData>,
    interface_lookup: FxHashMap<RootScopeRef, Idx<InterfaceData>>,

    /// The namespaces that are declared in this project.
    namespace_lookup: FxHashSet<RootNs>,
}

#[salsa_macros::tracked(return_ref)]
pub fn project_def_map(db: &dyn DefDatabase, project_id: Project) -> DefMap {
    let _p = tracing::info_span!("project_def_map_query", project = ?project_id.extra_data(db).project_name.as_str()).entered();

    let def_map = DefMap::empty(project_id);
    collector::collect_defs(db, def_map)
}

impl DefMap {
    pub(crate) fn empty(project: Project) -> Self {
        Self {
            project,
            implementations: Arena::default(),
            implementation_lookup: FxHashMap::default(),
            classes: Arena::default(),
            class_lookup: FxHashMap::default(),
            interfaces: Arena::default(),
            interface_lookup: FxHashMap::default(),
            namespace_lookup: FxHashSet::default(),
        }
    }

    fn shrink_to_fit(&mut self) {
        let Self {
            project: _,
            implementations,
            implementation_lookup,
            classes,
            class_lookup,
            interfaces,
            interface_lookup,
            namespace_lookup,
        } = self;

        implementations.shrink_to_fit();
        implementation_lookup.shrink_to_fit();
        classes.shrink_to_fit();
        class_lookup.shrink_to_fit();
        interfaces.shrink_to_fit();
        interface_lookup.shrink_to_fit();
        namespace_lookup.shrink_to_fit();
    }
}

impl DefMap {
    pub fn implements_in_file(&self, file_id: FileId) -> impl Iterator<Item = ImplementId> + '_ {
        self.implementations
            .iter()
            .filter(move |(_id, data)| data.file_id() == file_id)
            .map(|(id, _data)| ImplementId { project: self.project, idx: id })
    }
    pub fn class_in_file(&self, file_id: FileId) -> impl Iterator<Item = ClassId> + '_ {
        self.classes
            .iter()
            .filter(move |(_id, data)| data.file_id() == file_id)
            .map(|(id, _data)| ClassId { project: self.project, idx: id })
    }
    pub fn interfaces_in_file(&self, file_id: FileId) -> impl Iterator<Item = InterfaceId> + '_ {
        self.interfaces
            .iter()
            .filter(move |(_id, data)| data.file_id() == file_id)
            .map(|(id, _data)| InterfaceId { project: self.project, idx: id })
    }

    pub fn lookup_class(&self, lookup: &RootScopeRef) -> Option<ClassId> {
        self.class_lookup.get(lookup).map(|id| ClassId::new(self.project, *id))
    }
    pub fn lookup_interface(&self, look_for: &RootScopeRef) -> Option<InterfaceId> {
        self.interface_lookup.get(look_for).map(|id| InterfaceId::new(self.project, *id))
    }
    pub fn lookup_implement(&self, look_for: &RootScopeRef) -> Option<ImplementId> {
        self.implementation_lookup.get(look_for).map(|id| ImplementId::new(self.project, *id))
    }
}
impl std::ops::Index<&ImplementId> for DefMap {
    type Output = ImplementData;

    fn index(&self, id: &ImplementId) -> &Self::Output {
        &self.implementations[id.idx]
    }
}
impl std::ops::Index<&ClassId> for DefMap {
    type Output = ClassData;

    fn index(&self, id: &ClassId) -> &Self::Output {
        &self.classes[id.idx]
    }
}
impl std::ops::Index<&InterfaceId> for DefMap {
    type Output = InterfaceData;

    fn index(&self, id: &InterfaceId) -> &Self::Output {
        &self.interfaces[id.idx]
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RootScopeRef {
    pub(crate) ns_path: RootNsPath,
    pub(crate) name: Name,
}
impl RootScopeRef {
    #[inline]
    pub fn new(ns_path: RootNsPath, name: Name) -> Self {
        Self { ns_path, name }
    }

    pub fn lower_scope_name_decl(ast: &ast::ScopeNameDecl) -> Option<RootScopeRef> {
        let name = ast.ident_token()?;

        let ns_decl = ast
            .syntax()
            .parent()
            .and_then(|p| p.siblings(Direction::Prev).find_map(ast::NamespaceItem::cast));
        let ns = ns_decl
            .and_then(|i| i.namespace())
            .and_then(|ns| ns.namespace_value())
            .map(RootNs::make_root_ns)
            .into();

        Some(RootScopeRef::new(ns, Name::new(name.text())))
    }
}
impl From<ScopeRef> for RootScopeRef {
    fn from(value: ScopeRef) -> Self {
        Self::new(value.ns_path.into(), value.name)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ScopeRef {
    /// The namespace in front of the scope name.
    pub(crate) ns_path: NsPath,
    /// The name of the referred scope.
    pub(crate) name: Name,
    pub(crate) generics_count: u8,
}
impl ScopeRef {
    #[inline]
    pub fn new(ns_path: NsPath, name: Name, generics_count: u8) -> Self {
        Self { ns_path, name, generics_count }
    }

    pub fn from_ast(ast: &ast::ScopeRef) -> Option<ScopeRef> {
        let name = Name::new(ast.ident_token()?.text());
        let ns_path = ast.namespace_path().into();
        let generics_count = ast.generics().iter().flat_map(|g| g.type_args()).count() as u8;

        Some(Self { ns_path, name, generics_count })
    }

    fn set_root(&self, root_path: RootNsPath) -> RootScopeRef {
        RootScopeRef::new(root_path, self.name.clone())
    }
}
