use super::{ClassData, DefMap, ImplementData, InterfaceData};
use crate::{
    ClassFactDbLoc, ClassFactFunctorLoc, ClassFactVarLoc, ClassId, ClassPredicateLoc,
    ClassPropertyLoc, ClauseLoc, ConstantLoc, ConstructorLoc, DomainLoc,
    ExternalPredicateResolutionLoc, FactDbLoc, FactFunctorLoc, FactVarLoc, FunctorLoc,
    GuardClauseLoc, ImplementId, InterfaceDelegateLoc, InterfaceId, InterfaceResolutionLoc, Intern,
    PredicateDelegateLoc, PredicateFromLoc, PredicateLoc, PredicateResolutionLoc, PropertyFromLoc,
    PropertyLoc,
    db::DefDatabase,
    item_tree::{
        ClassFact, ClassSection, ClauseKind, Delegate, Fact, ImplementSection, InterfaceSection,
        ItemTree, ModItem, Resolution,
    },
    nameres::RootScopeRef,
};
use rustc_hash::FxHashSet;
use span::FileId;
use stdx::itertools::EitherOrBoth;

pub(super) fn collect_defs(db: &dyn DefDatabase, def_map: DefMap) -> DefMap {
    let mut collector = DefCollector { db, def_map, visited_files: FxHashSet::default() };

    collector.collect_project();
    let mut def_map = collector.finish();

    def_map.shrink_to_fit();
    def_map
}

/// Walks the tree of packages items recursively
struct DefCollector<'a> {
    db: &'a dyn DefDatabase,
    def_map: DefMap,
    visited_files: FxHashSet<FileId>,
}
impl DefCollector<'_> {
    fn collect_project(&mut self) {
        let _p = tracing::info_span!("collect_project").entered();

        let project = self.def_map.project.extra_data(self.db);
        for file_id in project.packages() {
            self.collect_file(*file_id);
        }
    }

    fn collect_file(&mut self, file_id: FileId) {
        if self.visited_files.insert(file_id) {
            for (_, item_tree) in self.db.file_item_trees(file_id).iter() {
                ItemCollector { def_collector: self, item_tree, file_id }.collect();
            }
        }
    }

    fn finish(mut self) -> DefMap {
        let project = self.def_map.project;
        let class_table = &self.def_map.class_lookup;
        let interface_table = &self.def_map.interface_lookup;
        let namespace_lookup = &self.def_map.namespace_lookup;

        let find_class = |resolve: &RootScopeRef| -> Option<ClassId> {
            class_table.get(resolve).map(|id| ClassId { project, idx: *id })
        };
        let find_interface = |resolve: &RootScopeRef| -> Option<InterfaceId> {
            interface_table.get(resolve).map(|id| InterfaceId { project, idx: *id })
        };
        let find_scope = |s: &RootScopeRef| match (find_class(s), find_interface(s)) {
            (Some(c), Some(i)) => Some(EitherOrBoth::Both(c, i)),
            (Some(c), None) => Some(EitherOrBoth::Left(c)),
            (None, Some(i)) => Some(EitherOrBoth::Right(i)),
            (None, None) => None,
        };

        for (_, class) in self.def_map.classes.iter_mut() {
            class.partition_openings(find_scope, namespace_lookup);
            class.resolve_construction_interface(find_interface);
        }

        for (_, interface) in self.def_map.interfaces.iter_mut() {
            interface.partition_openings(find_scope, namespace_lookup);
            interface.resolve_supports(find_interface);
        }

        for (_, implement) in self.def_map.implementations.iter_mut() {
            let related_class = find_class(implement.name());
            let object_type =
                related_class.and_then(|id| self.def_map.classes[id.idx].construction_interface);
            implement.partition_openings(find_scope, namespace_lookup, related_class);
            implement.resolve_inherits(find_class);
            implement.resolve_supports(find_interface, object_type);
        }

        self.def_map
    }
}

/// Walks a single item, populating defs and imports
struct ItemCollector<'a, 'b> {
    def_collector: &'a mut DefCollector<'b>,
    item_tree: &'a ItemTree,
    /// The file of the current item
    file_id: FileId,
}

macro_rules! collect {
    ($self:ident, $loc:ident, $item:ident, $id:ident, $update_def:ident) => {
        let loc = $loc::new($id, $self.file_id, $self.item_tree.tree_id(), $item);
        $update_def($self.def_collector, loc.intern($self.def_collector.db).into());
    };
}

impl ItemCollector<'_, '_> {
    fn collect(&mut self) {
        let Some(item) = self.item_tree.top_level_item() else {
            return;
        };
        let def_map = &mut self.def_collector.def_map;

        match item {
            ModItem::Class(class) => {
                let id = def_map.classes.alloc(ClassData::from(self.file_id, class.clone()));
                def_map.class_lookup.insert(class.name.clone(), id);

                self.collect_class(ClassId::new(self.def_collector.def_map.project, id));
            }
            ModItem::Implement(implement) => {
                let id = def_map
                    .implementations
                    .alloc(ImplementData::from(self.file_id, implement.clone()));
                def_map.implementation_lookup.insert(implement.name.clone(), id);

                self.collect_implement(ImplementId::new(self.def_collector.def_map.project, id));
            }
            ModItem::Interface(interface) => {
                let id =
                    def_map.interfaces.alloc(InterfaceData::from(self.file_id, interface.clone()));
                def_map.interface_lookup.insert(interface.name.clone(), id);

                self.collect_interface(InterfaceId::new(self.def_collector.def_map.project, id));
            }
            ModItem::Include(val) => {
                let path = self.def_collector.db.resolve_path(val.into(), def_map.project);
                if let Some(file_id) = path {
                    self.def_collector.collect_file(file_id);
                } else {
                    tracing::warn!("Failed to resolve include path: {:?}", val);
                }
            }
            ModItem::Namespace(ns) => {
                def_map.namespace_lookup.insert(ns.clone());
            }
        }
    }

    fn collect_class(&mut self, id: ClassId) {
        let update_def = |def_collector: &mut DefCollector<'_>, def_id| {
            def_collector.def_map.classes[id.idx].declare(def_id);
        };

        for section in self.def_collector.def_map.classes[id.idx].sections.clone() {
            match section {
                ClassSection::Constructors(constructors) => {
                    for constructor in constructors {
                        collect!(self, ConstructorLoc, constructor, id, update_def);
                    }
                }
                ClassSection::Predicates(preds) => {
                    for pred in preds {
                        collect!(self, PredicateLoc, pred, id, update_def);
                    }
                }
                ClassSection::Constants(constants) => {
                    for constant in constants {
                        collect!(self, ConstantLoc, constant, id, update_def);
                    }
                }
                ClassSection::Properties(props) => {
                    for prop in props {
                        collect!(self, PropertyLoc, prop, id, update_def);
                    }
                }
                ClassSection::Domains(vec) => {
                    for (d, functors) in vec {
                        collect!(self, DomainLoc, d, id, update_def);
                        for functor in functors {
                            collect!(self, FunctorLoc, functor, id, update_def);
                        }
                    }
                }
            }
        }
    }

    fn collect_implement(&mut self, id: ImplementId) {
        let update_def = |def_collector: &mut DefCollector<'_>, decl_id| {
            def_collector.def_map.implementations[id.idx].declare(decl_id);
        };

        for section in self.def_collector.def_map.implementations[id.idx].sections.clone() {
            match section {
                ImplementSection::ClassFacts(section) => {
                    if let Some(f_db) = section.fact_db {
                        collect!(self, ClassFactDbLoc, f_db, id, update_def);
                    };
                    for fact in section.facts {
                        match fact {
                            ClassFact::ClassFactVar(f) => {
                                collect!(self, ClassFactVarLoc, f, id, update_def);
                            }
                            ClassFact::ClassFactFunctor(f) => {
                                collect!(self, ClassFactFunctorLoc, f, id, update_def);
                            }
                        }
                    }
                }
                ImplementSection::Facts(section) => {
                    if let Some(fact_db) = section.fact_db {
                        collect!(self, FactDbLoc, fact_db, id, update_def);
                    };
                    for fact in section.facts {
                        match fact {
                            Fact::FactVar(f) => {
                                collect!(self, FactVarLoc, f, id, update_def);
                            }
                            Fact::FactFunctor(f) => {
                                collect!(self, FactFunctorLoc, f, id, update_def);
                            }
                        }
                    }
                }
                ImplementSection::Properties(props) => {
                    for prop in props {
                        collect!(self, PropertyLoc, prop, id, update_def);
                    }
                }
                ImplementSection::ClassProperties(props) => {
                    for p in props {
                        collect!(self, ClassPropertyLoc, p, id, update_def);
                    }
                }
                ImplementSection::Constructors(constructors) => {
                    for c in constructors {
                        collect!(self, ConstructorLoc, c, id, update_def);
                    }
                }
                ImplementSection::Predicates(predicates) => {
                    for pred in predicates.into_iter() {
                        collect!(self, PredicateLoc, pred, id, update_def);
                    }
                }
                ImplementSection::ClassPredicates(preds) => {
                    for p in preds {
                        collect!(self, ClassPredicateLoc, p, id, update_def);
                    }
                }
                ImplementSection::Clauses(clauses) => {
                    for clause in clauses {
                        match clause {
                            ClauseKind::Clause(c) => {
                                collect!(self, ClauseLoc, c, id, update_def);
                            }
                            ClauseKind::GuardClause(g) => {
                                collect!(self, GuardClauseLoc, g, id, update_def);
                            }
                        }
                    }
                }
                ImplementSection::Constants(constants) => {
                    for c in constants {
                        collect!(self, ConstantLoc, c, id, update_def);
                    }
                }
                ImplementSection::Delegates(delegates) => {
                    for d in delegates {
                        match d {
                            Delegate::InterfaceDelegate(d) => {
                                collect!(self, InterfaceDelegateLoc, d, id, update_def);
                            }
                            Delegate::PredicateDelegate(d) => {
                                collect!(self, PredicateDelegateLoc, d, id, update_def);
                            }
                        }
                    }
                }
                ImplementSection::Domains(domains) => {
                    for (d, functors) in domains {
                        collect!(self, DomainLoc, d, id, update_def);
                        for f in functors {
                            collect!(self, FunctorLoc, f, id, update_def);
                        }
                    }
                }
                ImplementSection::Resolve(resolves) => {
                    for r in resolves {
                        match r {
                            Resolution::InterfaceResolution(r) => {
                                collect!(self, InterfaceResolutionLoc, r, id, update_def);
                            }
                            Resolution::PredResolution(r) => {
                                collect!(self, PredicateResolutionLoc, r, id, update_def);
                            }
                            Resolution::PredResolutionExternal(r) => {
                                collect!(self, ExternalPredicateResolutionLoc, r, id, update_def);
                            }
                        }
                    }
                }
            }
        }
    }

    fn collect_interface(&mut self, id: InterfaceId) {
        let update_def = |def_collector: &mut DefCollector<'_>, dec_id| {
            def_collector.def_map.interfaces[id.idx].declare(dec_id);
        };

        for section in self.def_collector.def_map.interfaces[id.idx].sections.clone() {
            match section {
                InterfaceSection::Constants(constants) => {
                    for c in constants {
                        collect!(self, ConstantLoc, c, id, update_def);
                    }
                }
                InterfaceSection::Domains(domains) => {
                    for (d, functors) in domains {
                        collect!(self, DomainLoc, d, id, update_def);
                        for f in functors {
                            collect!(self, FunctorLoc, f, id, update_def);
                        }
                    }
                }
                InterfaceSection::Preds(preds) => {
                    for p in preds {
                        collect!(self, PredicateLoc, p, id, update_def);
                    }
                }
                InterfaceSection::Props(props) => {
                    for p in props {
                        collect!(self, PropertyLoc, p, id, update_def);
                    }
                }
                InterfaceSection::PropertiesFrom(props_from) => {
                    for p in props_from {
                        collect!(self, PropertyFromLoc, p, id, update_def);
                    }
                }
                InterfaceSection::PredicatesFrom(preds_from) => {
                    for p in preds_from {
                        collect!(self, PredicateFromLoc, p, id, update_def);
                    }
                }
            }
        }
    }
}
