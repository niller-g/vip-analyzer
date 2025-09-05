//! AST -> `ItemTree` lowering code.

use super::{
    Class, ClassFact, ClassFactDb, ClassFactFunctor, ClassFactVar, ClassPredicate, ClassProperty,
    ClassSection, Clause, ClauseKind, Constant, Constructor, Delegate, Domain, DomainDef,
    ExternalPredicateResolution, Fact, FactDb, FactFunctor, FactVar, FactsSection, FileItemTreeId,
    Functor, Functors, GuardClause, Implement, ImplementSection, Interface, InterfaceDelegate,
    InterfaceSection, ItemTree, ItemTreeData, ModItem, Predicate, PredicateDelegate, PredicateFrom,
    PredicateResolution, Property, PropertyFrom, Resolution,
};
use crate::{
    db::DefDatabase,
    hir::{Name, namespaces::RootNs},
    item_tree::{InterfaceResolution, PredicateFromSource},
    nameres::{RootScopeRef, ScopeRef},
};
use la_arena::{Arena, Idx};
use span::{AstIdMap, FileId};
use syntax::ast::{
    self, HasClassSections, HasImplementSections, HasInterfaceSections, HasItems,
    HasOpenQualifications, HasScopeNameDecl, HasStringSeq, HasSupportsQualifications, HasType,
};

fn id<N>(index: Idx<N>) -> FileItemTreeId<N> {
    FileItemTreeId(index)
}

pub(super) fn lower_file_items<T: HasItems>(
    db: &dyn DefDatabase,
    file: FileId,
    item_owner: &T,
) -> Arena<ItemTree> {
    let mut res = Arena::new();

    let ast_id_map = AstIdMap::from_source(&db.parse(file).syntax_node());
    item_owner.items_recursively().for_each(|item| {
        let tree_id = res.alloc(ItemTree::default());
        res[tree_id].tree_id = Some(tree_id);

        let mut tree_ctx =
            FileItemsCtx { trees: &mut res, tree_id, source_ast_id_map: &ast_id_map };
        res[tree_id].top_level = tree_ctx.lower_item(&item);
        res[tree_id].shrink_to_fit();
    });

    res
}

struct FileItemsCtx<'a> {
    trees: &'a mut Arena<ItemTree>,
    tree_id: Idx<ItemTree>,
    source_ast_id_map: &'a AstIdMap,
}
impl FileItemsCtx<'_> {
    fn data(&mut self) -> &mut ItemTreeData {
        self.trees[self.tree_id].data_mut()
    }

    fn lower_item(&mut self, item: &ast::Item) -> Option<ModItem> {
        match item {
            ast::Item::ClassItem(ast) => self.lower_class_item(ast).map(ModItem::Class),
            ast::Item::ImplementItem(ast) => self.lower_implement_item(ast).map(ModItem::Implement),
            ast::Item::InterfaceItem(ast) => self.lower_interface_item(ast).map(ModItem::Interface),
            ast::Item::GoalItem(_ast) => None,
            ast::Item::NamespaceItem(ast) => ast
                .namespace()
                .and_then(|ns| ns.namespace_value())
                .map(RootNs::make_root_ns)
                .map(ModItem::Namespace),
            ast::Item::IncludeItem(ast) => ast.string_value().map(ModItem::Include),
            ast::Item::RequiresDirective(_ast) => None,
            ast::Item::ExportItem(_ast) => None,
            ast::Item::ExternallyItem(_ast) => None,
            ast::Item::ErrorDirective(_ast) => None,
            ast::Item::OptionsItem(_ast) => None,
            ast::Item::MessageDirective(_ast) => None,
        }
    }

    fn lower_implement_item(&mut self, item: &ast::ImplementItem) -> Option<Implement> {
        let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
        let inherits =
            item.inherits_qualifications().filter_map(|ast| ScopeRef::from_ast(&ast)).collect();
        let supports =
            item.supports_qualifications().filter_map(|ast| ScopeRef::from_ast(&ast)).collect();
        let opens = item.open_qualifications().filter_map(|it| it.try_into().ok()).collect();
        let sections =
            item.implement_sections().flat_map(|s| self.lower_implement_section(&s)).collect();
        Some(Implement { name, inherits, supports, opens, sections })
    }
    fn lower_class_item(&mut self, item: &ast::ClassItem) -> Option<Class> {
        let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
        let construction_ty = item.scope_ref().and_then(|ast| ScopeRef::from_ast(&ast));
        let opens = item.open_qualifications().filter_map(|it| it.try_into().ok()).collect();
        let sections = item.class_sections().flat_map(|s| self.lower_class_section(&s)).collect();
        Some(Class { name, construction_ty, opens, sections })
    }
    fn lower_interface_item(&mut self, item: &ast::InterfaceItem) -> Option<Interface> {
        let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
        let supports =
            item.supports_qualifications().filter_map(|ast| ScopeRef::from_ast(&ast)).collect();
        let opens = item.open_qualifications().filter_map(|it| it.try_into().ok()).collect();
        let sections =
            item.interface_sections().flat_map(|s| self.lower_interface_section(&s)).collect();
        Some(Interface { name, supports, opens, sections })
    }

    fn lower_implement_section(
        &mut self,
        section: &ast::ImplementSection,
    ) -> Option<ImplementSection> {
        match section {
            ast::ImplementSection::ClassFactsSection(ast) => {
                ImplementSection::ClassFacts(self.lower_class_facts_section(ast))
            }
            ast::ImplementSection::FactsSection(ast) => {
                ImplementSection::Facts(self.lower_facts_section(ast))
            }
            ast::ImplementSection::ClassPropertiesSection(ast) => {
                ImplementSection::ClassProperties(
                    self.lower_class_properties(ast.class_properties()),
                )
            }
            ast::ImplementSection::PropertiesSection(ast) => {
                ImplementSection::Properties(self.lower_properties(ast.properties()))
            }
            ast::ImplementSection::ClassPredicatesSection(ast) => {
                ImplementSection::ClassPredicates(
                    self.lower_class_predicates(ast.class_predicates()),
                )
            }
            ast::ImplementSection::PredicatesSection(ast) => {
                ImplementSection::Predicates(self.lower_predicates(ast.predicates()))
            }
            ast::ImplementSection::ConstructorsSection(ast) => {
                ImplementSection::Constructors(self.lower_constructors(ast.constructors()))
            }
            ast::ImplementSection::ClausesSection(ast) => {
                ImplementSection::Clauses(self.lower_clauses(ast))
            }
            ast::ImplementSection::ConstantsSection(ast) => {
                ImplementSection::Constants(self.lower_constants(ast))
            }
            ast::ImplementSection::DelegateSection(ast) => {
                ImplementSection::Delegates(self.lower_delegates(ast))
            }
            ast::ImplementSection::DomainsSection(ast) => {
                ImplementSection::Domains(self.lower_domains(ast))
            }
            ast::ImplementSection::ResolveSection(ast) => {
                ImplementSection::Resolve(self.lower_resolutions(ast))
            }
            ast::ImplementSection::ErrorDirective(_)
            | ast::ImplementSection::MessageDirective(_)
            | ast::ImplementSection::RequiresDirective(_) => return None,
        }
        .into()
    }

    fn lower_class_section(&mut self, section: &ast::ClassSection) -> Option<ClassSection> {
        match section {
            ast::ClassSection::PropertiesSection(ast) => {
                ClassSection::Properties(self.lower_properties(ast.properties()))
            }
            ast::ClassSection::PredicatesSection(ast) => {
                ClassSection::Predicates(self.lower_predicates(ast.predicates()))
            }
            ast::ClassSection::ConstructorsSection(ast) => {
                ClassSection::Constructors(self.lower_constructors(ast.constructors()))
            }
            ast::ClassSection::ConstantsSection(ast) => {
                ClassSection::Constants(self.lower_constants(ast))
            }
            ast::ClassSection::DomainsSection(ast) => {
                ClassSection::Domains(self.lower_domains(ast))
            }
            ast::ClassSection::ErrorDirective(_)
            | ast::ClassSection::MessageDirective(_)
            | ast::ClassSection::RequiresDirective(_) => return None,
        }
        .into()
    }

    fn lower_interface_section(
        &mut self,
        section: &ast::InterfaceSection,
    ) -> Option<InterfaceSection> {
        match section {
            ast::InterfaceSection::PropertiesSection(ast) => {
                InterfaceSection::Props(self.lower_properties(ast.properties()))
            }
            ast::InterfaceSection::PredicatesSection(ast) => {
                InterfaceSection::Preds(self.lower_predicates(ast.predicates()))
            }
            ast::InterfaceSection::ConstantsSection(ast) => {
                InterfaceSection::Constants(self.lower_constants(ast))
            }
            ast::InterfaceSection::DomainsSection(ast) => {
                InterfaceSection::Domains(self.lower_domains(ast))
            }
            ast::InterfaceSection::PredicatesFromSection(ast) => {
                InterfaceSection::PredicatesFrom(self.lower_predicates_from(ast))
            }
            ast::InterfaceSection::PropertiesFromSection(ast) => {
                InterfaceSection::PropertiesFrom(self.lower_properties_from(ast))
            }
            ast::InterfaceSection::ErrorDirective(_)
            | ast::InterfaceSection::MessageDirective(_)
            | ast::InterfaceSection::RequiresDirective(_) => return None,
        }
        .into()
    }

    fn lower_facts_section(&mut self, section: &ast::FactsSection) -> FactsSection<FactDb, Fact> {
        FactsSection {
            fact_db: section.ident_token().map(|tok| {
                let ast_id = self.source_ast_id_map.ast_id(section);
                let name = Name::new(tok.text());
                let fact_db = FactDb { name, ast_id };
                id(self.data().fact_dbs.alloc(fact_db))
            }),
            facts: section
                .facts()
                .filter_map(|f| match f {
                    ast::Fact::FactFunctor(f) => {
                        let name = Name::new(f.ident_token()?.text());
                        let ast_id = self.source_ast_id_map.ast_id(&f);
                        let fact_functor = FactFunctor::new(name, ast_id);
                        Some(Fact::FactFunctor(id(self.data().fact_functors.alloc(fact_functor))))
                    }
                    ast::Fact::FactVar(f) => {
                        let name = Name::new(f.ident_token()?.text());
                        let ast_id = self.source_ast_id_map.ast_id(&f);
                        let fact_var = FactVar::new(name, ast_id);
                        Some(Fact::FactVar(id(self.data().fact_vars.alloc(fact_var))))
                    }
                })
                .collect(),
        }
    }

    fn lower_class_facts_section(
        &mut self,
        section: &ast::ClassFactsSection,
    ) -> FactsSection<ClassFactDb, ClassFact> {
        FactsSection {
            fact_db: section.ident_token().map(|tok| {
                let ast_id = self.source_ast_id_map.ast_id(section);
                let name = Name::new(tok.text());
                let fact_db = ClassFactDb { name, ast_id };
                id(self.data().class_fact_dbs.alloc(fact_db))
            }),
            facts: section
                .class_facts()
                .filter_map(|f| match f {
                    ast::ClassFact::ClassFactFunctor(f) => {
                        let name = Name::new(f.ident_token()?.text());
                        let ast_id = self.source_ast_id_map.ast_id(&f);
                        let fact_functor = ClassFactFunctor::new(name, ast_id);
                        Some(ClassFact::ClassFactFunctor(id(self
                            .data()
                            .class_fact_functors
                            .alloc(fact_functor))))
                    }
                    ast::ClassFact::ClassFactVar(f) => {
                        let name = Name::new(f.ident_token()?.text());
                        let ast_id = self.source_ast_id_map.ast_id(&f);
                        let fact_var = ClassFactVar::new(name, ast_id);
                        Some(ClassFact::ClassFactVar(id(self
                            .data()
                            .class_fact_vars
                            .alloc(fact_var))))
                    }
                })
                .collect(),
        }
    }

    fn lower_properties<T: Iterator<Item = ast::Property>>(
        &mut self,
        properties: T,
    ) -> Vec<FileItemTreeId<Property>> {
        properties
            .filter_map(|p| {
                let ast_id = self.source_ast_id_map.ast_id(&p);
                let property = Property { name: Name::new(p.ident_token()?.text()), ast_id };
                Some(id(self.data().properties.alloc(property)))
            })
            .collect()
    }
    fn lower_class_properties<T: Iterator<Item = ast::ClassProperty>>(
        &mut self,
        properties: T,
    ) -> Vec<FileItemTreeId<ClassProperty>> {
        properties
            .filter_map(|p| {
                let ast_id = self.source_ast_id_map.ast_id(&p);
                let property = ClassProperty { name: Name::new(p.ident_token()?.text()), ast_id };
                Some(id(self.data().class_properties.alloc(property)))
            })
            .collect()
    }

    fn lower_predicates<T: Iterator<Item = ast::Predicate>>(
        &mut self,
        predicates: T,
    ) -> Vec<FileItemTreeId<Predicate>> {
        predicates
            .filter_map(|p| {
                let name = Name::new(p.ident_token()?.text());
                let ast_id = self.source_ast_id_map.ast_id(&p);
                Some(id(self.data().predicates.alloc(Predicate { name, ast_id })))
            })
            .collect()
    }
    fn lower_class_predicates<T: Iterator<Item = ast::ClassPredicate>>(
        &mut self,
        predicates: T,
    ) -> Vec<FileItemTreeId<ClassPredicate>> {
        predicates
            .filter_map(|p| {
                let name = Name::new(p.ident_token()?.text());
                let ast_id = self.source_ast_id_map.ast_id(&p);
                Some(id(self.data().class_predicates.alloc(ClassPredicate { name, ast_id })))
            })
            .collect()
    }
    fn lower_constructors<T: Iterator<Item = ast::Constructor>>(
        &mut self,
        predicates: T,
    ) -> Vec<FileItemTreeId<Constructor>> {
        predicates
            .filter_map(|p| {
                let name = Name::new(p.ident_token()?.text());
                let ast_id = self.source_ast_id_map.ast_id(&p);
                Some(id(self.data().constructors.alloc(Constructor { name, ast_id })))
            })
            .collect()
    }

    fn lower_clauses<T: IntoIterator<Item = ast::ClauseKind>>(
        &mut self,
        clauses: T,
    ) -> Vec<ClauseKind> {
        clauses
            .into_iter()
            .filter_map(|c| match c {
                ast::ClauseKind::Clause(clause) => {
                    let clause = Clause {
                        name: Name::new(clause.ident_token()?.text()),
                        ast_id: self.source_ast_id_map.ast_id(&clause),
                    };
                    Some(ClauseKind::Clause(id(self.data().clauses.alloc(clause))))
                }
                ast::ClauseKind::GuardClause(clause) => {
                    let clause = GuardClause {
                        name: Name::new(clause.ident_token()?.text()),
                        ast_id: self.source_ast_id_map.ast_id(&clause),
                    };
                    Some(ClauseKind::GuardClause(id(self.data().guard_clauses.alloc(clause))))
                }
            })
            .collect()
    }

    fn lower_constants<T: IntoIterator<Item = ast::Constant>>(
        &mut self,
        section: T,
    ) -> Vec<FileItemTreeId<Constant>> {
        section
            .into_iter()
            .filter_map(|constant| {
                let constant = Constant {
                    name: Name::new(constant.ident_token()?.text()),
                    ast_id: self.source_ast_id_map.ast_id(&constant),
                };
                Some(id(self.data().constants.alloc(constant)))
            })
            .collect()
    }

    fn lower_delegates<T: IntoIterator<Item = ast::Delegate>>(
        &mut self,
        delegates: T,
    ) -> Vec<Delegate> {
        delegates
            .into_iter()
            .flat_map(|d| match d {
                ast::Delegate::InterfaceDelegate(ast) => {
                    let ast_id = self.source_ast_id_map.ast_id(&ast);
                    let interface = ScopeRef::from_ast(&ast.scope_ref()?)?;
                    let to = ast.ident_token()?;
                    let delegate =
                        InterfaceDelegate { ast_id, interface, to: Name::new(to.text()) };
                    Some(Delegate::InterfaceDelegate(id(self
                        .data()
                        .interface_delegates
                        .alloc(delegate))))
                }
                ast::Delegate::PredicateDelegate(ast) => {
                    let ast_id = self.source_ast_id_map.ast_id(&ast);
                    let pred_arity = ast.arity()?.try_into().ok()?;
                    let to = ast.ident_token()?;
                    let delegate =
                        PredicateDelegate { ast_id, pred_arity, to: Name::new(to.text()) };
                    Some(Delegate::PredicateDelegate(id(self
                        .data()
                        .predicate_delegates
                        .alloc(delegate))))
                }
            })
            .collect()
    }

    fn lower_domains<T: IntoIterator<Item = ast::Domain>>(
        &mut self,
        domains: T,
    ) -> Vec<(FileItemTreeId<Domain>, Functors)> {
        domains
            .into_iter()
            .filter_map(|dom| {
                let domain_def = self.lower_domain_def(dom.domain_def()?);

                let functors = match domain_def {
                    DomainDef::Alias => Box::new([]),
                    DomainDef::Functors(ref functors) => functors.clone(),
                    DomainDef::Numeric => Box::new([]),
                    DomainDef::PredDomain => Box::new([]),
                };

                let domain = Domain {
                    name: Name::new(dom.ident_token()?.text()),
                    ast_id: self.source_ast_id_map.ast_id(&dom),
                };
                Some((id(self.data().domains.alloc(domain)), functors))
            })
            .collect()
    }

    fn lower_domain_def(&mut self, domain_def: ast::DomainDef) -> DomainDef {
        match domain_def {
            ast::DomainDef::AliasDomain(_) => DomainDef::Alias,
            ast::DomainDef::FunctorDomain(ast) => {
                let functors = ast.functors().filter_map(|functor| {
                    let tok = match functor.ty()? {
                        ast::TypeKind::RefTerm(ref_term) => {
                            assert!(ref_term.scope_ref().is_none());
                            ref_term.ident_token()?
                        }
                        _ => unreachable!(),
                    };

                    let name = Name::new(tok.text());
                    let functor = Functor { name, ast_id: self.source_ast_id_map.ast_id(&functor) };

                    Some(id(self.data().functors.alloc(functor)))
                });

                DomainDef::Functors(functors.collect())
            }
            ast::DomainDef::NumericDomain(_) => DomainDef::Numeric,
            ast::DomainDef::PredicateDomain(_) => DomainDef::PredDomain,
        }
    }

    fn lower_resolutions<T: IntoIterator<Item = ast::Resolution>>(
        &mut self,
        resolutions: T,
    ) -> Vec<Resolution> {
        resolutions
            .into_iter()
            .filter_map(|r| match r {
                ast::Resolution::InterfaceResolution(r) => {
                    let ast_id = self.source_ast_id_map.ast_id(&r);
                    let resolve_interface = ScopeRef::from_ast(&r.interface_ref()?.scope_ref()?)?;
                    let from = ScopeRef::from_ast(&r.scope_ref()?)?;
                    let res = InterfaceResolution { resolve_interface, from, ast_id };
                    Some(Resolution::InterfaceResolution(id(self
                        .data()
                        .interface_resolutions
                        .alloc(res))))
                }
                ast::Resolution::PredicateResolution(r) => {
                    let pred_arity = r.arity()?.try_into().ok()?;

                    let ref_term = r.ref_term()?;
                    let from = match ref_term.coloncolon_token() {
                        Some(_) => PredicateFromSource::ClassAndRename(
                            ScopeRef::from_ast(&ref_term.scope_ref()?)?,
                            Name::new(ref_term.ident_token()?.text()),
                        ),
                        None => {
                            PredicateFromSource::Class(ScopeRef::from_ast(&ref_term.scope_ref()?)?)
                        }
                    };

                    let ast_id = self.source_ast_id_map.ast_id(&r);
                    let res = PredicateResolution { pred_arity, from, ast_id };
                    Some(Resolution::PredResolution(id(self
                        .data()
                        .predicate_resolutions
                        .alloc(res))))
                }
                ast::Resolution::ExternalPredicateResolution(r) => {
                    let ast_id = self.source_ast_id_map.ast_id(&r);
                    let pred_arity = r.arity()?.try_into().ok()?;
                    let res = ExternalPredicateResolution { pred_arity, ast_id };
                    Some(Resolution::PredResolutionExternal(id(self
                        .data()
                        .predicate_resolutions_external
                        .alloc(res))))
                }
            })
            .collect()
    }

    fn lower_predicates_from(
        &mut self,
        section: &ast::PredicatesFromSection,
    ) -> Vec<FileItemTreeId<PredicateFrom>> {
        let Some(from): Option<ScopeRef> =
            section.scope_ref().and_then(|ast| ScopeRef::from_ast(&ast))
        else {
            return vec![];
        };

        section
            .into_iter()
            .filter_map(|arity| {
                let ast_id = self.source_ast_id_map.ast_id(&arity);
                let arity = arity.try_into().ok()?;
                let pred_from = PredicateFrom { from: from.clone(), arity, ast_id };
                let p = id(self.data().predicates_from.alloc(pred_from));
                Some(p)
            })
            .collect()
    }

    fn lower_properties_from(
        &mut self,
        section: &ast::PropertiesFromSection,
    ) -> Vec<FileItemTreeId<PropertyFrom>> {
        let Some(from): Option<ScopeRef> =
            section.scope_ref().and_then(|ast| ScopeRef::from_ast(&ast))
        else {
            return vec![];
        };

        section
            .into_iter()
            .filter_map(|arity| {
                let ast_id = self.source_ast_id_map.ast_id(&arity);
                let arity = arity.try_into().ok()?;
                let prop_from = PropertyFrom { from: from.clone(), arity, ast_id };
                let p = id(self.data().properties_from.alloc(prop_from));
                Some(p)
            })
            .collect()
    }
}
