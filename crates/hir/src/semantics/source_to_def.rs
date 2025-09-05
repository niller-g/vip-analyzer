//! Maps *syntax* of various definitions to their semantic ids.
//!
//! This is a very interesting module, and, in some sense, can be considered the
//! heart of the IDE parts of rust-analyzer.
//!
//! This module solves the following problem:
//!
//! > Given a piece of syntax, find the corresponding semantic definition (def).
//!
//! This problem is a part of more-or-less every IDE feature implemented. Every
//! IDE functionality (like goto to definition), conceptually starts with a
//! specific cursor position in a file. Starting with this text offset, we first
//! figure out what syntactic construct are we at: is this a pattern, an
//! expression, an item definition.
//!
//! Knowing only the syntax gives us relatively little info. For example,
//! looking at the syntax of the function we can realize that it is a part of an
//! `impl` block, but we won't be able to tell what trait function the current
//! function overrides, and whether it does that correctly. For that, we need to
//! go from [`ast::Fn`] to [`crate::Function`], and that's exactly what this
//! module does.
//!
//! As syntax trees are values and don't know their place of origin/identity,
//! this module also requires [`InFile`] wrappers to understand which specific
//! real or macro-expanded file the tree comes from.
//!
//! The actual algorithm to resolve syntax to def is curious in two aspects:
//!
//! * It is recursive
//! * It uses the inverse algorithm (what is the syntax for this def?)
//!
//! Specifically, the algorithm goes like this:
//!
//! 1. Find the syntactic container for the syntax. For example, field's
//!    container is the struct, and structs container is a module.
//! 2. Recursively get the def corresponding to container.
//! 3. Ask the container def for all child defs. These child defs contain
//!    the answer and answer's siblings.
//! 4. For each child def, ask for it's source.
//! 5. The child def whose source is the syntax node we've started with
//!    is the answer.
//!
//! It's interesting that both Roslyn and Kotlin contain very similar code
//! shape.
//!
//! Let's take a look at Roslyn:
//!
//!   <https://github.com/dotnet/roslyn/blob/36a0c338d6621cc5fe34b79d414074a95a6a489c/src/Compilers/CSharp/Portable/Compilation/SyntaxTreeSemanticModel.cs#L1403-L1429>
//!   <https://sourceroslyn.io/#Microsoft.CodeAnalysis.CSharp/Compilation/SyntaxTreeSemanticModel.cs,1403>
//!
//! The `GetDeclaredType` takes `Syntax` as input, and returns `Symbol` as
//! output. First, it retrieves a `Symbol` for parent `Syntax`:
//!
//! * <https://sourceroslyn.io/#Microsoft.CodeAnalysis.CSharp/Compilation/SyntaxTreeSemanticModel.cs,1423>
//!
//! Then, it iterates parent symbol's children, looking for one which has the
//! same text span as the original node:
//!
//!   <https://sourceroslyn.io/#Microsoft.CodeAnalysis.CSharp/Compilation/SyntaxTreeSemanticModel.cs,1786>
//!
//! Now, let's look at Kotlin:
//!
//!   <https://github.com/JetBrains/kotlin/blob/a288b8b00e4754a1872b164999c6d3f3b8c8994a/idea/idea-frontend-fir/idea-fir-low-level-api/src/org/jetbrains/kotlin/idea/fir/low/level/api/FirModuleResolveStateImpl.kt#L93-L125>
//!
//! This function starts with a syntax node (`KtExpression` is syntax, like all
//! `Kt` nodes), and returns a def. It uses
//! `getNonLocalContainingOrThisDeclaration` to get syntactic container for a
//! current node. Then, `findSourceNonLocalFirDeclaration` gets `Fir` for this
//! parent. Finally, `findElementIn` function traverses `Fir` children to find
//! one with the same source we originally started with.
//!
//! One question is left though -- where does the recursion stops? This happens
//! when we get to the file syntax node, which doesn't have a syntactic parent.
//! In that case, we loop through all the crates that might contain this file
//! and look for a module whose source is the given file.
//!
//! Note that the logic in this module is somewhat fundamentally imprecise --
//! due to conditional compilation and `#[path]` attributes, there's no
//! injective mapping from syntax nodes to defs. This is not an edge case --
//! more or less every item in a `lib.rs` is a part of two distinct crates: a
//! library with `--cfg test` and a library without.
//!
//! At the moment, we don't really handle this well and return the first answer
//! that works. Ideally, we should first let the caller to pick a specific
//! active crate for a given position, and then provide an API to resolve all
//! syntax nodes against this specific crate.

use crate::{db::HirDatabase, semantics::child_by_source::ChildBySource};
use hir_def::{
    ClassFactDbId, ClassFactFunctorId, ClassFactVarId, ClassId, ClassPredicateId, ClassPropertyId,
    ClauseId, ConstantId, ConstructorId, DefWithBodyId, DomainId, FactDbId, FactFunctorId,
    FactVarId, FunctorId, GuardClauseId, ImplementId, InterfaceId, PredicateId, PropertyId,
    dyn_map::{
        DynMap,
        keys::{self, Key},
    },
    nameres::{RootScopeRef, project_def_map},
    src::InFile,
};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use span::FileId;
use stdx::impl_from;
use syntax::{
    AstNode, AstPtr, SyntaxNode,
    ast::{self, HasScopeNameDecl},
};

#[derive(Default)]
pub(super) struct SourceToDefCache {
    pub(super) dynmap_cache: FxHashMap<(ChildContainer, FileId), DynMap>,
    pub(super) file_to_implementation: FxHashMap<FileId, SmallVec<[ImplementId; 1]>>,
    pub(super) file_to_class: FxHashMap<FileId, SmallVec<[ClassId; 1]>>,
    pub(super) file_to_interface: FxHashMap<FileId, SmallVec<[InterfaceId; 1]>>,
    /// Rootnode to FileId cache
    pub(super) root_to_file_cache: FxHashMap<SyntaxNode, FileId>,
}
impl SourceToDefCache {
    pub(super) fn cache(
        root_to_file_cache: &mut FxHashMap<SyntaxNode, FileId>,
        root_node: SyntaxNode,
        file_id: FileId,
    ) {
        assert!(root_node.parent().is_none());
        let prev = root_to_file_cache.insert(root_node, file_id);
        assert!(prev.is_none() || prev == Some(file_id));
    }
}

pub(super) struct SourceToDefCtx<'db, 'cache> {
    pub(super) db: &'db dyn HirDatabase,
    pub(super) cache: &'cache mut SourceToDefCache,
}

impl SourceToDefCtx<'_, '_> {
    pub(super) fn matching_implements(
        &mut self,
        src: InFile<&ast::ImplementItem>,
    ) -> SmallVec<[ImplementId; 1]> {
        let implements =
            self.cache.file_to_implementation.entry(src.file_id).or_insert_with(|| {
                let mut res = SmallVec::new();
                for &project_id in self.db.all_projects().iter() {
                    let def_map = project_def_map(self.db, project_id);
                    res.extend(def_map.implements_in_file(src.file_id));
                }
                res
            });

        let Some(name) =
            src.value.scope_name_decl().as_ref().and_then(RootScopeRef::lower_scope_name_decl)
        else {
            return SmallVec::new();
        };
        implements
            .iter()
            .filter(|item| {
                let def_map = project_def_map(self.db, item.project);
                let data = &def_map[*item];
                &name == data.name()
            })
            .copied()
            .collect()
    }

    pub(super) fn implement_to_def(
        &mut self,
        src: InFile<&ast::ImplementItem>,
    ) -> Option<ImplementId> {
        self.matching_implements(src).first().copied()
    }

    pub(super) fn matching_classes(
        &mut self,
        src: InFile<&ast::ClassItem>,
    ) -> SmallVec<[ClassId; 1]> {
        let classes = self.cache.file_to_class.entry(src.file_id).or_insert_with(|| {
            let mut res = SmallVec::new();
            for &project_id in self.db.all_projects().iter() {
                let def_map = project_def_map(self.db, project_id);
                res.extend(def_map.class_in_file(src.file_id));
            }
            res
        });

        let Some(name) =
            src.value.scope_name_decl().as_ref().and_then(RootScopeRef::lower_scope_name_decl)
        else {
            return SmallVec::new();
        };
        classes
            .iter()
            .filter(|item| {
                let def_map = project_def_map(self.db, item.project);
                let data = &def_map[*item];
                &name == data.name()
            })
            .copied()
            .collect()
    }

    pub(super) fn class_to_def(&mut self, src: InFile<&ast::ClassItem>) -> Option<ClassId> {
        self.matching_classes(src).first().copied()
    }

    pub(super) fn matching_interfaces(
        &mut self,
        src: InFile<&ast::InterfaceItem>,
    ) -> SmallVec<[InterfaceId; 1]> {
        let interfaces = self.cache.file_to_interface.entry(src.file_id).or_insert_with(|| {
            let mut res = SmallVec::new();
            for &project_id in self.db.all_projects().iter() {
                let def_map = project_def_map(self.db, project_id);
                res.extend(def_map.interfaces_in_file(src.file_id));
            }
            res
        });

        let Some(name) =
            src.value.scope_name_decl().as_ref().and_then(RootScopeRef::lower_scope_name_decl)
        else {
            return SmallVec::new();
        };
        interfaces
            .iter()
            .filter(|item| {
                let def_map = project_def_map(self.db, item.project);
                let data = &def_map[*item];
                &name == data.name()
            })
            .copied()
            .collect()
    }

    pub(super) fn interface_to_def(
        &mut self,
        src: InFile<&ast::InterfaceItem>,
    ) -> Option<InterfaceId> {
        self.matching_interfaces(src).first().copied()
    }

    pub(super) fn predicate_to_def(&mut self, src: InFile<&ast::Predicate>) -> Option<PredicateId> {
        self.to_def(src, keys::PREDICATE)
    }
    pub(super) fn class_predicate_to_def(
        &mut self,
        src: InFile<&ast::ClassPredicate>,
    ) -> Option<ClassPredicateId> {
        self.to_def(src, keys::CLASS_PREDICATE)
    }
    pub(super) fn constructor_to_def(
        &mut self,
        src: InFile<&ast::Constructor>,
    ) -> Option<ConstructorId> {
        self.to_def(src, keys::CONSTRUCTOR)
    }
    pub(super) fn fact_db_to_def(&mut self, src: InFile<&ast::FactsSection>) -> Option<FactDbId> {
        self.to_def(src, keys::FACT_DB)
    }
    pub(super) fn class_fact_db_to_def(
        &mut self,
        src: InFile<&ast::ClassFactsSection>,
    ) -> Option<ClassFactDbId> {
        self.to_def(src, keys::CLASS_FACT_DB)
    }
    pub(super) fn fact_functor_to_def(
        &mut self,
        src: InFile<&ast::FactFunctor>,
    ) -> Option<FactFunctorId> {
        self.to_def(src, keys::FACT_FUNCTOR)
    }
    pub(super) fn class_fact_functor_to_def(
        &mut self,
        src: InFile<&ast::ClassFactFunctor>,
    ) -> Option<ClassFactFunctorId> {
        self.to_def(src, keys::CLASS_FACT_FUNCTOR)
    }
    pub(super) fn fact_var_to_def(&mut self, src: InFile<&ast::FactVar>) -> Option<FactVarId> {
        self.to_def(src, keys::FACT_VAR)
    }
    pub(super) fn class_fact_var_to_def(
        &mut self,
        src: InFile<&ast::ClassFactVar>,
    ) -> Option<ClassFactVarId> {
        self.to_def(src, keys::CLASS_FACT_VAR)
    }
    pub(super) fn property_to_def(&mut self, src: InFile<&ast::Property>) -> Option<PropertyId> {
        self.to_def(src, keys::PROPERTY)
    }
    pub(super) fn class_property_to_def(
        &mut self,
        src: InFile<&ast::ClassProperty>,
    ) -> Option<ClassPropertyId> {
        self.to_def(src, keys::CLASS_PROPERTY)
    }
    pub(super) fn constant_to_def(&mut self, src: InFile<&ast::Constant>) -> Option<ConstantId> {
        self.to_def(src, keys::CONSTANT)
    }
    pub(super) fn domain_to_def(&mut self, src: InFile<&ast::Domain>) -> Option<DomainId> {
        self.to_def(src, keys::DOMAIN)
    }
    pub(super) fn functor_to_def(&mut self, src: InFile<&ast::Functor>) -> Option<FunctorId> {
        self.to_def(src, keys::FUNCTOR)
    }
    pub(super) fn clause_to_def(&mut self, src: InFile<&ast::Clause>) -> Option<ClauseId> {
        self.to_def(src, keys::CLAUSE)
    }
    pub(super) fn guard_clause_to_def(
        &mut self,
        src: InFile<&ast::GuardClause>,
    ) -> Option<GuardClauseId> {
        self.to_def(src, keys::GUARD_CLAUSE)
    }

    fn to_def<Ast: AstNode + 'static, ID: Copy + 'static>(
        &mut self,
        src: InFile<&Ast>,
        key: Key<Ast, ID>,
    ) -> Option<ID> {
        self.dyn_map(src)?[key].get(&AstPtr::new(src.value)).copied()
    }

    fn dyn_map<Ast: AstNode + 'static>(&mut self, src: InFile<&Ast>) -> Option<&DynMap> {
        let container = self.find_container(src.map(|it| it.syntax()))?;
        Some(self.cache_for(container, src.file_id))
    }

    fn cache_for(&mut self, container: ChildContainer, file_id: FileId) -> &DynMap {
        let db = self.db;
        self.cache
            .dynmap_cache
            .entry((container, file_id))
            .or_insert_with(|| container.child_by_source(db, file_id))
    }

    pub(super) fn find_container(&mut self, src: InFile<&SyntaxNode>) -> Option<ChildContainer> {
        let _p = tracing::info_span!("find_container").entered();
        self.ancestors(src, |this, container| this.container_to_def(container))
    }

    fn ancestors<T>(
        &mut self,
        node: InFile<&SyntaxNode>,
        mut cb: impl FnMut(&mut Self, InFile<SyntaxNode>) -> Option<T>,
    ) -> Option<T> {
        let mut node = node.cloned();
        while let Some(parent) = node.as_ref().value.parent().map(|it| node.with_value(it)) {
            if let Some(res) = cb(self, parent.clone()) {
                return Some(res);
            }
            node = parent;
        }
        None
    }

    fn container_to_def(&mut self, container: InFile<SyntaxNode>) -> Option<ChildContainer> {
        let item = ast::Item::cast(container.value.clone())?;
        let cont = match item {
            ast::Item::ClassItem(class_item) => {
                self.class_to_def(container.with_value(&class_item))?.into()
            }
            ast::Item::ImplementItem(implement_item) => {
                self.implement_to_def(container.with_value(&implement_item))?.into()
            }
            ast::Item::InterfaceItem(interface_item) => {
                self.interface_to_def(container.with_value(&interface_item))?.into()
            }
            ast::Item::ExportItem(_)
            | ast::Item::ErrorDirective(_)
            | ast::Item::ExternallyItem(_)
            | ast::Item::OptionsItem(_)
            | ast::Item::MessageDirective(_) => return None,

            // TODO: Implement these
            ast::Item::GoalItem(_)
            | ast::Item::IncludeItem(_)
            | ast::Item::RequiresDirective(_)
            | ast::Item::NamespaceItem(_) => return None,
        };

        Some(cont)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum ChildContainer {
    DefWithBodyId(DefWithBodyId),
    ImplementId(ImplementId),
    ClassId(ClassId),
    InterfaceId(InterfaceId),
}
impl_from! {
    DefWithBodyId,
    ImplementId,
    ClassId,
    InterfaceId
    for ChildContainer
}
impl ChildContainer {
    fn child_by_source(self, db: &dyn HirDatabase, file_id: FileId) -> DynMap {
        let _p = tracing::info_span!("ChildContainer::child_by_source").entered();
        match self {
            ChildContainer::DefWithBodyId(it) => it.child_by_source(db, file_id),
            ChildContainer::ImplementId(it) => it.child_by_source(db, file_id),
            ChildContainer::ClassId(it) => it.child_by_source(db, file_id),
            ChildContainer::InterfaceId(it) => it.child_by_source(db, file_id),
        }
    }
}
