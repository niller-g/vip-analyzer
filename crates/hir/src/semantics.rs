//! See `Semantics`.

mod child_by_source;
mod source_to_def;

use crate::{
    Clause, HasSource, MemberDefinition, db::HirDatabase, source_analyzer::SourceAnalyzer,
};
use hir_def::{
    ClassId, DomainId, InterfaceId, hir::MemberDeclaration, resolver::HasResolver, src::InFile,
};
use itertools::{Either, EitherOrBoth};
use smallvec::SmallVec;
use source_to_def::{ChildContainer, SourceToDefCache, SourceToDefCtx};
use span::{FileId, TextSize};
use std::{cell::RefCell, fmt, ops};
use syntax::{
    AstNode, SyntaxNode,
    ast::{self, HasType},
};

/// Primary API to get semantic information, like types, from syntax trees.
pub struct Semantics<'db, DB> {
    pub db: &'db DB,
    imp: SemanticsImpl<'db>,
}

pub struct SemanticsImpl<'db> {
    pub db: &'db dyn HirDatabase,
    s2d_cache: RefCell<SourceToDefCache>,
}

impl<DB> fmt::Debug for Semantics<'_, DB> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Semantics {{ ... }}")
    }
}

impl<'db, DB> ops::Deref for Semantics<'db, DB> {
    type Target = SemanticsImpl<'db>;

    fn deref(&self) -> &Self::Target {
        &self.imp
    }
}

impl<DB: HirDatabase> Semantics<'_, DB> {
    pub fn new(db: &DB) -> Semantics<'_, DB> {
        let impl_ = SemanticsImpl::new(db);
        Semantics { db, imp: impl_ }
    }
}

impl<'db> SemanticsImpl<'db> {
    fn new(db: &'db dyn HirDatabase) -> Self {
        SemanticsImpl { db, s2d_cache: Default::default() }
    }

    pub fn parse(&self, file_id: FileId) -> ast::SourceFile {
        let tree = self.db.parse(file_id).tree();
        self.cache(tree.syntax().clone(), file_id);
        tree
    }

    fn with_ctx<F: FnOnce(&mut SourceToDefCtx<'_, '_>) -> T, T>(&self, f: F) -> T {
        let mut ctx = SourceToDefCtx { db: self.db, cache: &mut self.s2d_cache.borrow_mut() };
        f(&mut ctx)
    }

    pub fn matching_implements(
        &self,
        implement_item: InFile<&ast::ImplementItem>,
    ) -> SmallVec<[crate::ImplementItem; 1]> {
        self.with_ctx(move |ctx| {
            ctx.matching_implements(implement_item).into_iter().map(Into::into).collect()
        })
    }

    pub fn matching_classes(
        &self,
        class_item: InFile<&ast::ClassItem>,
    ) -> SmallVec<[crate::ClassItem; 1]> {
        self.with_ctx(move |ctx| {
            ctx.matching_classes(class_item).into_iter().map(Into::into).collect()
        })
    }

    pub fn matching_interfaces(
        &self,
        interface_item: InFile<&ast::InterfaceItem>,
    ) -> SmallVec<[crate::InterfaceItem; 1]> {
        self.with_ctx(move |ctx| {
            ctx.matching_interfaces(interface_item).into_iter().map(Into::into).collect()
        })
    }

    pub fn to_def<T: ToDef>(&self, src: &T) -> Option<T::Def> {
        let src = self.find_file(src.syntax()).with_value(src);
        T::to_def(self, src)
    }

    fn cache(&self, root_node: SyntaxNode, file_id: FileId) {
        SourceToDefCache::cache(
            &mut self.s2d_cache.borrow_mut().root_to_file_cache,
            root_node,
            file_id,
        );
    }

    pub fn assert_contains_node(&self, node: &SyntaxNode) {
        self.find_file(node);
    }

    fn lookup(&self, root_node: &SyntaxNode) -> Option<FileId> {
        let cache = self.s2d_cache.borrow();
        cache.root_to_file_cache.get(root_node).copied()
    }

    #[expect(unused)]
    fn wrap_node_infile<N: AstNode>(&self, node: N) -> InFile<N> {
        let InFile { file_id, .. } = self.find_file(node.syntax());
        InFile::new(file_id, node)
    }

    /// Wraps the node in a [`InFile`] with the file id it belongs to.
    fn find_file<'node>(&self, node: &'node SyntaxNode) -> InFile<&'node SyntaxNode> {
        let root_node = find_root(node);
        let file_id = self.lookup(&root_node).unwrap_or_else(|| {
            panic!(
                "\n\nFailed to lookup {:?} in this Semantics.\n\
                 Make sure to only query nodes derived from this instance of Semantics.\n\
                 root node:   {:?}\n\
                 known nodes: {}\n\n",
                node,
                root_node,
                self.s2d_cache
                    .borrow()
                    .root_to_file_cache
                    .keys()
                    .map(|it| format!("{it:?}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        });
        InFile::new(file_id, node)
    }

    /// Search for a definition's source and cache its syntax tree
    pub fn source<Def: HasSource>(&self, def: Def) -> Option<InFile<Def::Ast>>
    where
        Def::Ast: AstNode,
    {
        // FIXME: source call should go through the parse cache
        let res = def.source(self.db)?;
        self.cache(find_root(res.value.syntax()), res.file_id);
        Some(res)
    }

    /// Returns none if the file of the node is not part of a project.
    fn analyze(&self, node: &SyntaxNode) -> Option<SourceAnalyzer<'db>> {
        let node = self.find_file(node);
        self.analyze_impl(node, None)
    }

    fn analyze_impl(
        &self,
        node: InFile<&SyntaxNode>,
        offset: Option<TextSize>,
    ) -> Option<SourceAnalyzer<'db>> {
        let _p = tracing::info_span!("SemanticsImpl::analyze_impl").entered();

        let container = self.with_ctx(|ctx| ctx.find_container(node))?;

        let resolver = match container {
            ChildContainer::DefWithBodyId(def_with_body_id) => {
                return Some(SourceAnalyzer::new_for_body(self.db, def_with_body_id, node, offset));
            }
            ChildContainer::ImplementId(it) => it.resolver(self.db),
            ChildContainer::ClassId(it) => it.resolver(self.db),
            ChildContainer::InterfaceId(it) => it.resolver(self.db),
        };

        Some(SourceAnalyzer::new_for_resolver(resolver, node))
    }

    pub fn find_scope(&self, scope: &ast::ScopeRef) -> Option<EitherOrBoth<ClassId, InterfaceId>> {
        self.analyze(scope.syntax())?.find_scope(self.db, scope)
    }

    pub fn find_type(&self, ty: &ast::TypeKind) -> Option<Either<DomainId, InterfaceId>> {
        let ty_ref = match ty {
            ast::TypeKind::RefTerm(ref_term) => ref_term,
            ast::TypeKind::ListType(list_type) => {
                return self.find_type(&list_type.ty()?);
            }
            ast::TypeKind::ScopeType(_) => return None,
            ast::TypeKind::VarType(_) => return None,
        };

        self.analyze(ty.syntax())?.find_type(self.db, ty_ref)
    }

    pub fn find_member(
        &self,
        member: &ast::RefTerm,
        arity: Option<ops::RangeInclusive<u8>>,
    ) -> Option<MemberDeclaration> {
        self.analyze(member.syntax())?.find_member(self.db, member, arity)
    }

    /// Find the definition of some source node with the given declaration.
    /// Note: the `source` should be the usage site.
    pub fn find_definition(
        &self,
        source: &SyntaxNode,
        declaration: &MemberDeclaration,
    ) -> Option<MemberDefinition> {
        let def = self.analyze(source)?.find_definition(self.db, declaration)?;

        match def {
            hir_def::hir::MemberDefinition::ClauseFamily(clause_ids) => Some(
                MemberDefinition::ClauseFamily(clause_ids.into_iter().map(Into::into).collect()),
            ),
        }
    }

    pub fn find_clause_declaration(&self, clause: Clause) -> Option<MemberDeclaration> {
        let analyzer = self.analyze(self.source(clause)?.value.syntax())?;
        analyzer.find_clause_declaration(self.db, &self.db.clause_signature(clause.id))
    }
}

pub trait ToDef: AstNode + Clone {
    type Def;
    fn to_def(sema: &SemanticsImpl<'_>, src: InFile<&Self>) -> Option<Self::Def>;
}

macro_rules! to_def_impls {
    ($(($def:path, $ast:path, $meth:ident)),* ,) => {$(
        impl ToDef for $ast {
            type Def = $def;
            fn to_def(sema: &SemanticsImpl<'_>, src: InFile<&Self>) -> Option<Self::Def> {
                sema.with_ctx(|ctx| ctx.$meth(src)).map(<$def>::from)
            }
        }
    )*}
}

to_def_impls![
    (crate::ImplementItem, ast::ImplementItem, implement_to_def),
    (crate::ClassItem, ast::ClassItem, class_to_def),
    (crate::InterfaceItem, ast::InterfaceItem, interface_to_def),
    (crate::Predicate, ast::Predicate, predicate_to_def),
    (crate::ClassPredicate, ast::ClassPredicate, class_predicate_to_def),
    (crate::Constructor, ast::Constructor, constructor_to_def),
    (crate::FactDb, ast::FactsSection, fact_db_to_def),
    (crate::ClassFactDb, ast::ClassFactsSection, class_fact_db_to_def),
    (crate::FactFunctor, ast::FactFunctor, fact_functor_to_def),
    (crate::ClassFactFunctor, ast::ClassFactFunctor, class_fact_functor_to_def),
    (crate::FactVar, ast::FactVar, fact_var_to_def),
    (crate::ClassFactVar, ast::ClassFactVar, class_fact_var_to_def),
    (crate::Property, ast::Property, property_to_def),
    (crate::ClassProperty, ast::ClassProperty, class_property_to_def),
    (crate::Constant, ast::Constant, constant_to_def),
    (crate::Domain, ast::Domain, domain_to_def),
    (crate::Functor, ast::Functor, functor_to_def),
    (crate::Clause, ast::Clause, clause_to_def),
    (crate::GuardClause, ast::GuardClause, guard_clause_to_def),
];

fn find_root(node: &SyntaxNode) -> SyntaxNode {
    node.ancestors().last().unwrap()
}
