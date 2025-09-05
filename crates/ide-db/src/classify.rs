use crate::RootDatabase;
use hir::{
    ClassFactFunctor, ClassFactVar, ClassItem, ClassPredicate, ClassProperty, Clause, Constant,
    Constructor, Domain, FactFunctor, FactVar, Functor, ImplementItem, InterfaceItem,
    MemberDeclaration, Predicate, Property, Semantics,
};
use itertools::Either;
use stdx::impl_from;
use syntax::{
    SyntaxNode, SyntaxToken,
    ast::{self, AstNode},
    match_ast,
};

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum IdentClass {
    Definition(Definition),
    Declaration(Declaration),
}
impl_from!(Definition, Declaration for IdentClass);

impl IdentClass {
    pub fn classify_node(
        sema: &Semantics<'_, RootDatabase>,
        node: &SyntaxNode,
    ) -> Option<IdentClass> {
        let class = match_ast! {
            match node {

                // Already at their classification
                ast::ScopeNameDecl(name) => Self::classify_scope_name_decl(sema, &name)?,
                ast::Predicate(ast) => Declaration::Predicate(sema.to_def(&ast)?).into(),
                ast::ClassPredicate(ast) => Declaration::ClassPredicate(sema.to_def(&ast)?).into(),
                ast::Constructor(ast) => Declaration::Constructor(sema.to_def(&ast)?).into(),
                ast::FactFunctor(ast) => Declaration::FactFunctor(sema.to_def(&ast)?).into(),
                ast::ClassFactFunctor(ast) => Declaration::ClassFactFunctor(sema.to_def(&ast)?).into(),
                ast::FactVar(ast) => Declaration::FactVar(sema.to_def(&ast)?).into(),
                ast::ClassFactVar(ast) => Declaration::ClassFactVar(sema.to_def(&ast)?).into(),
                ast::Property(ast) => Declaration::Property(sema.to_def(&ast)?).into(),
                ast::ClassProperty(ast) => Declaration::ClassProperty(sema.to_def(&ast)?).into(),
                ast::Constant(ast) => Declaration::Constant(sema.to_def(&ast)?).into(),
                ast::Domain(ast) => Declaration::Domain(sema.to_def(&ast)?).into(),
                ast::Functor(ast) => Declaration::Functor(sema.to_def(&ast)?).into(),
                ast::Clause(ast) => Definition::Clause(sema.to_def(&ast)?).into(),

                // Resolve their classification
                ast::ScopeRef(scope_ref) => Declaration::classify_scope_ref(sema, scope_ref)?.into(),
                ast::RefTerm(ref_term) => Declaration::classify_ref_term(sema, ref_term)?.into(),
                ast::PathSegment(_) => Self::classify_node(sema, &node.parent()?)?,
                ast::NamespacePath(_) => Self::classify_node(sema, &node.parent()?)?,
                _ => return None,
            }
        };
        Some(class)
    }

    pub fn classify_token(
        sema: &Semantics<'_, RootDatabase>,
        token: &SyntaxToken,
    ) -> Option<IdentClass> {
        let parent = token.parent()?;
        Self::classify_node(sema, &parent)
    }

    fn classify_scope_name_decl(
        sema: &Semantics<'_, RootDatabase>,
        scope_name_decl: &ast::ScopeNameDecl,
    ) -> Option<IdentClass> {
        let parent = scope_name_decl.syntax().parent()?;
        let ident_class = match_ast! {
            match parent {
                ast::ClassItem(class) => Declaration::ClassItem(sema.to_def(&class)?).into(),
                ast::InterfaceItem(interface) => Declaration::InterfaceItem(sema.to_def(&interface)?).into(),
                ast::ImplementItem(implement) => Definition::ImplementItem(sema.to_def(&implement)?).into(),
                _ => return None,
            }
        };
        Some(ident_class)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum Definition {
    ImplementItem(ImplementItem),
    Clause(Clause),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum Declaration {
    ClassItem(ClassItem),
    InterfaceItem(InterfaceItem),
    Constant(Constant),
    Domain(Domain),
    Functor(Functor),
    FactFunctor(FactFunctor),
    ClassFactFunctor(ClassFactFunctor),
    FactVar(FactVar),
    ClassFactVar(ClassFactVar),
    Predicate(Predicate),
    ClassPredicate(ClassPredicate),
    Constructor(Constructor),
    Property(Property),
    ClassProperty(ClassProperty),
}
impl Declaration {
    fn classify_scope_ref(
        sema: &Semantics<'_, RootDatabase>,
        scope_ref: ast::ScopeRef,
    ) -> Option<Declaration> {
        let _p = tracing::info_span!("Declaration::classify_scope_ref").entered();

        let decl = match sema.find_scope(&scope_ref)? {
            itertools::EitherOrBoth::Both(cl, _i) => Declaration::ClassItem(cl.into()),
            itertools::EitherOrBoth::Left(cl) => Declaration::ClassItem(cl.into()),
            itertools::EitherOrBoth::Right(i) => Declaration::InterfaceItem(i.into()),
        };

        Some(decl)
    }

    fn classify_ref_term(
        sema: &Semantics<'_, RootDatabase>,
        ref_term: ast::RefTerm,
    ) -> Option<Declaration> {
        let _p = tracing::info_span!("Declaration::classify_ref_term").entered();

        let parent = ref_term.syntax().parent()?;
        let name_ref = match_ast! {
            match parent {
                ast::Type(ast) => {
                    match sema.find_type(&ast.type_kind()?)? {
                        Either::Left(domain) => Declaration::Domain(domain.into()),
                        Either::Right(interface) => Declaration::InterfaceItem(interface.into()),
                    }
                },
                ast::Predicate(_) => {
                    // Predicate -> RefTerm
                    // Must be a predicate's signature via a predicate domain.
                    let domain = sema.find_type(&ast::TypeKind::RefTerm(ref_term))?.left()?;
                    Declaration::Domain(domain.into())
                },
                _ => {
                    // TODO:
                    // 1. These should have more refined handling of arity and return types
                    // 2. Also it is too simple to just take the first member.
                    sema.find_member(&ref_term, None)
                        .map(|member| {
                            match member {
                                MemberDeclaration::Constant(id) => Declaration::Constant(id.into()),
                                MemberDeclaration::Predicate(id) => Declaration::Predicate(id.into()),
                                MemberDeclaration::ClassPredicate(id) => Declaration::ClassPredicate(id.into()),
                                MemberDeclaration::Constructor(id) => Declaration::Constructor(id.into()),
                                MemberDeclaration::FactFunctor(id) => Declaration::FactFunctor(id.into()),
                                MemberDeclaration::FactVar(id) => Declaration::FactVar(id.into()),
                                MemberDeclaration::ClassFactFunctor(id) => Declaration::ClassFactFunctor(id.into()),
                                MemberDeclaration::ClassFactVar(id) => Declaration::ClassFactVar(id.into()),
                                MemberDeclaration::Functor(id) => Declaration::Functor(id.into()),
                                MemberDeclaration::Property(id) => Declaration::Property(id.into()),
                                MemberDeclaration::ClassProperty(id) => Declaration::ClassProperty(id.into()),
                            }
                        })?
                },
            }
        };

        Some(name_ref)
    }
}
