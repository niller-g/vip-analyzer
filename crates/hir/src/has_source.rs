use hir_def::{
    Lookup,
    src::{HasSource as _, InFile},
};
use hir_ty::db::HirDatabase;
use syntax::ast;

use crate::{
    ClassFactFunctor, ClassFactVar, ClassPredicate, ClassProperty, Clause, Constant, Constructor,
    Domain, FactFunctor, FactVar, Functor, Predicate, Property,
};

pub trait HasSource {
    type Ast;
    /// Fetches the definition's source node.
    /// Using [`crate::Semantics::source`] is preferred when working with [`crate::Semantics`],
    /// as that caches the parsed file in the semantics' cache.
    ///
    /// The current some implementations can return `InFile` instead of `Option<InFile>`.
    /// But we made this method `Option` to support rlib in the future
    /// by <https://github.com/rust-lang/rust-analyzer/issues/6913>
    fn source(self, db: &dyn HirDatabase) -> Option<InFile<Self::Ast>>;
}

macro_rules! impl_has_source {
    ($ty:ty, $ast:ty) => {
        impl HasSource for $ty {
            type Ast = $ast;

            fn source(self, db: &dyn HirDatabase) -> Option<InFile<Self::Ast>> {
                Some(self.id.lookup(db).source(db))
            }
        }
    };
}

impl_has_source!(Clause, ast::Clause);
impl_has_source!(Domain, ast::Domain);
impl_has_source!(Constant, ast::Constant);
impl_has_source!(Predicate, ast::Predicate);
impl_has_source!(ClassPredicate, ast::ClassPredicate);
impl_has_source!(Constructor, ast::Constructor);
impl_has_source!(FactFunctor, ast::FactFunctor);
impl_has_source!(FactVar, ast::FactVar);
impl_has_source!(ClassFactFunctor, ast::ClassFactFunctor);
impl_has_source!(ClassFactVar, ast::ClassFactVar);
impl_has_source!(Functor, ast::Functor);
impl_has_source!(Property, ast::Property);
impl_has_source!(ClassProperty, ast::ClassProperty);
