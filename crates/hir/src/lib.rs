pub mod db;
mod from_id;
mod has_source;
mod semantics;
mod source_analyzer;

use {
    hir_def::{
        ClassFactDbId, ClassFactFunctorId, ClassFactVarId, ClassId, ClassPredicateId,
        ClassPropertyId, ClauseId, ConstantId, ConstructorId, DomainId, FactDbId, FactFunctorId,
        FactVarId, FunctorId, GuardClauseId, ImplementId, InterfaceId, PredicateId, PropertyId,
        db::DefDatabase, nameres::RootScopeRef,
    },
    syntax::ast::{self, HasItems, HasScopeNameDecl},
};

pub use {
    crate::{
        has_source::HasSource,
        semantics::{Semantics, SemanticsImpl},
    },
    hir_def::nameres::project_def_map,
};

// Be careful with these re-exports.
//
// `hir` is the boundary between the compiler and the IDE. It should try hard to
// isolate the compiler from the ide, to allow the two to be refactored
// independently. Re-exporting something from the compiler is the sure way to
// breach the boundary.
//
// Generally, a refactoring which *removes* a name from this list is a good
// idea!
pub use {
    hir_def::{hir::MemberDeclaration, src::InFile},
    intern::{Symbol, sym},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplementItem {
    pub(crate) id: ImplementId,
}
impl ImplementItem {
    pub fn source(self, db: &dyn hir_def::db::DefDatabase) -> Option<InFile<ast::ImplementItem>> {
        let def_map = self.id.def_map(db);
        let implement = &def_map[&self.id];
        let source = db.parse(implement.file_id()).tree();

        let item = source.items_recursively().find_map(|item| match item {
            ast::Item::ImplementItem(item) => {
                let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
                (implement.name() == &name).then_some(item)
            }
            _ => None,
        })?;

        Some(InFile { file_id: implement.file_id(), value: item })
    }

    pub fn declaration_source(
        self,
        db: &dyn hir_def::db::DefDatabase,
    ) -> Option<InFile<ast::ClassItem>> {
        let def_map = self.id.def_map(db);
        let id = self.id.find_class_declaration(def_map)?;
        let class = &def_map[&id];
        let source = db.parse(class.file_id()).tree();

        let item = source.items_recursively().find_map(|item| match item {
            ast::Item::ClassItem(item) => {
                let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
                (class.name() == &name).then_some(item)
            }
            _ => None,
        })?;

        Some(InFile { file_id: class.file_id(), value: item })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassItem {
    pub(crate) id: ClassId,
}
impl ClassItem {
    pub fn source(self, db: &dyn hir_def::db::DefDatabase) -> Option<InFile<ast::ClassItem>> {
        let def_map = self.id.def_map(db);
        let class = &def_map[&self.id];
        let source_file = db.parse(class.file_id()).tree();

        let item = source_file.items_recursively().find_map(|item| match item {
            ast::Item::ClassItem(item) => {
                let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
                (class.name() == &name).then_some(item)
            }
            _ => None,
        })?;

        Some(InFile::new(class.file_id(), item))
    }

    pub fn definition_source(&self, db: &dyn DefDatabase) -> Option<InFile<ast::ImplementItem>> {
        let def_map = self.id.def_map(db);
        let id = self.id.find_class_implementation(def_map)?;
        let implement = &def_map[&id];
        let source_file = db.parse(implement.file_id()).tree();

        let item = source_file.items_recursively().find_map(|item| match item {
            ast::Item::ImplementItem(item) => {
                let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
                (implement.name() == &name).then_some(item)
            }
            _ => None,
        })?;

        Some(InFile::new(implement.file_id(), item))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InterfaceItem {
    pub(crate) id: InterfaceId,
}
impl InterfaceItem {
    pub fn source(self, db: &dyn DefDatabase) -> Option<InFile<ast::InterfaceItem>> {
        let def_map = self.id.def_map(db);
        let interface_data = &def_map[&self.id];
        let source = db.parse(interface_data.file_id()).tree();

        let item = source.items_recursively().find_map(|item| match item {
            ast::Item::InterfaceItem(item) => {
                let name = RootScopeRef::lower_scope_name_decl(&item.scope_name_decl()?)?;
                (interface_data.name() == &name).then_some(item)
            }
            _ => None,
        })?;

        Some(InFile { file_id: interface_data.file_id(), value: item })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Clause {
    pub(crate) id: ClauseId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GuardClause {
    pub(crate) id: GuardClauseId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Domain {
    pub(crate) id: DomainId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Constant {
    pub(crate) id: ConstantId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Predicate {
    pub(crate) id: PredicateId,
}
impl From<Predicate> for MemberDeclaration {
    fn from(predicate: Predicate) -> Self {
        MemberDeclaration::Predicate(predicate.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassPredicate {
    pub(crate) id: ClassPredicateId,
}
impl From<ClassPredicate> for MemberDeclaration {
    fn from(predicate: ClassPredicate) -> Self {
        MemberDeclaration::ClassPredicate(predicate.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub(crate) id: ConstructorId,
}
impl From<Constructor> for MemberDeclaration {
    fn from(constructor: Constructor) -> Self {
        MemberDeclaration::Constructor(constructor.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FactFunctor {
    pub(crate) id: FactFunctorId,
}
impl From<FactFunctor> for MemberDeclaration {
    fn from(fact_functor: FactFunctor) -> Self {
        MemberDeclaration::FactFunctor(fact_functor.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FactVar {
    pub(crate) id: FactVarId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassFactFunctor {
    pub(crate) id: ClassFactFunctorId,
}
impl From<ClassFactFunctor> for MemberDeclaration {
    fn from(fact_functor: ClassFactFunctor) -> Self {
        MemberDeclaration::ClassFactFunctor(fact_functor.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassFactVar {
    pub(crate) id: ClassFactVarId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Functor {
    pub(crate) id: FunctorId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Property {
    pub(crate) id: PropertyId,
}
impl From<Property> for MemberDeclaration {
    fn from(property: Property) -> Self {
        MemberDeclaration::Property(property.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassProperty {
    pub(crate) id: ClassPropertyId,
}
impl From<ClassProperty> for MemberDeclaration {
    fn from(property: ClassProperty) -> Self {
        MemberDeclaration::ClassProperty(property.id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FactDb {
    pub(crate) id: FactDbId,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassFactDb {
    pub(crate) id: ClassFactDbId,
}

pub enum MemberDefinition {
    ClauseFamily(Vec<Clause>),
}
