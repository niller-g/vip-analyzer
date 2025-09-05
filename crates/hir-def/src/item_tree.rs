//! A simplified AST that only contains items.
//!
//! This is the primary IR used throughout `hir_def`. It is the input to the name resolution
//! algorithm, as well as to the queries defined in `adt.rs`, `data.rs`, and most things in
//! `attr.rs`.
//!
//! `ItemTree`s are built per `HirFileId`, from the syntax tree of the parsed file. This means that
//! they are crate-independent: they don't know which `#[cfg]`s are active or which module they
//! belong to, since those concepts don't exist at this level (a single `ItemTree` might be part of
//! multiple crates, or might be included into the same crate twice via `#[path]`).
//!
//! One important purpose of this layer is to provide an "invalidation barrier" for incremental
//! computations: when typing inside an item body, the `ItemTree` of the modified file is typically
//! unaffected, so we don't have to recompute name resolution results or item data (see `data.rs`).
//!
//! The `ItemTree` for the currently open file can be displayed by using the VS Code command
//! "rust-analyzer: Debug ItemTree".
//!
//! Compared to rustc's architecture, `ItemTree` has properties from both rustc's AST and HIR: many
//! syntax-level Rust features are already desugared to simpler forms in the `ItemTree`, but name
//! resolution has not yet been performed. `ItemTree`s are per-file, while rustc's AST and HIR are
//! per-crate, because we are interested in incrementally computing it.
//!
//! The representation of items in the `ItemTree` should generally mirror the surface syntax: it is
//! usually a bad idea to desugar a syntax-level construct to something that is structurally
//! different here. Name resolution needs to be able to process attributes and expand macros
//! (including attribute macros), and having a 1-to-1 mapping between syntax and the `ItemTree`
//! avoids introducing subtle bugs.
//!
//! In general, any item in the `ItemTree` stores its `AstId`, which allows mapping it back to its
//! surface syntax.

mod lower;

use crate::{
    db::DefDatabase,
    hir::{
        Arity, Name,
        namespaces::{NsPath, RootNs},
        type_ref::TypeRefId,
    },
    nameres::{self, ScopeRef},
};
use la_arena::{Arena, Idx, RawIdx};
use span::{AstIdNode, FileAstId, FileId};
use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::{Index, Range},
};
use stdx::never;
use syntax::{
    AstNode, SyntaxKind,
    ast::{self},
    match_ast,
};
use triomphe::Arc;

#[derive(Debug, Eq, PartialEq)]
pub enum ModItem {
    Implement(Implement),
    Class(Class),
    Interface(Interface),
    Include(String),
    Namespace(RootNs),
}

/// The item tree of a single item.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct ItemTree {
    /// The index of the item in the file.
    tree_id: Option<Idx<ItemTree>>,

    top_level: Option<ModItem>,

    data: Option<Box<ItemTreeData>>,
}
impl ItemTree {
    pub(crate) fn file_item_trees_query(
        db: &dyn DefDatabase,
        file_id: FileId,
    ) -> Arc<Arena<ItemTree>> {
        let _p = tracing::info_span!("file_item_tree_query", ?file_id).entered();

        let syntax = db.parse(file_id).syntax_node();

        let mut item_trees = match_ast! {
            match syntax {
                ast::SourceFile(source) => {
                    lower::lower_file_items(db, file_id, &source)
                },
                _ => {
                    if never!(syntax.kind() == SyntaxKind::ERROR, "{:?} from {:?} {}", file_id, syntax, syntax) {
                        return Default::default();
                    }
                    panic!("cannot create item tree for file {file_id:?} from {syntax:?} {syntax}");
                },
            }
        };

        item_trees.shrink_to_fit();
        Arc::new(item_trees)
    }

    pub fn tree_id(&self) -> Idx<ItemTree> {
        self.tree_id.expect("attempted to access tree_id of empty ItemTree")
    }

    /// Returns an the the top level item of the item tree.
    pub fn top_level_item(&self) -> &Option<ModItem> {
        &self.top_level
    }

    fn data(&self) -> &ItemTreeData {
        self.data.as_ref().expect("attempted to access data of empty ItemTree")
    }

    fn data_mut(&mut self) -> &mut ItemTreeData {
        self.data.get_or_insert_with(Box::default)
    }

    fn shrink_to_fit(&mut self) {
        if let Some(data) = &mut self.data {
            let ItemTreeData {
                fact_dbs,
                class_fact_dbs,
                fact_functors,
                fact_vars,
                class_fact_functors,
                class_fact_vars,
                predicates,
                class_predicates,
                constructors,
                properties,
                class_properties,
                clauses,
                guard_clauses,
                constants,
                domains,
                functors,
                interface_delegates,
                predicate_delegates,
                interface_resolutions,
                predicate_resolutions,
                predicate_resolutions_external,
                predicates_from: preds_from,
                properties_from: props_from,
            } = &mut **data;

            fact_dbs.shrink_to_fit();
            class_fact_dbs.shrink_to_fit();
            fact_functors.shrink_to_fit();
            fact_vars.shrink_to_fit();
            class_fact_functors.shrink_to_fit();
            class_fact_vars.shrink_to_fit();
            predicates.shrink_to_fit();
            class_predicates.shrink_to_fit();
            constructors.shrink_to_fit();
            properties.shrink_to_fit();
            class_properties.shrink_to_fit();
            clauses.shrink_to_fit();
            guard_clauses.shrink_to_fit();
            constants.shrink_to_fit();
            domains.shrink_to_fit();
            functors.shrink_to_fit();
            interface_delegates.shrink_to_fit();
            predicate_delegates.shrink_to_fit();
            interface_resolutions.shrink_to_fit();
            predicate_resolutions.shrink_to_fit();
            predicate_resolutions_external.shrink_to_fit();
            preds_from.shrink_to_fit();
            props_from.shrink_to_fit();
        }
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
struct ItemTreeData {
    fact_dbs: Arena<FactDb>,
    class_fact_dbs: Arena<ClassFactDb>,
    fact_functors: Arena<FactFunctor>,
    fact_vars: Arena<FactVar>,
    class_fact_functors: Arena<ClassFactFunctor>,
    class_fact_vars: Arena<ClassFactVar>,
    predicates: Arena<Predicate>,
    class_predicates: Arena<ClassPredicate>,
    constructors: Arena<Constructor>,
    properties: Arena<Property>,
    class_properties: Arena<ClassProperty>,
    clauses: Arena<Clause>,
    guard_clauses: Arena<GuardClause>,
    constants: Arena<Constant>,
    domains: Arena<Domain>,
    functors: Arena<Functor>,
    interface_delegates: Arena<InterfaceDelegate>,
    predicate_delegates: Arena<PredicateDelegate>,
    interface_resolutions: Arena<InterfaceResolution>,
    predicate_resolutions: Arena<PredicateResolution>,
    predicate_resolutions_external: Arena<ExternalPredicateResolution>,
    predicates_from: Arena<PredicateFrom>,
    properties_from: Arena<PropertyFrom>,
}

#[derive(Debug)]
pub struct ItemTreeId<N> {
    file_id: FileId,
    tree_id: Idx<ItemTree>,
    pub value: FileItemTreeId<N>,
}

impl<N> ItemTreeId<N> {
    #[inline]
    pub fn new(file_id: FileId, tree_id: Idx<ItemTree>, idx: FileItemTreeId<N>) -> Self {
        Self { file_id, tree_id, value: idx }
    }

    pub fn file_id(self) -> FileId {
        self.file_id
    }

    pub fn tree_id(self) -> Idx<ItemTree> {
        self.tree_id
    }
}

impl<N: ItemTreeNode> Index<FileItemTreeId<N>> for ItemTree {
    type Output = N;
    fn index(&self, id: FileItemTreeId<N>) -> &N {
        N::lookup(self, id.index())
    }
}

impl<N> Copy for ItemTreeId<N> {}
impl<N> Clone for ItemTreeId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N> PartialEq for ItemTreeId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.file_id == other.file_id && self.value == other.value
    }
}

impl<N> Eq for ItemTreeId<N> {}

impl<N> Hash for ItemTreeId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.file_id.hash(state);
        self.value.hash(state);
    }
}

pub struct FileItemTreeId<N>(Idx<N>);

impl<N> FileItemTreeId<N> {
    pub fn range_iter(range: Range<Self>) -> impl Iterator<Item = Self> + Clone {
        (range.start.index().into_raw().into_u32()..range.end.index().into_raw().into_u32())
            .map(RawIdx::from_u32)
            .map(Idx::from_raw)
            .map(Self)
    }
}

impl<N> FileItemTreeId<N> {
    pub fn index(&self) -> Idx<N> {
        self.0
    }
}

impl<N> Clone for FileItemTreeId<N> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<N> Copy for FileItemTreeId<N> {}

impl<N> PartialEq for FileItemTreeId<N> {
    fn eq(&self, other: &FileItemTreeId<N>) -> bool {
        self.0 == other.0
    }
}
impl<N> Eq for FileItemTreeId<N> {}

impl<N> Hash for FileItemTreeId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<N> fmt::Debug for FileItemTreeId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

macro_rules! mod_items {
    ( $( $typ:ident $(<$generic_params:ident>)? in $fld:ident -> $ast:ty ),+ $(,)? ) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub enum Section {
            $(
                $typ(FileItemTreeId<$typ>),
            )+
        }

        // impl Section {
        //     pub fn ast_id(&self, tree: &ItemTree) -> FileAstId<ast::Item> {
        //         match self {
        //             $(Section::$typ(it) => tree[it.index()].ast_id().upcast()),+
        //         }
        //     }
        // }

        $(
            impl From<FileItemTreeId<$typ>> for Section {
                fn from(id: FileItemTreeId<$typ>) -> Section {
                    Section::$typ(id)
                }
            }
        )+

        $(
            impl ItemTreeNode for $typ {
                type Source = $ast;

                fn ast_id(&self) -> FileAstId<Self::Source> {
                    self.ast_id
                }

                fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self {
                    &tree.data().$fld[index]
                }
            }

            impl Index<Idx<$typ>> for ItemTree {
                type Output = $typ;

                fn index(&self, index: Idx<$typ>) -> &Self::Output {
                    &self.data().$fld[index]
                }
            }
        )+
    };
}

mod_items! {
    FactDb in fact_dbs -> ast::FactsSection,
    ClassFactDb in class_fact_dbs -> ast::ClassFactsSection,
    FactFunctor in fact_functors -> ast::FactFunctor,
    FactVar in fact_vars -> ast::FactVar,
    ClassFactFunctor in class_fact_functors -> ast::ClassFactFunctor,
    ClassFactVar in class_fact_vars -> ast::ClassFactVar,
    Predicate in predicates -> ast::Predicate,
    ClassPredicate in class_predicates -> ast::ClassPredicate,
    Constructor in constructors -> ast::Constructor,
    Property in properties -> ast::Property,
    ClassProperty in class_properties -> ast::ClassProperty,
    Clause in clauses -> ast::Clause,
    GuardClause in guard_clauses -> ast::GuardClause,
    Constant in constants -> ast::Constant,
    Domain in domains -> ast::Domain,
    Functor in functors -> ast::Functor,
    InterfaceDelegate in interface_delegates -> ast::InterfaceDelegate,
    PredicateDelegate in predicate_delegates -> ast::PredicateDelegate,
    InterfaceResolution in interface_resolutions -> ast::InterfaceResolution,
    PredicateResolution in predicate_resolutions -> ast::PredicateResolution,
    ExternalPredicateResolution in predicate_resolutions_external -> ast::ExternalPredicateResolution,
    PredicateFrom in predicates_from -> ast::Arity,
    PropertyFrom in properties_from -> ast::Arity,
}

/// Trait implemented by all nodes in the item tree.
pub trait ItemTreeNode: Clone {
    type Source: AstIdNode;

    fn ast_id(&self) -> FileAstId<Self::Source>;

    /// Looks up an instance of `Self` in an item tree.
    fn lookup(tree: &ItemTree, index: Idx<Self>) -> &Self;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Implement {
    pub name: nameres::RootScopeRef,
    pub inherits: Box<[nameres::ScopeRef]>,
    pub supports: Box<[nameres::ScopeRef]>,
    pub opens: Box<[OpenQualification]>,
    pub sections: Box<[ImplementSection]>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Class {
    pub name: nameres::RootScopeRef,
    pub construction_ty: Option<nameres::ScopeRef>,
    pub opens: Box<[OpenQualification]>,
    pub sections: Box<[ClassSection]>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Interface {
    pub name: nameres::RootScopeRef,
    pub supports: Box<[nameres::ScopeRef]>,
    pub opens: Box<[OpenQualification]>,
    pub sections: Box<[InterfaceSection]>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OpenQualification {
    /// `open \`
    Root,
    /// `open bar\foo\`
    NamespacePath(NsPath),
    /// `open bar\foo`
    Ambiguous(nameres::ScopeRef),
}
impl TryFrom<ast::OpenQualification> for OpenQualification {
    type Error = ();

    fn try_from(open_qual: ast::OpenQualification) -> Result<Self, Self::Error> {
        match open_qual {
            ast::OpenQualification::NamespacePath(namespace_path) => {
                if namespace_path.path_segments().next().is_none()
                    && namespace_path.backslash_token().is_some()
                {
                    return Ok(OpenQualification::Root);
                }

                Ok(OpenQualification::NamespacePath(namespace_path.into()))
            }
            ast::OpenQualification::ScopeRef(ast) => {
                ScopeRef::from_ast(&ast).map(OpenQualification::Ambiguous).ok_or(())
            }
        }
    }
}

type Functors = Box<[FileItemTreeId<Functor>]>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Fact {
    FactVar(FileItemTreeId<FactVar>),
    FactFunctor(FileItemTreeId<FactFunctor>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ClassFact {
    ClassFactVar(FileItemTreeId<ClassFactVar>),
    ClassFactFunctor(FileItemTreeId<ClassFactFunctor>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ImplementSection {
    ClassFacts(FactsSection<ClassFactDb, ClassFact>),
    Facts(FactsSection<FactDb, Fact>),
    ClassProperties(Vec<FileItemTreeId<ClassProperty>>),
    Properties(Vec<FileItemTreeId<Property>>),
    ClassPredicates(Vec<FileItemTreeId<ClassPredicate>>),
    Predicates(Vec<FileItemTreeId<Predicate>>),
    Constructors(Vec<FileItemTreeId<Constructor>>),
    Clauses(Vec<ClauseKind>),
    Constants(Vec<FileItemTreeId<Constant>>),
    Delegates(Vec<Delegate>),
    Domains(Vec<(FileItemTreeId<Domain>, Functors)>),
    Resolve(Vec<Resolution>),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ClassSection {
    Constants(Vec<FileItemTreeId<Constant>>),
    Domains(Vec<(FileItemTreeId<Domain>, Functors)>),
    Constructors(Vec<FileItemTreeId<Constructor>>),
    Predicates(Vec<FileItemTreeId<Predicate>>),
    Properties(Vec<FileItemTreeId<Property>>),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InterfaceSection {
    Constants(Vec<FileItemTreeId<Constant>>),
    Domains(Vec<(FileItemTreeId<Domain>, Functors)>),
    Preds(Vec<FileItemTreeId<Predicate>>),
    Props(Vec<FileItemTreeId<Property>>),
    PropertiesFrom(Vec<FileItemTreeId<PropertyFrom>>),
    PredicatesFrom(Vec<FileItemTreeId<PredicateFrom>>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FactsSection<FDb, F> {
    pub fact_db: Option<FileItemTreeId<FDb>>,
    pub facts: Vec<F>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FactDb {
    name: Name,
    ast_id: FileAstId<ast::FactsSection>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassFactDb {
    name: Name,
    ast_id: FileAstId<ast::ClassFactsSection>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FactVar {
    pub name: Name,
    ast_id: FileAstId<ast::FactVar>,
}
impl FactVar {
    #[inline]
    pub fn new(name: Name, ast_id: FileAstId<ast::FactVar>) -> Self {
        Self { name, ast_id }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassFactVar {
    pub name: Name,
    ast_id: FileAstId<ast::ClassFactVar>,
}
impl ClassFactVar {
    #[inline]
    pub fn new(name: Name, ast_id: FileAstId<ast::ClassFactVar>) -> Self {
        Self { name, ast_id }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FactFunctor {
    pub name: Name,
    ast_id: FileAstId<ast::FactFunctor>,
}
impl FactFunctor {
    #[inline]
    pub fn new(name: Name, ast_id: FileAstId<ast::FactFunctor>) -> Self {
        Self { name, ast_id }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassFactFunctor {
    pub name: Name,
    ast_id: FileAstId<ast::ClassFactFunctor>,
}
impl ClassFactFunctor {
    #[inline]
    pub fn new(name: Name, ast_id: FileAstId<ast::ClassFactFunctor>) -> Self {
        Self { name, ast_id }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Property {
    pub name: Name,
    ast_id: FileAstId<ast::Property>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassProperty {
    pub name: Name,
    ast_id: FileAstId<ast::ClassProperty>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Predicate {
    pub name: Name,
    ast_id: FileAstId<ast::Predicate>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ClassPredicate {
    pub name: Name,
    ast_id: FileAstId<ast::ClassPredicate>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constructor {
    pub name: Name,
    ast_id: FileAstId<ast::Constructor>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamsKind {
    Inline { params: Box<[TypeRefId]>, ellipsis: bool, ret_type: Option<TypeRefId> },
    DomainRef(TypeRefId),
}
impl Default for ParamsKind {
    fn default() -> Self {
        ParamsKind::Inline {
            params: Default::default(),
            ellipsis: false,
            ret_type: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ClauseKind {
    Clause(FileItemTreeId<Clause>),
    GuardClause(FileItemTreeId<GuardClause>),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Clause {
    pub name: Name,
    ast_id: FileAstId<ast::Clause>,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GuardClause {
    pub name: Name,
    ast_id: FileAstId<ast::GuardClause>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constant {
    pub name: Name,
    ast_id: FileAstId<ast::Constant>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Delegate {
    InterfaceDelegate(FileItemTreeId<InterfaceDelegate>),
    PredicateDelegate(FileItemTreeId<PredicateDelegate>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InterfaceDelegate {
    interface: nameres::ScopeRef,
    to: Name,
    ast_id: FileAstId<ast::InterfaceDelegate>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PredicateDelegate {
    pred_arity: Arity,
    to: Name,
    ast_id: FileAstId<ast::PredicateDelegate>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Domain {
    pub name: Name,
    ast_id: FileAstId<ast::Domain>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DomainDef {
    Alias,
    Functors(Box<[FileItemTreeId<Functor>]>),
    Numeric,
    PredDomain,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Functor {
    pub name: Name,
    ast_id: FileAstId<ast::Functor>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Resolution {
    InterfaceResolution(FileItemTreeId<InterfaceResolution>),
    PredResolution(FileItemTreeId<PredicateResolution>),
    PredResolutionExternal(FileItemTreeId<ExternalPredicateResolution>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InterfaceResolution {
    pub resolve_interface: nameres::ScopeRef,
    pub from: nameres::ScopeRef,
    ast_id: FileAstId<ast::InterfaceResolution>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PredicateFromSource {
    Class(ScopeRef),
    ClassAndRename(ScopeRef, Name),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PredicateResolution {
    pub pred_arity: Arity,
    pub from: PredicateFromSource,
    ast_id: FileAstId<ast::PredicateResolution>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternalPredicateResolution {
    pub pred_arity: Arity,
    ast_id: FileAstId<ast::ExternalPredicateResolution>,
}

macro_rules! arity_like {
    ($($name:ident),+) => {
        $(
            #[derive(Debug, Clone, Eq, PartialEq)]
            pub struct $name {
                pub from: nameres::ScopeRef,
                pub arity: Arity,
                ast_id: FileAstId<ast::Arity>,
            }
        )+
    };
}
arity_like!(PredicateFrom, PropertyFrom);
