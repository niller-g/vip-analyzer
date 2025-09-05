pub mod namespaces;
pub mod type_ref;

use crate::{
    ClassFactFunctorId, ClassFactVarId, ClassPredicateId, ClassPropertyId, ClauseId, ConstantId,
    ConstructorId, FactFunctorId, FactVarId, FunctorId, PredicateId, PropertyId, nameres::ScopeRef,
};
use intern::Symbol;
use la_arena::Idx;
use smallvec::SmallVec;
use std::ops::RangeInclusive;
use syntax::ast::{
    self,
    operators::{ExprOp, PrefixOp, RelationOp, StmtOp},
};
use type_ref::TypeRefId;

pub enum MemberDefinition {
    ClauseFamily(Vec<ClauseId>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MemberDeclaration {
    Constant(ConstantId),
    Predicate(PredicateId),
    ClassPredicate(ClassPredicateId),
    Constructor(ConstructorId),
    FactFunctor(FactFunctorId),
    FactVar(FactVarId),
    ClassFactFunctor(ClassFactFunctorId),
    ClassFactVar(ClassFactVarId),
    Functor(FunctorId),
    Property(PropertyId),
    ClassProperty(ClassPropertyId),
}

pub type ExprId = Idx<Expr>;
pub type StmtId = Idx<Stmt>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    AccessTerm(AccessTerm),
    ApplyTerm(ApplyTerm),
    BinExpr { lhs: ExprId, op: ExprOp, rhs: ExprId },
    BinInclude,
    BinLiteral,
    Cons { heads: Box<[ExprId]>, tail: ExprId },
    Ellipsis,
    Erroneous,
    FindAll { element: ExprId, generator: StmtId },
    Float,
    IfTerm(IfTerm),
    Int,
    Lambda { args: Box<[ExprId]>, return_expr: Option<ExprId>, body: Option<StmtId> },
    List { elements: Box<[ExprId]> },
    Missing,
    ObjectExpr { interface: TypeRefId },
    ParenTerm(ExprOrStmt),
    PrefixExpr { op: PrefixOp, expr: ExprId },
    RefTerm(RefTerm),
    Str,
    StringInclude,
    TypeRef(TypeRefId),
    Var(Symbol),
    Wildcard,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TryHandler {
    Catch { exception: ExprId, body: StmtId },
    Finally { body: StmtId },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchArm {
    pub args: Box<[ExprId]>,
    pub body: Option<StmtId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct KeywordArg {
    pub keyword: Symbol,
    pub value: ExprId,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ApplyArg {
    Expr(ExprId),
    KeywordArg(KeywordArg),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunctorArg {
    Expr(ExprId),
    AtomStmt(StmtId),
    KeywordArg(KeywordArg),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ElseIf {
    pub cond: StmtId,
    pub then: Option<ExprOrStmt>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprOrStmt {
    Expr(ExprId),
    Stmt(StmtId),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AccessTerm {
    pub expr: ExprId,
    pub access: RefTerm,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ApplyTerm {
    pub receiver: ExprId,
    pub args: Box<[FunctorArg]>,
    pub functor_original: Option<ApplyArg>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfTerm {
    pub cond: StmtId,
    pub then: Option<ExprOrStmt>,
    pub else_ifs: Box<[ElseIf]>,
    pub else_: Option<ExprOrStmt>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
    AccessTerm(AccessTerm),
    ApplyTerm(ApplyTerm),
    AssignStmt { receiver: ExprId, initializer: ExprId },
    BinStmt { lhs: StmtId, op: StmtOp, rhs: StmtId },
    Cut,
    ExprStmt(ExprId),
    ForeachStmt { iter: StmtId, body: StmtId },
    IfTerm(IfTerm),
    InStmt { lhs: ExprId, rhs: ExprId },
    MatchStmt { args: Box<[ExprId]>, arms: Box<[MatchArm]> },
    Missing,
    ParenTerm(ExprOrStmt),
    RefTerm(RefTerm),
    RelationStmt { lhs: ExprId, op: RelationOp, rhs: ExprId },
    TryStmt { body: StmtId, handlers: Box<[TryHandler]> },
}

/// A HIR representation of a name.
/// It is guaranteed to be lowercase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(Symbol);
impl Name {
    pub fn new(name: &str) -> Self {
        Self(Symbol::intern(&name.to_lowercase()))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// A HIR representation of a variable name.
/// It is guaranteed to have the first letter upper cased and the rest lowercase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarName(Symbol);
impl VarName {
    pub fn new(name: &str) -> Self {
        let mut chars = name.chars();
        let name = match chars.next() {
            Some(first) => {
                first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
            }
            None => unreachable!("VarName should not be empty"),
        };
        Self(Symbol::intern(&name))
    }
}

/// A reference to a term.
///
/// Example syntax: `foo` or `ns\bar::bar{T}`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RefTerm {
    Scope(ScopeRef),
    Member { scope: ScopeRef, name: Name, name_generics: Box<[TypeRefId]> },
    Name { name: Name, name_generics: Box<[TypeRefId]> },
}
impl RefTerm {
    pub fn try_new<T>(scope_ref: Option<ScopeRef>, name: Option<(Name, T)>) -> Option<RefTerm>
    where
        T: IntoIterator<Item = TypeRefId>,
    {
        match (scope_ref, name) {
            (Some(scope), Some((name, name_generics))) => Some(RefTerm::Member {
                scope,
                name,
                name_generics: name_generics.into_iter().collect(),
            }),
            (Some(scope_ref), None) => Some(RefTerm::Scope(scope_ref)),
            (None, Some((name, name_generics))) => {
                Some(RefTerm::Name { name, name_generics: name_generics.into_iter().collect() })
            }
            _ => None,
        }
    }

    /// Checks that the term is a member or a name.
    pub fn member_or_name(self) -> Option<RefTerm> {
        matches!(self, RefTerm::Member { .. } | RefTerm::Name { .. }).then_some(self)
    }

    /// Checks that the final part of the term has no generics.
    pub fn not_generic(self) -> Option<RefTerm> {
        match &self {
            RefTerm::Name { name_generics, .. } if name_generics.is_empty() => Some(self),
            RefTerm::Scope(scope) if scope.generics_count == 0 => Some(self),
            RefTerm::Member { name_generics, .. } if name_generics.is_empty() => Some(self),
            _ => None,
        }
    }
}

pub type Bindings = Box<[BindingState]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingState {
    Bound,
    Free,
    /// Both bound and free.
    Inconsistent,
    Compound {
        functor_name: Name,
        bindings: Box<[BindingState]>,
    },
    List(Box<[BindingState]>),
}
impl From<&FlowArg> for BindingState {
    fn from(arg: &FlowArg) -> Self {
        match arg {
            FlowArg::In => BindingState::Bound,
            FlowArg::Out => BindingState::Free,
            FlowArg::Compound { functor_name, functor_flow_args } => BindingState::Compound {
                functor_name: functor_name.clone(),
                bindings: functor_flow_args.into_iter().map(BindingState::from).collect(),
            },
            FlowArg::List(flow_args) => {
                BindingState::List(flow_args.into_iter().map(BindingState::from).collect())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Params {
    RefTerm(RefTerm),
    Inline(Signature),
}
impl Default for Params {
    fn default() -> Self {
        Self::Inline(Default::default())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum Mode {
    Determ,
    Erroneous,
    Failure,
    Multi,
    Nondeterm,
    #[default]
    Procedure,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annotation {
    pub suspending: bool,
    pub mode: Mode,
    pub flow_pattern: FlowPattern,
}
impl Annotation {
    pub fn new(suspending: bool, mode: Option<Mode>, flow_pattern: FlowPattern) -> Self {
        Self { suspending, mode: mode.unwrap_or_default(), flow_pattern }
    }

    #[inline]
    pub fn binding_flow(&self) -> Option<Bindings> {
        self.flow_pattern.binding_flow()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyFlowPattern {
    In,
    Out,
    InOut,
}
impl PropertyFlowPattern {
    pub fn in_binding_flow(&self) -> Option<BindingState> {
        match self {
            PropertyFlowPattern::In | PropertyFlowPattern::InOut => Some(BindingState::Bound),
            PropertyFlowPattern::Out => None,
        }
    }

    pub fn out_binding_flow(&self) -> Option<BindingState> {
        match self {
            PropertyFlowPattern::Out | PropertyFlowPattern::InOut => Some(BindingState::Free),
            PropertyFlowPattern::In => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FlowArg {
    In,
    Out,
    Compound { functor_name: Name, functor_flow_args: Box<[FlowArg]> },
    List(Box<[FlowArg]>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FlowPattern {
    AnyFlow,
    FlowPattern { flow_args: Box<[FlowArg]>, ellipsis: bool },
}
impl FlowPattern {
    pub fn new(flow_args: Box<[FlowArg]>, ellipsis: bool) -> Self {
        Self::FlowPattern { flow_args, ellipsis }
    }

    pub fn binding_flow(&self) -> Option<Bindings> {
        match self {
            // TODO: handle anyflows. right now we just ignore them.
            FlowPattern::AnyFlow => None,
            FlowPattern::FlowPattern { flow_args, .. } => {
                Some(flow_args.iter().map(BindingState::from).collect())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArgAttribute {
    /// [in]
    In,
    /// [out]
    Out,
    /// [formatString]
    FormatString,
    /// [inline]
    Inline { size: Option<u32> },
    /// [byValue]
    ByValue,
    /// [default]
    Default,
    /// [this]
    This,
}

const _: () = assert!(std::mem::size_of::<FormalArg>() == 40, "consider adjusting smallvec size");
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FormalArg {
    pub ty: TypeRefId,
    pub default_value: Option<ExprId>,
    pub attributes: SmallVec<[ArgAttribute; 1]>,
}
impl FormalArg {
    pub fn new(
        ty: TypeRefId,
        default_value: Option<ExprId>,
        attributes: impl Iterator<Item = ArgAttribute>,
    ) -> Self {
        Self { ty, default_value, attributes: attributes.collect() }
    }

    #[inline]
    pub fn has_default(&self) -> bool {
        self.default_value.is_some()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Signature {
    pub params: Box<[FormalArg]>,
    pub ret_type: Option<TypeRefId>,
    pub annotations: Vec<Annotation>,
}
impl Signature {
    pub fn new(
        params: Box<[FormalArg]>,
        ret_type: Option<TypeRefId>,
        annotations: Vec<Annotation>,
    ) -> Self {
        Self { params, ret_type, annotations }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermType {
    Expr,
    Stmt,
    /// Unable to determine if the term is an expression or a statement.
    ExprStmt,
}

impl TermType {
    #[inline]
    pub fn can_be_expr(&self) -> bool {
        matches!(self, TermType::Expr | TermType::ExprStmt)
    }

    #[inline]
    pub fn can_be_stmt(&self) -> bool {
        matches!(self, TermType::Stmt | TermType::ExprStmt)
    }

    #[inline]
    pub fn most_specific(&self, other: TermType) -> TermType {
        match self {
            TermType::ExprStmt => other,
            _ => self.clone(),
        }
    }

    #[inline]
    pub fn from_bool(is_expr: bool) -> TermType {
        if is_expr { TermType::Expr } else { TermType::Stmt }
    }

    #[inline]
    pub fn compatible_with_bool(&self, is_expr: bool) -> bool {
        match self {
            TermType::ExprStmt => true,
            TermType::Expr => is_expr,
            TermType::Stmt => !is_expr,
        }
    }

    pub fn compatible_with(&self, other: &TermType) -> bool {
        match (self, other) {
            (TermType::ExprStmt, _) => true,
            (_, TermType::ExprStmt) => true,
            (TermType::Expr, TermType::Expr) => true,
            (TermType::Stmt, TermType::Stmt) => true,
            (TermType::Expr, TermType::Stmt) => false,
            (TermType::Stmt, TermType::Expr) => false,
        }
    }
}

/// An arity for a named item e.g. predicate or property.
///
/// Example syntax: `foo` or `bar/2->`, `baz/1...`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arity {
    /// The name the arity is associated with.
    pub name: Name,
    /// The parameters of the arity (parameter count, ellipsis, returns).
    params: Option<ArityParams>,
}
impl Arity {
    pub fn new<T: Into<Option<ArityParams>>>(name: Name, params: T) -> Self {
        Self { name, params: params.into() }
    }

    pub fn arg_count(&self) -> RangeInclusive<u8> {
        match &self.params {
            Some(params) => match (params.count, params.ellipsis) {
                (count, true) => count..=u8::MAX,
                (count, false) => count..=count,
            },
            None => 0..=u8::MAX,
        }
    }

    pub fn term_type(&self) -> TermType {
        match &self.params {
            Some(p) => match p.returns {
                true => TermType::Expr,
                false => TermType::Stmt,
            },
            None => TermType::ExprStmt,
        }
    }
}
impl TryFrom<ast::Arity> for Arity {
    type Error = ();

    fn try_from(ast: ast::Arity) -> Result<Self, Self::Error> {
        let ident = ast.ident_token().ok_or(())?;
        let name = Name::new(ident.text());
        let count = ast.int_number_token().and_then(|n| n.text().parse().ok());
        let ellipsis = ast.dotdotdot_token().is_some();
        let returns = ast.arrow_token().is_some();
        if count.is_some() || ellipsis || returns {
            Ok(Arity::new(name, ArityParams { count: count.unwrap_or(0), ellipsis, returns }))
        } else {
            Ok(Arity::new(name, None))
        }
    }
}

/// The parameters of an arity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArityParams {
    /// The number of parameters the arity has.
    count: u8,
    /// Whether the arity has an ellipsis `...`.
    ellipsis: bool,
    /// Whether the arity returns a value.
    returns: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StringOrConst {
    String(Symbol),
    Const(RefTerm),
}

/// The name of the `implement`, `class`, or `interface` item.
/// The generic parameters can only be scope parameters (i.e. starting with `@`).
///
/// Example syntax: `foo` or `bar{@T}`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeNameDecl {
    /// The same as `self.scope_name` but in lower case.
    pub(crate) scope_name: Name,
    /// The scope parameters passed to the scope name.
    /// This includes the '@' in the beginning of the parameter.
    /// Example: ["@Data", "@V"]
    pub(crate) params: Box<[TypeRefId]>,
}
impl ScopeNameDecl {
    pub fn new(name_str: &str, params: Box<[TypeRefId]>) -> Self {
        Self { scope_name: Name::new(name_str), params }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Functor {
    pub name: Name,
    pub params: Box<[FormalArg]>,
}
