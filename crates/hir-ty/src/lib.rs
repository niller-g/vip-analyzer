mod infer;
mod lower;

pub mod db;

use hir_def::hir::{Annotation, Name, VarName};
pub use {
    infer::{InferenceResult, InferenceTyDiagnosticSource},
    lower::{TyLoweringContext, diagnostics::*},
};

pub enum Ty {
    Error,
    /// A list of some type, e.g. `T*`.
    List(Box<Ty>),
    /// I.e. a scope type, e.g. `@T`.
    /// The [`VarName`] is without the `@` prefix.
    AssociatedType(VarName),
    /// I.e. a variable type, e.g. `T`.
    TypeParam(VarName),
    BoundVar(VarName),
    /// A function type, e.g. `(T) -> U`.
    Fn(FnSignature),
    CompoundDomain(CompoundDomain),
    Numeric,
}

pub struct FnSignature {
    pub params: Box<[Ty]>,
    pub return_ty: Option<Box<Ty>>,
    pub annotations: Box<[Annotation]>,
}

pub struct CompoundDomain {
    pub name: Name,
    pub functor_variants: Box<[FunctorVariant]>,
}

pub struct FunctorVariant {
    pub name: Name,
    pub params: Box<[Ty]>,
}
