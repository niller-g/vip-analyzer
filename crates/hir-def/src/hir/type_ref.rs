use super::{RefTerm, VarName};
use {
    super::{Functor, Signature},
    la_arena::Idx,
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeRef {
    Error,
    List(TypeRefId),
    /// The [`VarName`] is without the `@` prefix.
    ScopeType(VarName),
    VarType(VarName),
    Wildcard,
    Name(RefTerm),
    Fn(Signature),
    Compound(Box<[Functor]>),
    Numeric,
}

pub type TypeRefId = Idx<TypeRef>;
