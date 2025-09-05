use hir_def::hir::type_ref::TypeRefId;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TyLoweringDiagnostic {
    pub source: TypeRefId,
    pub kind: TyLoweringDiagnosticKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyLoweringDiagnosticKind {}
