use super::VipDiagnosticCode;

macro_rules! define_diagnostic_code {
    ($($(#[$meta:meta])* $name:ident = $code:expr,)*) => {
        impl VipDiagnosticCode {
            $(
                $(#[$meta])*
                pub const $name: &'static VipDiagnosticCode = &VipDiagnosticCode::Code($code);
            )*
        }
    };
}

define_diagnostic_code! {
    /// The error message is generated when the syntax error is detected.
    SYNTAX_ERROR = 150,
    /// The warning is generated if opening of a scope or namespace is not used and can be removed.
    SUPERFLUOUS_OPENING_OF_SCOPE_OR_NAMESPACE = 231,
    /// The warning message is generated for deprecated built-in entities, like trap/3, finally/2, findall/3, etc.
    DEPRECATED_BUILTIN_ENTITY = 303,
    /// The warning message is generated when the entity which declaration has the deprecated-attribute is used.
    /// OR
    /// The error message is generated when the entity which declaration has the retired-attribute is used.
    DEPRECATED_RETIRED_ENTITY = 304,
    /// The warning message is generated when a statically initialized object single fact is also
    /// explicitly initialized in all constructors.
    SUPERFLUOUS_STATIC_INITIALIZATION_FOR_OBJECT_SINGLE_FACT = 312,
    /// The error message is generated when a domain definition, interface definition or class declaration contains an used type parameter.
    SUPERFLUOUS_POLYMORPHIC_PARAMETER = 352,
    /// The warning message is generated when the variable is only created but is not used.
    UNUSED_VARIABLE = 507,
    /// he warning message is generated when the compiler detects that some codes
    /// are unreachable or excessive and therefore removes them.
    UNREACHABLE_OR_EXCESSIVE_CODE = 651,
    /// The warning message is generated when the compiler detects that a cut is superfluous and therefore removes it.
    SUPERFLUOUS_CUT = 652,
    /// The warning message is generated when the entity is not used (currently it is applied only
    /// to facts or fact variables).
    UNUSED_ENTITY = 654,
    /// The warning message is generated when the explicit conversion of a term with the built-in
    /// conversion predicates is superfluous.
    SUPERFLUOUS_CONVERSION = 655,
    /// The warning message is generated when the used fact (fact variable) was not initialized or asserted.
    NOT_ASSIGNED = 661,
    /// The warning message is generated when a fact (fact variable) is only initialized or asserted,
    /// but it is not used anywhere in the class.
    UNREFERENCED_ENTITY = 662,
    /// The warning message is generated when a fact variable is always initialized with a constant expression
    /// and it is never changed.
    FACT_IS_INITIALIZED_CONSTANT_EXPRESSION_AND_NEVER_CHANGES = 665,
}

impl VipDiagnosticCode {
    pub const fn is_unnecessary(&self) -> bool {
        matches!(
            self,
            Self::SUPERFLUOUS_OPENING_OF_SCOPE_OR_NAMESPACE
                | Self::SUPERFLUOUS_STATIC_INITIALIZATION_FOR_OBJECT_SINGLE_FACT
                | Self::SUPERFLUOUS_POLYMORPHIC_PARAMETER
                | Self::UNUSED_VARIABLE
                | Self::UNREACHABLE_OR_EXCESSIVE_CODE
                | Self::SUPERFLUOUS_CUT
                | Self::UNUSED_ENTITY
                | Self::SUPERFLUOUS_CONVERSION
                | Self::NOT_ASSIGNED
                | Self::UNREFERENCED_ENTITY
                | Self::FACT_IS_INITIALIZED_CONSTANT_EXPRESSION_AND_NEVER_CHANGES
        )
    }

    pub const fn is_deprecated(&self) -> bool {
        matches!(self, Self::DEPRECATED_BUILTIN_ENTITY | Self::DEPRECATED_RETIRED_ENTITY)
    }
}
