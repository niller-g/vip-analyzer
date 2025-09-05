//! Diagnostics rendering and fixits.
//!
//! Most of the diagnostics originate from the dark depth of the compiler, and
//! are originally expressed in term of IR. When we emit the diagnostic, we are
//! usually not in the position to decide how to best "render" it in terms of
//! user-authored source code. We are especially not in the position to offer
//! fixits, as the compiler completely lacks the infrastructure to edit the
//! source code.
//!
//! Instead, we "bubble up" raw, structured diagnostics until the `hir` crate,
//! where we "cook" them so that each diagnostic is formulated in terms of `hir`
//! types. Well, at least that's the aspiration, the "cooking" is somewhat
//! ad-hoc at the moment. Anyways, we get a bunch of ide-friendly diagnostic
//! structs from hir, and we want to render them to unified serializable
//! representation (span, level, message) here. If we can, we also provide
//! fixits. By the way, that's why we want to keep diagnostics structured
//! internally -- so that we have all the info to make fixes.
//!
//! We have one "handler" module per diagnostic code. Such a module contains
//! rendering, optional fixes and tests. It's OK if some low-level compiler
//! functionality ends up being tested via a diagnostic.
//!
//! There are also a couple of ad-hoc diagnostics implemented directly here, we
//! don't yet have a great pattern for how to do them properly.

use core::fmt;
use hir::{InFile, Semantics};
use ide_db::FileId;
use ide_db::base_db::RootQueryDb;
use ide_db::{
    RootDatabase,
    assists::{Assist, AssistResolveStrategy},
    base_db::FileRange,
};
use syntax::SyntaxNodePtr;
use toolchain::Severity;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DiagnosticCode {
    SyntaxError,
    VipAnalyzer(&'static str, Severity),
}
impl fmt::Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticCode::SyntaxError => write!(f, "syntax-error"),
            DiagnosticCode::VipAnalyzer(r, _) => write!(f, "{r}"),
        }
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    pub code: DiagnosticCode,
    pub message: String,
    pub range: FileRange,
    pub severity: Severity,
    pub unused: bool,
    pub fixes: Option<Vec<Assist>>,
    // The node that will be affected by `#[allow]` and similar attributes.
    pub main_node: Option<InFile<SyntaxNodePtr>>,
}

impl Diagnostic {
    fn new(
        code: DiagnosticCode,
        message: impl Into<String>,
        range: impl Into<FileRange>,
    ) -> Diagnostic {
        let message = message.into();
        Diagnostic {
            code: code.clone(),
            message,
            range: range.into(),
            severity: match code {
                DiagnosticCode::SyntaxError => Severity::Error,
                DiagnosticCode::VipAnalyzer(_, s) => s,
            },
            unused: false,
            fixes: None,
            main_node: None,
        }
    }

    #[expect(dead_code)]
    fn with_main_node(mut self, main_node: InFile<SyntaxNodePtr>) -> Diagnostic {
        self.main_node = Some(main_node);
        self
    }

    #[expect(dead_code)]
    fn with_fixes(mut self, fixes: Option<Vec<Assist>>) -> Diagnostic {
        self.fixes = fixes;
        self
    }

    #[expect(dead_code)]
    fn with_unused(mut self, unused: bool) -> Diagnostic {
        self.unused = unused;
        self
    }
}

/// Request parser level diagnostics for the given [`FileId`].
pub fn syntax_diagnostics(db: &RootDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let _p = tracing::info_span!("syntax_diagnostics").entered();

    // [#3434] Only take first 128 errors to prevent slowing down editor/ide, the number 128 is chosen arbitrarily.
    db.parse_errors(file_id)
        .into_iter()
        .flatten()
        .take(128)
        .map(|err| {
            Diagnostic::new(
                DiagnosticCode::SyntaxError,
                format!("Syntax Error: {err}"),
                FileRange { file_id, range: err.range() },
            )
        })
        .collect()
}

/// Request semantic diagnostics for the given [`FileId`]. The produced diagnostics may point to other files
/// due to macros.
pub fn semantic_diagnostics(
    db: &RootDatabase,
    _resolve: &AssistResolveStrategy,
    file_id: FileId,
) -> Vec<Diagnostic> {
    let _p = tracing::info_span!("semantic_diagnostics").entered();
    let sema = Semantics::new(db);
    let res = Vec::new();

    let _parse = sema.parse(file_id);

    res
}

/// Request both syntax and semantic diagnostics for the given [`FileId`].
pub fn full_diagnostics(
    db: &RootDatabase,
    resolve: &AssistResolveStrategy,
    file_id: FileId,
) -> Vec<Diagnostic> {
    let mut res = syntax_diagnostics(db, file_id);
    let sema = semantic_diagnostics(db, resolve, file_id);
    res.extend(sema);
    res
}
