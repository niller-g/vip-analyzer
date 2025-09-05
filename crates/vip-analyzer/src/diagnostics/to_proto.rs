//! This module provides the functionality needed to convert diagnostics from
//! `cargo check` json format to the LSP diagnostic format.

use std::path::PathBuf;

use super::Fix;
use crate::{
    global_state::GlobalStateSnapshot, line_index::PositionEncoding,
    lsp::to_proto::url_from_abs_path,
};
use project_model::Project;
use toolchain::Severity;
use vfs::AbsPathBuf;

/// Determines the LSP severity from a diagnostic
pub(crate) fn diagnostic_severity(severity: &Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::FatalError | Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::Info => lsp_types::DiagnosticSeverity::INFORMATION,
        Severity::SeeAlso => lsp_types::DiagnosticSeverity::HINT,
    }
}

/// Changes the casing of a file path to match the actual file path on disk
fn fix_casing(file: AbsPathBuf) -> Option<AbsPathBuf> {
    let path: PathBuf = file.into();
    let canonical = path.canonicalize().ok()?;

    paths::Utf8PathBuf::from_path_buf(canonical)
        .ok()
        .and_then(|it| vfs::AbsPathBuf::try_from(it).ok())
}

/// Converts a Rust span to a LSP location
fn location(
    span: &toolchain::Location,
    snap: &GlobalStateSnapshot,
    project: &Project,
) -> lsp_types::Location {
    let file_name = project
        .resolve_path(&span.file_name)
        .and_then(fix_casing)
        .unwrap_or_else(|| project.manifest_path().to_path_buf());
    let uri = url_from_abs_path(&file_name);

    let range = {
        let position_encoding = snap.config.negotiated_encoding();
        lsp_types::Range::new(
            position(&position_encoding, span, span.line_start, span.column_start),
            position(&position_encoding, span, span.line_start, span.column_start),
        )
    };
    lsp_types::Location::new(uri, range)
}

fn position(
    _position_encoding: &PositionEncoding,
    _span: &toolchain::Location,
    line_number: usize,
    column_offset_utf32: usize,
) -> lsp_types::Position {
    lsp_types::Position {
        line: (line_number as u32).saturating_sub(1),
        character: (column_offset_utf32 as u32).saturating_sub(1),
    }
}

#[derive(Debug)]
pub(crate) struct MappedVipDiagnostic {
    pub(crate) url: lsp_types::Url,
    pub(crate) diagnostic: lsp_types::Diagnostic,
    pub(crate) fix: Option<Box<Fix>>,
}

/// Converts a VIP root diagnostic to LSP form
pub(crate) fn map_vip_diagnostic_to_lsp(
    diag: &toolchain::VipBuilderDiagnostic,
    snap: &GlobalStateSnapshot,
    project: &Project,
) -> Vec<MappedVipDiagnostic> {
    let mut tags = Vec::new();
    if diag.code.as_ref().is_some_and(toolchain::VipDiagnosticCode::is_unnecessary) {
        tags.push(lsp_types::DiagnosticTag::UNNECESSARY);
    }
    if diag.code.as_ref().is_some_and(toolchain::VipDiagnosticCode::is_deprecated) {
        tags.push(lsp_types::DiagnosticTag::DEPRECATED);
    }

    let location = match &diag.location() {
        Some(loc) => location(loc, snap, project),
        None => lsp_types::Location::new(
            url_from_abs_path(project.manifest_path()),
            lsp_types::Range::default(),
        ),
    };
    let diagnostic = MappedVipDiagnostic {
        url: location.uri.clone(),
        diagnostic: lsp_types::Diagnostic {
            range: location.range,
            severity: Some(diagnostic_severity(&diag.severity)),
            code: diag
                .code
                .as_ref()
                .map(|code| lsp_types::NumberOrString::String(code.to_string())),
            code_description: None,
            source: Some("vipCompiler".to_owned()),
            message: diag.message.clone(),
            related_information: None,
            tags: if tags.is_empty() { None } else { Some(tags.clone()) },
            data: None,
        },
        fix: None,
    };

    vec![diagnostic]
}
