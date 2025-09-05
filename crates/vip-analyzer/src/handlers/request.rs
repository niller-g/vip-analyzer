//! This module is responsible for implementing handlers for Language Server
//! Protocol. This module specifically handles requests.

use crate::{
    config::Config,
    diagnostics::convert_diagnostic,
    global_state::{FetchWorkspaceRequest, GlobalState, GlobalStateSnapshot},
    line_index::LineEndings,
    lsp::{LspError, from_proto, to_proto},
    lsp_ext::{self},
};
use anyhow::Context;
use ide::FileRange;
use ide_db::{FxHashMap, assists::AssistResolveStrategy, text_edit::TextEdit};
use itertools::Itertools;
use lsp_server::ErrorCode;
use lsp_types::{Location, ResourceOp, ResourceOperationKind, TextDocumentIdentifier, Url};
use std::{io::Write as _, ops::Not, process::Stdio};
use stdx::format_to;
use syntax::{TextRange, TextSize};
use vfs::{AbsPath, VfsPath};

pub(crate) fn handle_workspace_reload(state: &mut GlobalState, _: ()) -> anyhow::Result<()> {
    let req = FetchWorkspaceRequest { path: None };
    state.fetch_workspaces_queue.request_op("reload workspace request".to_owned(), req);
    Ok(())
}

pub(crate) fn handle_analyzer_status(
    snap: GlobalStateSnapshot,
    params: lsp_ext::AnalyzerStatusParams,
) -> anyhow::Result<String> {
    let _p = tracing::info_span!("handle_analyzer_status").entered();

    let mut buf = String::new();

    // let mut file_id = None;
    if let Some(tdi) = params.text_document {
        match from_proto::file_id(&snap, &tdi.uri) {
            Ok(_it) => {
                // file_id = Some(it)
            }
            Err(_) => format_to!(buf, "file {} not found in vfs", tdi.uri),
        }
    }

    if snap.workspaces.is_empty() {
        buf.push_str("No workspaces\n")
    } else {
        buf.push_str("Workspaces:\n");
        format_to!(
            buf,
            "Loaded {} workspace{}.\n",
            snap.workspaces.len(),
            if snap.workspaces.len() == 1 { "" } else { "s" }
        );

        format_to!(
            buf,
            "Workspace root folders: {:?}",
            snap.workspaces.iter().map(|ws| ws.manifest_path().as_ref()).collect::<Vec<&AbsPath>>()
        );
    }

    buf.push_str("\nVersion: \n");
    format_to!(buf, "{}", crate::version());

    buf.push_str("\nConfiguration: \n");
    format_to!(buf, "{:?}", snap.config);

    Ok(buf)
}

pub(crate) fn handle_view_syntax_tree(
    snap: GlobalStateSnapshot,
    params: lsp_ext::ViewSyntaxTreeParams,
) -> anyhow::Result<String> {
    let _p = tracing::info_span!("handle_view_syntax_tree").entered();
    let id = from_proto::file_id(&snap, &params.text_document.uri)?;
    let res = snap.analysis.view_syntax_tree(id)?;
    Ok(res)
}

pub(crate) fn handle_view_file_text(
    snap: GlobalStateSnapshot,
    params: lsp_types::TextDocumentIdentifier,
) -> anyhow::Result<String> {
    let file_id = from_proto::file_id(&snap, &params.uri)?;
    Ok(snap.analysis.file_text(file_id)?.to_string())
}

pub(crate) fn handle_goto_definition(
    snap: GlobalStateSnapshot,
    params: lsp_types::GotoDefinitionParams,
) -> anyhow::Result<Option<lsp_types::GotoDefinitionResponse>> {
    let _p = tracing::info_span!("handle_goto_definition").entered();
    let position = from_proto::file_position(&snap, params.text_document_position_params)?;
    let nav_info = match snap.analysis.goto_definition(position)? {
        None => return Ok(None),
        Some(it) => it,
    };
    let src = FileRange { file_id: position.file_id, range: nav_info.range };
    let res = to_proto::goto_definition_response(&snap, Some(src), nav_info.info)?;
    Ok(Some(res))
}

pub(crate) fn handle_goto_declaration(
    snap: GlobalStateSnapshot,
    params: lsp_types::request::GotoDeclarationParams,
) -> anyhow::Result<Option<lsp_types::request::GotoDeclarationResponse>> {
    let _p = tracing::info_span!("handle_goto_declaration").entered();
    let position = from_proto::file_position(&snap, params.text_document_position_params.clone())?;
    let nav_info = match snap.analysis.goto_declaration(position)? {
        None => return handle_goto_definition(snap, params),
        Some(it) => it,
    };
    let src = FileRange { file_id: position.file_id, range: nav_info.range };
    let res = to_proto::goto_definition_response(&snap, Some(src), nav_info.info)?;
    Ok(Some(res))
}

pub(crate) fn handle_references(
    snap: GlobalStateSnapshot,
    params: lsp_types::ReferenceParams,
) -> anyhow::Result<Option<Vec<Location>>> {
    let _p = tracing::info_span!("handle_references").entered();
    let position = from_proto::file_position(&snap, params.text_document_position)?;

    let Some(refs) = snap.analysis.find_all_refs(position, None)? else {
        return Ok(None);
    };

    let include_declaration = params.context.include_declaration;
    let locations = refs
        .into_iter()
        .flat_map(|refs| {
            let decl = if include_declaration {
                refs.declaration
                    .map(|nav| FileRange { file_id: nav.file_id, range: nav.focus_or_full_range() })
            } else {
                None
            };
            refs.references
                .into_iter()
                .flat_map(|(file_id, refs)| {
                    refs.into_iter().map(move |(range, _)| FileRange { file_id, range })
                })
                .chain(decl)
        })
        .unique()
        .filter_map(|frange| to_proto::location(&snap, frange).ok())
        .collect();

    Ok(Some(locations))
}

pub(crate) fn handle_formatting(
    snap: GlobalStateSnapshot,
    params: lsp_types::DocumentFormattingParams,
) -> anyhow::Result<Option<Vec<lsp_types::TextEdit>>> {
    let _p = tracing::info_span!("handle_formatting").entered();

    run_vipfmt(&snap, params.text_document, None)
}

pub(crate) fn handle_document_diagnostics(
    snap: GlobalStateSnapshot,
    params: lsp_types::DocumentDiagnosticParams,
) -> anyhow::Result<lsp_types::DocumentDiagnosticReportResult> {
    let file_id = from_proto::file_id(&snap, &params.text_document.uri)?;

    let line_index = snap.file_line_index(file_id)?;
    let supports_related = snap.config.text_document_diagnostic_related_document_support();
    let mut related_documents = FxHashMap::default();
    let diagnostics = snap
        .analysis
        .full_diagnostics(AssistResolveStrategy::None, file_id)?
        .into_iter()
        .filter_map(|d| {
            let file = d.range.file_id;
            if file == file_id {
                let diagnostic = convert_diagnostic(&line_index, d);
                return Some(diagnostic);
            }
            if supports_related {
                let (diagnostics, line_index) = related_documents
                    .entry(file)
                    .or_insert_with(|| (Vec::new(), snap.file_line_index(file).ok()));
                let diagnostic = convert_diagnostic(line_index.as_mut()?, d);
                diagnostics.push(diagnostic);
            }
            None
        });
    Ok(lsp_types::DocumentDiagnosticReportResult::Report(
        lsp_types::DocumentDiagnosticReport::Full(lsp_types::RelatedFullDocumentDiagnosticReport {
            full_document_diagnostic_report: lsp_types::FullDocumentDiagnosticReport {
                result_id: Some("vip-analyzer".to_owned()),
                items: diagnostics.collect(),
            },
            related_documents: related_documents.is_empty().not().then(|| {
                related_documents
                    .into_iter()
                    .map(|(id, (items, _))| {
                        (
                            to_proto::url(&snap, id),
                            lsp_types::DocumentDiagnosticReportKind::Full(
                                lsp_types::FullDocumentDiagnosticReport {
                                    result_id: Some("vip-analyzer".to_owned()),
                                    items,
                                },
                            ),
                        )
                    })
                    .collect()
            }),
        }),
    ))
}

fn run_vipfmt(
    snap: &GlobalStateSnapshot,
    text_document: TextDocumentIdentifier,
    range: Option<lsp_types::Range>,
) -> anyhow::Result<Option<Vec<lsp_types::TextEdit>>> {
    let file_id = from_proto::file_id(snap, &text_document.uri)?;
    let file = snap.analysis.file_text(file_id)?;

    let line_index = snap.file_line_index(file_id)?;

    // try to chdir to the file so we can respect `rustfmt.toml`
    // FIXME: use `rustfmt --config-path` once
    // https://github.com/rust-lang/rustfmt/issues/4660 gets fixed
    let current_dir = match text_document.uri.to_file_path() {
        Ok(mut path) => {
            // pop off file name
            if path.pop() && path.is_dir() { path } else { std::env::current_dir()? }
        }
        Err(_) => {
            tracing::error!(text_document = ?text_document.uri, "Unable to get path");
            std::env::current_dir()?
        }
    };

    let mut command = toolchain::command(crate::vipfmt_path()?, current_dir);

    if range.is_some() {
        return Err(LspError::new(
            ErrorCode::InvalidRequest as i32,
            String::from("vipfmt range formatting is unavailable"),
        )
        .into());
    }

    let output = {
        let _p = tracing::info_span!("vipfmt", ?command).entered();

        let mut vipfmt = command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context(format!("Failed to spawn {command:?}"))?;

        vipfmt.stdin.as_mut().unwrap().write_all(file.as_bytes())?;

        vipfmt.wait_with_output()?
    };

    let captured_stdout = String::from_utf8(output.stdout)?;
    let captured_stderr = String::from_utf8(output.stderr).unwrap_or_default();

    if !output.status.success() {
        return match output.status.code() {
            Some(1) => {
                // While `vipfmt` doesn't have a specific exit code for parse errors this is the
                // likely cause exiting with 1. Most Language Servers swallow parse errors on
                // formatting because otherwise an error is surfaced to the user on top of the
                // syntax error diagnostics they're already receiving. This is especially jarring
                // if they have format on save enabled.
                tracing::warn!(
                    ?command,
                    %captured_stderr,
                    "vipfmt exited with status 1"
                );
                Ok(None)
            }
            _ => {
                // Something else happened - e.g. `vipfmt` is missing or caught a signal
                Err(LspError::new(
                    -32900,
                    format!(
                        r#"vipfmt exited with:
                           Status: {}
                           stdout: {captured_stdout}
                           stderr: {captured_stderr}"#,
                        output.status,
                    ),
                )
                .into())
            }
        };
    }

    let (new_text, new_line_endings) = LineEndings::normalize(captured_stdout);

    if line_index.endings != new_line_endings {
        // If line endings are different, send the entire file.
        // Diffing would not work here, as the line endings might be the only
        // difference.
        Ok(Some(to_proto::text_edit_vec(
            &line_index,
            TextEdit::replace(TextRange::up_to(TextSize::of(&*file)), new_text),
        )))
    } else if *file == new_text {
        // The document is already formatted correctly -- no edits needed.
        Ok(None)
    } else {
        Ok(Some(to_proto::text_edit_vec(&line_index, diff(&file, &new_text))))
    }
}

#[expect(unused)]
fn to_url(path: VfsPath) -> Option<Url> {
    let path = path.as_path()?;
    let str_path = path.as_os_str().to_str()?;
    Url::from_file_path(str_path).ok()
}

#[expect(unused)]
fn resource_ops_supported(config: &Config, kind: ResourceOperationKind) -> anyhow::Result<()> {
    if !matches!(config.workspace_edit_resource_operations(), Some(resops) if resops.contains(&kind))
    {
        return Err(LspError::new(
            ErrorCode::RequestFailed as i32,
            format!(
                "Client does not support {} capability.",
                match kind {
                    ResourceOperationKind::Create => "create",
                    ResourceOperationKind::Rename => "rename",
                    ResourceOperationKind::Delete => "delete",
                }
            ),
        )
        .into());
    }

    Ok(())
}

#[expect(unused)]
fn resolve_resource_op(op: &ResourceOp) -> ResourceOperationKind {
    match op {
        ResourceOp::Create(_) => ResourceOperationKind::Create,
        ResourceOp::Rename(_) => ResourceOperationKind::Rename,
        ResourceOp::Delete(_) => ResourceOperationKind::Delete,
    }
}

pub(crate) fn diff(left: &str, right: &str) -> TextEdit {
    use dissimilar::Chunk;

    let chunks = dissimilar::diff(left, right);

    let mut builder = TextEdit::builder();
    let mut pos = TextSize::default();

    let mut chunks = chunks.into_iter().peekable();
    while let Some(chunk) = chunks.next() {
        if let (Chunk::Delete(deleted), Some(&Chunk::Insert(inserted))) = (chunk, chunks.peek()) {
            chunks.next().unwrap();
            let deleted_len = TextSize::of(deleted);
            builder.replace(TextRange::at(pos, deleted_len), inserted.into());
            pos += deleted_len;
            continue;
        }

        match chunk {
            Chunk::Equal(text) => {
                pos += TextSize::of(text);
            }
            Chunk::Delete(deleted) => {
                let deleted_len = TextSize::of(deleted);
                builder.delete(TextRange::at(pos, deleted_len));
                pos += deleted_len;
            }
            Chunk::Insert(inserted) => {
                builder.insert(pos, inserted.into());
            }
        }
    }
    builder.finish()
}

#[test]
fn diff_smoke_test() {
    let mut original = String::from("fn foo(a:u32){\n}");
    let result = "fn foo(a: u32) {}";
    let edit = diff(&original, result);
    edit.apply(&mut original);
    assert_eq!(original, result);
}
