//! Conversion of rust-analyzer specific types to lsp_types equivalents.
#![expect(unused)]
use std::{
    iter::once,
    mem,
    sync::atomic::{AtomicU32, Ordering},
};

use ide::{
    Cancellable, FileId, FileRange, FileSystemEdit, Indel, NavigationTarget, SnippetEdit,
    SourceChange, TextEdit, TextRange, TextSize,
};
use ide_db::{FxHasher, source_change::ChangeAnnotationId};
use itertools::Itertools;
use paths::{Utf8Component, Utf8Prefix};
use semver::VersionReq;
use serde_json::to_value;
use vfs::AbsPath;

use crate::{
    config::Config,
    global_state::GlobalStateSnapshot,
    line_index::{LineEndings, LineIndex, PositionEncoding},
    lsp::LspError,
    lsp_ext::{self, SnippetTextEdit},
};

pub(crate) fn position(line_index: &LineIndex, offset: TextSize) -> lsp_types::Position {
    let line_col = line_index.index.line_col(offset);
    match line_index.encoding {
        PositionEncoding::Utf8 => lsp_types::Position::new(line_col.line, line_col.col),
        PositionEncoding::Wide(enc) => {
            let line_col = line_index.index.to_wide(enc, line_col).unwrap();
            lsp_types::Position::new(line_col.line, line_col.col)
        }
    }
}

pub(crate) fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    let start = position(line_index, range.start());
    let end = position(line_index, range.end());
    lsp_types::Range::new(start, end)
}

pub(crate) fn text_edit(line_index: &LineIndex, indel: Indel) -> lsp_types::TextEdit {
    let range = range(line_index, indel.delete);
    let new_text = match line_index.endings {
        LineEndings::Unix => indel.insert,
        LineEndings::Dos => indel.insert.replace('\n', "\r\n"),
    };
    lsp_types::TextEdit { range, new_text }
}

pub(crate) fn completion_text_edit(
    line_index: &LineIndex,
    insert_replace_support: Option<lsp_types::Position>,
    indel: Indel,
) -> lsp_types::CompletionTextEdit {
    let text_edit = text_edit(line_index, indel);
    match insert_replace_support {
        Some(cursor_pos) => lsp_types::InsertReplaceEdit {
            new_text: text_edit.new_text,
            insert: lsp_types::Range { start: text_edit.range.start, end: cursor_pos },
            replace: text_edit.range,
        }
        .into(),
        None => text_edit.into(),
    }
}

pub(crate) fn snippet_text_edit(
    line_index: &LineIndex,
    is_snippet: bool,
    indel: Indel,
    annotation: Option<ChangeAnnotationId>,
    client_supports_annotations: bool,
) -> lsp_ext::SnippetTextEdit {
    let annotation_id = annotation.filter(|_| client_supports_annotations).map(|it| it.to_string());
    let text_edit = text_edit(line_index, indel);
    let insert_text_format =
        if is_snippet { Some(lsp_types::InsertTextFormat::SNIPPET) } else { None };
    lsp_ext::SnippetTextEdit {
        range: text_edit.range,
        new_text: text_edit.new_text,
        insert_text_format,
        annotation_id,
    }
}

pub(crate) fn text_edit_vec(
    line_index: &LineIndex,
    text_edit: TextEdit,
) -> Vec<lsp_types::TextEdit> {
    text_edit.into_iter().map(|indel| self::text_edit(line_index, indel)).collect()
}

pub(crate) fn snippet_text_edit_vec(
    line_index: &LineIndex,
    is_snippet: bool,
    text_edit: TextEdit,
    clients_support_annotations: bool,
) -> Vec<lsp_ext::SnippetTextEdit> {
    let annotation = text_edit.change_annotation();
    text_edit
        .into_iter()
        .map(|indel| {
            self::snippet_text_edit(
                line_index,
                is_snippet,
                indel,
                annotation,
                clients_support_annotations,
            )
        })
        .collect()
}

pub(crate) fn url(snap: &GlobalStateSnapshot, file_id: FileId) -> lsp_types::Url {
    snap.file_id_to_url(file_id)
}

/// Returns a `Url` object from a given path, will lowercase drive letters if present.
/// This will only happen when processing windows paths.
///
/// When processing non-windows path, this is essentially the same as `Url::from_file_path`.
pub(crate) fn url_from_abs_path(path: &AbsPath) -> lsp_types::Url {
    let url = lsp_types::Url::from_file_path(path).unwrap();
    match path.components().next() {
        Some(Utf8Component::Prefix(prefix))
            if matches!(prefix.kind(), Utf8Prefix::Disk(_) | Utf8Prefix::VerbatimDisk(_)) =>
        {
            // Need to lowercase driver letter
        }
        _ => return url,
    }

    let driver_letter_range = {
        let (scheme, drive_letter, _rest) = match url.as_str().splitn(3, ':').collect_tuple() {
            Some(it) => it,
            None => return url,
        };
        let start = scheme.len() + ':'.len_utf8();
        start..(start + drive_letter.len())
    };

    // Note: lowercasing the `path` itself doesn't help, the `Url::parse`
    // machinery *also* canonicalizes the drive letter. So, just massage the
    // string in place.
    let mut url: String = url.into();
    url[driver_letter_range].make_ascii_lowercase();
    lsp_types::Url::parse(&url).unwrap()
}

pub(crate) fn optional_versioned_text_document_identifier(
    snap: &GlobalStateSnapshot,
    file_id: FileId,
) -> lsp_types::OptionalVersionedTextDocumentIdentifier {
    let url = url(snap, file_id);
    let version = snap.url_file_version(&url);
    lsp_types::OptionalVersionedTextDocumentIdentifier { uri: url, version }
}

pub(crate) fn location(
    snap: &GlobalStateSnapshot,
    frange: FileRange,
) -> Cancellable<lsp_types::Location> {
    let url = url(snap, frange.file_id);
    let line_index = snap.file_line_index(frange.file_id)?;
    let range = range(&line_index, frange.range);
    let loc = lsp_types::Location::new(url, range);
    Ok(loc)
}

/// Prefer using `location_link`, if the client has the cap.
pub(crate) fn location_from_nav(
    snap: &GlobalStateSnapshot,
    nav: NavigationTarget,
) -> Cancellable<lsp_types::Location> {
    let url = url(snap, nav.file_id);
    let line_index = snap.file_line_index(nav.file_id)?;
    let range = range(&line_index, nav.focus_or_full_range());
    let loc = lsp_types::Location::new(url, range);
    Ok(loc)
}

pub(crate) fn location_link(
    snap: &GlobalStateSnapshot,
    src: Option<FileRange>,
    target: NavigationTarget,
) -> Cancellable<lsp_types::LocationLink> {
    let origin_selection_range = match src {
        Some(src) => {
            let line_index = snap.file_line_index(src.file_id)?;
            let range = range(&line_index, src.range);
            Some(range)
        }
        None => None,
    };
    let (target_uri, target_range, target_selection_range) = location_info(snap, target)?;
    let res = lsp_types::LocationLink {
        origin_selection_range,
        target_uri,
        target_range,
        target_selection_range,
    };
    Ok(res)
}

fn location_info(
    snap: &GlobalStateSnapshot,
    target: NavigationTarget,
) -> Cancellable<(lsp_types::Url, lsp_types::Range, lsp_types::Range)> {
    let line_index = snap.file_line_index(target.file_id)?;

    let target_uri = url(snap, target.file_id);
    let target_range = range(&line_index, target.full_range);
    let target_selection_range =
        target.focus_range.map(|it| range(&line_index, it)).unwrap_or(target_range);
    Ok((target_uri, target_range, target_selection_range))
}

pub(crate) fn goto_definition_response(
    snap: &GlobalStateSnapshot,
    src: Option<FileRange>,
    targets: Vec<NavigationTarget>,
) -> Cancellable<lsp_types::GotoDefinitionResponse> {
    if snap.config.location_link() {
        let links = targets
            .into_iter()
            .unique_by(|nav| (nav.file_id, nav.full_range, nav.focus_range))
            .map(|nav| location_link(snap, src, nav))
            .collect::<Cancellable<Vec<_>>>()?;
        Ok(links.into())
    } else {
        let locations = targets
            .into_iter()
            .map(|nav| FileRange { file_id: nav.file_id, range: nav.focus_or_full_range() })
            .unique()
            .map(|range| location(snap, range))
            .collect::<Cancellable<Vec<_>>>()?;
        Ok(locations.into())
    }
}

fn outside_workspace_annotation_id() -> String {
    String::from("OutsideWorkspace")
}

pub(crate) fn implementation_title(count: usize) -> String {
    if count == 1 { "1 implementation".into() } else { format!("{count} implementations") }
}

pub(crate) fn reference_title(count: usize) -> String {
    if count == 1 { "1 reference".into() } else { format!("{count} references") }
}
