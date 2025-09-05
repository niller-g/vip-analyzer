//! File and span related types.

mod ast_id;

pub use self::ast_id::{AstIdMap, AstIdNode, ErasedFileAstId, FileAstId};

// pub use syntax::Edition;
pub use text_size::{TextRange, TextSize};
pub use vfs::FileId;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FilePosition {
    pub file_id: FileId,
    pub offset: TextSize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file_id: FileId,
    pub range: TextRange,
}

// The first index is always the root node's AstId
/// The root ast id always points to the encompassing file, using this in spans is discouraged as
/// any range relative to it will be effectively absolute, ruining the entire point of anchored
/// relative text ranges.
pub const ROOT_ERASED_FILE_AST_ID: ErasedFileAstId = ErasedFileAstId::from_raw(0);

/// FileId used as the span for syntax node fixups. Any Span containing this file id is to be
/// considered fake.
pub const FIXUP_ERASED_FILE_AST_ID_MARKER: ErasedFileAstId =
    // we pick the second to last for this in case we ever consider making this a NonMaxU32, this
    // is required to be stable for the proc-macro-server
    ErasedFileAstId::from_raw(!0 - 1);
