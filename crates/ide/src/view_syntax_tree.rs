use hir::Semantics;
use ide_db::{
    FileId, LineIndexDatabase, RootDatabase,
    line_index::{LineCol, LineIndex},
};
use span::{TextRange, TextSize};
use stdx::format_to;
use syntax::{
    AstNode, AstToken, NodeOrToken, SourceFile, SyntaxNode, SyntaxToken, WalkEvent,
    ast::{self},
};
use triomphe::Arc;

// Feature: Show Syntax Tree
//
// Shows a tree view with the syntax tree of the current file
//
// | Editor  | Panel Name |
// |---------|-------------|
// | VS Code | **Rust Syntax Tree** |
pub(crate) fn view_syntax_tree(db: &RootDatabase, file_id: FileId) -> String {
    let sema = Semantics::new(db);
    let line_index = db.line_index(file_id);
    let parse = sema.parse(file_id);

    let ctx = SyntaxTreeCtx { line_index, in_string: None };

    syntax_node_to_json(parse.syntax(), &ctx)
}

fn syntax_node_to_json(node: &SyntaxNode, ctx: &SyntaxTreeCtx) -> String {
    let mut result = String::new();
    for event in node.preorder_with_tokens() {
        match event {
            WalkEvent::Enter(it) => {
                let kind = it.kind();
                let (text_range, inner_range_str) = match &ctx.in_string {
                    Some(in_string) => {
                        let start_pos = TextPosition::new(&ctx.line_index, it.text_range().start());
                        let end_pos = TextPosition::new(&ctx.line_index, it.text_range().end());

                        let inner_start: u32 = it.text_range().start().into();
                        let inner_end: u32 = it.text_range().start().into();

                        let mut true_start = inner_start + in_string.offset;
                        let mut true_end = inner_end + in_string.offset;
                        for pos in &in_string.marker_positions {
                            if *pos >= inner_end {
                                break;
                            }

                            // We conditionally add to true_start in case
                            // the marker is between the start and end.
                            true_start += 2 * (*pos < inner_start) as u32;
                            true_end += 2;
                        }

                        let true_range = TextRange::new(true_start.into(), true_end.into());

                        (true_range, format!(r#","istart":{start_pos},"iend":{end_pos}"#,))
                    }
                    None => (it.text_range(), "".to_owned()),
                };

                let start = TextPosition::new(&ctx.line_index, text_range.start());
                let end = TextPosition::new(&ctx.line_index, text_range.end());

                match it {
                    NodeOrToken::Node(_) => {
                        format_to!(
                            result,
                            r#"{{"type":"Node","kind":"{kind:?}","start":{start},"end":{end}{inner_range_str},"children":["#
                        );
                    }
                    NodeOrToken::Token(token) => {
                        let comma = if token.next_sibling_or_token().is_some() { "," } else { "" };
                        match parse_vip_string(token, ctx) {
                            Some(parsed) => {
                                format_to!(
                                    result,
                                    r#"{{"type":"Node","kind":"{kind:?}","start":{start},"end":{end}{inner_range_str},"children":[{parsed}]}}{comma}"#
                                );
                            }
                            None => format_to!(
                                result,
                                r#"{{"type":"Token","kind":"{kind:?}","start":{start},"end":{end}{inner_range_str}}}{comma}"#
                            ),
                        }
                    }
                }
            }
            WalkEvent::Leave(it) => match it {
                NodeOrToken::Node(node) => {
                    let comma = if node.next_sibling_or_token().is_some() { "," } else { "" };
                    format_to!(result, "]}}{comma}")
                }
                NodeOrToken::Token(_) => (),
            },
        }
    }

    result
}

struct TextPosition {
    offset: TextSize,
    line: u32,
    col: u32,
}

impl std::fmt::Display for TextPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?},{},{}]", self.offset, self.line, self.col)
    }
}

impl TextPosition {
    pub(crate) fn new(line_index: &LineIndex, offset: TextSize) -> Self {
        let LineCol { line, col } = line_index.line_col(offset);
        Self { offset, line, col }
    }
}

fn parse_vip_string(token: SyntaxToken, ctx: &SyntaxTreeCtx) -> Option<String> {
    let string_node = ast::String::cast(token)?;
    let text = string_node.value()?;

    let mut trim_result = String::new();
    let mut marker_positions = Vec::new();
    let mut skipped = 0;
    let mut last_end = 0;
    for (start, part) in text.match_indices("$0") {
        marker_positions.push((start - skipped) as u32);
        trim_result.push_str(&text[last_end..start]);
        skipped += part.len();
        last_end = start + part.len();
    }
    trim_result.push_str(&text[last_end..text.len()]);

    let parsed = SourceFile::parse(&trim_result);

    if !parsed.errors().is_empty() {
        return None;
    }

    let node: &SyntaxNode = &parsed.syntax_node();

    if node.children().count() == 0 {
        // C'mon, you should have at least one node other than SOURCE_FILE
        return None;
    }

    let ctx = SyntaxTreeCtx {
        line_index: ctx.line_index.clone(),
        in_string: Some(InStringCtx {
            offset: string_node.text_range_between_quotes()?.start().into(),
            marker_positions,
        }),
    };

    Some(syntax_node_to_json(node, &ctx))
}

struct SyntaxTreeCtx {
    line_index: Arc<LineIndex>,
    in_string: Option<InStringCtx>,
}

struct InStringCtx {
    offset: u32,
    marker_positions: Vec<u32>,
}
