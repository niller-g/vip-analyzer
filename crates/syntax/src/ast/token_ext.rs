use super::AstNode;
use crate::ast::{self, AstToken};
use rowan::TextRange;

impl ast::Comment {
    pub fn kind(&self) -> CommentKind {
        CommentKind::from_text(self.text())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct CommentKind {
    pub shape: CommentShape,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CommentShape {
    Line,
    Block,
}

impl CommentShape {
    pub fn is_line(self) -> bool {
        self == CommentShape::Line
    }

    pub fn is_block(self) -> bool {
        self == CommentShape::Block
    }
}

impl CommentKind {
    const BY_PREFIX: [(&'static str, CommentKind); 2] = [
        ("%", CommentKind { shape: CommentShape::Line }),
        ("/*", CommentKind { shape: CommentShape::Block }),
    ];

    pub(crate) fn from_text(text: &str) -> CommentKind {
        let &(_prefix, kind) = CommentKind::BY_PREFIX
            .iter()
            .find(|&(prefix, _kind)| text.starts_with(prefix))
            .unwrap();
        kind
    }

    pub fn prefix(&self) -> &'static str {
        let &(prefix, _) =
            CommentKind::BY_PREFIX.iter().rev().find(|(_, kind)| kind == self).unwrap();
        prefix
    }
}

impl ast::Whitespace {
    pub fn spans_multiple_lines(&self) -> bool {
        let text = self.text();
        text.find('\n').is_some_and(|idx| text[idx + 1..].contains('\n'))
    }
}

#[derive(Debug)]

enum QuoteKind {
    Single,
    Double,
    Raw { open: char },
}
impl QuoteKind {
    fn get_closing_quote(&self) -> &char {
        match self {
            QuoteKind::Single => &'\'',
            QuoteKind::Double => &'"',
            QuoteKind::Raw { open } => match open {
                '(' => &')',
                ')' => &'(',
                '[' => &']',
                ']' => &'[',
                '{' => &'}',
                '}' => &'{',
                '<' => &'>',
                '>' => &'<',
                _ => open,
            },
        }
    }
}

#[derive(Debug)]
struct QuotedOffsets {
    #[expect(unused)]
    pub quotes: (TextRange, TextRange),
    pub quote_kind: QuoteKind,
    pub contents: TextRange,
}
impl QuotedOffsets {
    fn new(literal: &str) -> Option<QuotedOffsets> {
        let len = literal.len();
        let (left_quote, kind) = match literal.get(0..1)? {
            "\"" if len >= 2 => (0..1, QuoteKind::Double),
            "'" if len >= 2 => (0..1, QuoteKind::Single),
            "@" if len >= 3 => (0..2, QuoteKind::Raw { open: literal.chars().nth(1)? }),
            _ => return None,
        };
        let left_quote = left_quote.start.into()..left_quote.end.into();
        let right_quote = (len - 1)..len;
        if literal.get(right_quote)? != kind.get_closing_quote().to_string() {
            return None;
        }
        let right_quote = ((len - 1) as u32).into()..(len as u32).into();

        Some(QuotedOffsets {
            quotes: (
                TextRange::new(left_quote.start, left_quote.end),
                TextRange::new(right_quote.start, right_quote.end),
            ),
            quote_kind: kind,
            contents: TextRange::new(left_quote.end, right_quote.start),
        })
    }
}

impl ast::String {
    /// Multiple string tokens composes a single string sequence node.
    /// This method returns the string sequence node that contains this string token.
    pub fn string_seq(&self) -> Option<ast::StringSeq> {
        self.syntax()
            .parent()
            .and_then(ast::StringPart::cast)?
            .syntax()
            .parent()
            .and_then(ast::StringSeq::cast)
    }

    pub fn value(&self) -> Option<String> {
        let quoted = self.quote_offsets()?;
        let text = self.text();
        let text = &text[quoted.contents];

        match quoted.quote_kind {
            QuoteKind::Single | QuoteKind::Double => {
                let unescaped = unescape_quoted(text)?;
                Some(unescaped)
            }
            QuoteKind::Raw { open: _ } => {
                let unescaped = unescape_raw(text, *quoted.quote_kind.get_closing_quote())?;
                Some(unescaped)
            }
        }
    }

    pub fn text_range_between_quotes(&self) -> Option<TextRange> {
        self.quote_offsets().map(|it| it.contents)
    }

    fn quote_offsets(&self) -> Option<QuotedOffsets> {
        QuotedOffsets::new(self.text())
    }
}

fn unescape_quoted(src: &str) -> Option<String> {
    let mut res = String::new();
    let mut chars = src.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => res.push(match chars.next()? {
                '"' => '"',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '\'' => '\'',
                'u' => {
                    // Take the next 4 characters and try to parse them as a hex number
                    let mut hex = String::new();
                    for _ in 0..4 {
                        hex.push(chars.next()?);
                    }
                    let c = u32::from_str_radix(&hex, 16).ok()?;
                    std::char::from_u32(c)?
                }
                _ => return None,
            }),
            _ => res.push(c),
        }
    }

    Some(res)
}

fn unescape_raw(src: &str, closing_delimiter: char) -> Option<String> {
    let mut res = String::new();
    let mut chars = src.chars();
    while let Some(next) = chars.next() {
        let unescaped = if next == closing_delimiter {
            (chars.next()? == closing_delimiter).then_some(closing_delimiter)?
        } else {
            next
        };
        res.push(unescaped);
    }

    Some(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::make;

    fn check_quoted<'a>(lit: &str, expected: impl Into<Option<&'a str>> + Clone) {
        check_lit(&format!("'{lit}'"), expected.clone());
        check_lit(&format!("\"{lit}\""), expected);
    }
    fn check_raw<'a>(lit: &str, expected: impl Into<Option<&'a str>> + Clone) {
        check_lit(&format!("@\"{lit}\""), expected.clone());
        check_lit(&format!("@({lit})"), expected.clone());
        check_lit(&format!("@){lit}("), expected.clone());
        check_lit(&format!("@[{lit}]"), expected.clone());
        check_lit(&format!("@]{lit}["), expected.clone());
        check_lit(&format!("@{{{lit}}}"), expected.clone());
        check_lit(&format!("@}}{lit}{{"), expected.clone());
        check_lit(&format!("@<{lit}>"), expected.clone());
        check_lit(&format!("@>{lit}<"), expected.clone());
        check_lit(&format!("@|{lit}|"), expected);
    }
    fn check_lit<'a>(lit: &str, expected: impl Into<Option<&'a str>>) {
        assert_eq!(make::string_seq(lit).value().as_deref(), expected.into());
    }

    #[test]
    fn unescape_quoted_str() {
        check_quoted("", "");
        check_quoted(r"foo\", None);
        check_quoted(r"\foobar", None);
        check_quoted(r"\u000", None);
        check_quoted(r"foobar", "foobar");
        check_quoted(r"\nfoobar", "\nfoobar");
        check_quoted(r"C:\\Windows\\System32\\", "C:\\Windows\\System32\\");
        check_quoted(r#"\"foo\""#, "\"foo\"");
        check_quoted(r"\'bar\'", "\'bar\'");
        check_quoted(r"\t\t\t", "\t\t\t");
        check_quoted(r"\u00A9", "©");
    }

    #[test]
    fn unescape_raw_str() {
        check_raw("", "");
        check_raw(r"foobar", "foobar");
        check_raw(r"\foobar", "\\foobar");
        check_raw(r"\u000", "\\u000");
        check_raw(r"\nfoobar", "\\nfoobar");
        check_raw(r"C:\\Windows\\System32\\", "C:\\\\Windows\\\\System32\\\\");
        check_raw(r"C:\Windows\System32\", "C:\\Windows\\System32\\");
    }

    #[test]
    fn unescape_raw_quotes() {
        check_lit(r#"@[foo""bar["#, None);
        check_lit(r#"@[foo[bar[[baz]"#, "foo[bar[[baz");
        check_lit(r#"@"f\""#, "f\\");
        check_lit(r#"@"foo""bar""#, "foo\"bar");
        check_lit(r#"@(foo))bar)"#, "foo)bar");
    }
}
