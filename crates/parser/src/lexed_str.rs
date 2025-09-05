//! Lexing `&str` into a sequence of Visual Prolog tokens.
//!
//! Note that these tokens, unlike the tokens we feed into the parser, do
//! include info about comments and whitespace.

use std::ops;

use crate::{
    SyntaxKind::{self, *},
    T,
};
use lexer::closing_str_delimiter;

pub struct LexedStr<'a> {
    text: &'a str,
    kind: Vec<SyntaxKind>,
    start: Vec<u32>,
    error: Vec<LexError>,
}

struct LexError {
    msg: String,
    token: u32,
}

impl<'a> LexedStr<'a> {
    pub fn new(text: &'a str) -> LexedStr<'a> {
        let _p = tracing::info_span!("LexedStr::new").entered();
        let mut conv = Converter::new(text);

        while let Some(token) = lexer::tokenize(&text[conv.offset..]).next() {
            let token_text = &text[conv.offset..][..token.len as usize];

            conv.extend_token(&token.kind, token_text);
        }

        conv.finalize_with_eof()
    }

    pub fn single_token(text: &'a str) -> Option<(SyntaxKind, Option<String>)> {
        if text.is_empty() {
            return None;
        }

        let token = lexer::tokenize(text).next()?;
        if token.len as usize != text.len() {
            return None;
        }

        let mut conv = Converter::new(text);
        conv.extend_token(&token.kind, text);
        match &*conv.res.kind {
            [kind] => Some((*kind, conv.res.error.pop().map(|it| it.msg))),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        self.text
    }

    pub fn len(&self) -> usize {
        self.kind.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn kind(&self, i: usize) -> SyntaxKind {
        assert!(i < self.len());
        self.kind[i]
    }

    pub fn text(&self, i: usize) -> &str {
        self.range_text(i..i + 1)
    }

    pub fn range_text(&self, r: ops::Range<usize>) -> &str {
        assert!(r.start < r.end && r.end <= self.len());
        let lo = self.start[r.start] as usize;
        let hi = self.start[r.end] as usize;
        &self.text[lo..hi]
    }

    // Naming is hard.
    pub fn text_range(&self, i: usize) -> ops::Range<usize> {
        assert!(i < self.len());
        let lo = self.start[i] as usize;
        let hi = self.start[i + 1] as usize;
        lo..hi
    }
    pub fn text_start(&self, i: usize) -> usize {
        assert!(i <= self.len());
        self.start[i] as usize
    }
    pub fn text_len(&self, i: usize) -> usize {
        assert!(i < self.len());
        let r = self.text_range(i);
        r.end - r.start
    }

    pub fn error(&self, i: usize) -> Option<&str> {
        assert!(i < self.len());
        let err = self.error.binary_search_by_key(&(i as u32), |i| i.token).ok()?;
        Some(self.error[err].msg.as_str())
    }

    pub fn errors(&self) -> impl Iterator<Item = (usize, &str)> + '_ {
        self.error.iter().map(|it| (it.token as usize, it.msg.as_str()))
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.kind.push(kind);
        self.start.push(offset as u32);
    }
}

struct Converter<'a> {
    res: LexedStr<'a>,
    offset: usize,
}

impl<'a> Converter<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            res: LexedStr { text, kind: Vec::new(), start: Vec::new(), error: Vec::new() },
            offset: 0,
        }
    }

    fn finalize_with_eof(mut self) -> LexedStr<'a> {
        self.res.push(EOF, self.offset);
        self.res
    }

    fn push(&mut self, kind: SyntaxKind, len: usize, err: Option<&str>) {
        self.res.push(kind, self.offset);
        self.offset += len;

        if let Some(err) = err {
            let token = self.res.len() as u32;
            let msg = err.to_owned();
            self.res.error.push(LexError { msg, token });
        }
    }

    fn extend_token(&mut self, kind: &lexer::TokenKind, token_text: &str) {
        // A note on an intended tradeoff:
        // We drop some useful information here (see patterns with double dots `..`)
        // Storing that info in `SyntaxKind` is not possible due to its layout requirements of
        // being `u16` that come from `rowan::SyntaxKind`.
        let mut err = "";

        let syntax_kind = {
            match kind {
                lexer::TokenKind::LineComment => COMMENT,
                lexer::TokenKind::BlockComment { terminated } => {
                    if !terminated {
                        err = "Missing trailing `*/` symbols to terminate the block comment";
                    }
                    COMMENT
                }
                lexer::TokenKind::Whitespace => WHITESPACE,
                lexer::TokenKind::Ident => {
                    SyntaxKind::from_strict_keyword(token_text).unwrap_or(IDENT)
                }
                lexer::TokenKind::Var => VAR,
                lexer::TokenKind::InvalidIdent => {
                    err = "Ident contains invalid characters";
                    IDENT
                }
                lexer::TokenKind::Directive => {
                    SyntaxKind::from_directive(token_text).unwrap_or(ERROR)
                }
                lexer::TokenKind::Literal { kind } => {
                    self.extend_literal(token_text.len(), kind);
                    return;
                }
                lexer::TokenKind::Semi => T![;],
                lexer::TokenKind::Comma => T![,],
                lexer::TokenKind::Dot => T![.],
                lexer::TokenKind::DotDot => T![..],
                lexer::TokenKind::OpenParen => T!['('],
                lexer::TokenKind::CloseParen => T![')'],
                lexer::TokenKind::OpenBrace => T!['{'],
                lexer::TokenKind::CloseBrace => T!['}'],
                lexer::TokenKind::OpenBracket => T!['['],
                lexer::TokenKind::CloseBracket => T![']'],
                lexer::TokenKind::At => T![@],
                lexer::TokenKind::Tilde => T![~],
                lexer::TokenKind::Colon => T![:],
                lexer::TokenKind::Dollar => T![$],
                lexer::TokenKind::Eq => T![=],
                lexer::TokenKind::Bang => T![!],
                lexer::TokenKind::Lt => T![<],
                lexer::TokenKind::Gt => T![>],
                lexer::TokenKind::Minus => T![-],
                lexer::TokenKind::Pipe => T![|],
                lexer::TokenKind::Plus => T![+],
                lexer::TokenKind::Star => T![*],
                lexer::TokenKind::Slash => T![/],
                lexer::TokenKind::Caret => T![^],
                lexer::TokenKind::Backslash => T!['\\'],
                lexer::TokenKind::Unknown => ERROR,
                lexer::TokenKind::Eof => EOF,
            }
        };

        let err = if err.is_empty() { None } else { Some(err) };
        self.push(syntax_kind, token_text.len(), err);
    }

    fn extend_literal(&mut self, len: usize, kind: &lexer::LiteralKind) {
        let mut err = "";
        let msg;

        let syntax_kind = match *kind {
            lexer::LiteralKind::Int { base: _, empty_int } => {
                if empty_int {
                    err = "Missing digits after the integer base prefix";
                }
                INT_NUMBER
            }
            lexer::LiteralKind::Float { base: _, empty_exponent } => {
                if empty_exponent {
                    err = "Missing digits after the exponent symbol";
                }
                FLOAT_NUMBER
            }
            lexer::LiteralKind::Str { open, terminated } => {
                if !terminated {
                    msg = format!(
                        "Missing trailing `{}` symbol to terminate the string literal",
                        closing_str_delimiter(&open)
                    );
                    err = &msg;
                }
                STRING
            }
        };

        let err = if err.is_empty() { None } else { Some(err) };
        self.push(syntax_kind, len, err);
    }
}
