//! Low-level Visual Prolog lexer.
//!
//! The purpose of this crate is to convert raw sources into a labeled sequence
//! of well-known token types, so building an actual Visual Prolog token stream will
//! be easier.
//!
//! The main entity of this crate is the [`TokenKind`] enum which represents common
//! lexeme types.
//!
//! Tokens produced by this lexer are not yet ready for parsing the Visual Prolog syntax.
//! E.g `++` and other multi-symbol punctuation have to be glued together.
//! Also contextual keywords are not distinguished in this phase.

mod cursor;

#[cfg(test)]
mod tests;

pub use crate::cursor::Cursor;

use self::LiteralKind::*;
use self::TokenKind::*;
use unicode_properties::UnicodeEmoji;

/// Parsed token.
/// It doesn't contain information about data that has been parsed,
/// only the type of the token and its size.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    fn new(kind: TokenKind, len: u32) -> Token {
        Token { kind, len }
    }
}

/// Enum representing common lexeme types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Multi-char tokens:
    /// "% comment"
    LineComment,

    /// `/* block comment */`
    ///
    /// Block comments can be recursive, so a sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment {
        terminated: bool,
    },

    /// Any whitespace character sequence.
    Whitespace,

    /// "ident" or "continue"
    ///
    /// At this step, keywords are also considered identifiers.
    Ident,
    Var,

    /// Like the above, but containing invalid unicode codepoints.
    InvalidIdent,

    Directive,

    /// See [LiteralKind] for more details.
    Literal {
        kind: LiteralKind,
    },

    // One-char tokens:
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// ".."
    /// This token is the only composite token in the lexer. It is necessary distinguish ranges from number literals.
    DotDot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "@"
    At,
    /// "~"
    Tilde,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Eq,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "|"
    Pipe,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "^"
    Caret,
    /// "\\"
    Backslash,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,

    /// End of input.
    Eof,
}

/// Enum representing the literal types supported by the lexer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// "12_u8", "0o100", "0b120i99", "1f32".
    Int { base: Base, empty_int: bool },
    /// "12.34f32", "1e3", but not "1f32".
    Float { base: Base, empty_exponent: bool },
    /// ""abc"", ""abc", "'a'", "'\\'", "'''", "';"
    Str { open: char, terminated: bool },
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Literal starts with "0o".
    Octal = 8,
    /// Literal doesn't contain a prefix.
    Decimal = 10,
    /// Literal starts with "0x".
    Hexadecimal = 16,
}

/// Returns the closing delimiter for a given opening delimiter.
pub fn closing_str_delimiter(open: &char) -> &char {
    match open {
        '\'' => &'\'',
        '"' => &'"',
        '(' => &')',
        ')' => &'(',
        '[' => &']',
        ']' => &'[',
        '{' => &'}',
        '}' => &'{',
        '<' => &'>',
        '>' => &'<',
        _ => open,
    }
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof { Some(token) } else { None }
    })
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    pub fn advance_token(&mut self) -> Token {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return Token::new(TokenKind::Eof, 0),
        };
        let token_kind = match first_char {
            // Comments
            '%' => {
                self.eat_while(|c| c != '\n');
                LineComment
            }
            '/' => match self.first() {
                '*' => self.block_comment(),
                _ => Slash,
            },

            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Identifiers and keywords.
            c if c.is_lowercase() => self.ident_continue(Ident),
            c if c.is_uppercase() || c == '_' => self.ident_continue(Var),

            // Numeric literal.
            c @ '0'..='9' => {
                let literal_kind = self.number(c);

                TokenKind::Literal { kind: literal_kind }
            }

            '.' => match self.first() {
                '0'..='9' => TokenKind::Literal { kind: self.number('.') },
                '.' => {
                    self.bump();
                    DotDot
                }
                _ => Dot,
            },

            // One-symbol tokens.
            ';' => Semi,
            ',' => Comma,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '~' => Tilde,
            ':' => Colon,
            '$' => Dollar,
            '=' => Eq,
            '!' => Bang,
            '<' => Lt,
            '>' => Gt,
            '-' => Minus,
            '|' => Pipe,
            '+' => Plus,
            '*' => Star,
            '^' => Caret,
            '\\' => Backslash,

            // Single quoted strings literal.
            '\'' => Literal { kind: Str { open: '\'', terminated: self.quoted_str('\'') } },
            // Double quoted strings literal.
            '"' => Literal { kind: Str { open: '\"', terminated: self.quoted_str('"') } },
            '@' => {
                let open = self.first();
                if !open.is_ascii_alphabetic() && open != '_' && !open.is_ascii_control() {
                    self.bump();

                    Literal {
                        kind: Str {
                            open,
                            terminated: self.delimited_str(closing_str_delimiter(&open)),
                        },
                    }
                } else {
                    At
                }
            }
            '#' => {
                if self.first().is_ascii_alphabetic() {
                    self.eat_while(|c| c.is_ascii_alphanumeric());
                    Directive
                } else {
                    Unknown
                }
            }

            // Identifier starting with an emoji. Only lexed for graceful error recovery.
            c if !c.is_ascii() && c.is_emoji_char() => self.fake_ident(),
            _ => Unknown,
        };
        let res = Token::new(token_kind, self.pos_within_token());
        self.reset_pos_within_token();
        res
    }

    fn block_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '/' && self.first() == '*');
        self.bump();

        let mut depth = 1usize;
        while let Some(c) = self.bump() {
            match c {
                '/' if self.first() == '*' => {
                    self.bump();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.bump();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                _ => (),
            }
        }

        BlockComment { terminated: depth == 0 }
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        Whitespace
    }

    fn ident_continue(&mut self, kind: TokenKind) -> TokenKind {
        debug_assert!(
            self.prev().is_lowercase() || self.prev().is_uppercase() || self.prev() == '_'
        );

        // Start is already eaten, eat the rest of identifier.
        self.eat_while(is_id_continue);

        match self.first() {
            c if !c.is_ascii() && c.is_emoji_char() => self.fake_ident(),
            _ => kind,
        }
    }

    fn fake_ident(&mut self) -> TokenKind {
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(|c| {
            unicode_xid::UnicodeXID::is_xid_continue(c)
                || (!c.is_ascii() && c.is_emoji_char())
                || c == '\u{200d}'
        });

        InvalidIdent
    }

    fn number(&mut self, first_char: char) -> LiteralKind {
        debug_assert!(
            self.prev().is_ascii_digit() || (self.prev() == '.' && self.first().is_ascii_digit())
        );

        let mut base = Base::Decimal;
        match first_char {
            '0' => {
                match self.first() {
                    'o' => {
                        base = Base::Octal;
                        self.bump();
                        if !self.eat_decimal_digits() {
                            return Int { base, empty_int: true };
                        }
                    }
                    'x' => {
                        base = Base::Hexadecimal;
                        self.bump();
                        if !self.eat_hexadecimal_digits() {
                            return Int { base, empty_int: true };
                        }
                    }
                    // Not a base prefix; consume additional digits.
                    '0'..='9' | '_' => {
                        self.eat_decimal_digits();
                    }

                    // Also not a base prefix; nothing more to do here.
                    '.' | 'e' | 'E' => {}

                    // Just a 0.
                    _ => return Int { base, empty_int: false },
                }
            }
            '1'..='9' => {
                // No base prefix, parse number in the usual way.
                self.eat_decimal_digits();
            }
            '.' if self.first().is_ascii_digit() => {
                // Only the fractional part is present. Eg. `.123`
                self.eat_decimal_digits();
                match self.first() {
                    'e' | 'E' => {
                        self.bump();
                        let empty_exponent = !self.eat_float_exponent();
                        return Float { base, empty_exponent };
                    }
                    _ => return Float { base, empty_exponent: false },
                }
            }
            _ => unreachable!(),
        };

        match self.first() {
            '.' if self.second().is_ascii_digit() => {
                self.bump();
                self.eat_decimal_digits();
                match self.first() {
                    'e' | 'E' => {
                        self.bump();
                        let empty_exponent = !self.eat_float_exponent();
                        Float { base, empty_exponent }
                    }
                    _ => Float { base, empty_exponent: false },
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();
                Float { base, empty_exponent }
            }
            _ => Int { base, empty_int: false },
        }
    }

    fn quoted_str(&mut self, quote: char) -> bool {
        debug_assert!(self.prev() == quote);

        while let Some(c) = self.bump() {
            match c {
                c if c == quote => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == quote => {
                    // Bump again to skip escaped character.
                    self.bump();
                }
                _ => (),
            }
        }
        // End of file reached.
        false
    }

    /// https://wiki.visual-prolog.com/index.php?title=Language_Reference/Lexical_Elements#String_Literals
    fn delimited_str(&mut self, close_delimiter: &char) -> bool {
        while let Some(c) = self.bump() {
            if &c == close_delimiter {
                if &self.first() == close_delimiter {
                    // Double closing quote is an escaped sequence.
                    self.bump();
                } else {
                    return true;
                }
            }
        }
        // End of file reached.
        false
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');

        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }
}
