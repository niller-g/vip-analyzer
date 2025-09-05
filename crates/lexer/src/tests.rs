use super::*;
use expect_test::{Expect, expect};

fn check_lexing(src: &str, expect: Expect) {
    let actual = tokenize(src).fold("".to_owned(), |acc, token| acc + &format!("{:?}\n", token));
    expect.assert_eq(&actual)
}

#[test]
fn empty() {
    check_lexing("", expect![[r#""#]]);
}

#[test]
fn smoke_test() {
    check_lexing(
        "/* my source file */ fn main() { println!(\"zebra\"); }\n",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 20 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 4 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: OpenBrace, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 7 }
            Token { kind: Bang, len: 1 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: Literal { kind: Str { open: '"', terminated: true } }, len: 7 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Semi, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: CloseBrace, len: 1 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn comment_flavors() {
    check_lexing(
        r"
% line
%% line as well
% line % as % well %
/* block */
/**/
/*** also block */
",
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 6 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 15 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 20 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 11 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: BlockComment { terminated: true }, len: 18 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn nested_block_comments() {
    check_lexing(
        "/* /* */ */'a'",
        expect![[r#"
            Token { kind: BlockComment { terminated: true }, len: 11 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 3 }
        "#]],
    )
}

#[test]
fn characters() {
    check_lexing(
        "'a' ' ' '\\n' '\\t' '\\\\' \\",
        expect![[r#"
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Backslash, len: 1 }
        "#]],
    );
}

#[test]
fn unterminated() {
    check_lexing(
        "'''",
        expect![[r#"
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 2 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: false } }, len: 1 }
        "#]],
    );
    check_lexing(
        "'abc ",
        expect![[r#"
            Token { kind: Literal { kind: Str { open: '\'', terminated: false } }, len: 5 }
        "#]],
    );
    check_lexing(
        "\"abc ",
        expect![[r#"
            Token { kind: Literal { kind: Str { open: '"', terminated: false } }, len: 5 }
        "#]],
    );
    check_lexing(
        "@[",
        expect![[r#"
            Token { kind: Literal { kind: Str { open: '[', terminated: false } }, len: 2 }
        "#]],
    );
    check_lexing(
        "/* abc ",
        expect![[r#"
            Token { kind: BlockComment { terminated: false }, len: 7 }
        "#]],
    );
}

#[test]
fn literals() {
    check_lexing(
        r#"
'a'
b'a'
"a"
b"a"
1234
0xABC
1.0
1.0e10
2us
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '"', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 1 }
            Token { kind: Literal { kind: Str { open: '"', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Hexadecimal, empty_int: false } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 6 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    )
}

#[test]
fn integer_literals() {
    check_lexing(
        r#"
0
1_2_3
1_2_3_
0o123
0xFF0000
0x_99_33_33
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 6 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Octal, empty_int: false } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Hexadecimal, empty_int: false } }, len: 8 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Hexadecimal, empty_int: false } }, len: 11 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn real_literals() {
    check_lexing(
        r#"
.0
2.0
1_2.3_4
1_2_3.4_5_6e7_8_9
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 17 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn real_exponents() {
    check_lexing(
        r#"
0e1
.0E1
0.0e+1
0E+1
.0e-1
0.0E-1
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 6 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 6 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn invalid_real_literals() {
    check_lexing(
        r#"
1_.0
2.0_
3e
1._
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: false } }, len: 4 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Float { base: Decimal, empty_exponent: true } }, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Int { base: Decimal, empty_int: false } }, len: 1 }
            Token { kind: Dot, len: 1 }
            Token { kind: Var, len: 1 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn string_literals() {
    check_lexing(
        r#"
""
''
@[]
@() 
@())) 
@(a))b)
@)( 
@)((( 
@)a((b(
@[] 
@[]]] 
@[a]]b]
@][ 
@][[[ 
@]a[[b[
@{} 
@{}}} 
@{a}}b}
@}{ 
@}{{{ 
@}a{{b{
@<> 
@<>>> 
@<a>>b>
@>< 
@><<< 
@>a<<b<
@|| 
@|||| 
@|a||b|
@|"| 
@|'| 
@|\n|
@|\"|
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '"', terminated: true } }, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '\'', terminated: true } }, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '[', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '(', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '(', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '(', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: ')', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: ')', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: ')', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '[', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '[', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '[', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: ']', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: ']', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: ']', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '{', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '{', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '{', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '}', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '}', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '}', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '<', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '<', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '<', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '>', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '>', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '>', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 4 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 4 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '|', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn string_at_underscore() {
    check_lexing(
        r#"
@_bar_
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: At, len: 1 }
            Token { kind: Var, len: 5 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn string_at_literal() {
    check_lexing(
        r#"
@££
@①①
@¾¾
@  
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '£', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '①', terminated: true } }, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: '¾', terminated: true } }, len: 5 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { open: ' ', terminated: true } }, len: 3 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn dots() {
    check_lexing(
        r#"
.
..
...
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: DotDot, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: DotDot, len: 2 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn exotic() {
    check_lexing(
        r#"
с
привет
привет_мир
δ
Δ
䕸useræøåÆØÅ
"#,
        expect![[r#"
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 12 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 19 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Var, len: 2 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Unknown, len: 3 }
            Token { kind: Ident, len: 16 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}

#[test]
fn implement() {
    check_lexing(
        r#"% Copyright

implement main

clauses
    run() :-
        TaskWindow = taskWindow::new(), %
        TaskWindow:show().

end implement main

goal
    mainExe::run(main::run).
"#,
        expect![[r#"
            Token { kind: LineComment, len: 11 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Ident, len: 9 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 4 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Ident, len: 7 }
            Token { kind: Whitespace, len: 5 }
            Token { kind: Ident, len: 3 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Colon, len: 1 }
            Token { kind: Minus, len: 1 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Var, len: 10 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Eq, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 10 }
            Token { kind: Colon, len: 1 }
            Token { kind: Colon, len: 1 }
            Token { kind: Ident, len: 3 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Comma, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: LineComment, len: 1 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Var, len: 10 }
            Token { kind: Colon, len: 1 }
            Token { kind: Ident, len: 4 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Ident, len: 3 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 9 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 4 }
            Token { kind: Whitespace, len: 2 }
            Token { kind: Ident, len: 4 }
            Token { kind: Whitespace, len: 5 }
            Token { kind: Ident, len: 7 }
            Token { kind: Colon, len: 1 }
            Token { kind: Colon, len: 1 }
            Token { kind: Ident, len: 3 }
            Token { kind: OpenParen, len: 1 }
            Token { kind: Ident, len: 4 }
            Token { kind: Colon, len: 1 }
            Token { kind: Colon, len: 1 }
            Token { kind: Ident, len: 3 }
            Token { kind: CloseParen, len: 1 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 1 }
        "#]],
    );
}
