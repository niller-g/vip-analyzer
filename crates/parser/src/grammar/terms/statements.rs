use expressions::{ExprMode, apply_arg, expr, expr_mode, exprs};
use items::sections::clauses::clause_body;
use {super::*, crate::incremental_bp};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StmtMode {
    Stmt,
    ExprElseStmt,
}
impl From<StmtMode> for ExprMode {
    fn from(mode: StmtMode) -> Self {
        match mode {
            StmtMode::Stmt => ExprMode::Stmt,
            StmtMode::ExprElseStmt => ExprMode::ExprElseStmt,
        }
    }
}

pub(crate) fn stmt(p: &mut Parser<'_>) -> Option<(CompletedMarker, TermType)> {
    stmt_bp(p, MIN_BP, StmtMode::Stmt)
}

pub(crate) fn expr_else_stmt(p: &mut Parser<'_>) -> Option<(CompletedMarker, TermType)> {
    stmt_bp(p, MIN_BP, StmtMode::ExprElseStmt)
}

fn stmt_bp(
    p: &mut Parser<'_>,
    min_bp: StmtBP,
    mode: StmtMode,
) -> Option<(CompletedMarker, TermType)> {
    let (mut lhs, mut lhs_type) = atom_stmt(p, mode)?;
    while !p.at(EOF) {
        let (op_bp, op) = bin_op(p);
        if op_bp < min_bp {
            break;
        }
        let error_marker = p.start();
        let m = lhs.precede(p);
        p.bump(op);
        lhs_type = TermType::Stmt;
        lhs = match stmt_bp(p, op_bp, StmtMode::Stmt) {
            Some(_) => {
                error_marker.abandon(p);
                m.complete(p, BIN_STMT)
            }
            None => {
                m.abandon(p);
                p.error(format!("expected statement after {op:?}"));
                error_marker.complete(p, ERROR)
            }
        };
    }

    Some((lhs, lhs_type))
}

type StmtBP = u8;
incremental_bp![ORELSE_BP, OR_BP, AND_BP,];

/// Operator precedence table for statements. They are always right-associative.
fn bin_op(p: &Parser<'_>) -> (StmtBP, SyntaxKind) {
    const NOT_AN_OP: (StmtBP, SyntaxKind) = (NONE_BP, T![@]);
    match p.current() {
        T![,] => (AND_BP, T![,]),
        T![and] => (AND_BP, T![and]),

        T![;] => (OR_BP, T![;]),
        T![or] => (OR_BP, T![or]),

        T![orelse] => (ORELSE_BP, T![orelse]),

        _ => NOT_AN_OP,
    }
}

fn relation_op(p: &Parser<'_>) -> SyntaxKind {
    match p.current() {
        T![>] if p.at(T![>=]) => T![>=],
        T![>] if p.at(T![><]) => T![><],
        T![>] => T![>],
        T![<] if p.at(T![<>]) => T![<>],
        T![<] if p.at(T![<=]) => T![<=],
        T![<] => T![<],
        T![=] if p.at(T![==]) => T![==],
        T![=] => T![=],
        _ => unreachable!("{:?}", p.current()),
    }
}

const RELATION_START: TokenSet = TokenSet::new(&[T![>], T![<], T![=]]);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TermType {
    Expr,
    Stmt,
    Ambiguous,
}

pub(crate) fn atom_stmt(p: &mut Parser<'_>, mode: StmtMode) -> Option<(CompletedMarker, TermType)> {
    let atom_stmt = match p.current() {
        T![!] => {
            let m = p.start();
            p.bump(T![!]);
            m.complete(p, CUT_STMT)
        }
        T![foreach] => foreach_stmt(p),
        T![try] => try_stmt(p),
        T![match] => match_stmt(p),
        _ => {
            let (lhs, lhs_is_atom) = expr_mode(p, mode.into())?;
            match p.current() {
                T![:] if p.at(T![:=]) => {
                    if lhs_is_atom {
                        assign_stmt(p, lhs)
                    } else {
                        let lhs = lhs.precede(p).complete(p, ERROR);
                        p.error("cannot assign to this expression");
                        assign_stmt(p, lhs)
                    }
                }
                _ if p.at_ts(RELATION_START) => {
                    let m = lhs.precede(p);
                    let op = relation_op(p);
                    p.bump(op);
                    if expr(p).is_none() {
                        p.error(format!("expected expression after {op:?}"));
                    }
                    m.complete(p, RELATION_STMT)
                }
                IDENT if p.at_contextual_kw(T![in]) => {
                    let m = lhs.precede(p);
                    p.bump_remap(T![in]);
                    expr(p);
                    m.complete(p, IN_STMT)
                }
                _ => {
                    let lhs_kind = lhs.kind();
                    match lhs_kind {
                        _ if is_ambiguous(lhs_kind) => return Some((lhs, TermType::Ambiguous)),
                        ERROR => return Some((lhs, TermType::Expr)),
                        _ => {
                            return if mode == StmtMode::Stmt {
                                let m = lhs.precede(p);
                                p.error("expected statement, found expression");
                                Some((m.complete(p, ERROR), TermType::Expr))
                            } else {
                                Some((lhs, TermType::Expr))
                            };
                        }
                    }
                }
            }
        }
    };

    Some((atom_stmt, TermType::Stmt))
}

#[inline]
/// These nodes can be both statements and expressions.
pub(crate) fn is_ambiguous(kind: SyntaxKind) -> bool {
    matches!(kind, APPLY_TERM | ACCESS_TERM | REF_TERM | PAREN_TERM | IF_TERM)
}

fn assign_stmt(p: &mut Parser<'_>, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(T![:=]);
    if !p.eat(T![erroneous]) {
        expr(p);
    }
    m.complete(p, ASSIGN_STMT)
}

fn foreach_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![foreach]);

    let iter_m = p.start();
    if stmt(p).is_none() {
        iter_m.abandon(p);
        p.error("expected statement");
    } else {
        iter_m.complete(p, ITER_STMT);
    }

    p.expect(T![do]);
    stmt(p);
    end_foreach(p);
    m.complete(p, FOREACH_STMT)
}

fn end_foreach(p: &mut Parser<'_>) {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.expect(T![foreach]);
        m.complete(p, END_FOREACH);
    } else {
        p.error("expected `end foreach`");
    }
}

fn try_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    fn catch_handler(p: &mut Parser<'_>) -> CompletedMarker {
        let m = p.start();
        p.bump(T![catch]);
        expr(p);
        p.expect(T![do]);
        stmt(p);
        m.complete(p, CATCH_HANDLER)
    }
    fn finally_handler(p: &mut Parser<'_>) -> CompletedMarker {
        let m = p.start();
        p.bump(T![finally]);
        stmt(p);
        m.complete(p, FINALLY_HANDLER)
    }

    let m = p.start();
    p.bump(T![try]);
    stmt(p);

    let mut any_handler = false;
    while !p.at(EOF) {
        match p.current() {
            T![catch] => catch_handler(p),
            T![finally] => finally_handler(p),
            _ => break,
        };
        any_handler = true;
    }
    if !any_handler {
        p.error("expected `catch` or `finally`");
    }
    end_try(p);
    m.complete(p, TRY_STMT)
}

fn end_try(p: &mut Parser<'_>) {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.expect(T![try]);
        m.complete(p, END_TRY);
    } else {
        p.error("expected `end try`");
    }
}

fn match_stmt(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![match]);
    if p.eat(T!['(']) {
        if apply_args(p).is_none() {
            p.error("match must have arguments");
        }
        p.expect(T![')']);
    } else {
        p.error_until("expected `(` after `match`", |p| {
            p.at(T![')'])
                || p.at(T![case])
                || p.at(T![:-])
                || (p.at(T![end]) && p.nth_at(1, T![match]))
        });
        p.eat(T![')']);
    }

    if !case_arms(p) {
        p.error("expected cases");
    }
    if end_match(p).is_none() {
        p.error_until("expected `end match`", |p| {
            p.at(T![.]) || p.at(T![:-]) || (p.at(T![end]) && p.nth_at(1, T![match]))
        });
    }

    m.complete(p, MATCH_STMT)
}

fn apply_args(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();
    let mut any = false;
    while !p.at(EOF) {
        if apply_arg(p) {
            any = true;
        }
        if p.at(T![,]) {
            if TokenSet::from([T![,], T![')']]).contains(p.nth(1)) {
                p.err_and_bump("unexpected `,`");
                break;
            }
            p.bump(T![,]);
        } else {
            break;
        }
    }

    m.complete_if(p, APPLY_ARGS, any)
}

fn case_arms(p: &mut Parser<'_>) -> bool {
    let mut any = false;
    while p.at(T![case]) {
        case_arm(p);
        any = true;
    }
    any
}

fn case_arm(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![case]);
    if p.eat(T!['(']) {
        if exprs(p, T![')'].into()).is_none() {
            p.error("expected case arguments");
        }
        p.expect(T![')']);
    } else {
        p.error_until("expected `(` after `case`", |p| {
            p.at(T![')'])
                || p.at(T![case])
                || p.at(T![:-])
                || (p.at(T![end]) && p.nth_at(1, T![match]))
        });
        p.eat(T![')']);
    }
    if clause_body(p).is_none() && !(p.at(T![case]) || p.at(T![end]) && p.nth_at(1, T![match])) {
        p.error_until("expected `:-` before case body", |p| {
            p.at(T![.]) || p.at(T![case]) || (p.at(T![end]) && p.nth_at(1, T![match]))
        });
    }
    m.complete(p, CASE_ARM)
}

fn end_match(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.expect(T![match]);
        Some(m.complete(p, END_MATCH))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_err, parse_ok};
    use expect_test::expect;

    #[test]
    fn assignment() {
        parse_err!(
            stmt,
            "a+ 2 := b",
            expect![[r#"
                SOURCE_FILE
                  ASSIGN_STMT
                    ERROR
                      BIN_EXPR
                        REF_TERM
                          IDENT "a"
                        PLUS "+"
                        WHITESPACE " "
                        NUMERIC_LITERAL
                          INT_NUMBER "2"
                    WHITESPACE " "
                    COLONEQ ":="
                    WHITESPACE " "
                    REF_TERM
                      IDENT "b"
                error 4: cannot assign to this expression
            "#]]
        );
    }

    #[test]
    fn relation_stmt() {
        parse_ok!(
            stmt,
            "a <= b, _ = 1 + 2",
            expect![[r#"
                SOURCE_FILE
                  BIN_STMT
                    RELATION_STMT
                      REF_TERM
                        IDENT "a"
                      WHITESPACE " "
                      LTEQ "<="
                      WHITESPACE " "
                      REF_TERM
                        IDENT "b"
                    COMMA ","
                    WHITESPACE " "
                    RELATION_STMT
                      VAR_EXPR
                        VAR "_"
                      WHITESPACE " "
                      EQ "="
                      WHITESPACE " "
                      BIN_EXPR
                        NUMERIC_LITERAL
                          INT_NUMBER "1"
                        WHITESPACE " "
                        PLUS "+"
                        WHITESPACE " "
                        NUMERIC_LITERAL
                          INT_NUMBER "2"
            "#]]
        );
    }

    #[test]
    fn and_stmt() {
        parse_ok!(
            stmt,
            "a, b",
            expect![[r#"
                SOURCE_FILE
                  BIN_STMT
                    REF_TERM
                      IDENT "a"
                    COMMA ","
                    WHITESPACE " "
                    REF_TERM
                      IDENT "b"
            "#]]
        );
        parse_err!(
            stmt,
            "1 and 2 ; 3, 4",
            expect![[r#"
                SOURCE_FILE
                  BIN_STMT
                    BIN_STMT
                      ERROR
                        NUMERIC_LITERAL
                          INT_NUMBER "1"
                      WHITESPACE " "
                      AND_KW "and"
                      WHITESPACE " "
                      ERROR
                        NUMERIC_LITERAL
                          INT_NUMBER "2"
                    WHITESPACE " "
                    SEMICOLON ";"
                    WHITESPACE " "
                    BIN_STMT
                      ERROR
                        NUMERIC_LITERAL
                          INT_NUMBER "3"
                      COMMA ","
                      WHITESPACE " "
                      ERROR
                        NUMERIC_LITERAL
                          INT_NUMBER "4"
                error 1: expected statement, found expression
                error 7: expected statement, found expression
                error 11: expected statement, found expression
                error 14: expected statement, found expression
            "#]]
        );
    }

    #[test]
    fn stmt_as_arg() {
        parse_ok!(
            stmt,
            "not(StreamType = out(_))",
            expect![[r#"
                SOURCE_FILE
                  APPLY_TERM
                    REF_TERM
                      IDENT "not"
                    L_PAREN "("
                    FUNCTOR_ARGS
                      RELATION_STMT
                        VAR_EXPR
                          VAR "StreamType"
                        WHITESPACE " "
                        EQ "="
                        WHITESPACE " "
                        APPLY_TERM
                          REF_TERM
                            IDENT "out"
                          L_PAREN "("
                          FUNCTOR_ARGS
                            VAR_EXPR
                              VAR "_"
                          R_PAREN ")"
                    R_PAREN ")"
            "#]]
        );
    }

    #[test]
    fn stmt_as_arg2() {
        parse_ok!(
            stmt,
            r#"not(try
                _ = ""
            catch _ do
                fail()
            end try)"#,
            expect![[r#"
                SOURCE_FILE
                  APPLY_TERM
                    REF_TERM
                      IDENT "not"
                    L_PAREN "("
                    FUNCTOR_ARGS
                      TRY_STMT
                        TRY_KW "try"
                        WHITESPACE "\n                "
                        RELATION_STMT
                          VAR_EXPR
                            VAR "_"
                          WHITESPACE " "
                          EQ "="
                          WHITESPACE " "
                          STRING_SEQ
                            STRING_PART
                              STRING "\"\""
                        WHITESPACE "\n            "
                        CATCH_HANDLER
                          CATCH_KW "catch"
                          WHITESPACE " "
                          VAR_EXPR
                            VAR "_"
                          WHITESPACE " "
                          DO_KW "do"
                          WHITESPACE "\n                "
                          APPLY_TERM
                            REF_TERM
                              IDENT "fail"
                            L_PAREN "("
                            R_PAREN ")"
                        WHITESPACE "\n            "
                        END_TRY
                          END_KW "end"
                          WHITESPACE " "
                          TRY_KW "try"
                    R_PAREN ")"
            "#]]
        );
    }

    #[test]
    fn stmt_as_arg3() {
        parse_ok!(
            stmt,
            "_ = conv(u16, math::round((S - S2) * h * t))",
            expect![[r#"
                SOURCE_FILE
                  RELATION_STMT
                    VAR_EXPR
                      VAR "_"
                    WHITESPACE " "
                    EQ "="
                    WHITESPACE " "
                    APPLY_TERM
                      REF_TERM
                        IDENT "conv"
                      L_PAREN "("
                      FUNCTOR_ARGS
                        REF_TERM
                          IDENT "u16"
                        COMMA ","
                        WHITESPACE " "
                        APPLY_TERM
                          REF_TERM
                            SCOPE_REF
                              IDENT "math"
                            COLONCOLON "::"
                            IDENT "round"
                          L_PAREN "("
                          FUNCTOR_ARGS
                            BIN_EXPR
                              BIN_EXPR
                                PAREN_TERM
                                  L_PAREN "("
                                  BIN_EXPR
                                    VAR_EXPR
                                      VAR "S"
                                    WHITESPACE " "
                                    MINUS "-"
                                    WHITESPACE " "
                                    VAR_EXPR
                                      VAR "S2"
                                  R_PAREN ")"
                                WHITESPACE " "
                                STAR "*"
                                WHITESPACE " "
                                REF_TERM
                                  IDENT "h"
                              WHITESPACE " "
                              STAR "*"
                              WHITESPACE " "
                              REF_TERM
                                IDENT "t"
                          R_PAREN ")"
                      R_PAREN ")"
            "#]]
        );
    }

    #[test]
    fn stmt_as_arg4() {
        parse_ok!(
            stmt,
            "not(1 = 2)",
            expect![[r#"
                SOURCE_FILE
                  APPLY_TERM
                    REF_TERM
                      IDENT "not"
                    L_PAREN "("
                    FUNCTOR_ARGS
                      RELATION_STMT
                        NUMERIC_LITERAL
                          INT_NUMBER "1"
                        WHITESPACE " "
                        EQ "="
                        WHITESPACE " "
                        NUMERIC_LITERAL
                          INT_NUMBER "2"
                    R_PAREN ")"
            "#]]
        );
    }

    #[test]
    fn stmt_as_arg5() {
        parse_ok!(
            stmt,
            "not((0 = DX and 0 = DY))",
            expect![[r#"
                SOURCE_FILE
                  APPLY_TERM
                    REF_TERM
                      IDENT "not"
                    L_PAREN "("
                    FUNCTOR_ARGS
                      PAREN_TERM
                        L_PAREN "("
                        BIN_STMT
                          RELATION_STMT
                            NUMERIC_LITERAL
                              INT_NUMBER "0"
                            WHITESPACE " "
                            EQ "="
                            WHITESPACE " "
                            VAR_EXPR
                              VAR "DX"
                          WHITESPACE " "
                          AND_KW "and"
                          WHITESPACE " "
                          RELATION_STMT
                            NUMERIC_LITERAL
                              INT_NUMBER "0"
                            WHITESPACE " "
                            EQ "="
                            WHITESPACE " "
                            VAR_EXPR
                              VAR "DY"
                        R_PAREN ")"
                    R_PAREN ")"
            "#]]
        );
    }
}
