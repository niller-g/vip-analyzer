use self::items::sections::{self, clauses};
use super::*;
use statements::{StmtMode, TermType, atom_stmt};

pub(crate) fn expr(p: &mut Parser<'_>) -> Option<(CompletedMarker, bool)> {
    expr_bp(p, MIN_BP, ExprMode::Expr)
}

pub(crate) fn expr_mode(p: &mut Parser<'_>, mode: ExprMode) -> Option<(CompletedMarker, bool)> {
    expr_bp(p, MIN_BP, mode)
}

fn expr_bp(p: &mut Parser<'_>, min_bp: ExprBP, mode: ExprMode) -> Option<(CompletedMarker, bool)> {
    let (mut lhs, mut atom) = lhs(p, mode)?;
    while !p.at(EOF) {
        let (op_bp, op, associativity) = bin_op(p);
        if op_bp < min_bp {
            break;
        }
        atom = false;
        let m = lhs.precede(p);
        p.bump(op);
        let op_bp = match associativity {
            Associativity::Left => op_bp + 1,
            Associativity::Right => op_bp,
        };
        expr_bp(p, op_bp, ExprMode::Expr);
        lhs = m.complete(p, BIN_EXPR);
    }

    Some((lhs, atom))
}

fn lhs(p: &mut Parser<'_>, mode: ExprMode) -> Option<(CompletedMarker, bool)> {
    match p.current() {
        T![+] | T![-] => {
            let m = p.start();
            p.bump_any();
            expr_bp(p, UNARY_BP, ExprMode::Expr);
            Some((m.complete(p, PREFIX_EXPR), false))
        }
        T![~] if p.at(T![~~]) => {
            let m = p.start();
            p.bump(T![~~]);
            expr_bp(p, UNARY_BP, ExprMode::Expr);
            Some((m.complete(p, PREFIX_EXPR), false))
        }
        _ => atom_expr(p, mode).ok().map(|m| (m, true)),
    }
}

enum Associativity {
    Left,
    Right,
}

#[macro_export]
macro_rules! incremental_bp {
    ( $first:ident $(, $rest:ident)* $(,)? ) => {
        const NONE_BP: u8 = 0;
        const $first: u8 = NONE_BP + 1;
        const MIN_BP: u8 = $first;
        incremental_bp!(@rest $first; $($rest),*);
    };
    (@rest $_prev:ident; ) => {};
    (@rest $prev:ident; $next:ident $(, $tail:ident)* ) => {
        const $next: u8 = $prev + 1;
        incremental_bp!(@rest $next; $($tail),*);
    };
}

type ExprBP = u8;
incremental_bp![OTHERWISE_BP, BITOR_BP, BITAND_BP, SHIFT_BP, ADD_BP, MUL_BP, UNARY_BP, POW_BP];

fn bin_op(p: &Parser<'_>) -> (ExprBP, SyntaxKind, Associativity) {
    use Associativity::*;
    const NOT_AN_OP: (ExprBP, SyntaxKind, Associativity) = (NONE_BP, T![@], Left);

    match p.current() {
        T![^] if p.at(T![^^]) => (BITOR_BP, T![^^], Left),
        T![^] => (POW_BP, T![^], Right),

        T![*] if p.at(T![**]) => (BITAND_BP, T![**], Left),

        T![*] => (MUL_BP, T![*], Left),
        T![/] => (MUL_BP, T![/], Left),
        T![div] => (MUL_BP, T![div], Left),
        T![mod] => (MUL_BP, T![mod], Left),
        T![quot] => (MUL_BP, T![quot], Left),
        T![rem] => (MUL_BP, T![rem], Left),

        T![+] if p.at(T![++]) => (BITOR_BP, T![++], Left),
        T![+] => (ADD_BP, T![+], Left),
        T![-] if p.at(T![--]) => (BITOR_BP, T![--], Left),
        T![-] => (ADD_BP, T![-], Left),

        T![>] if p.at(T![>>]) => (SHIFT_BP, T![>>], Left),
        T![<] if p.at(T![<<]) => (SHIFT_BP, T![<<], Left),

        T![otherwise] => (OTHERWISE_BP, T![otherwise], Right),

        _ => NOT_AN_OP,
    }
}

fn atom_expr<'a>(p: &mut Parser<'_>, mode: ExprMode) -> Result<CompletedMarker, &'a str> {
    let mut allowed_access = Access::ApplyAccess;
    let mut allow_apply = false;

    let m = match p.current() {
        T![..] if p.at(T![...]) => {
            allowed_access = Access::NoAccess;
            let m = p.start();
            p.bump(T![...]);
            m.complete(p, ELLIPSIS)
        }
        VAR => {
            allow_apply = true;
            allowed_access = Access::MemberAccess;
            let m = p.start();
            p.bump(VAR);
            m.complete(p, VAR_EXPR)
        }
        T![@] => {
            allowed_access = Access::NoAccess;
            scope_items::scope_type(p)?
        }
        T!['['] => list_expr(p),
        T!['{'] => {
            allow_apply = true;
            lambda_expr(p)
        }
        T![if] => {
            allow_apply = true;
            allowed_access = Access::MemberAccess;
            if_expr_else_stmt(p, mode == ExprMode::Expr)
        }
        T![implement] => {
            allowed_access = Access::MemberAccess;
            object_expr(p)
        }
        T!['('] => match mode {
            ExprMode::Stmt | ExprMode::ExprElseStmt => {
                let (paren_marker, term_type) = paren_expr_else_stmt(p);
                match term_type {
                    TermType::Ambiguous | TermType::Expr => {
                        allow_apply = true;
                        allowed_access = Access::MemberAccess;
                    }
                    TermType::Stmt => {
                        allowed_access = Access::NoAccess;
                    }
                };
                paren_marker
            }
            ExprMode::Expr => {
                allow_apply = true;
                allowed_access = Access::MemberAccess;
                paren_expr(p)
            }
        },
        T![$] => {
            let m = p.start();
            p.bump(T![$]);
            p.expect(T!['[']);
            exprs(p, T![']'].into());
            p.expect(T![']']);
            m.complete(p, BIN_LITERAL)
        }
        STRING => string_opt(p).expect("STRING was expected"),
        INT_NUMBER | FLOAT_NUMBER => {
            let m = p.start();
            p.bump_any();
            m.complete(p, NUMERIC_LITERAL)
        }
        T![#bininclude] => include_directive(p).complete(p, BIN_INCLUDE),
        T![#stringinclude] => include_directive(p).complete(p, STRING_INCLUDE),
        _ if at_ref_term(p) => {
            allow_apply = true;
            allowed_access = Access::MemberAccess;
            match ref_term(p, true) {
                Some((cm, _)) => cm,
                None => {
                    return Err("expected named term");
                }
            }
        }
        _ => {
            return Err("expected expression");
        }
    };

    Ok(postfix_term(p, m, allowed_access, allow_apply))
}

fn paren_expr_else_stmt(p: &mut Parser<'_>) -> (CompletedMarker, TermType) {
    let m = p.start();
    p.bump(T!['(']);
    let term_type = match statements::expr_else_stmt(p) {
        Some((_, TermType::Expr)) => TermType::Expr,
        Some((_, TermType::Ambiguous)) => TermType::Expr,
        Some((_, TermType::Stmt)) => TermType::Stmt,
        None => {
            p.error("expected statement or expression");
            TermType::Expr
        }
    };
    p.expect(T![')']);
    (m.complete(p, PAREN_TERM), term_type)
}

fn paren_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['(']);
    if expr(p).is_none() {
        p.error("expected expression");
    }
    p.expect(T![')']);
    m.complete(p, PAREN_TERM)
}

fn postfix_term(
    p: &mut Parser<'_>,
    mut lhs: CompletedMarker,
    mut allowed_access: Access,
    mut allow_apply: bool,
) -> CompletedMarker {
    while !p.at(EOF) {
        match p.current() {
            T![:] if allowed_access.may_access() && !p.at(T![:=]) && !p.at(T![:-]) => {
                debug_assert_ne!(allowed_access, Access::NoAccess);
                lhs = access_postfix(p, lhs);
                if let Access::ApplyAccess = allowed_access {
                    if p.at(T!['(']) {
                        lhs = apply_postfix(p, lhs);
                        // test value_like_apply
                        // implement a
                        //     clauses
                        //         b() :- []:c().
                        // end implement
                    } else {
                        // test_err value_like_no_apply
                        // implement a
                        //     clauses
                        //         b() :- []:c.
                        // end implement
                        p.error("cannot access value-like expression");
                        lhs = lhs.precede(p).complete(p, ERROR);
                    }
                }
                allowed_access = Access::MemberAccess;
                allow_apply = true;
            }
            T!['('] if allow_apply => {
                lhs = apply_postfix(p, lhs);
                allowed_access = Access::MemberAccess;
                allow_apply = true;
            }
            _ => {
                break;
            }
        }
    }

    lhs
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Access {
    /// E.g. `x:b`, `x:b()` or `x:c::d()`
    MemberAccess,
    /// E.g. `x:b()` or `x:c::d()`
    ApplyAccess,
    /// Means that the current expression cannot be accessed.
    NoAccess,
}
impl Access {
    #[inline]
    fn may_access(&self) -> bool {
        match self {
            Access::MemberAccess | Access::ApplyAccess => true,
            Access::NoAccess => false,
        }
    }
}

fn access_postfix(p: &mut Parser<'_>, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(T![:]);
    if terms::ref_term(p, false).is_none() {
        p.error("expected named term");
    }
    m.complete(p, ACCESS_TERM)
}

fn apply_postfix(p: &mut Parser<'_>, lhs: CompletedMarker) -> CompletedMarker {
    assert!(p.at(T!['(']));
    let m = lhs.precede(p);
    p.bump(T!['(']);
    functor_args(p);
    // test functor_original
    // implement a
    //     clauses
    //         b(D) :-
    //             c( | D),
    //             c(:Name = Name | :Other = D).
    // end implement
    if p.eat(T![|]) && !apply_arg(p) {
        p.error("expected expression after `|` in functor original");
    }
    p.expect(T![')']);
    m.complete(p, APPLY_TERM)
}

pub(crate) fn apply_arg(p: &mut Parser<'_>) -> bool {
    match p.current() {
        T![:] if p.nth_at(1, VAR) => {
            // test keyword_arg
            // implement a
            //     clauses
            //         b() :-
            //             c(:D = "").
            // end implement
            keyword_arg(p);
            true
        }
        _ => expr(p).is_some(),
    }
}

pub(crate) fn exprs(p: &mut Parser<'_>, after_exprs: TokenSet) -> Option<CompletedMarker> {
    let m = p.start();
    let any = exprs_raw(p, after_exprs);
    m.complete_if(p, EXPRS, any)
}

pub(crate) fn exprs_raw(p: &mut Parser<'_>, after_exprs: TokenSet) -> bool {
    let mut any = false;
    while !p.at(EOF) {
        any |= expr(p).is_some();
        if p.at(T![,]) {
            if after_exprs.contains(p.nth(1)) {
                p.err_and_bump("unexpected `,`");
                break;
            }
            p.bump(T![,]);
        } else {
            break;
        }
    }
    any
}

pub(crate) fn functor_args(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let m = p.start();
    let mut any = false;
    while !p.at(EOF) {
        any |= functor_arg(p);
        if p.at(T![,]) {
            if TokenSet::from([T![,], T![|], T![')']]).contains(p.nth(1)) {
                p.err_and_bump("unexpected `,`");
                break;
            }
            p.bump(T![,]);
        } else {
            break;
        }
    }
    m.complete_if(p, FUNCTOR_ARGS, any)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExprMode {
    Stmt,
    Expr,
    ExprElseStmt,
}

fn functor_arg(p: &mut Parser<'_>) -> bool {
    if p.at(T![:]) && p.nth_at(1, VAR) {
        keyword_arg(p);
        return true;
    }

    atom_stmt(p, StmtMode::ExprElseStmt).is_some()
}

fn keyword_arg(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![:]);
    p.bump(VAR);
    p.expect(T![=]);
    if expr(p).is_none() {
        p.error("expected expression");
    }
    m.complete(p, KEYWORD_ARG)
}

pub(crate) fn string_or_const(p: &mut Parser<'_>) {
    match p.current() {
        STRING => {
            string_opt(p);
        }
        _ if at_ref_term(p) => {
            ref_term(p, false);
        }
        _ => {
            p.error("expected string or constant");
        }
    }
}

pub(crate) fn string_opt(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if !p.at(STRING) {
        return None;
    }

    let m = p.start();
    while p.at(STRING) {
        let m = p.start();
        p.bump(STRING);
        m.complete(p, STRING_PART);
    }
    Some(m.complete(p, STRING_SEQ))
}

fn list_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['[']);
    let kind = list_content(p);
    p.expect(T![']']);
    m.complete(p, kind)
}

#[must_use]
fn include_directive(p: &mut Parser<'_>) -> Marker {
    assert!(matches!(p.current(), T![#bininclude] | T![#stringinclude]));
    let m = p.start();
    p.bump_any();
    p.expect(T!['(']);
    if expressions::string_opt(p).is_none() {
        p.error("expected a string");
    }
    p.expect(T![')']);
    m
}

#[must_use]
fn list_content(p: &mut Parser<'_>) -> SyntaxKind {
    fn comma_sep_exprs(p: &mut Parser<'_>) -> bool {
        let mut any = false;
        while !p.at(EOF) {
            if expr(p).is_some() {
                any = true;
            }
            if !p.eat(T![,]) {
                break;
            }
        }
        any
    }

    let args = p.start();
    match expr(p) {
        Some(_) if p.eat(T![||]) => {
            args.abandon(p);
            if statements::stmt(p).is_none() {
                p.error("expected statement after ||");
            }
            FIND_ALL_EXPR
        }
        Some(_) => {
            if p.eat(T![,]) {
                comma_sep_exprs(p);
            }
            args.complete(p, EXPRS);
            if p.eat(T![|]) {
                // test cons_expr
                // class a constants b = [1, 2 | A]. end class
                if expressions::expr(p).is_none() {
                    p.error("expected expression after |");
                    // test_err cons_missing_expr
                    // class a constants b = [1 | ]. end class
                }
                CONS_EXPR
            } else {
                // test list_expr
                // class a constants b = [[], [1]]. end class
                LIST_EXPR
            }
        }
        None => {
            args.abandon(p);
            if p.eat(T![||]) {
                // test_err findall_missing_expr_before
                // class a constants b = [ || _ ]. end class
                p.error("expected expression before ||");
                if statements::stmt(p).is_none() {
                    // test_err findall_missing_expr_after
                    // class a constants b = [ 1 ||  ]. end class
                    p.error("expected statement");
                }
                FIND_ALL_EXPR
            } else if p.eat(T![|]) {
                // test_err cons_missing_expr_before
                // class a constants b = [  | _ ]. end class
                p.error("expected list arguments before |");
                if expressions::expr(p).is_none() {
                    // test_err cons_missing_expr_after
                    // class a constants b = [  |  ]. end class
                    p.error("expected expression after |");
                }
                CONS_EXPR
            } else {
                // test list_expr_empty
                // class a constants b = []. end class
                LIST_EXPR
            }
        }
    }
}

fn lambda_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T!['{']);
    if p.eat(T!['(']) {
        exprs(p, T![')'].into());
        p.expect(T![')']);
    }
    if p.eat(T![=]) && expressions::expr(p).is_none() {
        p.error("expected expression");
    }
    if p.eat(T![:-]) && statements::stmt(p).is_none() {
        p.error("expected statements");
    }
    p.expect(T!['}']);
    m.complete(p, LAMBDA_EXPR)
}

fn take_remaining_if_branch(p: &mut Parser<'_>) {
    let next = |p: &Parser<'_>| {
        p.at_ts([T![elseif], T![else], T![.]]) || p.at(T![end]) && p.nth_at(1, T![if])
    };
    if !next(p) {
        let msg = format!("expected `elseif`, `else` or `end if`, found {:?}", p.current());
        p.error_until(msg, |p| next(p));
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BranchType {
    Expr,
    Stmt,
    Unknown,
}

fn if_branch_determine_type(p: &mut Parser<'_>) -> BranchType {
    let branch_type;
    if let Some((_body_marker, body_ty)) = statements::expr_else_stmt(p) {
        match body_ty {
            TermType::Ambiguous => branch_type = BranchType::Unknown,
            TermType::Expr => branch_type = BranchType::Expr,
            TermType::Stmt => branch_type = BranchType::Stmt,
        }
    } else {
        branch_type = BranchType::Stmt; // When the branch is empty, it must be a statement branch since it does not return a value.
    }
    branch_type
}

fn if_expr_else_stmt(p: &mut Parser<'_>, force_expr: bool) -> CompletedMarker {
    let m = p.start();
    p.bump(T![if]);
    if_condition(p);
    p.expect(T![then]);
    let mut branch_type = if force_expr {
        if expr(p).is_none() {
            p.error("missing expression");
        }
        BranchType::Expr
    } else {
        if_branch_determine_type(p)
    };
    take_remaining_if_branch(p);

    while p.at(T![elseif]) {
        let m = p.start();
        p.bump(T![elseif]);
        if_condition(p);
        p.expect(T![then]);
        match branch_type {
            BranchType::Expr => {
                if expr(p).is_none() {
                    p.error("missing expression");
                }
            }
            BranchType::Stmt => {
                statements::stmt(p);
            }
            BranchType::Unknown => branch_type = if_branch_determine_type(p),
        }
        take_remaining_if_branch(p);
        m.complete(p, ELSE_IF_TERM);
    }

    if p.at(T![else]) {
        let m = p.start();
        p.bump(T![else]);
        match branch_type {
            BranchType::Expr => {
                if expr(p).is_none() {
                    p.error("missing expression");
                }
            }
            BranchType::Stmt => {
                statements::stmt(p);
            }
            BranchType::Unknown => {
                if_branch_determine_type(p);
            }
        }
        take_remaining_if_branch(p);
        m.complete(p, ELSE_TERM);
    } else if !p.at(T![else]) && branch_type == BranchType::Expr {
        p.error("missing `else` branch");
    }

    end_if(p);

    m.complete(p, IF_TERM)
}

pub(crate) fn if_condition(p: &mut Parser<'_>) {
    let m = p.start();
    if statements::stmt(p).is_none() {
        m.abandon(p);
        p.error_until("expected condition statement", |p| {
            p.at_ts([T![then], T![elseif], T![else]]) || p.at(T![end]) && p.nth_at(1, T![if])
        });
    } else {
        m.complete(p, CONDITION);
    }
}

pub(crate) fn end_if(p: &mut Parser<'_>) {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.expect(T![if]);
        m.complete(p, END_IF);
    } else {
        p.error("expected `end if`");
    }
}

fn object_expr(p: &mut Parser<'_>) -> CompletedMarker {
    let m = p.start();
    p.bump(T![implement]);
    p.expect(T![:]);
    if scope_items::interface_ref(p).is_none() {
        p.error("expected an interface name");
    }
    if p.at_ts(scope_items::SCOPE_QUALIFICATION_START) {
        scope_items::qualifications(p);
    } else if p.at(IDENT) {
        clauses::clauses(p);
    }
    sections::implement_sections(p);
    end_implement(p);
    m.complete(p, OBJECT_EXPR)
}

fn end_implement(p: &mut Parser<'_>) {
    if p.at(T![end]) {
        let m = p.start();
        p.bump(T![end]);
        p.expect(T![implement]);
        m.complete(p, END_IMPLEMENT);
    } else {
        p.error("expected `end implement`");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_ok;
    use expect_test::expect;

    #[test]
    fn postfixes() {
        parse_ok!(
            expr,
            "a():b:c::d()",
            expect![[r#"
                SOURCE_FILE
                  APPLY_TERM
                    ACCESS_TERM
                      ACCESS_TERM
                        APPLY_TERM
                          REF_TERM
                            IDENT "a"
                          L_PAREN "("
                          R_PAREN ")"
                        COLON ":"
                        REF_TERM
                          IDENT "b"
                      COLON ":"
                      REF_TERM
                        SCOPE_REF
                          IDENT "c"
                        COLONCOLON "::"
                        IDENT "d"
                    L_PAREN "("
                    R_PAREN ")"
            "#]]
        );
    }

    #[test]
    fn add() {
        parse_ok!(
            expr,
            "1 + 2",
            expect![[r#"
                SOURCE_FILE
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
    fn mul() {
        parse_ok!(
            expr,
            "1 * 2 + 3",
            expect![[r#"
                SOURCE_FILE
                  BIN_EXPR
                    BIN_EXPR
                      NUMERIC_LITERAL
                        INT_NUMBER "1"
                      WHITESPACE " "
                      STAR "*"
                      WHITESPACE " "
                      NUMERIC_LITERAL
                        INT_NUMBER "2"
                    WHITESPACE " "
                    PLUS "+"
                    WHITESPACE " "
                    NUMERIC_LITERAL
                      INT_NUMBER "3"
            "#]]
        );
    }

    #[test]
    fn unary_minus() {
        parse_ok!(
            expr,
            "-2 * 2",
            expect![[r#"
                SOURCE_FILE
                  BIN_EXPR
                    PREFIX_EXPR
                      MINUS "-"
                      NUMERIC_LITERAL
                        INT_NUMBER "2"
                    WHITESPACE " "
                    STAR "*"
                    WHITESPACE " "
                    NUMERIC_LITERAL
                      INT_NUMBER "2"
            "#]]
        );
    }

    #[test]
    fn pow() {
        parse_ok!(
            expr,
            "-2 ^ 2",
            expect![[r#"
                SOURCE_FILE
                  PREFIX_EXPR
                    MINUS "-"
                    BIN_EXPR
                      NUMERIC_LITERAL
                        INT_NUMBER "2"
                      WHITESPACE " "
                      CARET "^"
                      WHITESPACE " "
                      NUMERIC_LITERAL
                        INT_NUMBER "2"
            "#]]
        );
        parse_ok!(
            expr,
            "1 ^ 2 ^ 3",
            expect![[r#"
                SOURCE_FILE
                  BIN_EXPR
                    NUMERIC_LITERAL
                      INT_NUMBER "1"
                    WHITESPACE " "
                    CARET "^"
                    WHITESPACE " "
                    BIN_EXPR
                      NUMERIC_LITERAL
                        INT_NUMBER "2"
                      WHITESPACE " "
                      CARET "^"
                      WHITESPACE " "
                      NUMERIC_LITERAL
                        INT_NUMBER "3"
            "#]]
        );
    }
}
