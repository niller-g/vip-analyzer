#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StmtOp {
    /// `,`
    Comma,
    /// `and`
    And,
    /// `;`
    Semicolon,
    /// `or`
    Or,
    /// `orelse`
    OrElse,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PrefixOp {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `~~`
    BitNot,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExprOp {
    /// `^^`
    BitXor,
    /// `**`
    BitAnd,
    /// `++`
    BitOr,
    /// `--`
    BitClear,
    /// `^`
    Pow,
    /// `*`
    Mul,
    /// `/`
    DivReal,
    /// `div`
    DivInt,
    /// `mod`
    Mod,
    /// `rem`
    Rem,
    /// `quot`
    Quot,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `otherwise`
    Otherwise,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RelationOp {
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `<>`
    Ne,
    /// `><`
    NeAlt,
    /// `==`
    MustUnify,
    /// `=`
    Eq,
}
