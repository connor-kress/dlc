#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimativeType {
    Int,
    Float,
    Bool,
    Char,
    Void,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Lparen,
    Rparen,
    Lbrack,
    Rbrack,
    Lcurly,
    Rcurly,
    Comma,
    Semi,

    Type(PrimativeType),
    Op(String),
    Symbol(String),

    // Keywords
    Fn,
    For,
    If,
    Else,
    While,
    Return,

    // Literals
    StrLit(String),
    NumLit(String),
}
