use crate::lexer::{Loc, PrimitiveType};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct IdWithLoc {
    pub id: String,
    pub loc: Loc,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Id(String),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypeWithLoc {
    pub type_: Type,
    pub loc: Loc,
}

impl TypeWithLoc {
    pub fn new(type_: Type, loc: Loc) -> Self {
        Self { type_, loc }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ParamList {
    pub params: Vec<(IdWithLoc, TypeWithLoc)>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Function {
    pub name: IdWithLoc,
    pub param_list: ParamList,
    pub ret_type: TypeWithLoc,
    pub body: Box<Statement>,
}

// This is temporary, everything should be an expression
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Block(Vec<Statement>),
    VarDecl {
        name: String,
        type_: Option<Type>,
        val: Option<Box<Expr>>,
    },
    Loop {
        body: Box<Expr>,
    },
    WhileLoop {
        pred: Box<Expr>,
        body: Box<Expr>,
    },
    ForLoop {
        start: Box<Expr>,
        pred: Box<Expr>,
        step: Box<Expr>,
        body: Box<Expr>,
    },
    Break {
        val: Option<Box<Expr>>,
    },
    Continue,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    Id(String),
    Uniop {
        op: Uniop,
        arg: Box<Expr>,
    },
    Binop {
        op: Binop,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    FuncCall {
        name: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Uniop {
    Lnot,
    Neg,
    BitNeg,
    Ref,
    Deref,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Binop {
    Assign,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Lor,
    Land,
    BitOr,
    BitAnd,
    BitXor,
    In, // for iters
}
