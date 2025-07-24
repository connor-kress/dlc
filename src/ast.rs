use crate::lexer::{Binop, Loc, PrimitiveType, Uniop};

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
    pub body: Vec<StatementWithLoc>,
    pub loc: Loc,
}

// This is temporary, everything should be an expression
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Statement {
    Expr(ExprWithLoc),
    Block(Vec<Statement>),
    VarDecl {
        name: IdWithLoc,
        type_: Option<TypeWithLoc>,
        val: Option<Box<ExprWithLoc>>,
    },
    Loop {
        body: Box<StatementWithLoc>,
    },
    WhileLoop {
        pred: Box<ExprWithLoc>,
        body: Box<StatementWithLoc>,
    },
    ForLoop {
        start: Box<ExprWithLoc>,
        pred: Box<ExprWithLoc>,
        step: Box<ExprWithLoc>,
        body: Box<StatementWithLoc>,
    },
    Break,
    Continue,
    Return {
        val: Option<Box<ExprWithLoc>>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct StatementWithLoc {
    pub statement: Statement,
    pub loc: Loc,
}

impl StatementWithLoc {
    pub fn new(statement: Statement, loc: Loc) -> Self {
        Self { statement, loc }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    Id(String),
    IntLit(i32),
    FloatLit(f64),
    StrLit(String),
    Uniop {
        op: Uniop,
        arg: Box<ExprWithLoc>,
    },
    Binop {
        op: Binop,
        left: Box<ExprWithLoc>,
        right: Box<ExprWithLoc>,
    },
    FuncCall {
        name: Box<ExprWithLoc>,
        args: Vec<ExprWithLoc>,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ExprWithLoc {
    pub expr: Expr,
    pub loc: Loc,
}

impl ExprWithLoc {
    pub fn new(expr: Expr, loc: Loc) -> Self {
        Self { expr, loc }
    }
}
