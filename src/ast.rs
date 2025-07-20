#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub struct Point {
    line: usize,
    col: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Loc {
    // file_name: String,
    start: Point,
    end: Point,
}

#[derive(Debug, Clone)]
pub enum Type {} // TODO

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Block {
    body: Vec<Expr>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ParamList {
    params: Vec<(String, Type)>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Function {
    param_list: ParamList,
    ret_type: Type,
    body: Box<Expr>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
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
    Block(Block),
    Function(Function),
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
