#[allow(dead_code)]
pub struct Point {
    line: usize,
    col: usize,
}

pub enum Loc {
    Point(Point),
    Span { start: Point, end: Point },
}

pub enum Type {} // TODO

#[allow(dead_code)]
pub struct Block {
    body: Vec<Expr>,
}

#[allow(dead_code)]
pub struct ParamList {
    params: Vec<(Symbol, Type)>,
}

#[allow(dead_code)]
pub struct Function {
    param_list: ParamList,
    ret_type: Type,
    body: Box<Expr>,
}

#[allow(dead_code)]
pub struct Symbol {
    val: String,
}

pub enum Expr {
    Symbol(Symbol),
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
    VarDecl {
        name: Symbol,
        type_: Option<Type>,
        val: Option<Box<Expr>>,
    },
    FuncCall {
        name: Box<Expr>,
        args: Vec<Expr>,
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

pub enum Uniop {
    Lnot,
    Neg,
    BitNeg,
    Ref,
    Deref,
}

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
