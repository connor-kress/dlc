use std::fmt;

use crate::lexer::{Binop, Loc, PrimitiveType, Uniop};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct IdWithLoc {
    pub id: String,
    pub loc: Loc,
}

impl IdWithLoc {
    pub fn new(id: String, loc: Loc) -> Self {
        Self { id, loc }
    }
}

impl fmt::Display for IdWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Id(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "Primative({:?})", ty),
            Type::Id(id) => write!(f, "Id({})", id),
        }
    }
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

impl fmt::Display for TypeWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.type_)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    Id(IdWithLoc),
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Id(id) => write!(f, "{}", id),
            Expr::IntLit(n) => write!(f, "{}", n),
            Expr::FloatLit(n) => write!(f, "{}", n),
            Expr::StrLit(s) => write!(f, "\"{}\"", s),
            Expr::Uniop { op, arg } => write!(f, "{:?}({})", op, arg),
            Expr::Binop { op, left, right } => {
                write!(f, "{:?}({}, {})", op, left, right)
            }
            Expr::FuncCall { name, args } => {
                write!(f, "FuncCall({}", name)?;
                for arg in args {
                    write!(f, ", {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
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

impl fmt::Display for ExprWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ParamList {
    pub params: Vec<(IdWithLoc, TypeWithLoc)>,
}

impl fmt::Display for ParamList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, (id, type_)) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", id.id, type_)?;
        }
        write!(f, ")")
    }
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Expr(expr) => write!(f, "{}", expr),
            // TODO indentation for all loops/blocks
            Statement::Block(statements) => {
                write!(f, "Block {{\n")?;
                for statement in statements {
                    write!(f, "    {}", statement)?;
                }
                write!(f, "\n}}")
            }
            Statement::VarDecl { name, type_, val } => {
                let type_str = match type_ {
                    Some(type_) => format!(": {}", type_),
                    None => "".to_string(),
                };
                if let Some(val) = val {
                    write!(f, "VarDecl({}{}, {})", name, type_str, val)
                } else {
                    write!(f, "VarDecl({}{})", name, type_str)
                }
            }
            Statement::Loop { body } => write!(f, "Loop {{\n{}\n}}", body),
            Statement::WhileLoop { pred, body } => {
                write!(f, "While({}) {{\n{}\n}}", pred, body)
            }
            Statement::ForLoop {
                start,
                pred,
                step,
                body,
            } => {
                write!(f, "For({}, {}, {}) {{\n{}\n}}", start, pred, step, body)
            }
            Statement::Break => write!(f, "Break"),
            Statement::Continue => write!(f, "Continue"),
            Statement::Return { val } => {
                if let Some(val) = val {
                    write!(f, "Return({})", val)
                } else {
                    write!(f, "Return(Void)")
                }
            }
        }
    }
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

impl fmt::Display for StatementWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.statement)
    }
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({}, {})", self.name, self.param_list, self.ret_type)?;
        for statement in &self.body {
            write!(f, "\n    {}", statement)?;
        }
        Ok(())
    }
}
