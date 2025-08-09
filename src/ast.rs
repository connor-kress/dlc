use std::fmt;

use crate::lexer::{Binop, Loc, PrimitiveType, Uniop};

static INDENT_WIDTH: usize = 4;

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
    Ptr(Box<TypeWithLoc>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "{:?}", ty),
            Type::Id(id) => write!(f, "Id({})", id),
            Type::Ptr(ty) => write!(f, "Ptr({})", ty),
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

impl Expr {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        _indent: usize,
    ) -> fmt::Result {
        match self {
            Expr::Id(id) => write!(f, "{}", id),
            Expr::IntLit(n) => write!(f, "{}", n),
            Expr::FloatLit(n) => write!(f, "{}", n),
            Expr::StrLit(s) => write!(f, "\"{}\"", s),
            Expr::Uniop { op, arg } => {
                write!(f, "{:?}({})", op, arg)
            }
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
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

impl ExprWithLoc {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        self.expr.fmt_with_indent(f, indent)
    }
}

impl fmt::Display for ExprWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
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
    If {
        cond: Box<ExprWithLoc>,
        if_block: Vec<StatementWithLoc>,
        else_block: Option<Vec<StatementWithLoc>>,
    },
    Loop {
        body: Vec<StatementWithLoc>,
    },
    WhileLoop {
        pred: Box<ExprWithLoc>,
        body: Vec<StatementWithLoc>,
    },
    ForLoop {
        start: Box<ExprWithLoc>,
        pred: Box<ExprWithLoc>,
        step: Box<ExprWithLoc>,
        body: Vec<StatementWithLoc>,
    },
    Break,
    Continue,
    Return {
        val: Option<Box<ExprWithLoc>>,
    },
}

impl Statement {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        let indent_str = " ".repeat(indent * INDENT_WIDTH);
        match self {
            Statement::Expr(expr) => {
                write!(f, "{}", indent_str)?;
                expr.fmt_with_indent(f, indent)?;
            }
            Statement::Block(statements) => {
                write!(f, "{}Block:", indent_str)?;
                for statement in statements {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            Statement::VarDecl { name, type_, val } => {
                write!(f, "{}VarDecl({}", indent_str, name)?;
                if let Some(type_) = type_ {
                    write!(f, ": {}", type_)?;
                }
                if let Some(val) = val {
                    write!(f, ", {}", val)?;
                }
                write!(f, ")")?;
            }
            Statement::If {
                cond,
                if_block,
                else_block,
            } => {
                write!(f, "{}If({}):", indent_str, cond)?;
                for statement in if_block {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
                if let Some(else_block) = else_block {
                    write!(f, "\n{}Else:", indent_str)?;
                    for statement in else_block {
                        write!(f, "\n")?;
                        statement.fmt_with_indent(f, indent + 1)?;
                    }
                }
            }
            Statement::Loop { body } => {
                write!(f, "{}Loop:", indent_str)?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            Statement::WhileLoop { pred, body } => {
                write!(f, "{}While({}):", indent_str, pred)?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            Statement::ForLoop {
                start,
                pred,
                step,
                body,
            } => {
                write!(f, "{}For({}, {}, {}):", indent_str, start, pred, step)?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            Statement::Break => write!(f, "{}Break", indent_str)?,
            Statement::Continue => write!(f, "{}Continue", indent_str)?,
            Statement::Return { val } => {
                write!(f, "{}Return(", indent_str)?;
                if let Some(val) = val {
                    write!(f, "{})", val)?;
                } else {
                    write!(f, "Void)")?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
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

impl StatementWithLoc {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        self.statement.fmt_with_indent(f, indent)
    }
}

impl fmt::Display for StatementWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
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

impl Function {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        let indent_str = " ".repeat(indent * INDENT_WIDTH);
        write!(f, "{}", indent_str)?;
        write!(
            f,
            "{}({}, {}):\n",
            self.name, self.param_list, self.ret_type
        )?;
        for statement in &self.body {
            statement.fmt_with_indent(f, indent + 1)?;
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}
