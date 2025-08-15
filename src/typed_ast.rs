use std::fmt::{self, Write};

use crate::{
    ast::{IdWithLoc, Type, TypeWithLoc, INDENT_WIDTH},
    lexer::{Binop, Loc, Uniop},
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedId {
    pub id: String,
    pub ty: Type,
    pub loc: Loc,
}

impl TypedId {
    pub fn new(id: String, ty: Type, loc: Loc) -> Self {
        Self { id, ty, loc }
    }
}

impl fmt::Display for TypedId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Id(TypedId),
    IntLit(i32),
    FloatLit(f64),
    StrLit(String),
    BoolLit(bool),
    Uniop {
        op: Uniop,
        arg: Box<TypedExpr>,
    },
    Binop {
        op: Binop,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    FuncCall {
        name: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },
    Index {
        array: Box<TypedExpr>,
        index: Box<TypedExpr>,
    },
}

impl TypedExprKind {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        _indent: usize,
    ) -> fmt::Result {
        match self {
            TypedExprKind::Id(id) => write!(f, "{id}"),
            TypedExprKind::IntLit(n) => write!(f, "{n}"),
            TypedExprKind::FloatLit(n) => write!(f, "{n}"),
            TypedExprKind::StrLit(s) => write!(f, "{s:?}"),
            TypedExprKind::BoolLit(b) => write!(f, "{b}"),
            TypedExprKind::Uniop { op, arg } => {
                write!(f, "{op:?}({arg})")
            }
            TypedExprKind::Binop { op, left, right } => {
                write!(f, "{op:?}({left}, {right})")
            }
            TypedExprKind::FuncCall { name, args } => {
                let mut args_str = String::new();
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        args_str.push_str(", ");
                    }
                    write!(&mut args_str, "{arg}")?;
                }
                write!(f, "FuncCall({name}, [{args_str}])")
            }
            TypedExprKind::Index { array, index } => {
                write!(f, "Index({array}, {index})")
            }
        }
    }
}

impl fmt::Display for TypedExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub expr: TypedExprKind,
    pub ty: Type,
    pub loc: Loc,
}

impl TypedExpr {
    pub fn new(expr: TypedExprKind, ty: Type, loc: Loc) -> Self {
        Self { expr, ty, loc }
    }
}

impl TypedExpr {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        self.expr.fmt_with_indent(f, indent)
    }
}

impl fmt::Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ParamList {
    pub params: Vec<TypedId>,
}

impl fmt::Display for ParamList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, id) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", id.id, id.ty)?;
        }
        write!(f, ")")
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TypedStatement {
    Expr(TypedExpr),
    Block(Vec<TypedStatement>),
    VarDecl {
        name: IdWithLoc,
        type_: Option<TypeWithLoc>,
        val: Option<Box<TypedExpr>>,
    },
    If {
        cond: Box<TypedExpr>,
        if_block: Vec<TypedStatementWithLoc>,
        else_block: Option<Vec<TypedStatementWithLoc>>,
    },
    Loop {
        body: Vec<TypedStatementWithLoc>,
    },
    WhileLoop {
        pred: Box<TypedExpr>,
        body: Vec<TypedStatementWithLoc>,
    },
    ForLoop {
        start: Box<TypedExpr>,
        pred: Box<TypedExpr>,
        step: Box<TypedExpr>,
        body: Vec<TypedStatementWithLoc>,
    },
    Break,
    Continue,
    Return {
        val: Option<Box<TypedExpr>>,
    },
}

impl TypedStatement {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        let i_str = " ".repeat(indent * INDENT_WIDTH);
        match self {
            TypedStatement::Expr(expr) => {
                write!(f, "{i_str}")?;
                expr.fmt_with_indent(f, indent)?;
            }
            TypedStatement::Block(statements) => {
                write!(f, "{i_str}Block:")?;
                for statement in statements {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatement::VarDecl { name, type_, val } => {
                write!(f, "{i_str}VarDecl({name}")?;
                if let Some(type_) = type_ {
                    write!(f, ": {type_}")?;
                }
                if let Some(val) = val {
                    write!(f, ", {val}")?;
                }
                write!(f, ")")?;
            }
            TypedStatement::If {
                cond,
                if_block,
                else_block,
            } => {
                write!(f, "{i_str}If({cond}):")?;
                for statement in if_block {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
                if let Some(else_block) = else_block {
                    write!(f, "\n{i_str}Else:")?;
                    for statement in else_block {
                        write!(f, "\n")?;
                        statement.fmt_with_indent(f, indent + 1)?;
                    }
                }
            }
            TypedStatement::Loop { body } => {
                write!(f, "{i_str}Loop:")?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatement::WhileLoop { pred, body } => {
                write!(f, "{i_str}While({pred}):")?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatement::ForLoop {
                start,
                pred,
                step,
                body,
            } => {
                write!(f, "{i_str}For({start}, {pred}, {step}):")?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatement::Break => write!(f, "{i_str}Break")?,
            TypedStatement::Continue => write!(f, "{i_str}Continue")?,
            TypedStatement::Return { val } => {
                write!(f, "{i_str}Return(")?;
                if let Some(val) = val {
                    write!(f, "{val})")?;
                } else {
                    write!(f, "Void)")?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for TypedStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedStatementWithLoc {
    pub statement: TypedStatement,
    pub loc: Loc,
}

impl TypedStatementWithLoc {
    pub fn new(statement: TypedStatement, loc: Loc) -> Self {
        Self { statement, loc }
    }
}

impl TypedStatementWithLoc {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        self.statement.fmt_with_indent(f, indent)
    }
}

impl fmt::Display for TypedStatementWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: IdWithLoc,
    pub param_list: ParamList,
    pub ret_type: TypeWithLoc,
    pub body: Vec<TypedStatementWithLoc>,
    pub loc: Loc,
}

impl TypedFunction {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        let i_str = " ".repeat(indent * INDENT_WIDTH);
        write!(
            f,
            "{i_str}{}({}, {}):\n",
            self.name, self.param_list, self.ret_type
        )?;
        for statement in &self.body {
            statement.fmt_with_indent(f, indent + 1)?;
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl fmt::Display for TypedFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

pub struct TypedProgram {
    pub functions: Vec<TypedFunction>,
}

impl fmt::Display for TypedProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for function in &self.functions {
            write!(f, "{function}\n\n")?;
        }
        Ok(())
    }
}
