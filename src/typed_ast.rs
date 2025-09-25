use std::fmt::{self, Write};

use crate::{
    ast::{IdWithLoc, ParamList, TypeWithLoc, INDENT_WIDTH},
    lexer::{Binop, Loc, Uniop},
    types::Type,
};

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Id(String),
    IntLit(i64),
    FloatLit(f64),
    StrLit(String),
    CharLit(char),
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
    Cast {
        expr: Box<TypedExpr>,
        type_: Type,
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
            TypedExprKind::CharLit(c) => write!(f, "{c:?}"),
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
            TypedExprKind::Cast { expr, type_ } => {
                write!(f, "Cast({expr}, {type_})")
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
pub enum TypedStatementKind {
    Expr(TypedExpr),
    Block(Vec<TypedStatement>),
    VarDecl {
        name: IdWithLoc,
        type_: Option<TypeWithLoc>,
        val: Option<Box<TypedExpr>>,
    },
    If {
        cond: Box<TypedExpr>,
        if_block: Vec<TypedStatement>,
        else_block: Option<Vec<TypedStatement>>,
    },
    Loop {
        body: Vec<TypedStatement>,
    },
    WhileLoop {
        pred: Box<TypedExpr>,
        body: Vec<TypedStatement>,
    },
    ForLoop {
        start: Box<TypedExpr>,
        pred: Box<TypedExpr>,
        step: Box<TypedExpr>,
        body: Vec<TypedStatement>,
    },
    Break,
    Continue,
    Return {
        val: Option<Box<TypedExpr>>,
    },
}

impl TypedStatementKind {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        let i_str = " ".repeat(indent * INDENT_WIDTH);
        match self {
            TypedStatementKind::Expr(expr) => {
                write!(f, "{i_str}")?;
                expr.fmt_with_indent(f, indent)?;
            }
            TypedStatementKind::Block(statements) => {
                write!(f, "{i_str}Block:")?;
                for statement in statements {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatementKind::VarDecl { name, type_, val } => {
                write!(f, "{i_str}VarDecl({name}")?;
                if let Some(type_) = type_ {
                    write!(f, ": {type_}")?;
                }
                if let Some(val) = val {
                    write!(f, ", {val}")?;
                }
                write!(f, ")")?;
            }
            TypedStatementKind::If {
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
            TypedStatementKind::Loop { body } => {
                write!(f, "{i_str}Loop:")?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatementKind::WhileLoop { pred, body } => {
                write!(f, "{i_str}While({pred}):")?;
                for statement in body {
                    write!(f, "\n")?;
                    statement.fmt_with_indent(f, indent + 1)?;
                }
            }
            TypedStatementKind::ForLoop {
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
            TypedStatementKind::Break => write!(f, "{i_str}Break")?,
            TypedStatementKind::Continue => write!(f, "{i_str}Continue")?,
            TypedStatementKind::Return { val } => {
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

impl fmt::Display for TypedStatementKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedStatement {
    pub statement: TypedStatementKind,
    pub loc: Loc,
}

impl TypedStatement {
    pub fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter,
        indent: usize,
    ) -> fmt::Result {
        self.statement.fmt_with_indent(f, indent)
    }
}

impl fmt::Display for TypedStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: IdWithLoc,
    pub param_list: ParamList,
    pub ret_type_expr: TypeWithLoc,
    pub ret_type: Type,
    pub body: Vec<TypedStatement>,
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

#[derive(Debug, Clone)]
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
