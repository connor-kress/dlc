use std::collections::HashMap;

use crate::{
    ast::{
        Expr, ExprWithLoc, Function, Program, Statement, StatementWithLoc,
        Type, TypeWithLoc,
    },
    lexer::{Binop, PrimitiveType},
    typed_ast::{
        TypedExpr, TypedExprKind, TypedFunction, TypedProgram, TypedStatement,
        TypedStatementKind,
    },
};

#[derive(Clone, Debug)]
struct TypeCheckerContext<'a> {
    program: &'a Program,
    func: TypeCheckerFunctionContext,
}

impl<'a> TypeCheckerContext<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            func: TypeCheckerFunctionContext::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct TypeCheckerFunctionContext {
    locals: HashMap<String, Type>,
}

impl<'a> TypeCheckerFunctionContext {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }

    fn get_local(&self, name: &str) -> Option<Type> {
        self.locals.get(name).cloned()
    }

    fn add_local(&mut self, name: String, ty: Type) {
        self.locals.insert(name, ty);
    }

    fn clear(&mut self) {
        self.locals.clear();
    }
}

fn is_valid_arithmetic_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Primitive(
            PrimitiveType::Int8
                | PrimitiveType::Int16
                | PrimitiveType::Int32
                | PrimitiveType::Int64
                | PrimitiveType::Float32
                | PrimitiveType::Float64
        )
    )
}

fn get_binop_type(
    op: &Binop,
    left: &Type,
    right: &Type,
) -> Result<Type, String> {
    if left != right {
        return Err(format!(
            "Invalid types for operator `{op:?}`: `{left}` and `{right}`"
        ));
    }
    Ok(match op {
        Binop::Add | Binop::Sub | Binop::Mul | Binop::Div => {
            if !is_valid_arithmetic_type(left) {
                return Err(format!(
                    "Invalid types for operation `{op:?}`: `{left}` and `{right}`"
                ));
            }
            left.clone()
        }
        Binop::Eq => Type::Primitive(PrimitiveType::Bool),
        Binop::Neq => Type::Primitive(PrimitiveType::Bool),
        Binop::Lt => Type::Primitive(PrimitiveType::Bool),
        Binop::Le => Type::Primitive(PrimitiveType::Bool),
        Binop::Gt => Type::Primitive(PrimitiveType::Bool),
        Binop::Ge => Type::Primitive(PrimitiveType::Bool),
        Binop::Land => Type::Primitive(PrimitiveType::Bool),
        Binop::Lor => Type::Primitive(PrimitiveType::Bool),
        _ => return Err(format!("Invalid binary operation `{op:?}`")),
    })
}

fn check_expr(
    expr: &ExprWithLoc,
    ctx: &mut TypeCheckerContext,
) -> Result<TypedExpr, String> {
    Ok(match &expr.expr {
        Expr::Id(id) => TypedExpr {
            expr: TypedExprKind::Id(id.id.clone()),
            ty: ctx
                .func
                .get_local(&id.id)
                .ok_or_else(|| format!("Undefined local variable `{id}`"))?,
            loc: expr.loc.clone(),
        },
        Expr::IntLit(n) => TypedExpr {
            expr: TypedExprKind::IntLit(*n),
            ty: Type::Primitive(PrimitiveType::Int64), // TODO
            loc: expr.loc.clone(),
        },
        Expr::FloatLit(n) => TypedExpr {
            expr: TypedExprKind::FloatLit(*n),
            ty: Type::Primitive(PrimitiveType::Float64), // TODO
            loc: expr.loc.clone(),
        },
        Expr::StrLit(s) => TypedExpr {
            expr: TypedExprKind::StrLit(s.clone()),
            ty: Type::Ptr(Box::new(TypeWithLoc::new(
                Type::Primitive(PrimitiveType::Int8),
                expr.loc.clone(), // TODO: this shouldn't have a loc?
            ))),
            loc: expr.loc.clone(),
        },
        Expr::BoolLit(b) => TypedExpr {
            expr: TypedExprKind::BoolLit(*b),
            ty: Type::Primitive(PrimitiveType::Bool),
            loc: expr.loc.clone(),
        },
        Expr::Uniop { op, arg } => TypedExpr {
            expr: TypedExprKind::Uniop {
                op: op.clone(),
                arg: Box::new(check_expr(&arg, ctx)?),
            },
            ty: Type::Primitive(PrimitiveType::Void), // TODO
            loc: expr.loc.clone(),
        },
        Expr::Binop { op, left, right } => {
            let left_typed = check_expr(&left, ctx)?;
            let right_typed = check_expr(&right, ctx)?;
            let left_ty = left_typed.ty.clone();
            let right_ty = right_typed.ty.clone();
            TypedExpr {
                expr: TypedExprKind::Binop {
                    op: op.clone(),
                    left: Box::new(left_typed),
                    right: Box::new(right_typed),
                },
                ty: get_binop_type(&op, &left_ty, &right_ty)?,
                loc: expr.loc.clone(),
            }
        }
        Expr::FuncCall { name, args } => {
            let mut typed_args = Vec::new();
            for arg in args {
                let typed_arg = check_expr(&arg, ctx)?;
                typed_args.push(typed_arg);
            }
            TypedExpr {
                expr: TypedExprKind::FuncCall {
                    name: Box::new(check_expr(&name, ctx)?),
                    args: typed_args,
                },
                ty: Type::Primitive(PrimitiveType::Void), // TODO
                loc: expr.loc.clone(),
            }
        }
        Expr::Index { array, index } => TypedExpr {
            expr: TypedExprKind::Index {
                array: Box::new(check_expr(&array, ctx)?),
                index: Box::new(check_expr(&index, ctx)?),
            },
            ty: Type::Primitive(PrimitiveType::Void), // TODO
            loc: expr.loc.clone(),
        },
    })
}

fn check_statement(
    statement: &StatementWithLoc,
    ctx: &mut TypeCheckerContext,
) -> Result<TypedStatement, String> {
    let typed_statement = match &statement.statement {
        Statement::Expr(expr) => {
            TypedStatementKind::Expr(check_expr(&expr, ctx)?)
        }
        Statement::Block(statements) => {
            let mut typed_statements = Vec::new();
            for statement in statements {
                let typed_statement = check_statement(&statement, ctx)?;
                typed_statements.push(typed_statement);
            }
            TypedStatementKind::Block(typed_statements)
        }
        Statement::VarDecl { name, type_, val } => {
            let typed_val = match val {
                Some(val) => Some(Box::new(check_expr(&val, ctx)?)),
                None => None,
            };
            if let Some(ref val) = typed_val {
                let ty = val.ty.clone();
                ctx.func.add_local(name.id.clone(), ty);
            } else if let Some(type_) = type_ {
                ctx.func.add_local(name.id.clone(), type_.type_.clone());
            } else {
                todo!("Multi-statement type inference");
            }
            TypedStatementKind::VarDecl {
                name: name.clone(),
                type_: type_.clone(),
                val: typed_val,
            }
        }
        Statement::If {
            cond,
            if_block,
            else_block,
        } => {
            let mut typed_if_block = Vec::new();
            for statement in if_block {
                let typed_statement = check_statement(&statement, ctx)?;
                typed_if_block.push(typed_statement);
            }
            let typed_else_block = match else_block {
                Some(else_block) => {
                    let mut typed_else_block = Vec::new();
                    for statement in else_block {
                        let typed_statement = check_statement(&statement, ctx)?;
                        typed_else_block.push(typed_statement);
                    }
                    Some(typed_else_block)
                }
                None => None,
            };
            TypedStatementKind::If {
                cond: Box::new(check_expr(&cond, ctx)?),
                if_block: typed_if_block,
                else_block: typed_else_block,
            }
        }
        Statement::Loop { body } => {
            let mut typed_body = Vec::new();
            for statement in body {
                let typed_statement = check_statement(&statement, ctx)?;
                typed_body.push(typed_statement);
            }
            TypedStatementKind::Loop { body: typed_body }
        }
        Statement::WhileLoop { pred, body } => {
            let mut typed_body = Vec::new();
            for statement in body {
                let typed_statement = check_statement(&statement, ctx)?;
                typed_body.push(typed_statement);
            }
            TypedStatementKind::WhileLoop {
                pred: Box::new(check_expr(&pred, ctx)?),
                body: typed_body,
            }
        }
        Statement::ForLoop { .. } => {
            todo!("For loops are not yet implemented")
        }
        Statement::Break => todo!("Break statements are not yet implemented"),
        Statement::Continue => {
            todo!("Continue statements are not yet implemented")
        }
        Statement::Return { val } => {
            let typed_val = match val {
                Some(val) => Some(Box::new(check_expr(&val, ctx)?)),
                None => None,
            };
            TypedStatementKind::Return { val: typed_val }
        }
    };
    Ok(TypedStatement {
        statement: typed_statement,
        loc: statement.loc.clone(),
    })
}

fn check_function(
    function: &Function,
    ctx: &mut TypeCheckerContext,
) -> Result<TypedFunction, String> {
    for (param, type_) in &function.param_list.params {
        ctx.func.add_local(param.id.clone(), type_.type_.clone());
    }
    let mut typed_statements = Vec::new();
    for statement in &function.body {
        let typed_statement = check_statement(&statement, ctx)?;
        typed_statements.push(typed_statement);
    }
    Ok(TypedFunction {
        name: function.name.clone(),
        param_list: function.param_list.clone(),
        ret_type: function.ret_type.clone(),
        body: typed_statements,
        loc: function.loc.clone(),
    })
}

pub fn check_program(program: &Program) -> Result<TypedProgram, String> {
    let mut ctx = TypeCheckerContext::new(&program);
    let mut typed_functions = Vec::new();
    for function in &program.functions {
        ctx.func.clear();
        let typed_function = check_function(&function, &mut ctx)?;
        typed_functions.push(typed_function);
        println!("{:#?}", ctx.func);
    }
    Ok(TypedProgram {
        functions: typed_functions,
    })
}
