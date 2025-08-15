use crate::{
    ast::{
        Expr, ExprWithLoc, Function, Program, Statement, StatementWithLoc,
        Type, TypeWithLoc,
    },
    lexer::PrimitiveType,
    typed_ast::{
        TypedExpr, TypedExprKind, TypedFunction, TypedProgram, TypedStatement,
        TypedStatementKind,
    },
};

fn check_expr(expr: &ExprWithLoc) -> Result<TypedExpr, String> {
    Ok(match &expr.expr {
        Expr::Id(id) => TypedExpr {
            expr: TypedExprKind::Id(id.id.clone()),
            ty: Type::Primitive(PrimitiveType::Void), // TODO
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
                arg: Box::new(check_expr(&arg)?),
            },
            ty: Type::Primitive(PrimitiveType::Void), // TODO
            loc: expr.loc.clone(),
        },
        Expr::Binop { op, left, right } => TypedExpr {
            expr: TypedExprKind::Binop {
                op: op.clone(),
                left: Box::new(check_expr(&left)?),
                right: Box::new(check_expr(&right)?),
            },
            ty: Type::Primitive(PrimitiveType::Void), // TODO
            loc: expr.loc.clone(),
        },
        Expr::FuncCall { name, args } => {
            let mut typed_args = Vec::new();
            for arg in args {
                let typed_arg = check_expr(&arg)?;
                typed_args.push(typed_arg);
            }
            TypedExpr {
                expr: TypedExprKind::FuncCall {
                    name: Box::new(check_expr(&name)?),
                    args: typed_args,
                },
                ty: Type::Primitive(PrimitiveType::Void), // TODO
                loc: expr.loc.clone(),
            }
        }
        Expr::Index { array, index } => TypedExpr {
            expr: TypedExprKind::Index {
                array: Box::new(check_expr(&array)?),
                index: Box::new(check_expr(&index)?),
            },
            ty: Type::Primitive(PrimitiveType::Void), // TODO
            loc: expr.loc.clone(),
        },
    })
}

fn check_statement(
    statement: &StatementWithLoc,
) -> Result<TypedStatement, String> {
    let typed_statement = match &statement.statement {
        Statement::Expr(expr) => TypedStatementKind::Expr(check_expr(&expr)?),
        Statement::Block(statements) => {
            let mut typed_statements = Vec::new();
            for statement in statements {
                let typed_statement = check_statement(&statement)?;
                typed_statements.push(typed_statement);
            }
            TypedStatementKind::Block(typed_statements)
        }
        Statement::VarDecl { name, type_, val } => {
            let typed_val = match val {
                Some(val) => Some(Box::new(check_expr(&val)?)),
                None => None,
            };
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
                let typed_statement = check_statement(&statement)?;
                typed_if_block.push(typed_statement);
            }
            let typed_else_block = match else_block {
                Some(else_block) => {
                    let mut typed_else_block = Vec::new();
                    for statement in else_block {
                        let typed_statement = check_statement(&statement)?;
                        typed_else_block.push(typed_statement);
                    }
                    Some(typed_else_block)
                }
                None => None,
            };
            TypedStatementKind::If {
                cond: Box::new(check_expr(&cond)?),
                if_block: typed_if_block,
                else_block: typed_else_block,
            }
        }
        Statement::Loop { body } => {
            let mut typed_body = Vec::new();
            for statement in body {
                let typed_statement = check_statement(&statement)?;
                typed_body.push(typed_statement);
            }
            TypedStatementKind::Loop { body: typed_body }
        }
        Statement::WhileLoop { pred, body } => {
            let mut typed_body = Vec::new();
            for statement in body {
                let typed_statement = check_statement(&statement)?;
                typed_body.push(typed_statement);
            }
            TypedStatementKind::WhileLoop {
                pred: Box::new(check_expr(&pred)?),
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
                Some(val) => Some(Box::new(check_expr(&val)?)),
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

fn check_function(function: &Function) -> Result<TypedFunction, String> {
    let mut typed_statements = Vec::new();
    for statement in &function.body {
        let typed_statement = check_statement(&statement)?;
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
    let mut typed_functions = Vec::new();
    for function in &program.functions {
        let typed_function = check_function(&function)?;
        typed_functions.push(typed_function);
    }
    Ok(TypedProgram {
        functions: typed_functions,
    })
}
