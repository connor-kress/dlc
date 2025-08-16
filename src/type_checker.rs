use std::collections::HashMap;

use crate::{
    ast::{
        Expr, ExprWithLoc, Function, IdWithLoc, Program, Statement,
        StatementWithLoc, Type as AstType, TypeWithLoc,
    },
    lexer::{Binop, PrimitiveType},
    typed_ast::{
        TypedExpr, TypedExprKind, TypedFunction, TypedProgram, TypedStatement,
        TypedStatementKind,
    },
    types::{FuncType, Type},
};

#[derive(Clone, Debug)]
struct TypeCheckerContext {
    functions: HashMap<String, FuncType>,
    func: TypeCheckerFunctionContext,
}

impl TypeCheckerContext {
    fn new(program: &Program) -> Result<Self, String> {
        // Structs will be handled before here so they will be
        // available for function headers
        let mut ctx = Self {
            functions: HashMap::new(),
            func: TypeCheckerFunctionContext::new(),
        };
        let mut functions = HashMap::new();
        for function in &program.functions {
            println!("Parsing function: {}", function.name.id);
            let mut params = Vec::new();
            for (id, type_) in &function.param_list.params {
                params
                    .push((id.id.clone(), convert_ast_type(&type_, &mut ctx)?));
            }
            let func_type = FuncType {
                params,
                ret_type: Box::new(convert_ast_type(
                    &function.ret_type,
                    &mut ctx,
                )?),
            };
            functions.insert(function.name.id.clone(), func_type);
        }
        ctx.functions = functions;
        Ok(ctx)
    }

    fn get_func_type(&self, name: &str) -> Option<FuncType> {
        self.functions.get(name).cloned()
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

fn convert_ast_type(
    ty: &TypeWithLoc,
    ctx: &mut TypeCheckerContext,
) -> Result<Type, String> {
    Ok(match &ty.type_ {
        AstType::Primitive(ty) => Type::Primitive(*ty),
        AstType::Id(id) => todo!("Custom types are not yet implemented: {id}"),
        AstType::Ptr(ty) => Type::Ptr(Box::new(convert_ast_type(ty, ctx)?)),
    })
}

fn find_type_of_id(
    id: &IdWithLoc,
    ctx: &mut TypeCheckerContext,
) -> Result<Type, String> {
    if let Some(ty) = ctx.func.get_local(&id.id) {
        Ok(ty.clone())
    } else if let Some(func_type) = ctx.get_func_type(&id.id) {
        Ok(Type::Func(func_type))
    } else {
        println!("{ctx:#?}");
        Err(format!("Undefined local variable `{id}`"))
    }
}

fn are_arithmetic_compatible_types(lhs: &Type, _rhs: &Type) -> bool {
    matches!(
        lhs,
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
    let get_error_msg = || {
        format!("Invalid types for operator `{op:?}`: `{left}` and `{right}`")
    };
    if left != right {
        return Err(get_error_msg());
    }
    use Binop as B;
    Ok(match op {
        B::Add | B::Sub | B::Mul | B::Div => {
            if !are_arithmetic_compatible_types(left, right) {
                return Err(format!(
                    "Invalid types for operation `{op:?}`: `{left}` and `{right}`"
                ));
            }
            left.clone()
        }
        B::Eq | B::Neq => {
            // TODO: Check if types are equatable
            Type::Primitive(PrimitiveType::Bool)
        }
        B::Lt | B::Le | B::Gt | B::Ge => {
            // TODO: Check if types are ordinal
            Type::Primitive(PrimitiveType::Bool)
        }
        B::Land | B::Lor => {
            if !left.is_bool() || !right.is_bool() {
                return Err(get_error_msg());
            }
            Type::Primitive(PrimitiveType::Bool)
        }
        B::Assign => {
            if left != right {
                return Err(get_error_msg());
            }
            right.clone()
        }
        B::AssignAdd | B::AssignSub | B::AssignMul | B::AssignDiv => {
            if !are_arithmetic_compatible_types(left, right) {
                return Err(format!(
                    "Invalid types for operation `{op:?}`: `{left}` and `{right}`"
                ));
            }
            left.clone()
        }
        _ => todo!("type checking for binary operator `{op:?}`"),
    })
}

fn check_expr(
    expr: &ExprWithLoc,
    ctx: &mut TypeCheckerContext,
) -> Result<TypedExpr, String> {
    Ok(match &expr.expr {
        Expr::Id(id) => TypedExpr {
            expr: TypedExprKind::Id(id.id.clone()),
            ty: find_type_of_id(&id, ctx)?,
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
            ty: Type::Ptr(Box::new(Type::Primitive(PrimitiveType::Int8))),
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
            let Expr::Id(id) = &name.expr else {
                return Err(format!("Invalid function name: `{}`", name.expr));
            };
            let Some(func_type) = ctx.get_func_type(&id.id) else {
                return Err(format!("Undefined function `{}`", id.id));
            };
            if func_type.params.len() != args.len() {
                return Err(format!(
                    "Invalid number of arguments for function `{}`: {} expected, {} provided",
                    id.id, func_type.params.len(), args.len()
                ));
            }
            let mut typed_args = Vec::new();
            for arg in args {
                let typed_arg = check_expr(&arg, ctx)?;
                typed_args.push(typed_arg);
            }
            for (param, arg) in func_type.params.iter().zip(typed_args.iter()) {
                if param.1 != arg.ty {
                    return Err(format!(
                        "Invalid argument type for function `{}`: expected `{}`, got `{}`",
                        id.id, param.1, arg.ty
                    ));
                }
            }
            TypedExpr {
                expr: TypedExprKind::FuncCall {
                    name: Box::new(check_expr(&name, ctx)?),
                    args: typed_args,
                },
                ty: *func_type.ret_type,
                loc: expr.loc.clone(),
            }
        }

        Expr::Index { array, index } => {
            let array = check_expr(&array, ctx)?;
            let index = check_expr(&index, ctx)?;
            let Type::Ptr(item_ty) = &array.ty else {
                return Err(format!(
                    "Invalid type for array indexing: `{}`",
                    array.ty
                ));
            };
            let item_ty = *item_ty.clone();
            if !matches!(index.ty, Type::Primitive(PrimitiveType::Int64)) {
                return Err(format!(
                    "Invalid type for array index: `{}`",
                    index.ty
                ));
            }
            TypedExpr {
                expr: TypedExprKind::Index {
                    array: Box::new(array),
                    index: Box::new(index),
                },
                ty: item_ty,
                loc: expr.loc.clone(),
            }
        }
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
            match (&typed_val, type_) {
                (Some(val), Some(type_)) => {
                    let val_ty = val.ty.clone();
                    let type_ty = convert_ast_type(type_, ctx)?;
                    if val_ty != type_ty {
                        return Err(format!(
                            "Invalid type for variable `{}`: expected `{}`, got `{}`",
                            name.id, type_ty, val_ty
                        ));
                    }
                    ctx.func.add_local(name.id.clone(), type_ty);
                }
                (Some(val), None) => {
                    let ty = val.ty.clone();
                    ctx.func.add_local(name.id.clone(), ty);
                }
                (None, Some(type_)) => {
                    let ty = convert_ast_type(type_, ctx)?;
                    ctx.func.add_local(name.id.clone(), ty);
                }
                (None, None) => {
                    todo!("Multi-statement type inference");
                }
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
        let ty = convert_ast_type(&type_, ctx)?;
        ctx.func.add_local(param.id.clone(), ty);
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
    let mut ctx = TypeCheckerContext::new(&program)?;
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
