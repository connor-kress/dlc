use std::collections::HashMap;

use crate::{
    ast::{
        Expr, ExprWithLoc, Function, IdWithLoc, Program, Statement,
        StatementWithLoc, Type as AstType, TypeWithLoc,
    },
    lexer::{Binop, Primative, Uniop},
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
        for func in &program.functions {
            let mut params = Vec::new();
            for (id, type_) in &func.param_list.params {
                params
                    .push((id.id.clone(), convert_ast_type(&type_, &mut ctx)?));
            }
            let func_type = FuncType {
                params,
                is_variadic: func.is_variadic,
                ret_type: Box::new(convert_ast_type(&func.ret_type, &mut ctx)?),
            };
            ctx.functions.insert(func.name.id.clone(), func_type);
        }
        for extern_func in &program.externs {
            let mut params = Vec::new();
            for (id, type_) in &extern_func.param_list.params {
                params
                    .push((id.id.clone(), convert_ast_type(&type_, &mut ctx)?));
            }
            let func_type = FuncType {
                params,
                is_variadic: extern_func.is_variadic,
                ret_type: Box::new(convert_ast_type(
                    &extern_func.ret_type,
                    &mut ctx,
                )?),
            };
            ctx.functions.insert(extern_func.name.id.clone(), func_type);
        }
        Ok(ctx)
    }

    fn get_func_type(&self, name: &str) -> Option<FuncType> {
        self.functions.get(name).cloned()
    }

    fn get_current_func_type(&self) -> Result<FuncType, String> {
        let name = self
            .func
            .func_name
            .as_ref()
            .ok_or("Not in a function".to_string())?;
        self.get_func_type(name)
            .ok_or(format!("Could not find current function `{name}`"))
    }

    fn get_current_func_name(&self) -> Result<String, String> {
        self.func
            .func_name
            .clone()
            .ok_or("Not in a function".to_string())
    }
}

#[derive(Clone, Debug)]
struct TypeCheckerFunctionContext {
    locals: HashMap<String, Type>,
    func_name: Option<String>,
}

impl<'a> TypeCheckerFunctionContext {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            func_name: None,
        }
    }

    fn get_local(&self, name: &str) -> Option<Type> {
        self.locals.get(name).cloned()
    }

    fn add_local(&mut self, name: String, ty: Type) {
        self.locals.insert(name, ty);
    }

    fn reset_with_name(&mut self, func_name: String) {
        self.locals.clear();
        self.func_name = Some(func_name);
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

fn can_implicit_cast(from: &Type, to: &Type) -> bool {
    match (from, to) {
        (Type::Primitive(from), Type::Primitive(to)) => {
            if to.is_sint() {
                if from.is_sint() {
                    from.bit_size() <= to.bit_size()
                } else if from.is_uint() {
                    from.bit_size() < to.bit_size()
                } else {
                    false
                }
            } else if to.is_uint() {
                from.is_uint() && from.bit_size() <= to.bit_size()
            } else if to.is_float() {
                from.is_float() && from.bit_size() <= to.bit_size()
            } else {
                to == from
            }
        }
        (Type::Ptr(_), Type::Ptr(_)) => true,
        (Type::Ptr(_), Type::Primitive(to)) => {
            matches!(to, Primative::Uint64)
        }
        (Type::Primitive(from), Type::Ptr(_)) => from.is_uint(),
        _ => false,
    }
}

fn get_arithmetic_type(lhs: &Type, rhs: &Type, op: &Binop) -> Option<Type> {
    match (lhs, rhs) {
        (Type::Ptr(_), Type::Primitive(rhs)) => {
            // (p + i) and (p - i)
            if !rhs.is_int() || !matches!(op, Binop::Add | Binop::Sub) {
                return None;
            }
            Some(lhs.clone())
        }
        (Type::Primitive(lhs), Type::Ptr(_)) => {
            // (i + p) but not (i - p)
            if !lhs.is_int() || !matches!(op, Binop::Add) {
                return None;
            }
            Some(rhs.clone())
        }
        _ => {
            if can_implicit_cast(lhs, rhs) {
                Some(rhs.clone())
            } else if can_implicit_cast(rhs, lhs) {
                Some(lhs.clone())
            } else {
                None
            }
        }
    }
}

fn get_binop_type(
    op: &Binop,
    left: &Type,
    right: &Type,
) -> Result<Type, String> {
    let get_error_msg = || {
        format!("Invalid types for operator `{op:?}`: `{left}` and `{right}`")
    };
    use Binop as B;
    Ok(match op {
        B::Add | B::Sub | B::Mul | B::Div => {
            get_arithmetic_type(left, right, op).ok_or_else(get_error_msg)?
        }
        B::Eq | B::Neq => {
            if get_arithmetic_type(left, right, op).is_none() {
                return Err(get_error_msg());
            }
            Type::Primitive(Primative::Bool)
        }
        B::Lt | B::Le | B::Gt | B::Ge => {
            if left.is_ptr() && right.is_ptr() {
                return Ok(Type::Primitive(Primative::Bool));
            }
            let res_type = get_arithmetic_type(left, right, op)
                .ok_or_else(get_error_msg)?;
            if !res_type.is_arithmetic() {
                return Err(get_error_msg());
            }
            Type::Primitive(Primative::Bool)
        }
        B::Land | B::Lor => {
            if !left.is_bool() || !right.is_bool() {
                return Err(get_error_msg());
            }
            Type::Primitive(Primative::Bool)
        }
        B::Assign => {
            if !can_implicit_cast(right, left) {
                return Err(get_error_msg());
            }
            left.clone()
        }
        B::AssignAdd | B::AssignSub | B::AssignMul | B::AssignDiv => {
            if !can_implicit_cast(right, left) {
                return Err(get_error_msg());
            }
            left.clone()
        }
        _ => todo!("type checking for binary operator `{op:?}`"),
    })
}

fn get_uniop_type(op: &Uniop, arg: &Type) -> Result<Type, String> {
    let get_error_msg =
        || format!("Invalid type for operator `{op:?}`: `{arg}`");
    Ok(match op {
        Uniop::Ref => Type::Ptr(Box::new(arg.clone())),
        Uniop::Deref => match arg {
            Type::Ptr(ty) => *ty.clone(),
            _ => return Err(get_error_msg()),
        },
        Uniop::Lnot => {
            if !arg.is_bool() {
                return Err(get_error_msg());
            }
            Type::Primitive(Primative::Bool)
        }
        Uniop::Plus | Uniop::Minus | Uniop::BitNot => {
            let Type::Primitive(prim) = arg else {
                return Err(get_error_msg());
            };
            if !prim.is_int() && !prim.is_float() {
                return Err(get_error_msg());
            }
            arg.clone()
        }
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

        Expr::IntLit(n, hint) => TypedExpr {
            expr: TypedExprKind::IntLit(*n),
            ty: Type::Primitive(hint.unwrap_or(Primative::Int32)),
            loc: expr.loc.clone(),
        },

        Expr::FloatLit(n, hint) => TypedExpr {
            expr: TypedExprKind::FloatLit(*n),
            ty: Type::Primitive(hint.unwrap_or(Primative::Float64)),
            loc: expr.loc.clone(),
        },

        Expr::StrLit(s) => TypedExpr {
            expr: TypedExprKind::StrLit(s.clone()),
            ty: Type::Ptr(Box::new(Type::Primitive(Primative::Uint8))),
            loc: expr.loc.clone(),
        },

        Expr::BoolLit(b) => TypedExpr {
            expr: TypedExprKind::BoolLit(*b),
            ty: Type::Primitive(Primative::Bool),
            loc: expr.loc.clone(),
        },

        Expr::Uniop { op, arg } => {
            let inner_typed = check_expr(&arg, ctx)?;
            let inner_ty = inner_typed.ty.clone();
            TypedExpr {
                expr: TypedExprKind::Uniop {
                    op: op.clone(),
                    arg: Box::new(inner_typed),
                },
                ty: get_uniop_type(&op, &inner_ty)?,
                loc: expr.loc.clone(),
            }
        }

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
            if func_type.is_variadic {
                if args.len() < func_type.params.len() {
                    return Err(format!(
                        "Invalid number of arguments for variadic function `{}`: {}+ expected, {} provided",
                        id.id, func_type.params.len(), args.len()
                    ));
                }
            } else {
                if args.len() != func_type.params.len() {
                    return Err(format!(
                        "Invalid number of arguments for function `{}`: {} expected, {} provided",
                        id.id, func_type.params.len(), args.len()
                    ));
                }
            }
            let mut typed_args = Vec::new();
            for arg in args {
                let typed_arg = check_expr(&arg, ctx)?;
                typed_args.push(typed_arg);
            }
            for (param, arg) in func_type.params.iter().zip(typed_args.iter()) {
                if !can_implicit_cast(&arg.ty, &param.1) {
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
            if !matches!(index.ty, Type::Primitive(Primative::Uint64)) {
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
        Expr::Cast { expr: inner, type_ } => {
            let inner_expr = check_expr(&inner, ctx)?;
            let type_ = convert_ast_type(&type_, ctx)?;
            // TODO: Check if cast is valid
            // if !can_cast(&inner_expr.ty, &type_) {
            //     return Err(format!(
            //         "Invalid cast from `{}` to `{}`",
            //         inner_expr.ty, type_
            //     ));
            // }
            TypedExpr {
                expr: TypedExprKind::Cast {
                    expr: Box::new(inner_expr),
                    type_: type_.clone(),
                },
                ty: type_,
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
            let typed_cond = check_expr(&cond, ctx)?;
            if !typed_cond.ty.is_bool() {
                return Err(format!(
                    "Invalid type for if condition: `{}`",
                    typed_cond.ty
                ));
            }
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
                cond: Box::new(typed_cond),
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
            let typed_pred = check_expr(&pred, ctx)?;
            if !typed_pred.ty.is_bool() {
                return Err(format!(
                    "Invalid type for while condition: `{}`",
                    typed_pred.ty
                ));
            }
            let mut typed_body = Vec::new();
            for statement in body {
                let typed_statement = check_statement(&statement, ctx)?;
                typed_body.push(typed_statement);
            }
            TypedStatementKind::WhileLoop {
                pred: Box::new(typed_pred),
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
            if let Some(val) = &typed_val {
                let val_ty = val.ty.clone();
                let func_type = ctx.get_current_func_type()?;
                if val_ty != *func_type.ret_type {
                    let func_name = ctx.get_current_func_name()?;
                    return Err(format!(
                        "Invalid return type for function `{}`: expected `{}`, got `{}`",
                        func_name, func_type.ret_type, val_ty
                    ));
                }
            } else {
                let func_type = ctx.get_current_func_type()?;
                if !func_type.ret_type.is_void() {
                    let func_name = ctx.get_current_func_name()?;
                    return Err(format!(
                        "Invalid return type for function `{}`: expected `{}`, got `Void`",
                        func_name, func_type.ret_type
                    ));
                }
            }
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
        ctx.func.reset_with_name(function.name.id.clone());
        let typed_function = check_function(&function, &mut ctx)?;
        typed_functions.push(typed_function);
        println!("{:#?}", ctx.func);
    }
    Ok(TypedProgram {
        functions: typed_functions,
    })
}
