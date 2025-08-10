use crate::{
    ast::{Expr, ExprWithLoc, Function, Statement, StatementWithLoc},
    ir::{Arg, IRFunction, IRProgram, Op},
};

#[derive(Clone, Debug)]
struct FnContext {
    pub stack: Vec<Option<String>>,
    pub ops: Vec<Op>,
}

impl FnContext {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            ops: Vec::new(),
        }
    }

    fn add_local(&mut self, name: String) -> usize {
        self.stack.push(Some(name));
        self.stack.len() - 1
    }

    fn add_tmp_local(&mut self) -> usize {
        self.stack.push(None);
        self.stack.len() - 1
    }

    fn is_tmp_local(&self, index: usize) -> bool {
        self.stack[index].is_none()
    }

    fn reassign_local(&mut self, index: usize, name: String) {
        self.stack[index] = Some(name);
    }

    fn get_local(&self, name: &str) -> Option<usize> {
        self.stack.iter().position(|s| {
            if let Some(s) = s {
                s == name
            } else {
                false
            }
        })
    }
}

fn compile_expr(
    expr: &ExprWithLoc,
    ctx: &mut FnContext,
) -> Result<Arg, String> {
    // Should the stack have optional strings for unamed temp values?
    // Should there be a separate IRBinop/Uniop type?
    let arg = match &expr.expr {
        Expr::Id(id) => {
            let index = ctx.get_local(&id.id).ok_or_else(|| {
                format!("Undefined local variable \"{}\"", id.id)
            })?;
            Arg::Local(index)
        }
        Expr::IntLit(val) => Arg::Literal((*val).into()),
        Expr::Binop { op, left, right } if op.is_assignment() => {
            match &left.expr {
                Expr::Id(id) => {
                    let index = ctx.get_local(&id.id).ok_or_else(|| {
                        format!("Undefined local variable \"{}\"", id.id)
                    })?;
                    let rhs = compile_expr(&right, ctx)?;
                    if let Some(assign_op) = op.assign_op()? {
                        ctx.ops.push(Op::Binop {
                            binop: assign_op,
                            index,
                            lhs: Arg::Local(index),
                            rhs: rhs.clone(),
                        });
                    } else {
                        ctx.ops.push(Op::LocalAssign {
                            index,
                            arg: rhs.clone(),
                        });
                    }
                    rhs
                }
                _ => {
                    return Err(format!(
                        "Invalid left-hand side of assignment: `{}`",
                        left,
                    ));
                }
            }
        }
        Expr::Binop { op, left, right } => {
            let lhs = compile_expr(&left, ctx)?;
            let rhs = compile_expr(&right, ctx)?;
            let index = ctx.add_tmp_local();
            ctx.ops.push(Op::Binop {
                binop: op.clone(),
                index,
                lhs,
                rhs,
            });
            Arg::Local(index)
        }
        other => {
            println!("Missing compile_expr: {:?}", other);
            todo!();
        }
    };
    Ok(arg)
}

fn compile_stmt(
    stmt: &StatementWithLoc,
    ctx: &mut FnContext,
) -> Result<(), String> {
    match &stmt.statement {
        Statement::Expr(expr) => {
            let _ = compile_expr(expr, ctx)?;
        }
        Statement::VarDecl { name, val, .. } => {
            if let Some(val) = val {
                let arg = compile_expr(val, ctx)?;
                let mut assigned_tmp = false;
                if let Arg::Local(index) = &arg {
                    if ctx.is_tmp_local(*index) {
                        ctx.reassign_local(*index, name.id.clone());
                        assigned_tmp = true;
                    }
                }
                if !assigned_tmp {
                    let index = ctx.add_local(name.id.clone());
                    ctx.ops.push(Op::LocalAssign { index, arg });
                }
            }
        }
        Statement::Return { val } => {
            if let Some(val) = val {
                let arg = compile_expr(val, ctx)?;
                ctx.ops.push(Op::Return { arg });
            } else {
                ctx.ops.push(Op::Return {
                    arg: Arg::Literal(0),
                });
            }
        }
        _ => todo!(),
    }
    Ok(())
}

fn compile_function(func: &Function) -> Result<IRFunction, String> {
    let mut ctx = FnContext::new();
    for param in func.param_list.params.iter() {
        ctx.add_local(param.0.id.clone());
    }
    for stmt in func.body.iter() {
        compile_stmt(&stmt, &mut ctx)?;
    }
    println!("stack: {:?}", ctx.stack);
    let arg_count = func.param_list.params.len();
    let ir_func = IRFunction::new(
        func.name.id.clone(),
        arg_count,
        ctx.stack.len() - arg_count, // local_count
        ctx.ops,
    );
    println!("{}", ir_func);
    Ok(ir_func)
}

pub fn compile_program(funcs: &Vec<Function>) -> Result<IRProgram, String> {
    let mut ir_funcs = Vec::new();
    for func in funcs.iter() {
        let ir_func = compile_function(func)?;
        ir_funcs.push(ir_func);
    }
    let program = IRProgram::new(ir_funcs);
    Ok(program)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize_string;
    use crate::parser::parse_program;

    static PROGRAM: &str = r#"
fn foo(a: int64, b: int64, c: int64) -> int64 {
    let res = a + 2;
    res += c;
    return res;
}

fn main(argc: int64, argv: **char) -> int64 {
    let x = 100;
    x -= 10*3;
    x /= 3;
    x *= 3;
    return x;
}
"#;

    #[test]
    fn test_compile_program() {
        let tokens = tokenize_string(PROGRAM).expect("tokenize");
        let functions = parse_program(tokens).expect("parse");
        let ir = compile_program(&functions).expect("compile");
        assert!(!ir.functions.is_empty());
        // TODO: assert exact IR output
    }
}
