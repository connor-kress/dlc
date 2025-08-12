use crate::{
    ast::{Expr, ExprWithLoc, Function, Statement, StatementWithLoc},
    ir::{Arg, IRFunction, IRProgram, Op},
};

#[derive(Clone, Debug)]
struct ProgramContext {
    pub string_literals: Vec<String>,
    label_count: usize,
}

impl ProgramContext {
    fn new() -> Self {
        Self {
            string_literals: Vec::new(),
            label_count: 0,
        }
    }

    fn add_string_literal(&mut self, s: String) -> usize {
        self.string_literals.push(s);
        self.string_literals.len() - 1
    }

    fn new_label(&mut self, name: &str) -> String {
        let label = format!(".{name}{count}", count = self.label_count);
        self.label_count += 1;
        label
    }
}

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
    fn_ctx: &mut FnContext,
    proc_ctx: &mut ProgramContext,
) -> Result<Arg, String> {
    // Should the stack have optional strings for unamed temp values?
    // Should there be a separate IRBinop/Uniop type?
    let arg = match &expr.expr {
        Expr::Id(id) => {
            let index = fn_ctx.get_local(&id.id).ok_or_else(|| {
                format!("Undefined local variable \"{}\"", id.id)
            })?;
            Arg::Local(index)
        }
        Expr::IntLit(val) => Arg::Literal((*val).into()),
        Expr::StrLit(s) => {
            let index = proc_ctx.add_string_literal(s.clone());
            Arg::DataLabel(format!(".STR{index}"))
        }
        Expr::Binop { op, left, right } if op.is_assignment() => {
            match &left.expr {
                Expr::Id(id) => {
                    let index = fn_ctx.get_local(&id.id).ok_or_else(|| {
                        format!("Undefined local variable \"{}\"", id.id)
                    })?;
                    let rhs = compile_expr(&right, fn_ctx, proc_ctx)?;
                    if let Some(assign_op) = op.assign_op()? {
                        fn_ctx.ops.push(Op::Binop {
                            binop: assign_op,
                            index,
                            lhs: Arg::Local(index),
                            rhs: rhs.clone(),
                        });
                    } else {
                        fn_ctx.ops.push(Op::LocalAssign {
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
            let lhs = compile_expr(&left, fn_ctx, proc_ctx)?;
            let rhs = compile_expr(&right, fn_ctx, proc_ctx)?;
            let index = fn_ctx.add_tmp_local();
            fn_ctx.ops.push(Op::Binop {
                binop: op.clone(),
                index,
                lhs,
                rhs,
            });
            Arg::Local(index)
        }
        Expr::FuncCall { name, args } => {
            let mut ir_args = Vec::new();
            for arg in args.iter() {
                ir_args.push(compile_expr(arg, fn_ctx, proc_ctx)?);
            }
            let Expr::Id(name) = &name.expr else {
                // TODO: function pointers
                return Err(format!("Invalid function name: `{}`", name.expr));
            };
            let index = fn_ctx.add_tmp_local();
            fn_ctx.ops.push(Op::FuncCall {
                func: name.id.clone(),
                ret: index,
                args: ir_args,
            });
            Arg::Local(index)
        }
        other => {
            todo!("Missing compile_expr: {}", other);
        }
    };
    Ok(arg)
}

fn compile_stmt(
    stmt: &StatementWithLoc,
    fn_ctx: &mut FnContext,
    proc_ctx: &mut ProgramContext,
) -> Result<(), String> {
    match &stmt.statement {
        Statement::Expr(expr) => {
            let _ = compile_expr(expr, fn_ctx, proc_ctx)?;
        }
        Statement::VarDecl { name, val, .. } => {
            if let Some(val) = val {
                let arg = compile_expr(val, fn_ctx, proc_ctx)?;
                let mut assigned_tmp = false;
                if let Arg::Local(index) = &arg {
                    if fn_ctx.is_tmp_local(*index) {
                        fn_ctx.reassign_local(*index, name.id.clone());
                        assigned_tmp = true;
                    }
                }
                if !assigned_tmp {
                    let index = fn_ctx.add_local(name.id.clone());
                    fn_ctx.ops.push(Op::LocalAssign { index, arg });
                }
            }
        }
        Statement::Return { val } => {
            if let Some(val) = val {
                let arg = compile_expr(val, fn_ctx, proc_ctx)?;
                fn_ctx.ops.push(Op::Return { arg });
            } else {
                fn_ctx.ops.push(Op::Return {
                    arg: Arg::Literal(0),
                });
            }
        }
        Statement::If {
            cond,
            if_block,
            else_block,
        } => {
            let cond = compile_expr(cond, fn_ctx, proc_ctx)?;
            let otherwise_label = proc_ctx.new_label("otherwise");
            fn_ctx.ops.push(Op::JumpIfZero {
                label: otherwise_label.clone(),
                arg: cond,
            });
            for stmt in if_block.iter() {
                compile_stmt(&stmt, fn_ctx, proc_ctx)?;
            }
            if let Some(else_block) = else_block {
                let after_else_label = proc_ctx.new_label("after_else");
                fn_ctx.ops.push(Op::Jump {
                    label: after_else_label.clone(),
                });
                fn_ctx.ops.push(Op::Label(otherwise_label));
                for stmt in else_block.iter() {
                    compile_stmt(&stmt, fn_ctx, proc_ctx)?;
                }
                fn_ctx.ops.push(Op::Label(after_else_label));
            } else {
                fn_ctx.ops.push(Op::Label(otherwise_label));
            }
        }
        Statement::WhileLoop { pred, body } => {
            let loop_label = proc_ctx.new_label("loop");
            let after_loop_label = proc_ctx.new_label("after_loop");
            fn_ctx.ops.push(Op::Label(loop_label.clone()));
            let cond = compile_expr(pred, fn_ctx, proc_ctx)?;
            fn_ctx.ops.push(Op::JumpIfZero {
                label: after_loop_label.clone(),
                arg: cond,
            });
            for stmt in body.iter() {
                compile_stmt(&stmt, fn_ctx, proc_ctx)?;
            }
            fn_ctx.ops.push(Op::Jump {
                label: loop_label.clone(),
            });
            fn_ctx.ops.push(Op::Label(after_loop_label));
        }
        _ => todo!(),
    }
    Ok(())
}

fn compile_function(
    func: &Function,
    proc_ctx: &mut ProgramContext,
) -> Result<IRFunction, String> {
    let mut fn_ctx = FnContext::new();
    for param in func.param_list.params.iter() {
        fn_ctx.add_local(param.0.id.clone());
    }
    for stmt in func.body.iter() {
        compile_stmt(&stmt, &mut fn_ctx, proc_ctx)?;
    }
    println!("stack: {:?}", fn_ctx.stack);
    let arg_count = func.param_list.params.len();
    let ir_func = IRFunction::new(
        func.name.id.clone(),
        arg_count,
        fn_ctx.stack.len() - arg_count, // local_count
        fn_ctx.ops,
    );
    println!("{}", ir_func);
    Ok(ir_func)
}

pub fn compile_program(funcs: &Vec<Function>) -> Result<IRProgram, String> {
    let mut proc_ctx = ProgramContext::new();
    let mut ir_funcs = Vec::new();
    for func in funcs.iter() {
        let ir_func = compile_function(func, &mut proc_ctx)?;
        ir_funcs.push(ir_func);
    }
    let program = IRProgram {
        functions: ir_funcs,
        string_literals: proc_ctx.string_literals,
    };
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
