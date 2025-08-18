use crate::{
    ast::{Expr, ExprWithLoc, Function, Program, Statement, StatementWithLoc},
    ir::{Arg, IRFunction, IRProgram, Op},
    lexer::{Binop, Loc, Primative, Uniop},
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
enum StackItem {
    Local(String),
    Temp,
    Free,
}

#[derive(Clone, Debug)]
struct FnContext {
    pub stack: Vec<StackItem>,
    pub ops: Vec<Op>,
}

impl FnContext {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            ops: Vec::new(),
        }
    }

    fn first_free_local(&self) -> Option<usize> {
        self.stack.iter().position(|s| matches!(s, StackItem::Free))
    }

    fn add_local(&mut self, name: String) -> usize {
        if let Some(index) = self.first_free_local() {
            self.stack[index] = StackItem::Local(name);
            index
        } else {
            self.stack.push(StackItem::Local(name));
            self.stack.len() - 1
        }
    }

    fn add_tmp_local(&mut self) -> usize {
        if let Some(index) = self.first_free_local() {
            self.stack[index] = StackItem::Temp;
            index
        } else {
            self.stack.push(StackItem::Temp);
            self.stack.len() - 1
        }
    }

    fn is_tmp_local(&self, index: usize) -> bool {
        matches!(self.stack[index], StackItem::Temp)
    }

    fn reassign_local(&mut self, index: usize, name: String) {
        self.stack[index] = StackItem::Local(name);
    }

    fn free_local(&mut self, index: usize) {
        self.stack[index] = StackItem::Free;
    }

    fn free_tmp_arg(&mut self, arg: &Arg) {
        match arg {
            Arg::Local(index) => {
                if self.is_tmp_local(*index) {
                    self.free_local(*index);
                }
            }
            Arg::Deref(arg) => self.free_tmp_arg(arg),
            _ => {}
        }
    }

    fn get_local(&self, name: &str) -> Option<usize> {
        self.stack.iter().position(|s| {
            if let StackItem::Local(s) = s {
                s == name
            } else {
                false
            }
        })
    }
}

fn convert_index_expr_to_deref(
    array: &ExprWithLoc,
    index: &ExprWithLoc,
    loc: Loc,
) -> Result<ExprWithLoc, String> {
    let offset_in_bytes = ExprWithLoc::new(
        Expr::Binop {
            op: Binop::Mul,
            left: Box::new(index.clone()),
            right: Box::new(ExprWithLoc::new(
                Expr::IntLit(8, Some(Primative::Uint64)),
                index.loc.clone(),
            )),
        },
        index.loc.clone(),
    );
    let index_expr = ExprWithLoc::new(
        Expr::Binop {
            op: Binop::Add,
            left: Box::new(array.clone()),
            right: Box::new(offset_in_bytes),
        },
        index.loc.clone(),
    );
    Ok(ExprWithLoc::new(
        Expr::Uniop {
            op: Uniop::Deref,
            arg: Box::new(index_expr),
        },
        loc,
    ))
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
        Expr::IntLit(val, _) => Arg::Literal((*val).into()),
        Expr::StrLit(s) => {
            let index = proc_ctx.add_string_literal(s.clone());
            Arg::DataLabel(format!(".STR{index}"))
        }
        Expr::BoolLit(b) => Arg::Literal((*b).into()),
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
                        fn_ctx.free_tmp_arg(&rhs);
                        Arg::Local(index)
                    } else {
                        fn_ctx.ops.push(Op::LocalAssign {
                            index,
                            arg: rhs.clone(),
                        });
                        fn_ctx.free_tmp_arg(&rhs);
                        rhs
                    }
                }
                Expr::Uniop {
                    op: Uniop::Deref,
                    arg: ptr_arg,
                } => {
                    let ptr = compile_expr(ptr_arg, fn_ctx, proc_ctx)?;
                    let ptr_index = fn_ctx.add_tmp_local();
                    fn_ctx.ops.push(Op::LocalAssign {
                        index: ptr_index,
                        arg: ptr.clone(),
                    });
                    let rhs = compile_expr(&right, fn_ctx, proc_ctx)?;
                    let res = if let Some(assign_op) = op.assign_op()? {
                        let val_index = fn_ctx.add_tmp_local();
                        fn_ctx.ops.push(Op::Binop {
                            binop: assign_op,
                            index: val_index,
                            lhs: Arg::Deref(Box::new(ptr.clone())),
                            rhs: rhs.clone(),
                        });
                        fn_ctx.ops.push(Op::Store {
                            index: ptr_index,
                            arg: Arg::Local(val_index),
                        });
                        fn_ctx.free_tmp_arg(&rhs);
                        Arg::Local(val_index)
                    } else {
                        fn_ctx.ops.push(Op::Store {
                            index: ptr_index,
                            arg: rhs.clone(),
                        });
                        rhs
                    };
                    fn_ctx.free_tmp_arg(&ptr);
                    fn_ctx.free_local(ptr_index);
                    res
                }
                Expr::Index { array, index } => {
                    let index_expr = convert_index_expr_to_deref(
                        array,
                        index,
                        expr.loc.clone(),
                    )?;
                    let assignment_expr = ExprWithLoc::new(
                        Expr::Binop {
                            op: op.clone(),
                            left: Box::new(index_expr),
                            right: right.clone(),
                        },
                        expr.loc.clone(),
                    );
                    compile_expr(&assignment_expr, fn_ctx, proc_ctx)?
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
            fn_ctx.free_tmp_arg(&lhs);
            fn_ctx.free_tmp_arg(&rhs);
            let index = fn_ctx.add_tmp_local();
            fn_ctx.ops.push(Op::Binop {
                binop: op.clone(),
                index,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            });
            Arg::Local(index)
        }
        Expr::Uniop { op, arg } => {
            let arg = compile_expr(&arg, fn_ctx, proc_ctx)?;
            let index = fn_ctx.add_tmp_local();
            if matches!(op, Uniop::Plus) {
                arg // noop
            } else {
                fn_ctx.free_tmp_arg(&arg);
                fn_ctx.ops.push(Op::Uniop {
                    uniop: op.clone(),
                    index,
                    arg: arg.clone(),
                });
                Arg::Local(index)
            }
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
                args: ir_args.clone(),
            });
            for arg in ir_args.iter() {
                fn_ctx.free_tmp_arg(arg);
            }
            Arg::Local(index)
        }
        Expr::Index { array, index } => {
            let index_expr =
                convert_index_expr_to_deref(array, index, expr.loc.clone())?;
            compile_expr(&index_expr, fn_ctx, proc_ctx)?
        }
        Expr::Cast {
            expr,
            type_: _type_,
        } => {
            // TODO: implement casting
            compile_expr(&expr, fn_ctx, proc_ctx)?
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
            let arg = compile_expr(expr, fn_ctx, proc_ctx)?;
            fn_ctx.free_tmp_arg(&arg);
        }
        Statement::VarDecl { name, val, .. } => {
            match (fn_ctx.get_local(&name.id), val) {
                (Some(index), Some(val)) => {
                    let arg = compile_expr(val, fn_ctx, proc_ctx)?;
                    fn_ctx.ops.push(Op::LocalAssign {
                        index,
                        arg: arg.clone(),
                    });
                    fn_ctx.free_tmp_arg(&arg);
                }
                (Some(_index), None) => {}
                (None, Some(val)) => {
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
                        fn_ctx.ops.push(Op::LocalAssign {
                            index,
                            arg: arg.clone(),
                        });
                    }
                    fn_ctx.free_tmp_arg(&arg);
                }
                (None, None) => {
                    let _ = fn_ctx.add_local(name.id.clone());
                }
            }
        }
        Statement::Return { val } => {
            if let Some(val) = val {
                let arg = compile_expr(val, fn_ctx, proc_ctx)?;
                fn_ctx.ops.push(Op::Return { arg: arg.clone() });
                fn_ctx.free_tmp_arg(&arg);
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
                arg: cond.clone(),
            });
            fn_ctx.free_tmp_arg(&cond);
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
                arg: cond.clone(),
            });
            fn_ctx.free_tmp_arg(&cond);
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
    // println!("stack: {:?}", fn_ctx.stack);
    let arg_count = func.param_list.params.len();
    let ir_func = IRFunction::new(
        func.name.id.clone(),
        arg_count,
        fn_ctx.stack.len() - arg_count, // local_count
        fn_ctx.ops,
        proc_ctx.new_label("exit"),
    );
    Ok(ir_func)
}

pub fn compile_program(program: &Program) -> Result<IRProgram, String> {
    let mut proc_ctx = ProgramContext::new();
    let mut ir_funcs = Vec::new();
    for func in program.functions.iter() {
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
