use crate::{
    ast::{Expr, Function, Statement},
    ir::{Arg, IRFunction, IRProgram, Op},
};

// For reference:
// let foo = IRFunction::new(
//     "foo".to_string(),
//     3, // arg_count
//     2, // local_count
//     vec![
//         Op::Binop {
//             binop: Binop::Add,
//             index: 3,
//             lhs: Arg::Local(0),
//             rhs: Arg::Literal(2),
//         },
//         Op::Binop {
//             binop: Binop::Add,
//             index: 4,
//             lhs: Arg::Local(3),
//             rhs: Arg::Local(2),
//         },
//         Op::Return { arg: Arg::Local(4) },
//     ],
// );

fn compile_expr(
    _expr: &Expr,
    _ops: &mut Vec<Op>,
    _stack: &mut Vec<String>,
) -> Result<Arg, String> {
    // TODO: handle recursively and special case for assignment
    Ok(Arg::Literal(69))
}

fn compile_function(func: &Function) -> Result<IRFunction, String> {
    let mut stack = Vec::new();
    let mut ops = Vec::new();
    for param in func.param_list.params.iter() {
        stack.push(param.0.id.clone());
    }
    for stmt in func.body.iter() {
        match &stmt.statement {
            Statement::Expr(expr) => {
                let _ = compile_expr(&expr.expr, &mut ops, &mut stack)?;
            }
            Statement::VarDecl { name, val, .. } => {
                let index = stack.len();
                stack.push(name.id.clone());
                if let Some(val) = val {
                    let arg = compile_expr(&val.expr, &mut ops, &mut stack)?;
                    ops.push(Op::LocalAssign { index, arg });
                }
            }
            Statement::Return { val } => {
                if let Some(val) = val {
                    let arg = compile_expr(&val.expr, &mut ops, &mut stack)?;
                    ops.push(Op::Return { arg });
                } else {
                    ops.push(Op::Return {
                        arg: Arg::Literal(0),
                    });
                }
            }
            _ => todo!(),
        }
    }
    let arg_count = func.param_list.params.len();
    let ir_func = IRFunction::new(
        func.name.id.clone(),
        arg_count,
        stack.len() - arg_count, // local_count
        ops,
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
