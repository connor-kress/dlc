use crate::{
    ast::Function,
    ir::{Arg, IRFunction, IRProgram, Op},
    lexer::Binop,
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

fn compile_function(func: &Function) -> Result<IRFunction, String> {
    let ir_func = IRFunction::new(
        func.name.id.clone(),
        0, // arg_count
        3, // local_count
        vec![
            Op::LocalAssign {
                index: 0,
                arg: Arg::Literal(34),
            },
            Op::LocalAssign {
                index: 1,
                arg: Arg::Literal(35),
            },
            Op::Binop {
                binop: Binop::Add,
                index: 2,
                lhs: Arg::Local(0),
                rhs: Arg::Local(1),
            },
            Op::Return { arg: Arg::Local(2) },
        ],
    );
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
