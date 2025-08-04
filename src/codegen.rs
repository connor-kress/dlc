use crate::ir::{Arg, IRFunction, IRProgram, Op};

/// Generate an x86_64 assembly function from an IR function.
pub fn generate_function<W: std::io::Write>(
    func: &IRFunction,
    out: &mut W,
) -> Result<(), String> {
    writeln!(out, ".globl {}", func.name).unwrap();
    writeln!(out, ".type {}, @function", func.name).unwrap();
    writeln!(out, "{}:", func.name).unwrap();

    writeln!(out, "    pushq %rbp").unwrap();
    writeln!(out, "    movq %rsp, %rbp").unwrap();
    for (_i, op) in func.body.iter().enumerate() {
        match op {
            Op::Binop { .. } => todo!("binop op"),
            Op::AutoAssign { .. } => todo!("autoassign op"),
            Op::Return { arg } => {
                if let Some(arg) = arg {
                    match arg {
                        Arg::AutoVar(_) => todo!("auto var"),
                        Arg::Literal(val) => {
                            writeln!(out, "    movq ${}, -8(%rbp)", val)
                                .unwrap();
                            writeln!(out, "    movq -8(%rbp), %rax").unwrap();
                        }
                        Arg::DataOffset(_) => todo!("data offset"),
                    }
                }
                // This is written at the end aleady?
                // writeln!(out, "    ret").unwrap();
            }
        }
    }
    writeln!(out, "    popq %rbp").unwrap();
    writeln!(out, "    ret").unwrap();
    Ok(())
}

/// Generate an x86_64 assembly program from an IR program.
pub fn generate_program<W: std::io::Write>(
    program: &IRProgram,
    out: &mut W,
) -> Result<(), String> {
    write!(out, ".text\n\n").unwrap();
    for (i, func) in program.functions.iter().enumerate() {
        if i != 0 {
            write!(out, "\n").unwrap();
        }
        generate_function(func, out)?;
    }
    Ok(())
}
