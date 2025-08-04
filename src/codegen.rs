use crate::{
    ir::{Arg, IRFunction, IRProgram, Op},
    lexer::Binop,
};

#[allow(dead_code)]
static ARG_REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
// return register is "rax"

fn load_arg<W: std::io::Write>(
    arg: &Arg,
    reg: &str,
    func: &IRFunction,
    out: &mut W,
) {
    match arg {
        Arg::Local(i) => {
            assert!(*i < func.stack_size, "load_arg: index out of bounds");
            writeln!(out, "    movq {}(%rbp), %{}", func.slot_offset(*i), reg)
                .unwrap();
        }
        Arg::Literal(val) => {
            writeln!(out, "    movq ${}, %{}", val, reg).unwrap();
        }
        Arg::DataOffset(_) => todo!("load data offset"),
    }
}

fn store_reg<W: std::io::Write>(
    reg: &str,
    index: usize,
    func: &IRFunction,
    out: &mut W,
) {
    assert!(index < func.stack_size, "store_reg: index out of bounds");
    writeln!(out, "    movq %{}, {}(%rbp)", reg, func.slot_offset(index))
        .unwrap();
}

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

    if func.arg_count > ARG_REGISTERS.len() {
        return Err(format!("Too many arguments for function {}", func.name));
    }
    // load args onto stack
    for i in 0..func.arg_count {
        store_reg(ARG_REGISTERS[i], i, func, out);
    }
    for (i, op) in func.body.iter().enumerate() {
        match op {
            Op::LocalAssign { index, arg } => {
                load_arg(arg, "rax", func, out);
                writeln!(
                    out,
                    "    movq %rax, {}(%rbp)",
                    func.slot_offset(*index)
                )
                .unwrap();
            }
            Op::Binop {
                binop,
                index,
                lhs,
                rhs,
            } => {
                load_arg(lhs, "rdx", func, out);
                load_arg(rhs, "rax", func, out);
                match binop {
                    Binop::Add => writeln!(out, "    addq %rdx, %rax").unwrap(),
                    _ => todo!("binop op"),
                }
                store_reg("rax", *index, func, out);
            }
            Op::Return { arg } => {
                load_arg(arg, "rax", func, out);
                // TODO: early returns
                assert_eq!(i, func.body.len() - 1);
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
