use crate::{
    ir::{Arg, IRFunction, IRProgram, Op},
    lexer::{Binop, Uniop},
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
            let offset = func.slot_offset(*i);
            writeln!(out, "    movq {offset}(%rbp), %{reg}").unwrap();
        }
        Arg::Literal(val) => {
            writeln!(out, "    movq ${val}, %{reg}").unwrap();
        }
        Arg::DataLabel(s) => {
            writeln!(out, "    movq ${s}, %{reg}").unwrap();
        }
        Arg::Deref(inner) => {
            load_arg(inner, reg, func, out);
            writeln!(out, "    movq (%{reg}), %{reg}").unwrap();
        }
    }
}

fn store_reg<W: std::io::Write>(
    reg: &str,
    index: usize,
    func: &IRFunction,
    out: &mut W,
) {
    assert!(index < func.stack_size, "store_reg: index out of bounds");
    let offset = func.slot_offset(index);
    writeln!(out, "    movq %{reg}, {offset}(%rbp)").unwrap();
}

/// Generate an x86_64 assembly function from an IR function.
pub fn emit_function<W: std::io::Write>(
    func: &IRFunction,
    out: &mut W,
) -> Result<(), String> {
    writeln!(out, ".globl {}", func.name).unwrap();
    writeln!(out, ".type {}, @function", func.name).unwrap();
    writeln!(out, "{}:", func.name).unwrap();
    writeln!(out, "    pushq %rbp").unwrap();
    writeln!(out, "    movq %rsp, %rbp").unwrap();
    let should_save_stack =
        func.body.iter().any(|op| matches!(op, Op::FuncCall { .. }));
    if should_save_stack {
        writeln!(out, "    subq ${}, %rsp", func.stack_size).unwrap();
    }

    if func.arg_count > ARG_REGISTERS.len() {
        return Err(format!("Too many parameters for function {}", func.name));
    }
    // load args onto stack
    for i in 0..func.arg_count {
        store_reg(ARG_REGISTERS[i], i, func, out);
    }
    let mut used_exit_label = false;
    for (i, op) in func.body.iter().enumerate() {
        match op {
            Op::LocalAssign { index, arg } => {
                load_arg(arg, "rax", func, out);
                let offset = func.slot_offset(*index);
                writeln!(out, "    movq %rax, {offset}(%rbp)").unwrap();
            }

            Op::Store { index, arg } => {
                load_arg(arg, "rax", func, out);
                load_arg(&Arg::Local(*index), "rdx", func, out);
                writeln!(out, "    movq %rax, (%rdx)").unwrap();
            }

            Op::Binop {
                binop,
                index,
                lhs,
                rhs,
            } => {
                load_arg(lhs, "rax", func, out);
                match binop {
                    Binop::Add => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    addq %rdx, %rax").unwrap();
                    }
                    Binop::Sub => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    subq %rdx, %rax").unwrap();
                    }
                    Binop::Mul => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    imulq %rdx, %rax").unwrap()
                    }
                    Binop::Div => {
                        load_arg(rhs, "rcx", func, out);
                        // sign-extend rax into rdx:rax
                        writeln!(out, "    cqto").unwrap();
                        // rax = quotient, rdx = remainder
                        writeln!(out, "    idivq %rcx").unwrap();
                    }
                    Binop::Eq => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    cmpq %rdx, %rax").unwrap();
                        writeln!(out, "    sete %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Neq => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    cmpq %rdx, %rax").unwrap();
                        writeln!(out, "    setne %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Lt => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    cmpq %rdx, %rax").unwrap();
                        writeln!(out, "    setl %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Gt => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    cmpq %rdx, %rax").unwrap();
                        writeln!(out, "    setg %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Le => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    cmpq %rdx, %rax").unwrap();
                        writeln!(out, "    setle %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Ge => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    cmpq %rdx, %rax").unwrap();
                        writeln!(out, "    setge %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    // TODO: short-circuit for land/lor
                    Binop::Land => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    andq %rdx, %rax").unwrap();
                        writeln!(out, "    setne %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Lor => {
                        load_arg(rhs, "rdx", func, out);
                        writeln!(out, "    orq %rdx, %rax").unwrap();
                        writeln!(out, "    setne %al").unwrap();
                        writeln!(out, "    movzbq %al, %rax").unwrap();
                    }
                    Binop::Assign
                    | Binop::AssignAdd
                    | Binop::AssignSub
                    | Binop::AssignMul
                    | Binop::AssignDiv => {
                        unreachable!("assignment ops should already be handled")
                    }
                    _ => todo!("binop codegen: {binop:?}"),
                }
                store_reg("rax", *index, func, out);
            }

            Op::Uniop { uniop, index, arg } => {
                load_arg(arg, "rax", func, out);
                match uniop {
                    Uniop::Deref => {
                        writeln!(out, "    movq (%rax), %rax").unwrap();
                    }
                    Uniop::Ref => match arg {
                        Arg::Local(i) => {
                            let offset = func.slot_offset(*i);
                            writeln!(out, "    leaq {}(%rbp), %rax", offset)
                                .unwrap();
                        }
                        _ => {
                            return Err(format!(
                                "`&` not supported for arg type: {arg:?}"
                            ));
                        }
                    },
                    _ => todo!("uniop codegen: {uniop:?}"),
                }
                store_reg("rax", *index, func, out);
            }

            Op::Return { arg } => {
                load_arg(arg, "rax", func, out);
                if i != func.body.len() - 1 {
                    used_exit_label = true;
                    writeln!(out, "    jmp {}", func.exit_label).unwrap();
                }
            }

            Op::FuncCall {
                func: func_name,
                ret,
                args,
            } => {
                if args.len() >= ARG_REGISTERS.len() {
                    return Err(format!(
                        "Too many arguments for function call to {func}"
                    ));
                }
                for (i, arg) in args.iter().enumerate() {
                    load_arg(arg, ARG_REGISTERS[i], func, out);
                }
                // TODO: set %eax for variadic functions
                // Set value to the number of vector registers
                writeln!(out, "    call {func_name}").unwrap();
                store_reg("rax", *ret, func, out);
            }

            Op::Label(label) => {
                writeln!(out, "    {label}:").unwrap();
            }

            Op::Jump { label } => {
                writeln!(out, "    jmp {label}").unwrap();
            }

            Op::JumpIfZero { arg, label } => {
                load_arg(arg, "rax", func, out);
                writeln!(out, "    testq %rax, %rax").unwrap();
                writeln!(out, "    je {label}").unwrap();
            }

            Op::JumpIfNotZero { arg, label } => {
                load_arg(arg, "rax", func, out);
                writeln!(out, "    testq %rax, %rax").unwrap();
                writeln!(out, "    jne {label}").unwrap();
            }
        }
    }

    if used_exit_label {
        writeln!(out, "    {}:", func.exit_label).unwrap();
    }
    if should_save_stack {
        writeln!(out, "    movq %rbp, %rsp").unwrap();
    }
    writeln!(out, "    popq %rbp").unwrap();
    writeln!(out, "    ret").unwrap();
    Ok(())
}

/// Generate an x86_64 assembly program from an IR program.
pub fn emit_program<W: std::io::Write>(
    program: &IRProgram,
    out: &mut W,
) -> Result<(), String> {
    if !program.string_literals.is_empty() {
        writeln!(out, ".section .rodata").unwrap();
        for (i, str_lit) in program.string_literals.iter().enumerate() {
            writeln!(out, ".STR{i}:").unwrap();
            writeln!(out, "    .string {str_lit:?}").unwrap();
        }
        write!(out, "\n").unwrap();
    }

    write!(out, ".text\n\n").unwrap();
    for (i, func) in program.functions.iter().enumerate() {
        if i != 0 {
            write!(out, "\n").unwrap();
        }
        emit_function(func, out)?;
    }
    Ok(())
}
