use crate::lexer::{Binop, Uniop};
use std::fmt::{self, Write};

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Arg {
    /// Local stack variable with stack offset (in bytes)
    Local(usize),
    /// Int literal with raw value
    Literal(i64),
    /// Data label into .data or .rodata section
    DataLabel(String),
    // Dereference a pointer
    Deref(Box<Arg>),
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::Local(i) => write!(f, "Local({i})"),
            Arg::Literal(i) => write!(f, "{i}"),
            Arg::DataLabel(s) => write!(f, "DataLabel({s:?})"),
            Arg::Deref(arg) => write!(f, "Deref({arg})"),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Op {
    LocalAssign {
        index: usize,
        arg: Arg,
    },
    Store {
        index: usize,
        arg: Arg,
    },
    Binop {
        binop: Binop,
        index: usize,
        lhs: Arg,
        rhs: Arg,
    },
    Uniop {
        uniop: Uniop,
        index: usize,
        arg: Arg,
    },
    Return {
        arg: Arg,
    },
    FuncCall {
        func: String,
        ret: usize,
        args: Vec<Arg>,
    },
    Label(String),
    Jump {
        label: String,
    },
    JumpIfZero {
        label: String,
        arg: Arg,
    },
    JumpIfNotZero {
        label: String,
        arg: Arg,
    },
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Binop {
                binop,
                index,
                lhs,
                rhs,
            } => write!(f, "Binop({binop:?}, {index}, {lhs}, {rhs})"),
            Op::Uniop { uniop, index, arg } => {
                write!(f, "Uniop({uniop:?}, {index}, {arg})")
            }
            Op::LocalAssign { index, arg } => {
                write!(f, "LocalAssign({index}, {arg})")
            }
            Op::Store { index, arg } => {
                write!(f, "Store({index}, {arg})")
            }
            Op::Return { arg } => {
                write!(f, "ret({arg})")
            }
            Op::FuncCall { func, args, ret } => {
                let mut args_str = String::new();
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        args_str.push_str(", ");
                    }
                    write!(&mut args_str, "{arg}")?;
                }
                write!(f, "Call({func}, {ret}, [{args_str}])")
            }
            Op::Label(label) => write!(f, "Label({label:?})"),
            Op::Jump { label } => write!(f, "Jump({label:?})"),
            Op::JumpIfZero { label, arg } => {
                write!(f, "JumpIfZero({label:?}, {arg})")
            }
            Op::JumpIfNotZero { label, arg } => {
                write!(f, "JumpIfNotZero({label:?}, {arg})")
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct IRFunction {
    pub name: String,
    pub arg_count: usize,
    pub local_count: usize,
    pub stack_size: usize,
    pub body: Vec<Op>,
    pub exit_label: String,
}

impl IRFunction {
    pub fn new(
        name: String,
        arg_count: usize,
        local_count: usize,
        body: Vec<Op>,
        exit_label: String,
    ) -> Self {
        let mut stack_size = (arg_count + local_count) * 8;
        if stack_size % 16 != 0 {
            stack_size += 8;
        }
        Self {
            name,
            arg_count,
            local_count,
            stack_size,
            body,
            exit_label,
        }
    }

    pub fn slot_offset(&self, slow_index: usize) -> isize {
        -((slow_index + 1) as isize * 8)
    }
}

impl fmt::Display for IRFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "{}(args={}, locals={}, stack_size={}):",
            self.name, self.arg_count, self.local_count, self.stack_size,
        )?;
        for op in &self.body {
            writeln!(f, "    {op}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
    pub string_literals: Vec<String>,
}

impl fmt::Display for IRProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, func) in self.functions.iter().enumerate() {
            if i != 0 {
                write!(f, "\n")?;
            }
            writeln!(f, "{func}")?;
        }
        Ok(())
    }
}
