use crate::lexer::Binop;
use std::fmt;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Arg {
    /// Local stack variable with stack offset (in bytes)
    Local(usize),
    /// Int literal with raw value
    Literal(u64),
    /// Byte offset in global data section (statics and globals)
    DataOffset(usize),
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::Local(i) => write!(f, "Local({})", i),
            Arg::Literal(i) => write!(f, "{}", i),
            Arg::DataOffset(i) => write!(f, "DataOffset({})", i),
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
    Binop {
        binop: Binop,
        index: usize,
        lhs: Arg,
        rhs: Arg,
    },
    Return {
        arg: Option<Arg>,
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
            } => write!(f, "Binop({:?}, {}, {}, {})", binop, index, lhs, rhs),
            Op::LocalAssign { index, arg } => {
                write!(f, "LocalAssign({}, {})", index, arg)
            }
            Op::Return { arg } => {
                if let Some(arg) = arg {
                    write!(f, "ret({})", arg)
                } else {
                    write!(f, "ret")
                }
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
}

impl IRFunction {
    pub fn new(
        name: String,
        arg_count: usize,
        local_count: usize,
        body: Vec<Op>,
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
        }
    }
}

impl IRFunction {
    pub fn slot_offset(&self, slow_index: usize) -> isize {
        -((slow_index + 1) as isize * 8)
    }
}

impl fmt::Display for IRFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} (stack={}):", self.name, self.stack_size)?;
        for op in &self.body {
            writeln!(f, "    {}", op)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub struct IRProgram {
    pub functions: Vec<IRFunction>,
    // pub data: Vec<u8>,
}

impl IRProgram {
    pub fn new(functions: Vec<IRFunction>) -> Self {
        Self {
            functions,
            // data: Vec::new(),
        }
    }
}

impl fmt::Display for IRProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, func) in self.functions.iter().enumerate() {
            if i != 0 {
                write!(f, "\n")?;
            }
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}
