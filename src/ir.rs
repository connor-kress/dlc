use crate::lexer::Binop;
use std::fmt;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Arg {
    AutoVar(usize),
    Literal(u64),
    DataOffset(usize),
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Arg::AutoVar(i) => write!(f, "Auto({})", i),
            Arg::Literal(i) => write!(f, "{}", i),
            Arg::DataOffset(i) => write!(f, "DataOffset({})", i),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Op {
    Binop {
        binop: Binop,
        index: usize,
        lhs: Arg,
        rhs: Arg,
    },
    AutoAssign {
        index: usize,
        arg: Arg,
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
            Op::AutoAssign { index, arg } => {
                write!(f, "AutoAssign({}, {})", index, arg)
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

#[derive(Clone, Debug)]
pub struct IRFunction {
    pub name: String,
    pub body: Vec<Op>,
}

impl IRFunction {
    pub fn new(name: String, body: Vec<Op>) -> Self {
        Self { name, body }
    }
}

impl fmt::Display for IRFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "fn {}(", self.name)?;
        for (i, arg) in self.body.iter().enumerate() {
            writeln!(f, "    arg{}: {},", i, arg)?;
        }
        writeln!(f, ") {{")?;
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
