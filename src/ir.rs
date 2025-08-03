use crate::lexer::{Binop, Loc};
use std::fmt;

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct OpWithLoc {
    pub op: Op,
    pub loc: Loc,
}

impl fmt::Display for OpWithLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.op)
    }
}
