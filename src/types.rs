use crate::lexer::Primative;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<(String, Type)>,
    pub is_variadic: bool,
    pub ret_type: Box<Type>,
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut params = String::new();
        for (i, (name, ty)) in self.params.iter().enumerate() {
            if i > 0 {
                params.push_str(", ");
            }
            params.push_str(&format!("{name}: {ty}"));
        }
        if self.is_variadic {
            params.push_str(", ...");
        }
        write!(f, "({params}) -> {ret}", ret = self.ret_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(Primative),
    Func(FuncType),
    Ptr(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(ty) => write!(f, "{ty:?}"),
            Type::Func(ty) => write!(f, "{ty}"),
            Type::Ptr(ty) => write!(f, "Ptr({ty})"),
        }
    }
}

#[allow(dead_code)]
impl Type {
    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Primitive(Primative::Bool))
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Type::Primitive(Primative::Void))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    pub fn is_fn_ptr(&self) -> bool {
        let Type::Ptr(ty) = self else {
            return false;
        };
        matches!(ty.as_ref(), Type::Func(_))
    }

    pub fn is_obj_ptr(&self) -> bool {
        let Type::Ptr(ty) = self else {
            return false;
        };
        !matches!(ty.as_ref(), Type::Func(_))
    }

    pub fn is_int(&self) -> bool {
        let Type::Primitive(prim) = self else {
            return false;
        };
        prim.is_int()
    }

    pub fn is_float(&self) -> bool {
        let Type::Primitive(prim) = self else {
            return false;
        };
        prim.is_float()
    }

    pub fn is_arithmetic(&self) -> bool {
        let Type::Primitive(prim) = self else {
            return false;
        };
        prim.is_arithmetic()
    }
}
