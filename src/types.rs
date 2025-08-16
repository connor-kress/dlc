use crate::lexer::PrimitiveType;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<(String, Type)>,
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
        write!(f, "({params}) -> {ret}", ret = self.ret_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
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
