mod ast;
mod lexer;
use lexer::tokenize_string;

use crate::{
    ast::{Function, IdWithLoc, ParamList, Statement, Type, TypeWithLoc},
    lexer::{PrimitiveType, Token, TokenWithLoc},
};

struct Parser {
    tokens: Vec<TokenWithLoc>,
    current_token: usize,
}

impl Parser {
    fn new(tokens: Vec<TokenWithLoc>) -> Self {
        Self {
            tokens,
            current_token: 0,
        }
    }

    fn peek_optional_token(&self) -> Option<&TokenWithLoc> {
        if self.current_token >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.current_token])
    }

    fn peek_token(&self) -> Result<&TokenWithLoc, String> {
        self.peek_optional_token()
            .ok_or_else(|| "Unexpected end of program".into())
    }

    fn get_token(&mut self) -> Result<TokenWithLoc, String> {
        let token = self.peek_token()?.clone();
        self.current_token += 1;
        Ok(token)
    }

    fn expect_token(&mut self, token: Token) -> Result<TokenWithLoc, String> {
        let peek = self
            .peek_token()
            .map_err(|_| format!("Expected {:?}, got end of program", token))?;
        if peek.token != token {
            return Err(format!("Expected {:?}, got {:?}", token, peek));
        }
        self.get_token()
    }

    fn expect_id(&mut self) -> Result<IdWithLoc, String> {
        let peek = self
            .peek_token()
            .map_err(|_| "Expected identifier, got end of program".to_string())?
            .clone();
        if let Token::Id(id) = peek.token {
            self.get_token().unwrap();
            Ok(IdWithLoc { id, loc: peek.loc })
        } else {
            return Err(format!("Expected identifier, got {:?}", peek));
        }
    }
}

fn parse_function(parser: &mut Parser) -> Result<Function, String> {
    parser.expect_token(Token::Fn)?;
    let name = parser.expect_id()?;
    parser.expect_token(Token::Lparen)?;
    let params = Vec::<(IdWithLoc, TypeWithLoc)>::new(); // TODO
    parser.expect_token(Token::Rparen)?;
    let peek = parser.peek_token()?.clone();
    let ret_type = match peek.token {
        Token::Op(op) => {
            if op != "->" {
                return Err("Expected '->' or '{' after parameter list".into());
            }
            parser.get_token().unwrap();
            let type_token = parser.peek_token()?.clone();
            let ret_type = match type_token.token {
                Token::Type(ty) => Type::Primitive(ty),
                Token::Id(id) => Type::Id(id.clone()),
                _ => {
                    return Err("Expected type after '->'".into());
                }
            };
            parser.get_token().unwrap();
            TypeWithLoc::new(ret_type, type_token.loc)
        }
        Token::Lcurly => {
            TypeWithLoc::new(Type::Primitive(PrimitiveType::Void), peek.loc)
        }
        _ => {
            return Err("Expected '->' or '{' after parameter list".into());
        }
    };
    parser.expect_token(Token::Lcurly)?;
    let body = Vec::<Statement>::new(); // TODO
    parser.expect_token(Token::Rcurly)?;
    Ok(Function {
        name,
        param_list: ParamList { params },
        ret_type,
        body: Box::new(Statement::Block(body)),
    })
}

fn parse_program(tokens: Vec<TokenWithLoc>) -> Result<Vec<Function>, String> {
    let mut parser = Parser::new(tokens);
    let mut functions = Vec::new();
    while parser.current_token < parser.tokens.len() {
        let function = parse_function(&mut parser)?;
        functions.push(function);
    }
    Ok(functions)
}

#[allow(dead_code)]
static PROGRAM: &str = r#"
fn main(argc: int, argv: char) -> int {
    let x;
    x = "hello";
    return 0;
}
"#;

#[allow(dead_code)]
static PROGRAM_FUNCTION_HEADER: &str = r#"
fn main() -> int {
}
"#;

fn main() -> Result<(), String> {
    let tokens = tokenize_string(PROGRAM_FUNCTION_HEADER);
    let raw_tokens = tokens
        .clone()
        .into_iter()
        .map(|t| t.token)
        .collect::<Vec<_>>();
    println!("{:?}", raw_tokens);
    let functions = parse_program(tokens)?;
    println!("{:?}", functions);
    Ok(())
}
