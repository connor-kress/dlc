use crate::{
    ast::{Function, IdWithLoc, ParamList, Statement, Type, TypeWithLoc},
    lexer::{Loc, PrimitiveType, Token, TokenWithLoc},
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

    fn at_end(&self) -> bool {
        self.current_token >= self.tokens.len()
    }

    fn peek_optional_token(&self) -> Option<TokenWithLoc> {
        if self.current_token >= self.tokens.len() {
            return None;
        }
        Some(self.tokens[self.current_token].clone())
    }

    fn peek_token(&self) -> Result<TokenWithLoc, String> {
        self.peek_optional_token()
            .ok_or_else(|| "Unexpected end of program".into())
    }

    fn get_token(&mut self) -> Result<TokenWithLoc, String> {
        let token = self.peek_token()?;
        self.current_token += 1;
        Ok(token)
    }

    fn advance(&mut self) -> Result<(), String> {
        if self.current_token >= self.tokens.len() {
            return Err("Unexpected end of program".into());
        }
        self.current_token += 1;
        Ok(())
    }

    fn expect_token(&mut self, token: Token) -> Result<TokenWithLoc, String> {
        let peek = self.peek_optional_token().ok_or_else(|| {
            format!("Expected {:?}, got end of program", token)
        })?;
        if peek.token != token {
            return Err(format!("Expected {:?}, got {:?}", token, peek));
        }
        self.get_token()
    }

    fn expect_id(&mut self) -> Result<IdWithLoc, String> {
        let peek = self.peek_optional_token().ok_or_else(|| {
            "Expected identifier, got end of program".to_string()
        })?;
        if let Token::Id(id) = peek.token {
            self.advance().unwrap();
            Ok(IdWithLoc { id, loc: peek.loc })
        } else {
            return Err(format!("Expected identifier, got {:?}", peek.token));
        }
    }
}

fn parse_type(p: &mut Parser) -> Result<TypeWithLoc, String> {
    let type_token = p
        .peek_optional_token()
        .ok_or_else(|| "Expected type, got end of program".to_string())?;
    let type_ = match type_token.token {
        Token::Type(ty) => Type::Primitive(ty),
        Token::Id(id) => Type::Id(id),
        _ => {
            return Err(format!("Expected type, got {:?}", type_token.token));
        }
    };
    p.advance().unwrap();
    Ok(TypeWithLoc::new(type_, type_token.loc))
}

fn parse_function(p: &mut Parser) -> Result<Function, String> {
    let fn_start = p.expect_token(Token::Fn)?.loc.start;
    let name = p.expect_id()?;
    p.expect_token(Token::Lparen)?;
    let mut params = Vec::<(IdWithLoc, TypeWithLoc)>::new();
    let mut first = true;
    while p.peek_token()?.token != Token::Rparen {
        if !first && p.peek_token().unwrap().token == Token::Comma {
            p.advance().unwrap();
        }
        if p.peek_token().unwrap().token == Token::Rparen {
            break;
        }
        first = false;
        let id = p.expect_id()?;
        p.expect_token(Token::Colon)?;
        let type_ = parse_type(p)?;
        params.push((id, type_));
    }
    p.expect_token(Token::Rparen)?;
    let peek = p.peek_token()?;
    let ret_type = match peek.token {
        Token::Op(op) => {
            if op != "->" {
                return Err(format!(
                    "Expected '->' or '{{' after parameter list, got {:?}",
                    Token::Op(op),
                ));
            }
            p.advance().unwrap();
            parse_type(p)?
        }
        Token::Lcurly => {
            TypeWithLoc::new(Type::Primitive(PrimitiveType::Void), peek.loc)
        }
        t => {
            return Err(format!(
                "Expected '->' or '{{' after parameter list, got {:?}",
                t,
            ));
        }
    };
    p.expect_token(Token::Lcurly)?;
    let body = Vec::<Statement>::new(); // TODO
    let fn_end = p.expect_token(Token::Rcurly)?.loc.end;
    Ok(Function {
        name,
        param_list: ParamList { params },
        ret_type,
        body: Box::new(Statement::Block(body)),
        loc: Loc::new(fn_start, fn_end),
    })
}

pub fn parse_program(
    tokens: Vec<TokenWithLoc>,
) -> Result<Vec<Function>, String> {
    let mut p = Parser::new(tokens);
    let mut functions = Vec::new();
    while !p.at_end() {
        let function = parse_function(&mut p)?;
        functions.push(function);
    }
    Ok(functions)
}
