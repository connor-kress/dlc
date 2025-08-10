use std::cmp::{max, min};

use crate::{
    ast::{
        Expr, ExprWithLoc, Function, IdWithLoc, ParamList, Statement,
        StatementWithLoc, Type, TypeWithLoc,
    },
    lexer::{Binop, Loc, PrimitiveType, Token, TokenWithLoc, Uniop},
};

static MAX_PRECEDENCE: usize = 14;

fn is_right_associative(precedence: usize) -> bool {
    match precedence {
        2 | 13 | 14 => true, // unary, ternary, and assignment
        _ => false,
    }
}

impl Binop {
    // https://en.cppreference.com/w/c/language/operator_precedence.html
    fn precedence(&self) -> usize {
        match self {
            Binop::Mul | Binop::Div => 3,
            Binop::Add | Binop::Sub => 4,
            Binop::Lt | Binop::Le | Binop::Gt | Binop::Ge => 6,
            Binop::Eq | Binop::Neq => 7,
            Binop::BitAnd => 8,
            Binop::BitXor => 9,
            Binop::BitOr => 10,
            Binop::Land => 11,
            Binop::Lor => 12,
            Binop::Assign
            | Binop::AssignAdd
            | Binop::AssignSub
            | Binop::AssignMul
            | Binop::AssignDiv => 14,
        }
    }

    fn is_right_associative(&self) -> bool {
        is_right_associative(self.precedence())
    }

    fn as_unary_ops(&self) -> (Option<Uniop>, Option<Uniop>) {
        match self {
            Binop::Add => (Some(Uniop::Plus), None),
            Binop::Sub => (Some(Uniop::Minus), None),
            Binop::Mul => (Some(Uniop::Deref), None),
            Binop::BitAnd => (Some(Uniop::Ref), None),
            Binop::Land => (Some(Uniop::Ref), Some(Uniop::Ref)),
            _ => (None, None),
        }
    }
}

impl Uniop {
    fn precedence(&self) -> usize {
        2
    }
}

struct Parser {
    tokens: Vec<TokenWithLoc>,
    token_index: usize,
}

impl Parser {
    fn new(tokens: Vec<TokenWithLoc>) -> Self {
        Self {
            tokens,
            token_index: 0,
        }
    }

    #[allow(dead_code)]
    fn debug_position(&self) {
        let lower = max(0, self.token_index - 5);
        let upper = min(self.token_index + 5, self.tokens.len() - 1);
        println!("Tokens:");
        for i in lower..upper {
            if i == self.token_index {
                println!("  > {:?}", self.tokens[i].token);
            } else {
                println!("    {:?}", self.tokens[i].token);
            }
        }
        println!();
    }

    #[allow(dead_code)]
    fn get_parse_point(&self) -> usize {
        self.token_index
    }

    #[allow(dead_code)]
    fn set_parse_point(&mut self, parse_point: usize) {
        assert!(parse_point < self.tokens.len());
        self.token_index = parse_point;
    }

    fn at_end(&self) -> bool {
        self.token_index >= self.tokens.len()
    }

    fn peek_optional_token(&self) -> Option<TokenWithLoc> {
        if self.token_index >= self.tokens.len() {
            return None;
        }
        Some(self.tokens[self.token_index].clone())
    }

    fn peek_token(&self) -> Result<TokenWithLoc, String> {
        self.peek_optional_token()
            .ok_or_else(|| "Unexpected end of program".into())
    }

    fn get_token(&mut self) -> Result<TokenWithLoc, String> {
        let token = self.peek_token()?;
        self.token_index += 1;
        Ok(token)
    }

    fn advance(&mut self) -> Result<(), String> {
        if self.token_index >= self.tokens.len() {
            return Err("Unexpected end of program".into());
        }
        self.token_index += 1;
        Ok(())
    }

    fn expect_token(&mut self, token: Token) -> Result<TokenWithLoc, String> {
        let peek = self.peek_optional_token().ok_or_else(|| {
            format!("Expected {:?}, got end of program", token)
        })?;
        if peek.token != token {
            return Err(format!("Expected {:?}, got {:?}", token, peek.token));
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

fn parse_numeric_literal(num_str: &str) -> Result<Expr, String> {
    if num_str.contains('.') {
        let num = num_str
            .parse::<f64>()
            .map_err(|_| format!("Invalid float literal: {}", num_str))?;
        Ok(Expr::FloatLit(num))
    } else {
        let num = num_str
            .parse::<i32>()
            .map_err(|_| format!("Invalid integer literal: {}", num_str))?;
        Ok(Expr::IntLit(num))
    }
}

fn parse_atomic_expression(p: &mut Parser) -> Result<ExprWithLoc, String> {
    let peek = p
        .peek_optional_token()
        .ok_or_else(|| "Expected expression, got end of program".to_string())?;
    let expr = match peek.token {
        Token::Id(id) => Expr::Id(IdWithLoc::new(id, peek.loc.clone())),
        Token::NumLit(i) => parse_numeric_literal(&i)?,
        Token::StrLit(s) => Expr::StrLit(s),
        _ => {
            return Err(format!("Expected expression, got {:?}", peek.token));
        }
    };
    p.advance().unwrap();
    Ok(ExprWithLoc::new(expr, peek.loc))
}

fn parse_arg_list(p: &mut Parser) -> Result<(Vec<ExprWithLoc>, Loc), String> {
    let start = p.expect_token(Token::Lparen)?.loc.start;
    let mut args = Vec::new();
    loop {
        if p.peek_token()?.token == Token::Rparen {
            break;
        }
        args.push(parse_expression(p)?);
        if p.peek_token()?.token == Token::Rparen {
            break;
        }
        p.expect_token(Token::Comma)?;
    }
    let end = p.expect_token(Token::Rparen)?.loc.end;
    Ok((args, Loc::new(start, end)))
}

fn parse_expression_at_precedence(
    p: &mut Parser,
    precedence: usize,
) -> Result<ExprWithLoc, String> {
    // println!("Parsing expression at precedence {}", precedence);
    // p.debug_position();
    let peek = p.peek_token()?;
    let unary_op = if let Token::Uniop(op) = peek.token {
        (Some(op), None)
    } else if let Token::Binop(op) = peek.token {
        op.as_unary_ops()
    } else {
        (None, None)
    };
    let mut acc = match unary_op {
        (Some(op), None) => {
            let start = p.get_token()?.loc.start;
            let right = parse_expression_at_precedence(p, op.precedence())?;
            let loc = Loc::new(start, right.loc.end);
            ExprWithLoc::new(
                Expr::Uniop {
                    op,
                    arg: Box::new(right),
                },
                loc,
            )
        }
        (Some(outer_op), Some(inner_op)) => {
            let combined_loc = p.get_token()?.loc;
            let outer_start = combined_loc.start;
            let inner_start = combined_loc.end;
            let right =
                parse_expression_at_precedence(p, inner_op.precedence())?;
            let inner_loc = Loc::new(inner_start, right.loc.end);
            let tmp_expr = ExprWithLoc::new(
                Expr::Uniop {
                    op: inner_op,
                    arg: Box::new(right),
                },
                inner_loc,
            );
            let outer_loc = Loc::new(outer_start, tmp_expr.loc.end);
            ExprWithLoc::new(
                Expr::Uniop {
                    op: outer_op,
                    arg: Box::new(tmp_expr),
                },
                outer_loc,
            )
        }
        _ => {
            if peek.token == Token::Lparen {
                p.expect_token(Token::Lparen)?;
                let expr = parse_expression(p)?;
                p.expect_token(Token::Rparen)?;
                expr
            } else {
                parse_atomic_expression(p)?
            }
        }
    };

    while let Some(next) = p.peek_optional_token() {
        match next.token {
            Token::Lparen => {
                let (args, loc) = parse_arg_list(p)?;
                let loc = Loc::new(acc.loc.start, loc.end);
                acc = ExprWithLoc::new(
                    Expr::FuncCall {
                        name: Box::new(acc),
                        args,
                    },
                    loc,
                );
            }
            Token::Binop(op) => {
                if op.precedence() > precedence {
                    break;
                }
                p.advance().unwrap();
                let next_precedence = if op.is_right_associative() {
                    op.precedence()
                } else {
                    op.precedence() - 1
                };
                let right = parse_expression_at_precedence(p, next_precedence)?;
                let loc = Loc::new(acc.loc.start, right.loc.end);
                acc = ExprWithLoc::new(
                    Expr::Binop {
                        op,
                        left: Box::new(acc),
                        right: Box::new(right),
                    },
                    loc,
                );
            }
            // Unary postfix ops will need to be handled before making a new
            // call to parse_expression as we need to operate on `acc` instead
            // of parsing a new expression
            // Token::Uniop(op) if op.is_postfix() => ...
            _ => break,
        }
    }
    Ok(acc)
}

fn parse_expression(p: &mut Parser) -> Result<ExprWithLoc, String> {
    parse_expression_at_precedence(p, MAX_PRECEDENCE)
}

fn parse_statement(p: &mut Parser) -> Result<StatementWithLoc, String> {
    let peek = p
        .peek_optional_token()
        .ok_or_else(|| "Expected statement, got end of program".to_string())?;
    let statement = match peek.token {
        Token::Let => {
            let start = p.expect_token(Token::Let)?.loc.start;
            let id = p.expect_id()?;
            let mut type_ = None;
            if p.peek_token()?.token == Token::Colon {
                p.advance().unwrap();
                type_ = Some(parse_type(p)?);
            }
            let mut expr = None;
            if p.peek_token()?.token == Token::Binop(Binop::Assign) {
                p.advance().unwrap();
                expr = Some(Box::new(parse_expression(p)?));
            }
            let end = p.expect_token(Token::Semi)?.loc.end;
            StatementWithLoc::new(
                Statement::VarDecl {
                    name: id,
                    type_,
                    val: expr,
                },
                Loc::new(start, end),
            )
        }
        Token::If => {
            let if_start = p.expect_token(Token::If)?.loc.start;
            p.expect_token(Token::Lparen)?;
            let cond = parse_expression(p)?;
            p.expect_token(Token::Rparen)?;
            p.expect_token(Token::Lcurly)?;
            let mut if_block = Vec::new();
            while p.peek_token()?.token != Token::Rcurly {
                let statement = parse_statement(p)?;
                if_block.push(statement);
            }
            let mut if_end = p.expect_token(Token::Rcurly)?.loc.end;
            let else_block = if p.peek_token()?.token == Token::Else {
                p.advance().unwrap();
                p.expect_token(Token::Lcurly)?;
                let mut else_block = Vec::new();
                while p.peek_token()?.token != Token::Rcurly {
                    let statement = parse_statement(p)?;
                    else_block.push(statement);
                }
                if_end = p.expect_token(Token::Rcurly)?.loc.end;
                Some(else_block)
            } else {
                None
            };
            let loc = Loc::new(if_start, if_end);
            StatementWithLoc::new(
                Statement::If {
                    cond: Box::new(cond),
                    if_block,
                    else_block,
                },
                loc,
            )
        }
        Token::For => {
            todo!("for loop statement")
        }
        Token::While => {
            todo!("while loop statement")
        }
        Token::Return => {
            let start = p.expect_token(Token::Return)?.loc.start;
            if p.peek_token()?.token == Token::Semi {
                let end = p.expect_token(Token::Semi)?.loc.end;
                StatementWithLoc::new(
                    Statement::Return { val: None },
                    Loc::new(start, end),
                )
            } else {
                let expr = parse_expression(p)?;
                let end = p.expect_token(Token::Semi)?.loc.end;
                StatementWithLoc::new(
                    Statement::Return {
                        val: Some(Box::new(expr)),
                    },
                    Loc::new(start, end),
                )
            }
        }
        _ => {
            let expr = parse_expression(p)?;
            let expr_start = expr.loc.start;
            let expr_end = p.expect_token(Token::Semi)?.loc.end;
            StatementWithLoc::new(
                Statement::Expr(expr),
                Loc::new(expr_start, expr_end),
            )
        }
    };
    Ok(statement)
}

fn parse_type(p: &mut Parser) -> Result<TypeWithLoc, String> {
    let type_token = p
        .peek_optional_token()
        .ok_or_else(|| "Expected type, got end of program".to_string())?;
    let mut end = type_token.loc.end;
    let mut should_advance = true;
    let type_ = match type_token.token {
        Token::Type(ty) => Type::Primitive(ty),
        Token::Id(id) => Type::Id(id),
        Token::Binop(Binop::Mul) => {
            should_advance = false;
            p.advance().unwrap();
            let inner = parse_type(p)?;
            end = inner.loc.end;
            Type::Ptr(Box::new(inner))
        }
        _ => {
            return Err(format!("Expected type, got {:?}", type_token.token));
        }
    };
    if should_advance {
        p.advance().unwrap();
    }
    Ok(TypeWithLoc::new(type_, Loc::new(type_token.loc.start, end)))
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
        Token::ThinArrow => {
            p.advance().unwrap();
            parse_type(p)?
        }
        Token::Lcurly => {
            TypeWithLoc::new(Type::Primitive(PrimitiveType::Void), peek.loc)
        }
        _ => {
            return Err(format!(
                "Expected '->' or '{{' after parameter list, got {:?}",
                peek.token,
            ));
        }
    };
    p.expect_token(Token::Lcurly)?;
    let mut body = Vec::new();
    while p.peek_token()?.token != Token::Rcurly {
        let statement = parse_statement(p)?;
        body.push(statement);
    }
    let fn_end = p.expect_token(Token::Rcurly)?.loc.end;
    Ok(Function {
        name,
        param_list: ParamList { params },
        ret_type,
        body,
        loc: Loc::new(fn_start, fn_end),
    })
}

#[allow(dead_code)]
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
