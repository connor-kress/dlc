use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub struct Point {
    pub line: usize,
    pub col: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Loc {
    // pub file_name: String,
    pub start: Point,
    pub end: Point,
}

impl Loc {
    pub fn new(start: Point, end: Point) -> Self {
        Self { start, end }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int8,
    Int32,
    Int64,
    Float,
    Bool,
    Void,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Lparen,
    Rparen,
    Lbrack,
    Rbrack,
    Lcurly,
    Rcurly,
    Comma,
    Colon,
    Semi,
    ThinArrow,

    Type(PrimitiveType),
    Binop(Binop),
    Uniop(Uniop),
    Id(String),

    // Keywords
    Fn,
    Let,
    For,
    If,
    Else,
    While,
    Return,

    // Literals
    StrLit(String),
    NumLit(String),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Uniop {
    Lnot,
    BitNot,
    Ref,
    Deref,
    Plus,
    Minus,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Binop {
    Assign,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Lor,
    Land,
    BitOr,
    BitAnd,
    BitXor,
    // In, // for iters
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct TokenWithLoc {
    pub token: Token,
    pub loc: Loc,
}

impl TokenWithLoc {
    pub fn new(token: Token, loc: Loc) -> Self {
        Self { token, loc }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer {
    data: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
    last_point: Point,
}

impl Lexer {
    pub fn new(data: &str) -> Self {
        Self {
            data: data.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
            last_point: Point { line: 1, col: 1 },
        }
    }

    fn get_char(&mut self) -> Option<char> {
        let Some(&c) = self.data.get(self.index) else {
            return None;
        };
        self.last_point = self.get_point();
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        self.index += 1;
        Some(c)
    }

    fn peek_char(&mut self) -> Option<char> {
        self.data.get(self.index).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.get_char();
            } else {
                break;
            }
        }
    }

    fn get_point(&self) -> Point {
        Point {
            line: self.line,
            col: self.col,
        }
    }

    fn get_last_point(&self) -> Point {
        self.last_point
    }

    fn get_char_with_point(&mut self) -> Option<(char, Point)> {
        let point = self.get_point();
        let c = self.get_char()?;
        Some((c, point))
    }
}

static SINGLE_CHAR_TOKENS: LazyLock<HashMap<char, Token>> =
    LazyLock::new(|| {
        HashMap::from([
            ('(', Token::Lparen),
            (')', Token::Rparen),
            ('[', Token::Lbrack),
            (']', Token::Rbrack),
            ('{', Token::Lcurly),
            ('}', Token::Rcurly),
            (',', Token::Comma),
            (':', Token::Colon),
            (';', Token::Semi),
        ])
    });

static OPERATOR_CHARS: LazyLock<HashSet<char>> = LazyLock::new(|| {
    HashSet::from([
        '+', '-', '*', '/', '%', '|', '&', '^', '~', '!', '=', '<', '>', '.',
    ])
});

static KEYWORDS: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    HashMap::from([
        ("fn", Token::Fn),
        ("let", Token::Let),
        ("for", Token::For),
        ("if", Token::If),
        ("else", Token::Else),
        ("while", Token::While),
        ("return", Token::Return),
    ])
});

static BINARY_OPS: LazyLock<HashMap<&'static str, Binop>> =
    LazyLock::new(|| {
        HashMap::from([
            ("+", Binop::Add),
            ("-", Binop::Sub),
            ("*", Binop::Mul),
            ("/", Binop::Div),
            ("||", Binop::Lor),
            ("&&", Binop::Land),
            ("|", Binop::BitOr),
            ("&", Binop::BitAnd),
            ("^", Binop::BitXor),
            ("=", Binop::Assign),
            ("<", Binop::Lt),
            ("<=", Binop::Le),
            (">", Binop::Gt),
            (">=", Binop::Ge),
            ("==", Binop::Eq),
            ("!=", Binop::Neq),
        ])
    });

static UNARY_OPS: LazyLock<HashMap<&'static str, Uniop>> =
    LazyLock::new(|| {
        HashMap::from([
            ("!", Uniop::Lnot),
            // ("+", Uniop::Plus), // will initially lex to binary ops
            // ("-", Uniop::Minus),
            ("~", Uniop::BitNot),
            ("&", Uniop::Ref),
            ("*", Uniop::Deref),
        ])
    });

static PRIMITIVE_TYPES: LazyLock<HashMap<&'static str, PrimitiveType>> =
    LazyLock::new(|| {
        HashMap::from([
            ("int", PrimitiveType::Int32),
            ("int8", PrimitiveType::Int8),
            ("int32", PrimitiveType::Int32),
            ("int64", PrimitiveType::Int64),
            ("char", PrimitiveType::Int8),
            ("float", PrimitiveType::Float),
            ("bool", PrimitiveType::Bool),
            ("void", PrimitiveType::Void),
        ])
    });

fn read_alphabetic(l: &mut Lexer) -> TokenWithLoc {
    let (c, start) = l.get_char_with_point().unwrap();
    let mut acc = c.to_string();
    let mut end = start;
    while let Some(c) = l.peek_char() {
        if c.is_alphanumeric() || c == '_' {
            acc.push(l.get_char().unwrap());
        } else {
            end = l.get_last_point();
            break;
        }
    }
    let token = if let Some(token) = KEYWORDS.get(acc.as_str()) {
        token.clone()
    } else if let Some(&prim) = PRIMITIVE_TYPES.get(acc.as_str()) {
        Token::Type(prim)
    } else {
        Token::Id(acc)
    };
    TokenWithLoc::new(token, Loc::new(start, end))
}

fn read_numeric_literal(l: &mut Lexer) -> TokenWithLoc {
    let (c, start) = l.get_char_with_point().unwrap();
    let mut acc = c.to_string();
    let mut end = start;
    while let Some(c) = l.peek_char() {
        if c.is_alphanumeric() || c == '_' || c == '.' {
            acc.push(l.get_char().unwrap());
        } else {
            end = l.get_last_point();
            break;
        }
    }
    TokenWithLoc::new(Token::NumLit(acc), Loc::new(start, end))
}

fn read_string_literal(l: &mut Lexer) -> TokenWithLoc {
    let (_, start) = l.get_char_with_point().unwrap();
    let mut acc = String::new();
    let mut end = start;
    while let Some(c) = l.peek_char() {
        if c != '"' {
            acc.push(l.get_char().unwrap());
        } else {
            (_, end) = l.get_char_with_point().unwrap();
            break;
        }
    }
    TokenWithLoc::new(Token::StrLit(acc), Loc::new(start, end))
}

fn convert_operator_string(s: &str) -> Result<Token, String> {
    let token = if s == "->" {
        Token::ThinArrow
    } else if let Some(token) = BINARY_OPS.get(s) {
        Token::Binop(*token)
    } else if let Some(token) = UNARY_OPS.get(s) {
        Token::Uniop(*token)
    } else {
        return Err(format!("Invalid operator: {}", s));
    };
    Ok(token)
}

fn read_operator(l: &mut Lexer) -> Result<TokenWithLoc, String> {
    let (c, start) = l.get_char_with_point().unwrap();
    let mut acc = c.to_string();
    let mut end = start;
    while let Some(c) = l.peek_char() {
        if OPERATOR_CHARS.contains(&c) {
            let mut next_acc = acc.clone();
            next_acc.push(c);
            if convert_operator_string(&acc).is_ok()
                && convert_operator_string(&next_acc).is_err()
            {
                end = l.get_last_point();
                break;
            }
            acc.push(l.get_char().unwrap());
        } else {
            end = l.get_last_point();
            break;
        }
    }
    let token = convert_operator_string(&acc)?;
    Ok(TokenWithLoc::new(token, Loc::new(start, end)))
}

fn get_next_token(l: &mut Lexer) -> Result<Option<TokenWithLoc>, String> {
    l.skip_whitespace();
    let Some(next) = l.peek_char() else {
        return Ok(None);
    };
    let token = if let Some(token) = SINGLE_CHAR_TOKENS.get(&next) {
        let (_, p) = l.get_char_with_point().unwrap();
        TokenWithLoc::new(token.clone(), Loc::new(p, p))
    } else if next.is_alphabetic() || next == '_' {
        read_alphabetic(l)
    } else if next.is_numeric() {
        read_numeric_literal(l)
    } else if next == '"' {
        read_string_literal(l)
    } else {
        read_operator(l)?
    };
    Ok(Some(token))
}

#[allow(dead_code)]
pub fn tokenize_string(s: &str) -> Result<Vec<TokenWithLoc>, String> {
    let mut l = Lexer::new(s);
    l.skip_whitespace();
    let mut tokens = Vec::new();
    while let Some(token) = get_next_token(&mut l)? {
        tokens.push(token);
    }
    Ok(tokens)
}
