use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub struct Point {
    line: usize,
    col: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Loc {
    // file_name: String,
    start: Point,
    end: Point,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
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
    Semi,

    Type(PrimitiveType),
    Op(String),
    Id(String),

    // Keywords
    Fn,
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
#[derive(Debug, Clone)]
pub struct TokenWithLoc {
    pub token: Token,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub struct Lexer {
    data: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    pub fn new(data: &str) -> Self {
        let mut lexer = Lexer {
            data: data.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
        };
        lexer.skip_whitespace();
        lexer
    }

    fn get_char(&mut self) -> Option<char> {
        let Some(&c) = self.data.get(self.index) else {
            return None;
        };
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
        ("for", Token::For),
        ("if", Token::If),
        ("else", Token::Else),
        ("while", Token::While),
        ("return", Token::Return),
    ])
});

static PRIMITIVE_TYPES: LazyLock<HashMap<&'static str, PrimitiveType>> =
    LazyLock::new(|| {
        HashMap::from([
            ("int", PrimitiveType::Int),
            ("float", PrimitiveType::Float),
            ("bool", PrimitiveType::Bool),
            ("char", PrimitiveType::Char),
            ("void", PrimitiveType::Void),
        ])
    });

fn read_alphabetic(l: &mut Lexer) -> Token {
    let mut acc = l.get_char().unwrap().to_string();
    while l.peek_char().is_some() {
        let c = l.peek_char().unwrap();
        if c.is_alphanumeric() || c == '_' {
            acc.push(l.get_char().unwrap());
        } else {
            break;
        }
    }
    if let Some(token) = KEYWORDS.get(acc.as_str()) {
        token.clone()
    } else if let Some(&prim) = PRIMITIVE_TYPES.get(acc.as_str()) {
        Token::Type(prim)
    } else {
        Token::Id(acc)
    }
}

fn read_numeric_literal(l: &mut Lexer) -> Token {
    let mut acc = l.get_char().unwrap().to_string();
    while l.peek_char().is_some() {
        let c = l.peek_char().unwrap();
        if c.is_alphanumeric() || c == '_' || c == '.' {
            acc.push(l.get_char().unwrap());
        } else {
            break;
        }
    }
    Token::NumLit(acc)
}

fn read_string_literal(l: &mut Lexer) -> Token {
    let _ = l.get_char().unwrap();
    let mut acc = String::new();
    while l.peek_char().is_some() {
        let c = l.peek_char().unwrap();
        if c != '"' {
            acc.push(l.get_char().unwrap());
        } else {
            let _ = l.get_char().unwrap();
            break;
        }
    }
    Token::StrLit(acc)
}

fn read_operator(l: &mut Lexer) -> Token {
    let mut acc = l.get_char().unwrap().to_string();
    while l.peek_char().is_some() {
        let c = l.peek_char().unwrap();
        if OPERATOR_CHARS.contains(&c) {
            acc.push(l.get_char().unwrap());
        } else {
            break;
        }
    }
    Token::Op(acc)
}

fn get_next_token(l: &mut Lexer) -> Option<Token> {
    l.skip_whitespace();
    let next = l.peek_char()?;
    if let Some(token) = SINGLE_CHAR_TOKENS.get(&next) {
        l.get_char();
        Some(token.clone())
    } else if next.is_alphabetic() || next == '_' {
        Some(read_alphabetic(l))
    } else if next.is_numeric() {
        Some(read_numeric_literal(l))
    } else if next == '"' {
        Some(read_string_literal(l))
    } else {
        Some(read_operator(l))
    }
}

pub fn tokenize_string(s: &str) -> Vec<Token> {
    let mut l = Lexer::new(s);
    let mut tokens = Vec::new();
    while let Some(token) = get_next_token(&mut l) {
        tokens.push(token);
    }
    tokens
}
