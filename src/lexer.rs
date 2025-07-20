use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::sync::LazyLock;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

fn read_alphabetic<It: Iterator<Item = char>>(
    data: &mut Peekable<It>,
) -> Token {
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if c.is_alphanumeric() || *c == '_' {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    if let Some(token) = KEYWORDS.get(acc.as_str()) {
        token.clone()
    } else if let Some(prim) = PRIMITIVE_TYPES.get(acc.as_str()) {
        Token::Type(prim.clone())
    } else {
        Token::Id(acc)
    }
}

fn read_numeric_literal<It: Iterator<Item = char>>(
    data: &mut Peekable<It>,
) -> Token {
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if c.is_alphanumeric() || *c == '_' || *c == '.' {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    Token::NumLit(acc)
}

fn read_string_literal<It: Iterator<Item = char>>(
    data: &mut Peekable<It>,
) -> Token {
    let _ = data.next().unwrap();
    let mut acc = String::new();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if *c != '"' {
            acc.push(data.next().unwrap());
        } else {
            let _ = data.next().unwrap();
            break;
        }
    }
    Token::StrLit(acc)
}

fn read_operator<It: Iterator<Item = char>>(data: &mut Peekable<It>) -> Token {
    let mut acc = data.next().unwrap().to_string();
    while data.peek().is_some() {
        let c = data.peek().unwrap();
        if OPERATOR_CHARS.contains(c) {
            acc.push(data.next().unwrap());
        } else {
            break;
        }
    }
    Token::Op(acc)
}

fn get_next_token<It: Iterator<Item = char>>(
    data: &mut Peekable<It>,
) -> Option<Token> {
    while data.peek()?.is_whitespace() {
        let _ = data.next();
    }
    let next = data.peek()?;
    if let Some(token) = SINGLE_CHAR_TOKENS.get(next) {
        let _ = data.next();
        Some(token.clone())
    } else if next.is_alphabetic() || *next == '_' {
        Some(read_alphabetic(data))
    } else if next.is_numeric() {
        Some(read_numeric_literal(data))
    } else if *next == '"' {
        Some(read_string_literal(data))
    } else {
        Some(read_operator(data))
    }
}

pub fn tokenize_string(s: &str) -> Vec<Token> {
    let mut data = s.chars().peekable();
    let mut tokens = Vec::new();
    while let Some(token) = get_next_token(&mut data) {
        tokens.push(token);
    }
    tokens
}
