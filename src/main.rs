mod ast;
mod lexer;
use lexer::tokenize_string;

// use crate::lexer::Token;

// struct Parser {
//     tokens: Vec<Token>,
//     current_token: usize,
// }

static PROGRAM: &str = r#"
fn main() {
    let x;
    x = "hello";
    return 0;
}
"#;

fn main() {
    let tokens = tokenize_string(PROGRAM);
    println!("{:?}", tokens);
}
