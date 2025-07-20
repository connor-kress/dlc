mod ast;
mod lexer;
mod parser;

use lexer::tokenize_string;
use parser::parse_program;

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
fn main(argc: int, argv: char) -> int {}
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
