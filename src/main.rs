mod ast;
mod lexer;
mod parser;

use lexer::tokenize_string;
use parser::parse_program;

#[allow(dead_code)]
static PROGRAM: &str = r#"
fn main(argc: int, argv: char) -> int {
    return 5 + 6*2;
}
"#;

fn main() -> Result<(), String> {
    let tokens = tokenize_string(PROGRAM)?;
    let raw_tokens = tokens
        .clone()
        .into_iter()
        .map(|t| t.token)
        .collect::<Vec<_>>();
    println!("{:?}", raw_tokens);
    let functions = parse_program(tokens)?;
    println!("{:?}", functions[0].body);
    Ok(())
}
