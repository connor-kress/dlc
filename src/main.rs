mod ast;
mod lexer;
mod parser;

use lexer::tokenize_string;
use parser::parse_program;

#[allow(dead_code)]
static PROGRAM: &str = r#"
fn main(argc: int, argv: **char) -> int {
    let x = 5;
    let y: int = 6;
    let z: int = x + y;
    let p: *int = ptr_to(z);
    return 5 + 6*2 + foo(x, y);
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
    println!();
    let functions = parse_program(tokens)?;
    for function in functions {
        println!("{}", function);
    }
    Ok(())
}
