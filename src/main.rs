mod ast;
mod ir;
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
    let p: **int = &&z;
    if (1 + 2 == 3) {
        p = t = !1 + ~2;
        return p;
    } else {
        println("hello?");
    }
    return 5 + (z)*(z) - *&6**&2 + foo(x, y);
}
"#;

fn main() -> Result<(), String> {
    let tokens = tokenize_string(PROGRAM)?;
    let functions = parse_program(tokens)?;
    for function in functions {
        println!("{}", function);
    }
    Ok(())
}
