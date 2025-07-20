mod ast;
mod lexer;
use lexer::tokenize_string;

fn main() {
    let program_string = r#"
    fn main() {
        return 0;
    }
    "#;
    let tokens = tokenize_string(program_string);
    println!("{:?}", tokens);
}
