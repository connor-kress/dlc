mod ast;
mod codegen;
mod compiler;
mod ir;
mod lexer;
mod parser;

use codegen::generate_program;
use compiler::compile_program;
use lexer::tokenize_string;
use parser::parse_program;
use std::fs::File;
use std::process::Command;

#[allow(dead_code)]
static PROGRAM: &str = r#"
fn foo(a: int64, b: int64, c: int64) -> int64 {
    let sum = a + 2;
    let res = sum + c;
    return res;
}

fn main(argc: int64, argv: **char) -> int64 {
    let x = 34;
    let y = 35;
    let z = x + y;
    return z;
}
"#;

fn assemble_and_link_program(
    asm_filename: &str,
    obj_filename: &str,
    exe_filename: &str,
) -> Result<(), String> {
    println!("Assembling {} to {}...", asm_filename, obj_filename);
    let assemble_output = Command::new("as")
        .arg(asm_filename)
        .arg("-o")
        .arg(obj_filename)
        .output()
        .map_err(|e| format!("Failed to execute assembler ('as'): {}", e))?;

    if !assemble_output.status.success() {
        return Err(format!(
            "Assembly failed:\nStdout: {}\nStderr: {}",
            String::from_utf8_lossy(&assemble_output.stdout),
            String::from_utf8_lossy(&assemble_output.stderr)
        ));
    }
    println!("Assembled successfully.");

    println!("Linking {} to {}...", obj_filename, exe_filename);
    let link_output = Command::new("cc")
        .arg(obj_filename)
        .arg("-o")
        .arg(exe_filename)
        .output()
        .map_err(|e| format!("Failed to execute linker ('cc'): {}", e))?;

    if !link_output.status.success() {
        return Err(format!(
            "Linking failed:\nStdout: {}\nStderr: {}",
            String::from_utf8_lossy(&link_output.stdout),
            String::from_utf8_lossy(&link_output.stderr)
        ));
    }
    println!("Linked successfully.");
    Ok(())
}

fn main() -> Result<(), String> {
    let tokens = tokenize_string(PROGRAM)?;
    println!("Parsing program...");
    let functions = parse_program(tokens)?;
    for function in functions.iter() {
        println!("{}", function);
        println!();
    }

    println!("Compiling program...");
    let program = compile_program(&functions)?;

    let basename = "output";
    let asm_filename = format!("{}.s", basename);
    let obj_filename = format!("{}.o", basename);
    let exe_filename = format!("{}", basename);
    let mut file = File::create(&asm_filename)
        .map_err(|e| format!("Failed to create file: {}", e))?;
    generate_program(&program, &mut file)?;
    assemble_and_link_program(&asm_filename, &obj_filename, &exe_filename)?;
    Ok(())
}
