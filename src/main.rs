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
use std::process::{Command, Stdio};

#[allow(dead_code)]
static PARSING_TEST_PROGRAM: &str = r#"
fn foo(a: int64, b: int64, c: int64) -> int64 {
    let sum = a + 2;
    let res = sum + c;
    if (res) {
        res = res + 2*sum;
        res = res + 2*sum;
        if (res < 10) {
            print("Small");
        } else {
            print("Big");
        }
    } else {
        return 0;
    }
    return res;
}
"#;

#[allow(dead_code)]
static PROGRAM: &str = r#"
fn foo(a: int64, b: int64, c: int64) -> int64 {
    let res = a + 2;
    res += c;
    return res;
}

fn main(argc: int64, argv: **char) -> int64 {
    let x = 100;
    x -= 10*3;
    x /= 3;
    x *= 3;
    return x;
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

fn run_executable(exe_filename: &str) -> Result<(), String> {
    let exe_path = if exe_filename.contains('/') {
        exe_filename.to_string()
    } else {
        format!("./{}", exe_filename)
    };

    println!("Running: `{}`", exe_path);

    let status = Command::new(exe_path)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| format!("Failed to execute '{}': {}", exe_filename, e))?;

    println!();
    match status.code() {
        Some(0) => {
            println!("Program exited normally with code 0.");
        }
        Some(code) => {
            println!("Program exited with error code {}.", code);
        }
        None => {
            println!("Program terminated by signal.");
        }
    }
    Ok(())
}

fn main() -> Result<(), String> {
    // let tokens = tokenize_string(PARSING_TEST_PROGRAM)?;
    // println!("Parsing test...");
    // let parsing_test_functions = parse_program(tokens)?;
    // for function in parsing_test_functions.iter() {
    //     println!("{}", function);
    //     println!();
    // }

    let args = std::env::args().collect::<Vec<_>>();
    let should_run =
        args.len() >= 2 && matches!(args[1].as_str(), "-r" | "--run");
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

    if should_run {
        println!();
        run_executable(&exe_filename)?;
    }
    Ok(())
}
