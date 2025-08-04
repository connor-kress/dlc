mod ast;
mod codegen;
mod ir;
mod lexer;
mod parser;

use std::fs::File;
use std::process::Command;
// use lexer::tokenize_string;
// use parser::parse_program;

use crate::{
    codegen::generate_program,
    ir::{Arg, IRFunction, IRProgram, Op},
    lexer::Binop,
};

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
    // Use "cc" or "gcc" - "cc" is often a symlink to "gcc" or your default C compiler
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
    // let tokens = tokenize_string(PROGRAM)?;
    // let functions = parse_program(tokens)?;
    // for function in functions {
    //     println!("{}", function);
    // }

    let main = IRFunction::new(
        "main".to_string(),
        0, // arg_count
        3, // local_count
        vec![
            Op::LocalAssign {
                index: 0,
                arg: Arg::Literal(34),
            },
            Op::LocalAssign {
                index: 1,
                arg: Arg::Literal(35),
            },
            Op::Binop {
                binop: Binop::Add,
                index: 2,
                lhs: Arg::Local(0),
                rhs: Arg::Local(1),
            },
            Op::Return {
                arg: Some(Arg::Local(2)),
            },
        ],
    );
    let program = IRProgram::new(vec![main]);
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
