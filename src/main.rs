mod ast;
mod codegen;
mod compiler;
mod ir;
mod lexer;
mod parser;
mod stdlib;
mod type_checker;
mod typed_ast;
mod types;

use codegen::emit_program;
use compiler::compile_program;
use lexer::tokenize_string;
use parser::parse_module;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::ast::Program;
use crate::type_checker::check_program;

fn assemble_and_link_program(
    asm_filename: &Path,
    obj_filename: &Path,
    exe_filename: &Path,
) -> Result<(), String> {
    println!(
        "Assembling `{}` to `{}`...",
        asm_filename.display(),
        obj_filename.display()
    );
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

    println!(
        "Linking `{}` to `{}`...",
        obj_filename.display(),
        exe_filename.display()
    );
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

fn run_executable(exe_path: &Path) -> Result<(), String> {
    let run_target: PathBuf = if exe_path.components().count() == 1 {
        PathBuf::from(format!("./{}", exe_path.to_string_lossy()))
    } else {
        exe_path.to_path_buf()
    };

    println!("Running: `{}`", run_target.display());

    let status = Command::new(&run_target)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| {
            format!("Failed to execute '{}': {}", run_target.display(), e)
        })?;

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

struct CliOptions {
    should_run: bool,
    output_path: PathBuf,
    input_files: Vec<PathBuf>,
}

fn parse_cli_args() -> Result<CliOptions, String> {
    let args = std::env::args().collect::<Vec<_>>();

    let mut should_run = false;
    let mut output_path_opt = None;
    let mut input_files = Vec::<PathBuf>::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-r" | "--run" => {
                should_run = true;
                i += 1;
            }
            "-o" | "--output" => {
                if i + 1 >= args.len() {
                    return Err("Expected output name after -o".to_string());
                }
                output_path_opt = Some(PathBuf::from(args[i + 1].clone()));
                i += 2;
            }
            s if s.starts_with('-') => {
                return Err(format!("Unknown flag: {}", s));
            }
            _ => {
                input_files.push(PathBuf::from(&args[i]));
                i += 1;
            }
        }
    }

    if input_files.is_empty() {
        return Err(
            "No input file provided. Please specify a file to compile."
                .to_string(),
        );
    }
    if input_files.len() != 1 {
        return Err(format!(
            "Expected exactly 1 input file for now, got {}",
            input_files.len()
        ));
    }

    let input_path = &input_files[0];
    let output_path = if let Some(path) = output_path_opt {
        path
    } else {
        // Default to the input file's stem in the current directory
        let stem = input_path
            .file_stem()
            .unwrap_or_else(|| input_path.as_os_str())
            .to_os_string();
        PathBuf::from(stem)
    };

    Ok(CliOptions {
        should_run,
        output_path,
        input_files,
    })
}

fn main() -> Result<(), String> {
    let cli = parse_cli_args()?;

    let input_path = &cli.input_files[0];
    let mut program_source =
        std::fs::read_to_string(input_path).map_err(|e| {
            format!("Failed to read file '{}': {}", input_path.display(), e)
        })?;
    // TODO: This is a hack while the module system is not finished
    program_source.push_str(stdlib::STDLIB);

    println!("Parsing program...");
    let mut program = Program::new();
    for source_text in vec![program_source.as_str(), stdlib::STDLIB] {
        let tokens = tokenize_string(source_text)?;
        let module = parse_module(tokens)?;
        program.modules.push(module);
    }

    for (i, module) in program.modules.iter().enumerate() {
        println!("Module {}", i);
        for func in module.functions.iter() {
            println!("    fn {}(...);", func.name);
        }
        for extern_ in module.externs.iter() {
            println!("    extern fn {}(...);", extern_.name);
        }
        println!();
    }

    // println!("{}", &program);

    println!("Type checking program...");
    let _typed_program = check_program(&program)?;
    // println!("{typed_program}");

    println!("Compiling program...");
    // TODO: this should take in the typed program
    let ir_program = compile_program(&program)?;
    // println!("{ir_program}");

    let parent_dir = input_path.parent().unwrap_or_else(|| Path::new("."));
    let input_basename = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| input_path.to_string_lossy().to_string());
    let asm_filename = parent_dir.join(format!("{}.s", input_basename));
    let obj_filename = parent_dir.join(format!("{}.o", input_basename));
    let mut file = File::create(&asm_filename)
        .map_err(|e| format!("Failed to create file: {}", e))?;
    emit_program(&ir_program, &mut file)?;

    assemble_and_link_program(&asm_filename, &obj_filename, &cli.output_path)?;

    if cli.should_run {
        println!();
        run_executable(&cli.output_path)?;
    }
    Ok(())
}
