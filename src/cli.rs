use clap::*;
use std::{fmt, fs::{read_to_string, write}};
use acid::{*, LOGO_WITH_COLOR, targets::{self, Target}, parse::*, lir::*, vm::*};

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
enum TargetType {
    Run,
    CoreASM,
    StdASM,
    CoreVM,
    StdVM,
    C,
}

#[derive(clap::ValueEnum, Clone, Copy, Debug)]
enum SourceType {
    LowIR,
    CoreASM,
    StdASM,
    CoreVM,
    StdVM,
}

#[derive(Parser, Debug)]
#[clap(author, version, about = LOGO_WITH_COLOR, long_about = Some(LOGO_WITH_COLOR))]
struct Args {
    /// The input file to compiler.
    #[clap(value_parser)]
    input: String,

    /// The file to write the output of the compiler to.
    #[clap(short, long, value_parser, default_value = "out")]
    output: String,

    /// The source language to compile.
    #[clap(short, value_parser, default_value = "low-ir")]
    source_type: SourceType,

    /// The target language to compile to.
    #[clap(short, value_parser, default_value = "run")]
    target_type: TargetType,

    /// The number of cells allocated for the call stack.
    #[clap(short, long, value_parser, default_value = "8192")]
    allocated_recursion_depth: usize,
}

enum Error {
    IO(std::io::Error),
    Parse(String),
    AsmError(asm::Error),
    LirError(lir::Error),
    InterpreterError(String),
    BuildError(String),
    InvalidSource(String),
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::IO(e) => write!(f, "IO error: {:?}", e),
            Error::Parse(e) => write!(f, "Parse error: {}", e),
            Error::AsmError(e) => write!(f, "Assembly error: {:?}", e),
            Error::LirError(e) => write!(f, "LIR error: {:?}", e),
            Error::InterpreterError(e) => write!(f, "Interpreter error: {}", e),
            Error::BuildError(e) => write!(f, "Build error: {}", e),
            Error::InvalidSource(e) => write!(f, "Invalid source: {}", e),
        }
    }
}

fn compile_source_to_vm(src: String, src_type: SourceType, rec_depth: usize) -> Result<Result<acid::vm::CoreProgram, acid::vm::StandardProgram>, Error> {
    match src_type {
        SourceType::StdVM => {
            parse_vm(src).map_err(Error::Parse)
        }
        SourceType::CoreVM => {
            match parse_vm(src) {
                Ok(Ok(prog)) => Ok(Ok(prog)),
                Ok(Err(_)) => Err(Error::InvalidSource("expected core VM program, got standard VM program".to_string())),
                Err(e) => Err(Error::Parse(e)),
            }
        },
        SourceType::StdASM => {
            match parse_asm(src).map_err(Error::Parse)? {
                Ok(prog) => Ok(Ok(prog.assemble(rec_depth).map_err(Error::AsmError)?)),
                Err(prog) => Ok(Err(prog.assemble(rec_depth).map_err(Error::AsmError)?)),
            }
        }
        SourceType::CoreASM => {
            match parse_asm(src).map_err(Error::Parse)? {
                Ok(prog) => Ok(Ok(prog.assemble(rec_depth).map_err(Error::AsmError)?)),
                Err(_) => Err(Error::InvalidSource("expected core assembly program, got standard assembly program".to_string())),
            }
        }
        SourceType::LowIR => {
            match parse_lir(src) {
                Ok(expr) => {
                    match expr.compile() {
                        Ok(Ok(asm_code)) => {
                            Ok(Ok(asm_code.assemble(rec_depth).map_err(Error::AsmError)?))
                        }
                        Ok(Err(asm_code)) => {
                            Ok(Err(asm_code.assemble(rec_depth).map_err(Error::AsmError)?))
                        }
                        Err(e) => {
                            Err(Error::LirError(e))
                        }
                    }
                }
                Err(e) => {
                    Err(Error::Parse(e))
                },
            }
        }
    }
}

fn compile_source_to_asm(src: String, src_type: SourceType) -> Result<Result<acid::asm::CoreProgram, acid::asm::StandardProgram>, Error> {
    match src_type {
        SourceType::StdASM => {
            parse_asm(src).map_err(Error::Parse)
        }
        SourceType::CoreASM => {
            match parse_asm(src).map_err(Error::Parse)? {
                Ok(prog) => Ok(Ok(prog)),
                Err(_) => Err(Error::InvalidSource("expected core assembly program, got standard assembly program".to_string())),
            }
        }
        SourceType::LowIR => {
            match parse_lir(src) {
                Ok(expr) => {
                    expr.compile().map_err(Error::LirError)
                }
                Err(e) => {
                    Err(Error::Parse(e))
                },
            }
        }
        SourceType::CoreVM | SourceType::StdVM => Err(Error::InvalidSource("cannot compile a core VM program to assembly".to_string())),
    }
}

fn compile(src: String, src_type: SourceType, target: TargetType, output: String, rec_depth: usize) -> Result<(), Error> {
    match target {
        TargetType::Run => {
            match compile_source_to_vm(src, src_type, rec_depth)? {
                Ok(vm_code) => {
                    CoreInterpreter::new(StandardDevice)
                        .run(&vm_code)
                        .map_err(Error::InterpreterError)?;
                }
                Err(vm_code) => {
                    StandardInterpreter::new(StandardDevice)
                        .run(&vm_code)
                        .map_err(Error::InterpreterError)?;
                }
            }
        }
        TargetType::C => {
            match compile_source_to_vm(src, src_type, rec_depth)? {
                Ok(vm_code) => {
                    write_file(format!("{output}.c"), targets::C.build_core(&vm_code.flatten()).map_err(Error::BuildError)?)?
                }
                Err(vm_code) => {
                    write_file(format!("{output}.c"), targets::C.build_std(&vm_code.flatten()).map_err(Error::BuildError)?)?
                }
            }
        }
        TargetType::CoreVM => {
            match compile_source_to_vm(src, src_type, rec_depth)? {
                Ok(vm_code) => {
                    write_file(format!("{output}.vm"), vm_code.flatten().to_string())?
                }
                Err(_) => {
                    return Err(Error::InvalidSource("expected core VM program, got standard VM program".to_string()));
                }
            }
        }
        TargetType::StdVM => {
            match compile_source_to_vm(src, src_type, rec_depth)? {
                Ok(vm_code) => {
                    write_file(format!("{output}.vm"), vm_code.flatten().to_string())?
                }
                Err(vm_code) => {
                    write_file(format!("{output}.vm"), vm_code.flatten().to_string())?
                }
            }
        }
        TargetType::CoreASM => {
            match compile_source_to_asm(src, src_type)? {
                Ok(asm_code) => {
                    write_file(format!("{output}.asm.lsd"), asm_code.to_string())?
                }
                Err(_) => {
                    return Err(Error::InvalidSource("expected core assembly program, got standard assembly program".to_string()))
                }
            }
        }
        TargetType::StdASM => {
            match compile_source_to_asm(src, src_type)? {
                Ok(asm_code) => {
                    write_file(format!("{output}.asm.lsd"), asm_code.to_string())?
                }
                Err(asm_code) => {
                    write_file(format!("{output}.asm.lsd"), asm_code.to_string())?
                }
            }
        }
    }
    Ok(())
}

fn write_file(file: String, contents: String) -> Result<(), Error> {
    write(file, contents).map_err(Error::IO)
}

fn read_file(name: &str) -> Result<String, Error> {
    read_to_string(name).map_err(Error::IO)
}

fn main() -> Result<(), Error> {
    let args = Args::parse();

    compile(
        read_file(&args.input)?,
        args.source_type,
        args.target_type,
        args.output,
        args.allocated_recursion_depth,
    )
}