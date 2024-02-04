//! # Sage-Build Target
//!
//! This uses an interpreted Sage program in order to compile the program.

use super::{Architecture, CompiledTarget};
use crate::{
    side_effects::{Input, InputMode, Output, OutputMode, FFIBinding},
    lir::*,
    asm::*,
    vm::*,
    parse::*,
};
use lazy_static::lazy_static;
use ::std::{collections::VecDeque, io::{Write, Read}, sync::RwLock, path::{Path, PathBuf}};
use log::*;

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone)]
pub enum BuildError {
    ParseBuildCode(String),
    ParseBuildConfig(String),
    CompileUserProgram(String),
}

#[derive(Debug, Clone, Deserialize)]
pub struct SageBuild {
    name: String,
    supports_std: bool,
    supports_floats: bool,
    version: String,
    call_stack_size: usize,
    compile_code: String,
    output_code: String,
}


impl SageBuild {
    pub fn from_package_config(file: &Path) -> Result<Self, BuildError> {
        info!("Reading Sage-Build target configuration from {}", file.display());
        let mut config: SageBuild = toml::from_str(&::std::fs::read_to_string(file).unwrap()).unwrap();
        // If there's a setup file, read it
        if let Ok(mut compile_file) = ::std::fs::File::open(config.compile_code.clone()) {
            info!("Reading setup code from {}", config.compile_code);
            let mut compile_code = String::new();
            compile_file.read_to_string(&mut compile_code).unwrap();
            config.compile_code = compile_code;
        }
        if let Ok(mut output_file) = ::std::fs::File::open(config.output_code.clone()) {
            info!("Reading output code from {}", config.output_code);
            let mut output_code = String::new();
            output_file.read_to_string(&mut output_code).unwrap();
            config.output_code = output_code;
        }
        Ok(config)
    }

    pub fn build(&self, code: &str, filename: Option<&str>) -> Result<(), BuildError> {
        info!("Building Sage program with Sage-Build target");
        let result = parse_frontend(code, filename).unwrap();
        match result.compile().unwrap() {
            Ok(core_asm) => {
                info!("Compiled user program to core assembly");
                let vm = core_asm.assemble(self.call_stack_size).unwrap();
                info!("Assembled user core assembly program to VM");
                self.build_core(vm)?;
            }
            Err(std_asm) => {
                info!("Compiled user program to standard assembly");
                let vm = std_asm.assemble(self.call_stack_size).unwrap();
                info!("Assembled user standard assembly program to VM");
                self.build_std(vm)?;
            }
        }
        Ok(())
    }

    pub fn build_core(&self, program: crate::vm::CoreProgram) -> Result<(), BuildError> {
        info!("Building Sage program with Sage-Build target");
        // let mut code = self.compile_code.clone() + "\n";
        // Show percent done at 10% intervals
        let code_len = program.len();
        let mut input = String::new();
        for (i, op) in program.into_iter().enumerate() {
            // Show percent done at 10% intervals
            if i % (code_len / 10) == 0 {
                info!("Building Sage program with Sage-Build target: {}%", (i as f64 / code_len as f64 * 100.0) as i64);
            }
            input += &build_core_op(op);
            input.push('\n');
        }
        // ::std::fs::write("code-gen.sg", &self.compile_code).unwrap();
        ::std::fs::write("input.txt", &input).unwrap();
        info!("Created build script for Sage-Build target: compiling {} opcodes", code_len);
        trace!("Created core builder script: {}", input);

        if let Some(output) = eval(&self.compile_code, input, SAGE_BUILD_BINDINGS.clone(), self.call_stack_size, "sage-build") {
            eval(&self.output_code, output, SAGE_BUILD_BINDINGS.clone(), self.call_stack_size, "sage-build").ok_or(())
                .map_err(|_| BuildError::ParseBuildCode("Failed to eval core build script (output)".to_string()))?;
            info!("Built user program with Sage-Build target");
            Ok(())
        } else {
            Err(BuildError::ParseBuildCode("Failed to eval core build program script".to_string()))
        }
    }

    pub fn build_std(&self, program: crate::vm::StandardProgram) -> Result<(), BuildError> {
        use crate::vm::StandardOp::*;

        let code_len = program.len();
        let mut input = String::new();
        for (i, op) in program.into_iter().enumerate() {
            // Show percent done at 10% intervals
            if i % (code_len / 10) == 0 {
                info!("Building Sage program with Sage-Build target: {}%", (i as f64 / code_len as f64 * 100.0) as i64);
            }
            input += &match op {
                Call(ffi) => format!("call {}", ffi.name),
                Peek => "peek".to_string(),
                Poke => "poke".to_string(),
                Set(n) => format!("setf {}", n),
                ToInt => ">int".to_string(),
                ToFloat => ">flt".to_string(),
                ACos => "acos".to_string(),
                ASin => "asin".to_string(),
                ATan => "atan".to_string(),
                Sin => "sin_".to_string(),
                Cos => "cos_".to_string(),
                Tan => "tan_".to_string(),
                Add => "addf".to_string(),
                Sub => "subf".to_string(),
                Mul => "mulf".to_string(),
                Div => "divf".to_string(),
                Rem => "remf".to_string(),
                Pow => "powf".to_string(),
                IsNonNegative => "gezf".to_string(),
                Alloc => "aloc".to_string(),
                Free => "free".to_string(),
                CoreOp(op) => build_core_op(op),
            };
            input.push('\n');
        }
        // code += &self.cleanup_code;
        ::std::fs::write("input.txt", &input).unwrap();
        info!("Created build script for Sage-Build target: compiling {} opcodes", code_len);
        trace!("Created standard builder script: {}", input);

        if let Some(output) = eval(&self.compile_code, input, SAGE_BUILD_BINDINGS.clone(), self.call_stack_size, "sage-build") {
            eval(&self.output_code, output, SAGE_BUILD_BINDINGS.clone(), self.call_stack_size, "sage-build").ok_or(())
                .map_err(|_| BuildError::ParseBuildCode("Failed to eval standard build script (output)".to_string()))?;
            info!("Built user program with Sage-Build target");
            Ok(())
        } else {
            Err(BuildError::ParseBuildCode("Failed to eval standard build program script".to_string()))
        }
    }
}


pub fn eval(
    code: impl ToString,
    input: impl ToString,
    ffi_bindings: Vec<(impl Into<FFIBinding>, fn(&mut VecDeque<i64>, &mut Vec<i64>))>,
    call_stack_size: usize,
    filename: impl ToString,
) -> Option<String> {
    let ffi_bindings: Vec<(FFIBinding, _)> = ffi_bindings.into_iter().map(|(binding, f)| {
        (binding.into(), f)
    }).collect();
    let result = match parse_frontend(code, Some(&filename.to_string())) {
        Ok(result) => result,
        Err(e) => {
            error!("Failed to parse code: {}", e);
            return Option::None;
        }
    };
    info!("Parsed code");
    let compiled = match result.compile() {
        Ok(compiled) => compiled,
        Err(e) => {
            error!("Failed to compile code: {}", e);
            return Option::None;
        }
    };
    info!("Compiled code");

    match compiled {
        Ok(core_asm) => {
            let vm = core_asm.assemble(call_stack_size).ok()?;
            info!("Assembled core assembly program to VM");
            let mut device = TestingDevice::new(input);
            for (binding, f) in ffi_bindings {
                device.add_binding(binding, f);
            }
            info!("Running core VM program...");
            let output_dev = CoreInterpreter::new(device).run(&vm).ok()?;
            Some(output_dev.output_str().to_string())
        }
        Err(std_asm) => {
            let vm = std_asm.assemble(call_stack_size).ok()?;
            info!("Assembled standard assembly program to VM");
            let mut device = TestingDevice::new(input);
            for (binding, f) in ffi_bindings {
                device.add_binding(binding, f);
            }
            
            info!("Running standard VM program...");
            let output_dev = StandardInterpreter::new(device).run(&vm).ok()?;
            Some(output_dev.output_str().to_string())
        }
    }
}

fn read_sage_string(tape: &Vec<i64>, ptr: usize) -> String {
    let mut result = String::new();
    let mut i = ptr;
    while tape[i] != 0 {
        result.push(tape[i] as u8 as char);
        i += 1;
    }
    result
}

fn write_sage_string(tape: &mut Vec<i64>, ptr: usize, s: &str) {
    let mut i = ptr;
    for c in s.chars() {
        tape[i] = c as i64;
        i += 1;
    }
    tape[i] = 0;
}

use crate::lir::Type::*;
lazy_static! {
    static ref SAGE_PACKAGE_DIR: RwLock<PathBuf> = RwLock::new(::std::env::current_dir().unwrap());
    static ref SAGE_BUILD_DIR: RwLock<PathBuf> = RwLock::new(::std::env::current_dir().unwrap().join("build"));

    static ref SAGE_BUILD_BINDINGS: Vec<(FFIBinding, fn(&mut VecDeque<i64>, &mut Vec<i64>))> = vec![
        (
            ("write", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
                Pointer(Mutability::Immutable, Char.into())
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let contents_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let contents = read_sage_string(tape, contents_ptr as usize);
                if let Ok(mut f) = ::std::fs::File::create(&filename) {
                    write!(&mut f, "{}", contents).unwrap();
                    channel.push_back(1);
                } else {
                    channel.push_back(0);
                }
            }
        ),
        (
            ("append", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
                Pointer(Mutability::Immutable, Char.into())
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let contents_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let contents = read_sage_string(tape, contents_ptr as usize);
                if let Ok(mut f) = ::std::fs::OpenOptions::new().append(true).open(&filename) {
                    write!(&mut f, "{}", contents).unwrap();
                    channel.push_back(1);
                } else {
                    channel.push_back(0);
                }
            }
        ),
        (
            ("read", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
                Pointer(Mutability::Immutable, Char.into())
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let contents_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                if let Some(contents) = ::std::fs::read_to_string(filename).ok() {
                    write_sage_string(tape, contents_ptr as usize, &contents);
                    channel.push_back(1);
                } else {
                    write_sage_string(tape, contents_ptr as usize, "");
                    channel.push_back(0);
                }
            }
        ),
        (
            ("copy", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
                Pointer(Mutability::Immutable, Char.into())
            ]), Int).into(),
            |channel, tape| {
                let src_ptr = channel.pop_front().unwrap();
                let dest_ptr = channel.pop_front().unwrap();
                let src = read_sage_string(tape, src_ptr as usize);
                let dest = read_sage_string(tape, dest_ptr as usize);
                let result = ::std::fs::copy(src, dest).is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            ("exists", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let result = ::std::fs::metadata(filename).is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            ("size", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let result = ::std::fs::metadata(filename).ok().map(|m| m.len()).unwrap_or(0);
                channel.push_back(result as i64);
            }
        ),
        (
            ("delete", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let result = ::std::fs::remove_file(filename).is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            ("list", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
                Pointer(Mutability::Immutable, Char.into())
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let contents_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                if let Ok(entries) = ::std::fs::read_dir(filename) {
                    let mut result = String::new();
                    let mut count = 0;
                    for entry in entries {
                        if let Ok(entry) = entry {
                            count += 1;
                            if let Ok(name) = entry.file_name().into_string() {
                                result += &name;
                                result.push('\n');
                            }
                        }
                    }
                    write_sage_string(tape, contents_ptr as usize, &result);
                    channel.push_back(count);
                } else {
                    write_sage_string(tape, contents_ptr as usize, "");
                    channel.push_back(0);
                }
            }
        ),
        (
            ("mkdir", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let result = ::std::fs::create_dir(filename).is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            ("rmdir", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let result = ::std::fs::remove_dir(filename).is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            ("chdir", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let filename_ptr = channel.pop_front().unwrap();
                let filename = read_sage_string(tape, filename_ptr as usize);
                let result = ::std::env::set_current_dir(filename).is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            ("get_cwd", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                if let Some(contents) = ::std::env::current_dir().ok().and_then(|p| p.into_os_string().into_string().ok()) {
                    write_sage_string(tape, contents_ptr as usize, &contents);
                    channel.push_back(1);
                } else {
                    write_sage_string(tape, contents_ptr as usize, "");
                    channel.push_back(0);
                }
            }
        ),
        (
            // Run a shell command
            ("system", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let command_ptr = channel.pop_front().unwrap();
                let command = read_sage_string(tape, command_ptr as usize);
                // Separate command by spaces
                let mut parts = command.split(' ');
                let command = parts.next().unwrap();
                let args: Vec<&str> = parts.collect();
                let result = ::std::process::Command::new(command).args(args).status().is_ok();
                channel.push_back(result as i64);
            }
        ),
        (
            // Get the OS name
            ("get_os", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), Int).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                write_sage_string(tape, contents_ptr as usize, &::std::env::consts::OS.to_string());
                channel.push_back(1);
            }
        ),
        (
            // Get the word size of the machine
            ("get_word_size", Tuple(vec![]), Int).into(),
            |channel, tape| {
                channel.push_back(::std::mem::size_of::<usize>() as i64);
            }
        ),
        (
            ("sleep", Tuple(vec![
                Int,
            ]), Int).into(),
            |channel, tape| {
                let duration = channel.pop_front().unwrap();
                ::std::thread::sleep(::std::time::Duration::from_secs(duration as u64));
            }
        ),
        (
            ("exit", Tuple(vec![
                Int,
            ]), None).into(),
            |channel, tape| {
                let code = channel.pop_front().unwrap();
                ::std::process::exit(code as i32);
            }
        ),
        (
            ("info", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), None).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                let contents = read_sage_string(tape, contents_ptr as usize);
                info!("{}", contents);
            }
        ),
        (
            ("warn", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), None).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                let contents = read_sage_string(tape, contents_ptr as usize);
                warn!("{}", contents);
            }
        ),
        (
            ("error", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), None).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                let contents = read_sage_string(tape, contents_ptr as usize);
                error!("{}", contents);
            }
        ),
        (
            ("debug", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), None).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                let contents = read_sage_string(tape, contents_ptr as usize);
                debug!("{}", contents);
            }
        ),
        (
            ("trace", Tuple(vec![
                Pointer(Mutability::Immutable, Char.into()),
            ]), None).into(),
            |channel, tape| {
                let contents_ptr = channel.pop_front().unwrap();
                let contents = read_sage_string(tape, contents_ptr as usize);
                trace!("{}", contents);
            }
        )
    ];
}

fn build_core_op(op: crate::vm::CoreOp) -> String {
    use crate::vm::CoreOp::*;
    match op {
        Comment(text) => format!("cmnt {}", text),
        Function => "func".to_string(),
        While => "loop".to_string(),
        If => "if__".to_string(),
        Else => "else".to_string(),
        Set(n) => format!("set_ {}", n),
        Call => "call".to_string(),
        Return => "ret_".to_string(),
        Save => "save".to_string(),
        Restore => "load".to_string(),
        Move(n) => format!("move {}", n),
        Where => "loc_".to_string(),
        Deref => "dref".to_string(),
        Refer => "ref_".to_string(),
        Index => "indx".to_string(),
        BitwiseNand => "nand".to_string(),
        Add => "add_".to_string(),
        Sub => "sub_".to_string(),
        Mul => "mul_".to_string(),
        Div => "div_".to_string(),
        Rem => "rem_".to_string(),
        End => "end_".to_string(),
        IsNonNegative => "gez_".to_string(),
        Get(input) => match input.mode {
                InputMode::StdinChar => "getc".to_string(),
                InputMode::StdinInt => "geti".to_string(),
                InputMode::StdinFloat => "getf".to_string(),
                _ => todo!(),
        },
        Put(output) => match output.mode {
            OutputMode::StdoutChar => "putc".to_string(),
            OutputMode::StdoutInt => "puti".to_string(),
            OutputMode::StdoutFloat => "putf".to_string(),
            _ => todo!(),
        }
    }
}