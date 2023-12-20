//! # C Target
//!
//! An implementation of the virtual machine for the C language.
//!
//! This allows the virtual machine to target C programs.
use super::{Architecture, CompiledTarget};
use crate::{
    side_effects::{Input, InputMode, Output, OutputMode},
    vm::{CoreOp, StandardOp},
};

/// The type for the C target which implements the `Target` trait.
/// This allows the compiler to target the C language.
#[derive(Default)]
pub struct C;

impl Architecture for C {
    fn supports_input(&self, i: &Input) -> bool {
        matches!(
            i.mode,
            InputMode::StdinChar | InputMode::StdinFloat | InputMode::StdinInt
        )
    }

    fn supports_output(&self, o: &Output) -> bool {
        matches!(
            o.mode,
            OutputMode::StdoutChar | OutputMode::StdoutFloat | OutputMode::StdoutInt
        )
    }

    fn op(&mut self, op: &CoreOp) -> String {
        match op {
            CoreOp::Comment(text) => {
                format!("// {}", text.replace('\n', "\n// ").replace('\r', ""))
                // let mut comment = String::new();
                // for line in n.split('\n') {
                //     comment += &format!("// {}", line.trim());
                // }
                // comment
            }
            CoreOp::While => "while (reg.i) {".to_string(),
            CoreOp::If => "if (reg.i) {".to_string(),
            CoreOp::Else => "} else {".to_string(),
            CoreOp::Set(n) => format!("reg.i = {};", n),
            CoreOp::Call => "funs[reg.i]();".to_string(),
            CoreOp::Return => "return;".to_string(),
            CoreOp::Save => "*ptr = reg;".to_string(),
            CoreOp::Restore => "reg = *ptr;".to_string(),
            CoreOp::Move(n) => format!("ptr += {};", n),
            CoreOp::Where => "reg.p = ptr;".to_string(),
            CoreOp::Deref => "*ref++ = ptr; ptr = ptr->p;".to_string(),
            CoreOp::Refer => "ptr = *--ref;".to_string(),
            CoreOp::Index => "reg.p += ptr->i;".to_string(),
            CoreOp::BitwiseNand => "reg.i = ~(reg.i & ptr->i);".to_string(),
            CoreOp::Add => "reg.i += ptr->i;".to_string(),
            CoreOp::Sub => "reg.i -= ptr->i;".to_string(),
            CoreOp::Mul => "reg.i *= ptr->i;".to_string(),
            CoreOp::Div => "reg.i /= ptr->i;".to_string(),
            CoreOp::Rem => "reg.i %= ptr->i;".to_string(),
            CoreOp::IsNonNegative => "reg.i = reg.i >= 0;".to_string(),
            _ => unreachable!("Invalid op for C target {op:?}"),
        }
    }

    fn std_op(&mut self, op: &StandardOp) -> Result<String, String> {
        Ok(match op {
            StandardOp::Call(ffi) => format!("__{}();", ffi.name),
            StandardOp::Peek => self.peek()?,
            StandardOp::Poke => self.poke()?,
            StandardOp::Set(n) => format!("reg.f = {};", n),
            StandardOp::ToInt => "reg.i = reg.f;".to_string(),
            StandardOp::ToFloat => "reg.f = reg.i;".to_string(),
            StandardOp::ACos => "reg.f = acos(reg.f);".to_string(),
            StandardOp::ASin => "reg.f = asin(reg.f);".to_string(),
            StandardOp::ATan => "reg.f = atan(reg.f);".to_string(),
            StandardOp::Sin => "reg.f = sin(reg.f);".to_string(),
            StandardOp::Cos => "reg.f = cos(reg.f);".to_string(),
            StandardOp::Tan => "reg.f = tan(reg.f);".to_string(),
            StandardOp::Add => "reg.f += ptr->f;".to_string(),
            StandardOp::Sub => "reg.f -= ptr->f;".to_string(),
            StandardOp::Mul => "reg.f *= ptr->f;".to_string(),
            StandardOp::Div => "reg.f /= ptr->f;".to_string(),
            StandardOp::Rem => "reg.f = fmod(reg.f, ptr->f);".to_string(),
            StandardOp::Pow => "reg.f = pow(reg.f, ptr->f);".to_string(),
            StandardOp::IsNonNegative => "reg.i = reg.f >= 0;".to_string(),
            StandardOp::Alloc => "reg.p = malloc(reg.i * sizeof(reg));".to_string(),
            StandardOp::Free => "free(reg.p);".to_string(),
            _ => return Err(format!("Invalid standard op for C target {op:?}")),
        })
    }

    fn end(&mut self, matching: &CoreOp, fun: Option<usize>) -> String {
        match (matching, fun) {
            (CoreOp::Function | CoreOp::While | CoreOp::If | CoreOp::Else, _) => "}".to_string(),
            _ => unreachable!("Invalid matching op for end"),
        }
    }

    fn declare_proc(&mut self, label_id: usize) -> String {
        format!("void f{label_id}() {{")
    }

    fn name(&self) -> &str {
        "C"
    }
    fn version(&self) -> &str {
        "1.0"
    }

    fn supports_floats(&self) -> bool {
        true
    }

    fn get(&mut self, src: &Input) -> Result<String, String> {
        let ch = src.channel.0;
        match src.mode {
            InputMode::StdinChar => Ok("reg.i = getchar();".to_string()),
            InputMode::StdinInt => Ok("scanf(\"%ld\", &reg.i);".to_string()),
            InputMode::StdinFloat => Ok("scanf(\"%lf\", &reg.f);".to_string()),
            InputMode::Thermometer => Ok("reg.f = 293.15;".to_string()),
            InputMode::Clock => Ok("reg.i = time(NULL);".to_string()),
            InputMode::Random => Ok("reg.i = rand();".to_string()),
            InputMode::Button => Ok(format!(
                "printf(\"Button #{ch}: \"); reg.i = getchar() == 'y'; while (getchar() != 10);"
            )),
            _ => Err("Input not supported by this target".to_string()),
        }
    }

    fn put(&mut self, dst: &Output) -> Result<String, String> {
        match dst.mode {
            OutputMode::StdoutChar => Ok("putchar(reg.i);".to_string()),
            OutputMode::StdoutInt => Ok("printf(\"%ld\", reg.i);".to_string()),
            OutputMode::StdoutFloat => Ok("printf(\"%lf\", reg.f);".to_string()),
            OutputMode::StderrChar => Ok("fprintf(stderr, \"%c\", reg.i);".to_string()),
            OutputMode::StderrInt => Ok("fprintf(stderr, \"%lld\", reg.i);".to_string()),
            OutputMode::StderrFloat => Ok("fprintf(stderr, \"%lf\", reg.f);".to_string()),
            OutputMode::Heater => Ok("printf(\"Heating...\");".to_string()),
            OutputMode::Cooler => Ok("printf(\"Cooling...\");".to_string()),
            _ => Err("Output not supported by this target".to_string()),
        }
    }
    fn peek(&mut self) -> Result<String, String> {
        Ok("reg = *(ffi_ptr--);".to_string())
    }
    fn poke(&mut self) -> Result<String, String> {
        Ok("*(++ffi_ptr) = reg;".to_string())
    }
    fn prelude(&self, is_core: bool) -> Option<String> {
        let mut result = r#"#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

typedef union cell {
    int64_t i;
    double f;
    union cell *p;
} cell;

cell tape[200000], *refs[1024], *ptr = tape, **ref = refs, reg, ffi_channel[256], *ffi_ptr = ffi_channel;

unsigned int ref_ptr = 0;
void (*funs[10000])(void);
"#
        .to_string();

        if !is_core {
            result = "#include <stdlib.h>\n".to_string() + &result;
        }

        Some(result)
    }

    fn post_funs(&self, funs: Vec<i32>) -> Option<String> {
        let mut result = String::from("int main () {\n");
        for fun in funs {
            result += &format!("\tfuns[{fun}] = f{fun};\n", fun = fun)
        }
        Some(result)
    }

    fn postop(&self) -> Option<String> {
        Some("\n".to_string())
    }

    fn postlude(&self, _is_core: bool) -> Option<String> {
        Some("return 0;\n}".to_string())
    }
}

impl CompiledTarget for C {}
