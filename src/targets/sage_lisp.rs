//! # C Target
//!
//! An implementation of the virtual machine for the C language.
//!
//! This allows the virtual machine to target C programs.
use std::fmt::format;

use super::{Architecture, CompiledTarget};
use crate::{
    side_effects::{Input, InputMode, Output, OutputMode},
    vm::{CoreOp, StandardOp},
};
use log::warn;

/// The type for the C target which implements the `Target` trait.
/// This allows the compiler to target the C language.
#[derive(Default)]
pub struct SageLisp(sage_lisp::Env);

impl SageLisp {
    pub fn new(env: sage_lisp::Env) -> Self {
        Self(env)
    }
}

impl Architecture for SageLisp {
    fn supports_input(&self, i: &Input) -> bool {
        true
    }

    fn supports_output(&self, o: &Output) -> bool {
        true
    }

    fn op(&mut self, op: &CoreOp) -> String {
        String::new()


    //     match op {
    //         CoreOp::Comment(text) => {
    //             format!("// {}", text.replace('\n', "\n// ").replace('\r', ""))
    //             // let mut comment = String::new();
    //             // for line in n.split('\n') {
    //             //     comment += &format!("// {}", line.trim());
    //             // }
    //             // comment
    //         }
    //         CoreOp::While => "while (scalar_reg.i) {".to_string(),
    //         CoreOp::If => "if (scalar_reg.i) {".to_string(),
    //         CoreOp::Else => "} else {".to_string(),
    //         CoreOp::Set(n) => {
    //             let mut tmp = format!("scalar_reg.i = {};", n[0]);
    //             // // If all the values in the vector are the same, use `memset`
    //             // if n.iter().all(|&x| x == n[0]) {
    //             //     // tmp += &format!("memset(vector_reg, {}, {} * sizeof(cell));", n[0], n.len());
    //             // } else {
    //             // }
    //             for (i, val) in n.iter().enumerate() {
    //                 tmp += &format!("vector_reg[{}].i = {};", i, val);
    //             }
    //             tmp
    //             // format!("scalar_reg.i = {};", n)
    //         }
    //         CoreOp::Call => "funs[scalar_reg.i]();".to_string(),
    //         CoreOp::Return => "return;".to_string(),
    //         CoreOp::Store(1) => "*ptr = scalar_reg;".to_string(),
    //         CoreOp::Load(1) => "scalar_reg = *ptr; vector_reg[0] = scalar_reg;".to_string(),
    //         CoreOp::Store(n) => {
    //             if *n > 1024 {
    //                 warn!("Store with n > 1024 not supported by C target");
    //             }
    //             format!("vector_reg[0] = scalar_reg; memcpy(ptr, vector_reg, {n} * sizeof(cell));")
    //         }
    //         CoreOp::Load(n) => {
    //             if *n > 1024 {
    //                 warn!("Load with n > 1024 not supported by C target");
    //             }
    //             format!("memcpy(vector_reg, ptr, {n} * sizeof(cell)); scalar_reg = ptr[0];")
    //         }

    //         CoreOp::Move(n) => format!("ptr += {};", n),
    //         CoreOp::Where => "scalar_reg.p = ptr;".to_string(),
    //         CoreOp::Deref => "refs[ref_ptr++] = ptr; ptr = ptr->p;".to_string(),
    //         CoreOp::Refer => "ptr = refs[--ref_ptr];".to_string(),
    //         CoreOp::Offset(n, 1) => format!("scalar_reg.p += {};", n),
    //         CoreOp::Offset(n, size) => format!("for (int i = 0; i < {size}; i++) vector_reg[i].p += {n};"),

    //         CoreOp::Index(1) => "scalar_reg.p += ptr->i;".to_string(),
    //         CoreOp::Index(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].p += ptr[i].i;"),

    //         CoreOp::Add(1) => "scalar_reg.i += ptr->i;".to_string(),
    //         CoreOp::Add(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i += ptr[i].i;"),

    //         CoreOp::Sub(1) => "scalar_reg.i -= ptr->i;".to_string(),
    //         CoreOp::Sub(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i -= ptr[i].i;"),

    //         CoreOp::Mul(1) => "scalar_reg.i *= ptr->i;".to_string(),
    //         CoreOp::Mul(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i *= ptr[i].i;"),

    //         CoreOp::Div(1) => "scalar_reg.i /= ptr->i;".to_string(),
    //         CoreOp::Div(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i /= ptr[i].i;"),

    //         CoreOp::Rem(1) => "scalar_reg.i %= ptr->i;".to_string(),
    //         CoreOp::Rem(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i %= ptr[i].i;"),

    //         CoreOp::Neg(1) => "scalar_reg.i = -scalar_reg.i;".to_string(),
    //         CoreOp::Neg(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = -vector_reg[i].i;"),

    //         CoreOp::Inc(1) => "scalar_reg.i++;".to_string(),
    //         CoreOp::Inc(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i++;"),

    //         CoreOp::Dec(1) => "scalar_reg.i--;".to_string(),
    //         CoreOp::Dec(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i--;"),

    //         CoreOp::Swap(1) => "tmp_reg = scalar_reg; scalar_reg = *ptr; *ptr = tmp_reg;".to_string(),
    //         CoreOp::Swap(n) => format!(
    //             "for (int i = 0; i < {n}; i++) {{ tmp_reg = vector_reg[i]; vector_reg[i] = ptr[i]; ptr[i] = tmp_reg; }}",
    //         ),

    //         CoreOp::And(1) => "scalar_reg.i = scalar_reg.i && ptr->i;".to_string(),
    //         CoreOp::And(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = vector_reg[i].i && ptr[i].i;"),

    //         CoreOp::Or(1) => "scalar_reg.i = scalar_reg.i || ptr->i;".to_string(),
    //         CoreOp::Or(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = vector_reg[i].i || ptr[i].i;"),

    //         CoreOp::Not(1) => "scalar_reg.i = !scalar_reg.i;".to_string(),
    //         CoreOp::Not(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = !vector_reg[i].i;"),

    //         CoreOp::BitwiseNand(1) => "scalar_reg.i = ~(scalar_reg.i & ptr->i);".to_string(),
    //         CoreOp::BitwiseNand(n) => format!(
    //             "for (int i = 0; i < {n}; i++) vector_reg[i].i = ~(vector_reg[i].i & ptr[i].i);",
    //         ),

    //         CoreOp::BitwiseAnd(1) => "scalar_reg.i &= ptr->i;".to_string(),
    //         CoreOp::BitwiseAnd(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i &= ptr[i].i;"),

    //         CoreOp::BitwiseOr(1) => "scalar_reg.i |= ptr->i;".to_string(),
    //         CoreOp::BitwiseOr(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i |= ptr[i].i;"),

    //         CoreOp::BitwiseXor(1) => "scalar_reg.i ^= ptr->i;".to_string(),
    //         CoreOp::BitwiseXor(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i ^= ptr[i].i;",),

    //         CoreOp::BitwiseNot(1) => "scalar_reg.i = ~scalar_reg.i;".to_string(),
    //         CoreOp::BitwiseNot(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = ~vector_reg[i].i;",),

    //         CoreOp::LeftShift(1) => "scalar_reg.i <<= ptr->i;".to_string(),
    //         CoreOp::LeftShift(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i <<= ptr[i].i;",),

    //         CoreOp::LogicalRightShift(1) => "scalar_reg.i = (unsigned long long)scalar_reg.i >> ptr->i;".to_string(),
    //         CoreOp::LogicalRightShift(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = (unsigned long long)vector_reg[i].i >> ptr[i].i;",),

    //         CoreOp::ArithmeticRightShift(1) => "scalar_reg.i >>= ptr->i;".to_string(),
    //         CoreOp::ArithmeticRightShift(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i >>= ptr[i].i;",),

    //         CoreOp::IsNonNegative(1) => "scalar_reg.i = scalar_reg.i >= 0;".to_string(),
    //         CoreOp::IsNonNegative(n) => format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = vector_reg[i].i >= 0;",),

    //         CoreOp::End | CoreOp::Function | CoreOp::Put(_) | CoreOp::Get(_) => {
    //             unreachable!("Invalid core op for C target")
    //         }
    //     }
    }

    fn std_op(&mut self, op: &StandardOp) -> Result<String, String> {
        Ok(String::new())
        // Ok(match op {
        //     StandardOp::Call(ffi) => format!("__{}();", ffi.name),
        //     StandardOp::Peek => self.peek()?,
        //     StandardOp::Poke => self.poke()?,
        //     StandardOp::Set(n) => {
        //         let mut tmp = format!("scalar_reg.f = {};", n[0]);
        //         for (i, val) in n.iter().enumerate() {
        //             tmp += &format!("vector_reg[{}].f = {};", i, val);
        //         }
        //         tmp
        //     }

        //     StandardOp::ToInt(1) => "scalar_reg.i = scalar_reg.f;".to_string(),
        //     StandardOp::ToInt(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = vector_reg[i].f;",)
        //     }

        //     StandardOp::ToFloat(1) => "scalar_reg.f = scalar_reg.i;".to_string(),
        //     StandardOp::ToFloat(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = vector_reg[i].i;",)
        //     }

        //     StandardOp::ACos(1) => "scalar_reg.f = acos(scalar_reg.f);".to_string(),
        //     StandardOp::ACos(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = acos(vector_reg[i].f);",)
        //     }

        //     StandardOp::ASin(1) => "scalar_reg.f = asin(scalar_reg.f);".to_string(),
        //     StandardOp::ASin(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = asin(vector_reg[i].f);",)
        //     }

        //     StandardOp::ATan(1) => "scalar_reg.f = atan(scalar_reg.f);".to_string(),
        //     StandardOp::ATan(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = atan(vector_reg[i].f);",)
        //     }

        //     StandardOp::Sin(1) => "scalar_reg.f = sin(scalar_reg.f);".to_string(),
        //     StandardOp::Sin(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = sin(vector_reg[i].f);")
        //     }

        //     StandardOp::Cos(1) => "scalar_reg.f = cos(scalar_reg.f);".to_string(),
        //     StandardOp::Cos(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = cos(vector_reg[i].f);")
        //     }

        //     StandardOp::Tan(1) => "scalar_reg.f = tan(scalar_reg.f);".to_string(),
        //     StandardOp::Tan(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f = tan(vector_reg[i].f);")
        //     }

        //     StandardOp::Add(1) => "scalar_reg.f += ptr->f;".to_string(),
        //     StandardOp::Add(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f += ptr[i].f;")
        //     }

        //     StandardOp::Sub(1) => "scalar_reg.f -= ptr->f;".to_string(),
        //     StandardOp::Sub(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f -= ptr[i].f;")
        //     }

        //     StandardOp::Mul(1) => "scalar_reg.f *= ptr->f;".to_string(),
        //     StandardOp::Mul(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f *= ptr[i].f;")
        //     }

        //     StandardOp::Div(1) => "scalar_reg.f /= ptr->f;".to_string(),
        //     StandardOp::Div(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].f /= ptr[i].f;",)
        //     }

        //     StandardOp::Rem(1) => "scalar_reg.f = fmod(scalar_reg.f, ptr->f);".to_string(),
        //     StandardOp::Rem(n) => format!(
        //         "for (int i = 0; i < {n}; i++) vector_reg[i].f = fmod(vector_reg[i].f, ptr[i].f);"
        //     ),

        //     StandardOp::Pow(1) => "scalar_reg.f = pow(scalar_reg.f, ptr->f);".to_string(),
        //     StandardOp::Pow(n) => format!(
        //         "for (int i = 0; i < {n}; i++) vector_reg[i].f = pow(vector_reg[i].f, ptr[i].f);"
        //     ),

        //     StandardOp::IsNonNegative(1) => "scalar_reg.i = scalar_reg.f >= 0;".to_string(),
        //     StandardOp::IsNonNegative(n) => {
        //         format!("for (int i = 0; i < {n}; i++) vector_reg[i].i = vector_reg[i].f >= 0;")
        //     }

        //     StandardOp::Alloc => {
        //         "scalar_reg.p = (cell*)malloc(scalar_reg.i * sizeof(cell));".to_string()
        //     }
        //     StandardOp::Free => "free(scalar_reg.p);".to_string(),
        //     _ => return Err(format!("Invalid standard op for C target {op:?}")),
        // })
    }

    fn end(&mut self, matching: &CoreOp, fun: Option<usize>) -> String {
        String::new()
        // match (matching, fun) {
        //     (CoreOp::Function | CoreOp::While | CoreOp::If | CoreOp::Else, _) => "}".to_string(),
        //     _ => unreachable!("Invalid matching op for end"),
        // }
    }

    fn declare_proc(&mut self, label_id: usize) -> String {
        // format!("void f{label_id}() {{")
        String::new()
    }

    fn name(&self) -> &str {
        "Sage Lisp"
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
            InputMode::StdinChar => Ok("tmp = getchar(); scalar_reg.i = tmp == EOF? 0 : tmp;".to_string()),
            InputMode::StdinInt => Ok("scanf(\"%ld\", &tmp_scalar_reg.i); scalar_reg = tmp_scalar_reg;".to_string()),
            InputMode::StdinFloat => Ok("scanf(\"%lf\", &tmp_scalar_reg.f); scalar_reg = tmp_scalar_reg;".to_string()),
            InputMode::Thermometer => Ok("scalar_reg.f = 293.15;".to_string()),
            InputMode::Clock => Ok("scalar_reg.i = time(NULL);".to_string()),
            InputMode::Random => Ok("scalar_reg.i = rand();".to_string()),
            InputMode::Button => Ok(format!(
                "printf(\"Button #{ch}: \"); scalar_reg.i = getchar() == 'y'; while (getchar() != 10);"
            )),
            _ => Err("Input not supported by this target".to_string()),
        }
    }

    fn put(&mut self, dst: &Output) -> Result<String, String> {
        match dst.mode {
            OutputMode::StdoutChar => Ok("putchar(scalar_reg.i);".to_string()),
            OutputMode::StdoutInt => Ok("printf(\"%lld\", scalar_reg.i);".to_string()),
            OutputMode::StdoutFloat => Ok("printf(\"%.1lf\", scalar_reg.f);".to_string()),
            OutputMode::StderrChar => Ok("fprintf(stderr, \"%c\", scalar_reg.i);".to_string()),
            OutputMode::StderrInt => Ok("fprintf(stderr, \"%lld\", scalar_reg.i);".to_string()),
            OutputMode::StderrFloat => Ok("fprintf(stderr, \"%.1lf\", scalar_reg.f);".to_string()),
            OutputMode::Heater => Ok("printf(\"Heating...\");".to_string()),
            OutputMode::Cooler => Ok("printf(\"Cooling...\");".to_string()),
            _ => Err("Output not supported by this target".to_string()),
        }
    }
    fn peek(&mut self) -> Result<String, String> {
        Ok("scalar_reg = *(ffi_ptr--); vector_reg[0] = scalar_reg;".to_string())
    }
    fn poke(&mut self) -> Result<String, String> {
        Ok("*(++ffi_ptr) = scalar_reg;".to_string())
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

cell tape[67108864], *refs[1024], *ptr = tape, **ref = refs, scalar_reg, vector_reg[1024], tmp_reg, ffi_channel[256], *ffi_ptr = ffi_channel;

unsigned int ref_ptr = 0;
void (*funs[10000])(void);

#if __has_include("ffi.h")
#include "ffi.h"
#endif

int tmp;
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

impl CompiledTarget for SageLisp {
    fn build_core(&mut self, program: &crate::vm::CoreProgram) -> Result<String, String> {
        // println!("COMPILING {}", program);
        // println!("In environment: {:#?}", self.0);
        // Ok(String::new())

        // Try to evaluate the `build-core` function from the environment on the input.
        let arg = sage_lisp::Expr::serialize(program);
        let call = sage_lisp::Expr::symbol("build-core").apply(&[arg.quote()]);

        let result = self.0.eval(call);

        Ok(result.to_string())
    }

    fn build_std(&mut self, program: &crate::vm::StandardProgram) -> Result<String, String> {
        // Try to evaluate the `build-core` function from the environment on the input.
        let arg = sage_lisp::Expr::serialize(program);
        let call = sage_lisp::Expr::symbol("build-std").apply(&[arg.quote()]);

        let result = self.0.eval(call);

        Ok(result.to_string())
    }
}
