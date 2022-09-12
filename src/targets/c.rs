//! An implementation of the virtual machine for the C language.
//!
//! This allows the virtual machine to target C programs.
use super::Target;
use crate::vm::{CoreOp, CoreProgram, StandardOp, StandardProgram};

pub struct C;

impl Target for C {
    fn build_core(&self, program: &CoreProgram) -> Result<String, String> {
        let CoreProgram(ops) = program;
        let mut result = String::from(
            r#"#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
typedef union int_or_float {
    long long int i;
    double f;
    union int_or_float *p;
} int_or_float;
int_or_float tape[200000], *ref_stack[1024], *ptr = tape, reg, tmp;
unsigned int ref_stack_ptr = 0;
void (*funs[10000])(void);
int main() {
"#,
        );

        let mut matching = vec![];
        let mut funs = vec![];
        let mut fun = 0;

        let tab = "\t";
        result += tab;
        result += "reg.i = 0;\n";

        let mut comment = String::new();
        let mut indent = 1;
        for op in ops {
            result += &tab.repeat(indent);
            result += &match op {
                CoreOp::Comment(n) => {
                    comment.clear();
                    for line in n.split('\n') {
                        comment += &format!("\n{}// {}", tab.repeat(indent), line.trim());
                    }
                    comment.clone()
                }
                CoreOp::Set(n) => format!("reg.i = {};", n),
                CoreOp::Function => {
                    matching.push(op);

                    funs.push(fun);
                    let fun_header = format!("void f{}() {{", fun);
                    fun += 1;

                    indent += 1;
                    fun_header
                }
                CoreOp::Call => "funs[reg.i]();".to_string(),
                CoreOp::Return => "return;".to_string(),

                CoreOp::While => {
                    matching.push(op);
                    indent += 1;

                    "while (reg.i) {".to_string()
                }
                CoreOp::If => {
                    matching.push(op);
                    indent += 1;

                    "if (reg.i) {".to_string()
                }
                CoreOp::Else => {
                    if let Some(CoreOp::If) = matching.pop() {
                        matching.push(op);
                        format!("\n{}}} else {{", tab.repeat(indent - 1))
                    } else {
                        return Err("Unexpected else".to_string());
                    }
                }
                CoreOp::End => {
                    indent -= 1;
                    match matching.pop() {
                        Some(CoreOp::Function) => format!(
                            "\n{}}} funs[{fun}] = f{fun};",
                            tab.repeat(indent),
                            fun = funs.pop().unwrap()
                        ),
                        Some(CoreOp::While) | Some(CoreOp::If) | Some(CoreOp::Else) => {
                            "}".to_string()
                        }
                        _ => "".to_string(),
                    }
                }

                CoreOp::Save => "*ptr = reg;".to_string(),
                CoreOp::Restore => "reg = *ptr;".to_string(),

                CoreOp::Move(n) => format!("ptr += {};", n),
                CoreOp::Where => "reg.p = ptr;".to_string(),
                CoreOp::Deref => "ref_stack[ref_stack_ptr++] = ptr; ptr = ptr->p;".to_string(),
                CoreOp::Refer => "ptr = ref_stack[--ref_stack_ptr];".to_string(),

                CoreOp::Index => "reg.p += ptr->i;".to_string(),
                CoreOp::BitwiseNand => "reg.i = ~(reg.i & ptr->i);".to_string(),

                CoreOp::Add => "reg.i += ptr->i;".to_string(),
                CoreOp::Sub => "reg.i -= ptr->i;".to_string(),
                CoreOp::Mul => "reg.i *= ptr->i;".to_string(),
                CoreOp::Div => "reg.i /= ptr->i;".to_string(),
                CoreOp::Rem => "reg.i %= ptr->i;".to_string(),
                CoreOp::IsNonNegative => "reg.i = reg.i >= 0;".to_string(),

                CoreOp::Get => "reg.i = (reg.i = getchar()) == EOF? -1 : reg.i;".to_string(),
                CoreOp::Put => "putchar(reg.i);".to_string(),
            };
            result.push('\n')
        }

        Ok(result + tab + "return 0;\n}")
    }

    fn build_std(&self, program: &StandardProgram) -> Result<String, String> {
        let StandardProgram(ops) = program;
        let mut result = String::from(
            r#"#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
typedef union int_or_float {
    long long int i;
    double f;
    union int_or_float *p;
} int_or_float;
int_or_float tape[200000], *ref_stack[1024], *ptr = tape, reg, tmp;
unsigned int ref_stack_ptr = 0;
void (*funs[10000])(void);
int main() {
"#,
        );
        let mut matching = vec![];
        let mut funs = vec![];
        let mut fun = 0;

        let tab = "\t";
        result += tab;
        result += "reg.i = 0;\n";

        let mut indent = 1;
        for op in ops {
            match op {
                StandardOp::Set(v) => {
                    result += &format!("{}reg.f = {:?};\n", tab.repeat(indent), v)
                }
                StandardOp::GetChar => {
                    result += &format!(
                        "{}reg.i = (reg.i = getchar()) == EOF? -1 : reg.i;\n",
                        tab.repeat(indent)
                    )
                }
                StandardOp::PutChar => {
                    result += &format!("{}putchar(reg.i);\n", tab.repeat(indent))
                }
                StandardOp::GetInt => {
                    result += &format!("{}scanf(\"%lld\", &reg.i);\n", tab.repeat(indent));
                }
                StandardOp::PutInt => {
                    result += &format!("{}printf(\"%lld\", reg.i);\n", tab.repeat(indent));
                }
                StandardOp::GetFloat => {
                    result += &format!("{}scanf(\"%lf\", &reg.f);\n", tab.repeat(indent));
                }
                StandardOp::PutFloat => {
                    result += &format!("{}printf(\"%lg\", reg.f);\n", tab.repeat(indent));
                }

                StandardOp::Add => {
                    result += &format!("{}reg.f += ptr->f;\n", tab.repeat(indent));
                }
                StandardOp::Sub => {
                    result += &format!("{}reg.f -= ptr->f;\n", tab.repeat(indent));
                }
                StandardOp::Mul => {
                    result += &format!("{}reg.f *= ptr->f;\n", tab.repeat(indent));
                }
                StandardOp::Div => {
                    result += &format!("{}reg.f /= ptr->f;\n", tab.repeat(indent));
                }
                StandardOp::Rem => {
                    result += &format!("{}reg.f = fmod(reg.f, ptr->f);\n", tab.repeat(indent));
                }
                StandardOp::Pow => {
                    result += &format!("{}reg.f = powf(reg.f, ptr->f);\n", tab.repeat(indent));
                }
                StandardOp::IsNonNegative => {
                    result += &format!("{}reg.i = reg.f >= 0;\n", tab.repeat(indent));
                }

                StandardOp::Sin => {
                    result += &format!("{}reg.f = sin(reg.f);\n", tab.repeat(indent));
                }
                StandardOp::Cos => {
                    result += &format!("{}reg.f = cos(reg.f);\n", tab.repeat(indent));
                }
                StandardOp::Tan => {
                    result += &format!("{}reg.f = tan(reg.f);\n", tab.repeat(indent));
                }
                StandardOp::ASin => {
                    result += &format!("{}reg.f = asin(reg.f);\n", tab.repeat(indent));
                }
                StandardOp::ACos => {
                    result += &format!("{}reg.f = acos(reg.f);\n", tab.repeat(indent));
                }
                StandardOp::ATan => {
                    result += &format!("{}reg.f = atan(reg.f);\n", tab.repeat(indent));
                }

                StandardOp::Alloc => {
                    result += &format!(
                        "{}reg.p = malloc(reg.i * sizeof(int_or_float));\n",
                        tab.repeat(indent)
                    );
                }
                StandardOp::Free => {
                    result += &format!("{}free(reg.p);\n", tab.repeat(indent));
                }

                StandardOp::ToInt => {
                    result += &format!("{}reg.i = (int)reg.f;\n", tab.repeat(indent));
                }
                StandardOp::ToFloat => {
                    result += &format!("{}reg.f = (double)reg.i;\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::Set(n)) => {
                    result += &format!("{}reg.i = {};\n", tab.repeat(indent), n);
                }
                StandardOp::CoreOp(CoreOp::Comment(n)) => {
                    for line in n.split('\n') {
                        result += &tab.repeat(indent);
                        result += "// ";
                        result += line;
                        result += "\n";
                    }
                }
                StandardOp::CoreOp(CoreOp::Function) => {
                    matching.push(CoreOp::Function);

                    funs.push(fun);
                    result += &format!("{}void f{}() {{\n", tab.repeat(indent), fun);
                    fun += 1;

                    indent += 1;
                }
                StandardOp::CoreOp(CoreOp::Call) => {
                    result += &format!("{}funs[reg.i]();\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Return) => {
                    result += &format!("{}return;\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::While) => {
                    matching.push(CoreOp::While);

                    result += &format!("{}while (reg.i) {{\n", tab.repeat(indent));
                    indent += 1;
                }
                StandardOp::CoreOp(CoreOp::If) => {
                    matching.push(CoreOp::If);

                    result += &format!("{}if (reg.i) {{\n", tab.repeat(indent));
                    indent += 1;
                }
                StandardOp::CoreOp(CoreOp::Else) => {
                    result += &format!("{}}} else {{\n", tab.repeat(indent - 1));
                }
                StandardOp::CoreOp(CoreOp::End) => {
                    indent -= 1;
                    match matching.pop() {
                        Some(CoreOp::Function) => {
                            result += &format!(
                                "{}}} funs[{fun}] = f{fun};\n",
                                tab.repeat(indent),
                                fun = funs.pop().unwrap()
                            );
                        }
                        Some(CoreOp::While) | Some(CoreOp::If) | Some(CoreOp::Else) => {
                            result += &format!("{}}}\n", tab.repeat(indent));
                        }
                        _ => {}
                    }
                }

                StandardOp::CoreOp(CoreOp::Save) => {
                    result += &format!("{}*ptr = reg;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Restore) => {
                    result += &format!("{}reg = *ptr;\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::Move(n)) => {
                    if *n >= 0 {
                        result += &format!("{}ptr += {};\n", tab.repeat(indent), n);
                    } else {
                        result += &format!("{}ptr -= {};\n", tab.repeat(indent), -n);
                    }
                }
                StandardOp::CoreOp(CoreOp::Where) => {
                    result += &format!("{}reg.i = (long long int)ptr;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Deref) => {
                    result += &format!(
                        "{}ref_stack[ref_stack_ptr++] = ptr; ptr = ptr->p;\n",
                        tab.repeat(indent)
                    );
                }
                StandardOp::CoreOp(CoreOp::Refer) => {
                    result += &format!("{}ptr = ref_stack[--ref_stack_ptr];\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::Index) => {
                    result += &format!("{}reg.p += ptr->i;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::BitwiseNand) => {
                    result += &format!("{}reg.i = ~(reg.i & ptr->i);\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::Add) => {
                    result += &format!("{}reg.i += ptr->i;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Sub) => {
                    result += &format!("{}reg.i -= ptr->i;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Mul) => {
                    result += &format!("{}reg.i *= ptr->i;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Div) => {
                    result += &format!("{}reg.i /= ptr->i;\n", tab.repeat(indent));
                }
                StandardOp::CoreOp(CoreOp::Rem) => {
                    result += &format!("{}reg.i %= ptr->i;\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::IsNonNegative) => {
                    result += &format!("{}reg.i = reg.i >= 0;\n", tab.repeat(indent));
                }

                StandardOp::CoreOp(CoreOp::Get) => {
                    result += &format!(
                        "{}reg.i = (reg.i = getchar()) == EOF? -1 : reg.i;\n",
                        tab.repeat(indent)
                    );
                }
                StandardOp::CoreOp(CoreOp::Put) => {
                    result += &format!("{}putchar(reg.i);\n", tab.repeat(indent));
                }
            }
        }

        Ok(result + tab + "return 0;\n}")
    }
}
