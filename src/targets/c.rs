//! An implementation of the virtual machine for the C language.
//! 
//! This allows the virtual machine to target C programs.
use crate::{CompilerTarget, vm::{CoreProgram, CoreOp, StandardProgram, StandardOp}};

pub struct C;

impl CompilerTarget for C {
    fn compile_core(&self, program: &CoreProgram) -> Result<String, String> {
        let CoreProgram(ops) = program;
        let mut result = String::from(r#"#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
typedef union {
    long long int i;
    double f;
} int_or_float;
int_or_float tape[30000], *ref_stack[30000], *ptr = tape, reg, tmp;
unsigned int ref_stack_ptr = 0;
void (*funs[10000])(void);
int main() {
"#);

        let mut matching = vec![];
        let mut funs = vec![];
        let mut fun = 0;

        let tab = "\t";
        result += tab;
        result += "reg.i = 0;\n";

        let mut indent = 1;
        for op in ops {
            match op {
                CoreOp::Comment(n) => {
                    for line in n.split('\n') {
                        result += &tab.repeat(indent);
                        result += "// ";
                        result += line;
                        result += "\n";
                    }
                },
                CoreOp::Constant(n) => {
                    result += &format!("{}reg.i = {};\n", tab.repeat(indent), n);
                },
                CoreOp::Function => {
                    matching.push(op);
                    
                    funs.push(fun);
                    result += &format!("{}void f{}() {{\n", tab.repeat(indent), fun);
                    fun += 1;
                    
                    indent += 1;
                },
                CoreOp::Call => {
                    result += &format!("{}funs[reg.i]();\n", tab.repeat(indent));
                    
                },
                CoreOp::Return => {
                    result += &format!("{}return;\n", tab.repeat(indent));
                },

                CoreOp::While => {
                    matching.push(op);
                    
                    result += &format!("{}while (reg.i) {{\n", tab.repeat(indent));
                    indent += 1;
                },
                CoreOp::If => {
                    matching.push(op);
                    
                    result += &format!("{}if (reg.i) {{\n", tab.repeat(indent));
                    indent += 1;
                },
                CoreOp::Else => {
                    if let Some(CoreOp::If) = matching.pop() {
                        result += &format!("{}}} else {{\n", tab.repeat(indent-1));
                        matching.push(op);
                    } else {
                        return Err("Unexpected else".to_string());
                    }
                },
                CoreOp::End => {
                    indent -= 1;
                    match matching.pop() {
                        Some(CoreOp::Function) => {
                            result += &format!("{}}} funs[{fun}] = f{fun};\n", tab.repeat(indent), fun=funs.pop().unwrap());
                        }
                        Some(CoreOp::While) | Some(CoreOp::If) | Some(CoreOp::Else) => {
                            result += &format!("{}}}\n", tab.repeat(indent));
                        }
                        _ => {}
                    }
                },

                CoreOp::Save => {
                    result += &format!("{}*ptr = reg;\n", tab.repeat(indent));
                },
                CoreOp::Restore => {
                    result += &format!("{}reg = *ptr;\n", tab.repeat(indent));
                },
                
                CoreOp::Move(n) => {
                    if *n >= 0 {
                        result += &format!("{}ptr += {};\n", tab.repeat(indent), n);
                    } else {
                        result += &format!("{}ptr -= {};\n", tab.repeat(indent), -n);
                    }
                },
                CoreOp::Where => {
                    result += &format!("{}reg.i = (long long int)ptr;\n", tab.repeat(indent));
                },
                CoreOp::Deref => {
                    result += &format!("{}ref_stack[ref_stack_ptr++] = ptr; ptr = (int_or_float*)ptr->i;\n", tab.repeat(indent));
                },
                CoreOp::Refer => {
                    result += &format!("{}ptr = ref_stack[--ref_stack_ptr];\n", tab.repeat(indent));
                },

                CoreOp::Inc => {
                    result += &format!("{}reg.i++;\n", tab.repeat(indent));
                },
                CoreOp::Dec => {
                    result += &format!("{}reg.i--;\n", tab.repeat(indent));
                },
                CoreOp::Add => {
                    result += &format!("{}reg.i += ptr->i;\n", tab.repeat(indent));
                },
                CoreOp::Sub => {
                    result += &format!("{}reg.i -= ptr->i;\n", tab.repeat(indent));
                },
                CoreOp::Mul => {
                    result += &format!("{}reg.i *= ptr->i;\n", tab.repeat(indent));
                },
                CoreOp::Div => {
                    result += &format!("{}reg.i /= ptr->i;\n", tab.repeat(indent));
                },
                CoreOp::Rem => {
                    result += &format!("{}reg.i %= ptr->i;\n", tab.repeat(indent));
                },

                CoreOp::IsWhole => {
                    result += &format!("{}reg.i = reg.i >= 0;\n", tab.repeat(indent));
                },

                CoreOp::GetChar => {
                    result += &format!("{}reg.i = (reg.i = getchar()) == EOF? -1 : reg.i;\n", tab.repeat(indent));
                },
                CoreOp::PutChar => {
                    // result += &format!("{}printf(\"%d\\n\", reg.i);\n", tab.repeat(indent));
                    result += &format!("{}putchar(reg.i);\n", tab.repeat(indent));
                },
            }
        }

        Ok(result + tab + "return 0;\n}")
    }

    fn compile_standard(&self, program: &StandardProgram) -> Result<String, String> {
        let StandardProgram(ops) = program;
        let mut result = String::from(r#"#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
typedef union {
    long long int i;
    double f;
} int_or_float;
int_or_float tape[30000], *ref_stack[30000], *ptr = tape, reg, tmp;
unsigned int ref_stack_ptr = 0;
void (*funs[10000])(void);
int main() {
"#);
        let mut matching = vec![];
        let mut funs = vec![];
        let mut fun = 0;

        let tab = "\t";
        result += tab;
        result += "reg.i = 0;\n";

        let mut indent = 1;
        for op in ops {
            match op {
                StandardOp::GetInt => {
                    result += &format!("{}scanf(\"%lld\", &reg.i);\n", tab.repeat(indent));
                },
                StandardOp::PutInt => {
                    result += &format!("{}printf(\"%lld\", reg.i);\n", tab.repeat(indent));
                },
                StandardOp::GetFloat => {
                    result += &format!("{}scanf(\"%f\", &reg.f);\n", tab.repeat(indent));
                },
                StandardOp::PutFloat => {
                    result += &format!("{}printf(\"%f\", reg.f);\n", tab.repeat(indent));
                },

                StandardOp::Swap => {
                    result += &format!("{}tmp = reg; reg = *ptr; *ptr = tmp;\n", tab.repeat(indent));
                },
            
                StandardOp::Add => {
                    result += &format!("{}reg.f += ptr->f;\n", tab.repeat(indent));
                },
                StandardOp::Sub => {
                    result += &format!("{}reg.f -= ptr->f;\n", tab.repeat(indent));
                },
                StandardOp::Mul => {
                    result += &format!("{}reg.f *= ptr->f;\n", tab.repeat(indent));
                },
                StandardOp::Div => {
                    result += &format!("{}reg.f /= ptr->f;\n", tab.repeat(indent));
                },
                StandardOp::Rem => {
                    result += &format!("{}reg.f = fmod(reg.f, ptr->f);\n", tab.repeat(indent));
                },
                StandardOp::IsWhole => {
                    result += &format!("{}reg.i = reg.f >= 0;\n", tab.repeat(indent));
                },
            
                StandardOp::Sin => {
                    result += &format!("{}reg.f = sin(reg.f);\n", tab.repeat(indent));
                },
                StandardOp::Cos => {
                    result += &format!("{}reg.f = cos(reg.f);\n", tab.repeat(indent));
                },
                StandardOp::Tan => {
                    result += &format!("{}reg.f = tan(reg.f);\n", tab.repeat(indent));
                },
                StandardOp::ASin => {
                    result += &format!("{}reg.f = asin(reg.f);\n", tab.repeat(indent));
                },
                StandardOp::ACos => {
                    result += &format!("{}reg.f = acos(reg.f);\n", tab.repeat(indent));
                },
                StandardOp::ATan => {
                    result += &format!("{}reg.f = atan(reg.f);\n", tab.repeat(indent));
                },
            
                StandardOp::Alloc => {
                    result += &format!("{}reg.i = malloc(reg.i * sizeof(int_or_float));\n", tab.repeat(indent));
                },
                StandardOp::Free => {
                    result += &format!("{}free(reg.i)\n", tab.repeat(indent));
                },
                
                StandardOp::ToInt => {
                    result += &format!("{}reg.i = (int)reg.f;\n", tab.repeat(indent));
                },
                StandardOp::ToFloat => {
                    result += &format!("{}reg.f = (double)reg.i;\n", tab.repeat(indent));
                },

                StandardOp::CoreOp(CoreOp::Constant(n)) => {
                    result += &format!("{}reg.i = {};\n", tab.repeat(indent), n);
                },
                StandardOp::CoreOp(CoreOp::Comment(n)) => {
                    for line in n.split('\n') {
                        result += &tab.repeat(indent);
                        result += "// ";
                        result += line;
                        result += "\n";
                    }
                },
                StandardOp::CoreOp(CoreOp::Function) => {
                    matching.push(CoreOp::Function);
                    
                    funs.push(fun);
                    result += &format!("{}void f{}() {{\n", tab.repeat(indent), fun);
                    fun += 1;
                    
                    indent += 1;
                },
                StandardOp::CoreOp(CoreOp::Call) => {
                    result += &format!("{}funs[reg.i]();\n", tab.repeat(indent));
                    
                },
                StandardOp::CoreOp(CoreOp::Return) => {
                    result += &format!("{}return;\n", tab.repeat(indent));
                },

                StandardOp::CoreOp(CoreOp::While) => {
                    matching.push(CoreOp::While);
                    
                    result += &format!("{}while (reg.i) {{\n", tab.repeat(indent));
                    indent += 1;
                },
                StandardOp::CoreOp(CoreOp::If) => {
                    matching.push(CoreOp::If);
                    
                    result += &format!("{}if (reg.i) {{\n", tab.repeat(indent));
                    indent += 1;
                },
                StandardOp::CoreOp(CoreOp::Else) => {
                    result += &format!("{}}} else {{\n", tab.repeat(indent-1));
                },
                StandardOp::CoreOp(CoreOp::End) => {
                    indent -= 1;
                    match matching.pop() {
                        Some(CoreOp::Function) => {
                            result += &format!("{}}} funs[{fun}] = f{fun};\n", tab.repeat(indent), fun=funs.pop().unwrap());
                        }
                        Some(CoreOp::While) | Some(CoreOp::If) | Some(CoreOp::Else) => {
                            result += &format!("{}}}\n", tab.repeat(indent));
                        }
                        _ => {}
                    }
                },

                StandardOp::CoreOp(CoreOp::Save) => {
                    result += &format!("{}*ptr = reg;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Restore) => {
                    result += &format!("{}reg = *ptr;\n", tab.repeat(indent));
                },
                
                StandardOp::CoreOp(CoreOp::Move(n)) => {
                    if *n >= 0 {
                        result += &format!("{}ptr += {};\n", tab.repeat(indent), n);
                    } else {
                        result += &format!("{}ptr -= {};\n", tab.repeat(indent), -n);
                    }
                },
                StandardOp::CoreOp(CoreOp::Where) => {
                    result += &format!("{}reg.i = (long long int)ptr;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Deref) => {
                    result += &format!("{}ref_stack[ref_stack_ptr++] = ptr; ptr = ptr->i;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Refer) => {
                    result += &format!("{}ptr = (int_or_float*)ref_stack[--ref_stack_ptr];\n", tab.repeat(indent));
                },

                StandardOp::CoreOp(CoreOp::Inc) => {
                    result += &format!("{}reg.i++;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Dec) => {
                    result += &format!("{}reg.i--;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Add) => {
                    result += &format!("{}reg.i += ptr->i;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Sub) => {
                    result += &format!("{}reg.i -= ptr->i;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Mul) => {
                    result += &format!("{}reg.i *= ptr->i;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Div) => {
                    result += &format!("{}reg.i /= ptr->i;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::Rem) => {
                    result += &format!("{}reg.i %= ptr->i;\n", tab.repeat(indent));
                },

                StandardOp::CoreOp(CoreOp::IsWhole) => {
                    result += &format!("{}reg.i = reg.i >= 0;\n", tab.repeat(indent));
                },

                StandardOp::CoreOp(CoreOp::GetChar) => {
                    result += &format!("{}reg.i = (reg.i = getchar()) == EOF? -1 : reg.i;\n", tab.repeat(indent));
                },
                StandardOp::CoreOp(CoreOp::PutChar) => {
                    result += &format!("{}putchar(reg.i);\n", tab.repeat(indent));
                },
            }
        }

        Ok(result + tab + "return 0;\n}")
    }
}