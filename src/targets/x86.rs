//! # x86 Target
//!
//! An implementation of the virtual machine for x86.
//!
//! This allows the virtual machine to target the x86 CPU architecture.
use super::{Architecture, CompiledTarget};
use crate::{
    io::{Input, InputMode, Output, OutputMode},
    vm::{CoreOp, StandardOp},
};

/// The type for the x86 target which implements the `Target` trait.
/// This allows the compiler to target the C language.
#[derive(Default)]
pub struct X86 {
    branch_count: usize,
    fun_count: usize,
    branch_match: Vec<String>,
    float_defs: Vec<f64>,
}



impl Architecture for X86 {
    fn supports_input(&self, i: &Input) -> bool {
        matches!(i.mode, InputMode::StdinChar)
    }

    fn supports_output(&self, o: &Output) -> bool {
        matches!(
            o.mode,
            OutputMode::StdoutChar | OutputMode::StdoutFloat | OutputMode::StdoutInt
        )
    }

    fn op(&mut self, op: &CoreOp) -> String {
        let branch_count = self.branch_count;
        let indent = self.indentation().unwrap_or("    ".to_string());
        match op {
            CoreOp::Comment(_) => {
                "".to_string()
            }
            CoreOp::While => {
                let result = format!("while{branch_count}:\n{indent}movq reg(%rip), %rax\n{indent}testq %rax, %rax\n{indent}je while_end{branch_count}");
                self.branch_match.push(format!("jmp while{branch_count}\nwhile_end{branch_count}"));
                self.branch_count += 1;
                result
            }
            CoreOp::If => {
                let result = format!("movq reg(%rip), %rax\n{indent}\n{indent}testq   %rax, %rax\n{indent}je if_end{branch_count}");
                self.branch_match.push(format!("if_end{branch_count}"));
                self.branch_count += 1;
                result
            }
            CoreOp::Else => {
                let label = self.branch_match.pop().unwrap();
                let result = format!("jmp else_end{branch_count}\n{label}:");
                self.branch_match.push(format!("else_end{branch_count}"));
                self.branch_count += 1;
                result
            }
            CoreOp::Set(n) => format!("movq ${n}, %rax\n{indent}movq %rax, reg(%rip)"),
            CoreOp::Move(n) => format!("movq ptr(%rip), %rax\n{indent}addq ${}, %rax\n{indent}movq %rax, ptr(%rip)\n", n * 8),
            CoreOp::Call => format!("movq reg(%rip), %rax\n{indent}movq funs(,%rax,8), %rax\n{indent}call    *%rax"),
            CoreOp::Return => format!("popq %rbp\n{indent}ret"),
            CoreOp::Save => format!("movq ptr(%rip), %rax\n{indent}movq reg(%rip), %rdx\n{indent}movq %rdx, (%rax)"),
            CoreOp::Restore => format!("movq ptr(%rip), %rax\n{indent}movq (%rax), %rdx\n{indent}movq %rdx, reg(%rip)"),
            CoreOp::Add => format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}addq %rdx, %rax\n{indent}movq %rax, reg(%rip)"),
            CoreOp::Sub => {
                format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}subq %rax, %rdx\n{indent}movq %rdx, reg(%rip)")
            }
            CoreOp::Mul => {
                format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}imulq   %rdx, %rax\n{indent}movq %rax, reg(%rip)")
            }
            CoreOp::Div => {
                format!("movq reg(%rip), %rax\n{indent}movq ptr(%rip), %rdx\n{indent}movq (%rdx), %rsi\n{indent}cqto\n{indent}idivq   %rsi\n{indent}movq %rax, reg(%rip)")
            }
            CoreOp::Rem => {
                format!("movq reg(%rip), %rax\n{indent}movq ptr(%rip), %rdx\n{indent}movq (%rdx), %rcx\n{indent}cqto\n{indent}idivq   %rcx\n{indent}movq %rdx, %rax\n{indent}movq %rax, reg(%rip)")
            }
            CoreOp::Deref => {
                format!("movq ptr(%rip), %rdx\n{indent}movq ref(%rip), %rax\n{indent}leaq 8(%rax), %rcx\n{indent}movq %rcx, ref(%rip)\n{indent}movq %rdx, (%rax)\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}movq %rax, ptr(%rip)")
            }
            CoreOp::Refer => {
                format!("movq ref(%rip), %rax\n{indent}subq $8, %rax\n{indent}movq %rax, ref(%rip)\n{indent}movq ref(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}movq %rax, ptr(%rip)")
            }
            CoreOp::Where => {
                format!("movq ptr(%rip), %rax\n{indent}movq %rax, reg(%rip)")
            }
            CoreOp::Index => {
                format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}salq $3, %rax\n{indent}addq %rdx, %rax\n{indent}movq %rax, reg(%rip)")
            }
            CoreOp::BitwiseNand => {
                format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}andq %rdx, %rax\n{indent}notq %rax\n{indent}movq %rax, reg(%rip)")
            }
            CoreOp::IsNonNegative => {
                format!("movq reg(%rip), %rax\n{indent}notq %rax\n{indent}shrq $63, %rax\n{indent}movzbl  %al, %eax\n{indent}movq %rax, reg(%rip)")
            }
            op => panic!("Unexpected op: {:?}", op),
        }
    }

    fn std_op(&mut self, std_op: &StandardOp) -> Result<String, String> {
        let branch_count = self.branch_count;
        let indent = self.indentation().unwrap_or("    ".to_string());
        Ok(match std_op {
            StandardOp::Set(n) => {
                self.float_defs.push(*n);
                format!("{indent}movsd float_const{float_def_count}(%rip), %xmm0\n{indent}movss   %xmm0, reg(%rip)\n", float_def_count = self.float_defs.len() - 1, indent = indent)
            }
            StandardOp::Alloc => {
                format!("{indent}movq reg(%rip), %rax
{indent}movq %rax, %rbx     # Move the value in rax to rbx for multiplication
{indent}leaq (%rbx,%rbx,8), %rbx    # Multiply by 8 to get the number of bytes needed
{indent}movq %rbx, %rdi      # Pass the number of bytes to allocate to rdi
{indent}call malloc          # Allocate memory on the heap
{indent}movq %rax, %rax      # Store the pointer to the allocated memory in rax
{indent}movq %rax, reg(%rip)
", indent = self.indentation().unwrap_or("    ".to_string()))
            }
            StandardOp::Free => {
                "movq reg(%rip), %rdi\ncall free".to_string()
            }
            StandardOp::CoreOp(op) => match op {
                CoreOp::Comment(_) => {
                    "".to_string()
                }
                CoreOp::While => {
                let result = format!("while{branch_count}:\n{indent}movq reg(%rip), %rax\n{indent}testq %rax, %rax\n{indent}je while_end{branch_count}");
                    self.branch_match.push(format!("jmp while{branch_count}\nwhile_end{branch_count}"));
                    self.branch_count += 1;
                    result
                }
                CoreOp::If => {
                let result = format!("movq reg(%rip), %rax\n{indent}\n{indent}testq   %rax, %rax\n{indent}je if_end{branch_count}");
                    self.branch_match.push(format!("if_end{branch_count}"));
                    self.branch_count += 1;
                    result
                }
                CoreOp::Else => {
                let label = self.branch_match.pop().unwrap();
                    let result = format!("jmp else_end{branch_count}\n{label}:");
                    self.branch_match.push(format!("else_end{branch_count}"));
                    self.branch_count += 1;
                    result
                }
                CoreOp::Set(n) => format!("movq ${n}, reg(%rip)"),
                CoreOp::Move(n) => format!("movq ptr(%rip), %rax\n{indent}addq ${}, %rax\n{indent}movq %rax, ptr(%rip)\n", n * 8),
                CoreOp::Call => format!("movq reg(%rip), %rax\n{indent}movq funs(,%rax,8), %rax\n{indent}call    *%rax"),
                CoreOp::Return => format!("popq %rbp\n{indent}ret"),
                CoreOp::Save => format!("movq ptr(%rip), %rax\n{indent}movq reg(%rip), %rdx\n{indent}movq %rdx, (%rax)"),
                CoreOp::Restore => format!("movq ptr(%rip), %rax\n{indent}movq (%rax), %rdx\n{indent}movq %rdx, reg(%rip)"),
                CoreOp::Add => format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}addq %rdx, %rax\n{indent}movq %rax, reg(%rip)"),
                CoreOp::Sub => {
                    format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}subq %rax, %rdx\n{indent}movq %rdx, reg(%rip)")
                }
                CoreOp::Mul => {
                    format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}imulq   %rdx, %rax\n{indent}movq %rax, reg(%rip)")
                }
                CoreOp::Div => {
                    format!("movq reg(%rip), %rax\n{indent}movq ptr(%rip), %rdx\n{indent}movq (%rdx), %rsi\n{indent}cqto\n{indent}idivq   %rsi\n{indent}movq %rax, reg(%rip)")
                }
                CoreOp::Rem => {
                    format!("movq reg(%rip), %rax\n{indent}movq ptr(%rip), %rdx\n{indent}movq (%rdx), %rcx\n{indent}cqto\n{indent}idivq   %rcx\n{indent}movq %rdx, %rax\n{indent}movq %rax, reg(%rip)")
                }
                CoreOp::Deref => {
                    format!("movq ptr(%rip), %rdx\n{indent}movq ref(%rip), %rax\n{indent}leaq 8(%rax), %rcx\n{indent}movq %rcx, ref(%rip)\n{indent}movq %rdx, (%rax)\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}movq %rax, ptr(%rip)")
                }
                CoreOp::Refer => {
                    format!("movq ref(%rip), %rax\n{indent}subq $8, %rax\n{indent}movq %rax, ref(%rip)\n{indent}movq ref(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}movq %rax, ptr(%rip)")
                }
                CoreOp::Where => {
                    format!("movq ptr(%rip), %rax\n{indent}movq %rax, reg(%rip)")
                }
                CoreOp::Index => {
                    format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}salq $3, %rax\n{indent}addq %rdx, %rax\n{indent}movq %rax, reg(%rip)")
                }
                CoreOp::BitwiseNand => {
                    format!("movq reg(%rip), %rdx\n{indent}movq ptr(%rip), %rax\n{indent}movq (%rax), %rax\n{indent}andq %rdx, %rax\n{indent}notq %rax\n{indent}movq %rax, reg(%rip)")
                }
                CoreOp::IsNonNegative => {
                    format!("movq reg(%rip), %rax\n{indent}notq %rax\n{indent}shrq $63, %rax\n{indent}movzbl  %al, %eax\n{indent}movq %rax, reg(%rip)")
                }
                op => Err(format!("Invalid standard op for x86 target {op:?}"))?
            },
            op => Err(format!("Invalid standard op for x86 target {op:?}"))?
        })
    }

    fn end(&mut self, matching: &CoreOp, _fun: Option<usize>) -> String {
        match matching {
            CoreOp::Function => {
                let label = self.branch_match.pop().unwrap();
                format!(
                    "popq %rbp\n{indent}ret\n{label}:\n",
                    indent = self.indentation().unwrap(),
                    label = label
                )
            }
            CoreOp::While | CoreOp::If | CoreOp::Else => {
                let label = self.branch_match.pop().unwrap();
                format!("{label}:")
            }
            otherwise => panic!("Unexpected end: {:?}", otherwise),
        }
    }

    fn declare_proc(&mut self, _label_id: usize) -> String {
        let result = format!("movq $fun{fun_count}, funs+{}(%rip)\n{indent}jmp fun_end{fun_count}\nfun{fun_count}:\n{indent}pushq   %rbp\n{indent}movq %rsp, %rbp\n", self.fun_count * 8, fun_count = self.fun_count, indent = self.indentation().unwrap());
        self.branch_match
            .push(format!("fun_end{fun_count}", fun_count = self.fun_count));
        self.fun_count += 1;
        result
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
        let indent = self.indentation().unwrap();
        if src.mode == InputMode::StdinChar && ch == 0 {
            Ok(format!("call getchar\n{indent}movq %rdi, %rax"))
        } else {
            Err("Output not supported by this target".to_string())
        }
    }
    fn put(&mut self, dst: &Output) -> Result<String, String> {
        let indent = self.indentation().unwrap();
        match dst.mode {
            OutputMode::StdoutChar => Ok(format!(
                "movq reg(%rip), %rax\n{indent}movl %eax, %edi\n{indent}call    putchar"
            )),
            // OutputMode::StdoutInt => Ok(format!("movq reg(%rip), %rax\n{indent}movq %rax, %xmm0\n{indent}movl $int_print, %edi\n{indent}movl $1, %eax\n{indent}call    printf", indent = self.indentation().unwrap())),
            OutputMode::StdoutInt => Ok(format!(
                "{indent}movq reg(%rip), %rax            # Load the integer value into rax
{indent}movq %rax, %rsi
{indent}movq $int_print, %rdi           # Load the address of the format string
{indent}movq $0, %rax                   # Clear rax (return value)
{indent}call printf                     # Call the printf function",
                indent = self.indentation().unwrap()
            )),
            OutputMode::StdoutFloat => Ok(format!(
                "{indent}movq reg(%rip), %xmm0            # Load the integer value into rax
subq $8, %rsp                   # Allocate space on the stack for the float argument
movsd %xmm0, (%rsp)   # Move the double value to the stack
movq $1, %rax       # File descriptor 1 represents standard output
movq $float_print, %rdi      # Load the address of the format string into RDI
movq (%rsp), %xmm0  # Load the double value into XMM0
call printf        # Call printf function
addq $8, %rsp       # Deallocate the space on the stack",
                indent = self.indentation().unwrap()
            )),
            _ => Err("Output not supported by this target".to_string()),
        }
    }
    fn peek(&mut self) -> Result<String, String> {
        // Ok("reg.i = *ptr;".to_string())
        todo!()
    }
    fn poke(&mut self) -> Result<String, String> {
        // Ok("*ptr = reg;".to_string())
        todo!()
    }
    fn prelude(&self, _is_core: bool) -> Option<String> {
        let result = ".text
.globl main
main:
";
        Some(result.to_string())
    }

    fn post_funs(&self, _funs: Vec<i32>) -> Option<String> {
        None
    }

    fn postop(&self) -> Option<String> {
        Some("\n".to_string())
    }

    fn postlude(&self, _is_core: bool) -> Option<String> {
        let mut data = ".data
tape:
        .zero   1600000
refs:
        .zero   8192
ptr:
        .quad   tape
ref:
        .quad   refs
reg:
        .zero   8
ref_ptr:
        .zero   4
funs:
        .zero   80000
float_print:
        .string      \"%g\"
int_print:
        .string      \"%ld\"
"
        .to_string();
        for (i, val) in self.float_defs.iter().enumerate() {
            data += format!("float_const{i}: .quad {val}\n").as_str();
        }
        Some(format!(
            "{indent}movl $0, %eax\n{indent}ret\n{data}",
            indent = self.indentation().unwrap()
        ))
    }
}

impl CompiledTarget for X86 {}
