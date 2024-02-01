//! # Targets Module
//!
//! This module contains the code dealing with building the virtual
//! machine code to various targets, such as C source code.
//!
//! ## Current Structure
//!
//! Right now, this module is a bit empty, only implementing C (GCC only)
//! as a compiler target. This is due to the fact that it has been much
//! simpler to build the language on top of the virtual machine when there
//! are fewer implementations to change.
//!
//! ## Future Structure
//!
//! In the future, this module will be *much* more featured.
//! The frontend will use `Put` and `Get` to interact with the
//! virtual machine's I/O device, and this module will provide hooks
//! to add "system calls" to implement actual hardware-specific behaviors
//! using these instructions, **but with portability in mind**.
//!
//! Consider the `fork` system call on Unix systems. A backend might
//! implement a `fork` frontend for this virtual machine by executing
//! `fork` when a correct series of `Put` values are given to the I/O device.
//!
//! Other backends might not implement `fork` though, and so "catch all"
//! code can be implemented to *allow a backend to compile **as if** `fork`
//! is provided*. This might work by providing some frontend code which takes
//! two functions and runs them serially instead of in parallel, but still
//! accomplishing a simulated `fork`'s state.
//!
//! Other hardware specific instructions can be implemented this way very
//! nicely. Consider a VGA device which displays a screen. A hardware
//! specific implementation can be written for each supported target,
//! and a "catch all" implementation can work by trying to draw the screen
//! as ASCII/UNICODE with `PutChar. A hardware specific implementation may
//! also *choose* to fail under unsupported targets to prevent use where
//! not intended.

pub mod c;
pub use c::*;

pub mod sage_os;
pub use sage_os::*;

pub mod x86;
pub use x86::*;

pub mod flipperzero;
pub use flipperzero::*;

use log::info;

use crate::{
    side_effects::{Input, Output},
    vm::{self, *},
};

/// A trait for a target architecture to be compiled to.
pub trait Architecture {
    /// The name of the target architecture.
    fn name(&self) -> &str;
    /// The version of the target architecture.
    fn version(&self) -> &str;

    /// Whether or not the target architecture supports floating point.
    fn supports_floats(&self) -> bool;
    /// Whether or not the target architecture supports the given input (mode + channel).
    fn supports_input(&self, src: &Input) -> bool;
    /// Whether or not the target architecture supports the given output (mode + channel).
    fn supports_output(&self, dst: &Output) -> bool;

    /// Get a value from the given input stream (mode + channel).
    fn get(&mut self, src: &Input) -> Result<String, String>;
    /// Put a value to the given output stream (mode + channel).
    fn put(&mut self, dst: &Output) -> Result<String, String>;
    /// Peek a value from the device connected to the program.
    fn peek(&mut self) -> Result<String, String>;
    /// Poke a value to the device connected to the program.
    fn poke(&mut self) -> Result<String, String>;

    /// The code before the program starts.
    fn prelude(&self, _is_core: bool) -> Option<String> {
        None
    }

    /// The code after each instruction.
    fn postop(&self) -> Option<String> {
        None
    }

    /// The code after the program ends.
    fn postlude(&self, _is_core: bool) -> Option<String> {
        None
    }

    /// The code before the function definitions.
    fn pre_funs(&self, _funs: Vec<i32>) -> Option<String> {
        None
    }

    /// The code after the function definitions.
    fn post_funs(&self, _funs: Vec<i32>) -> Option<String> {
        None
    }

    /// The string used for indentation.
    fn indentation(&self) -> Option<String> {
        Some("\t".to_string())
    }

    /// Compile the declaration of a procedure.
    fn declare_proc(&mut self, label_id: usize) -> String;
    /// Compile an `End` instruction (with the matching `If` or `While` or `Function`)
    fn end(&mut self, matching: &CoreOp, fun: Option<usize>) -> String;
    /// Compile a `CoreOp` instruction.
    fn op(&mut self, op: &vm::CoreOp) -> String;
    /// Compile a `StandardOp` instruction.
    fn std_op(&mut self, op: &vm::StandardOp) -> Result<String, String>;
}

/// Implement a compiler for the given target.
pub trait CompiledTarget: Architecture {
    fn build_op(
        &mut self,
        op: &vm::CoreOp,
        matching_ops: &mut Vec<vm::CoreOp>,
        matching_funs: &mut Vec<usize>,
        current_fun: &mut usize,
        indent: &mut usize,
    ) -> Result<String, String> {
        Ok(match op {
            CoreOp::Function => {
                matching_ops.push(op.clone());

                matching_funs.push(*current_fun);
                let fun_header = self.declare_proc(*current_fun);
                *current_fun += 1;

                *indent += 1;
                fun_header
            }
            CoreOp::While | CoreOp::If => {
                matching_ops.push(op.clone());
                *indent += 1;

                self.op(op)
            }
            CoreOp::Else => {
                if let Some(CoreOp::If) = matching_ops.pop() {
                    matching_ops.push(op.clone());
                    self.op(op)
                } else {
                    return Err("Unexpected else".to_string());
                }
            }
            CoreOp::End => {
                *indent -= 1;
                if let Some(op) = matching_ops.pop() {
                    self.end(
                        &op,
                        if let CoreOp::Function = op {
                            matching_funs.pop()
                        } else {
                            None
                        },
                    )
                } else {
                    return Err("Unexpected end".to_string());
                }
            }

            CoreOp::Get(src) => {
                if !self.supports_input(src) {
                    return Err(format!(
                        "Input {:?} not supported on target {}",
                        src,
                        self.name()
                    ));
                }
                self.get(src)?
            }
            CoreOp::Put(dst) => {
                if !self.supports_output(dst) {
                    return Err(format!(
                        "Output {:?} not supported on target {}",
                        dst,
                        self.name()
                    ));
                }
                self.put(dst)?
            }
            other => self.op(other),
        })
    }

    fn build_std_op(
        &mut self,
        std_op: &vm::StandardOp,
        matching_ops: &mut Vec<vm::CoreOp>,
        matching_funs: &mut Vec<usize>,
        current_fun: &mut usize,
        indent: &mut usize,
    ) -> Result<String, String> {
        match std_op {
            StandardOp::CoreOp(op) => {
                self.build_op(op, matching_ops, matching_funs, current_fun, indent)
            }
            other => self.std_op(other),
        }
    }

    /// Compile the core variant of the machine code (must be implemented for every target).
    fn build_core(&mut self, program: &vm::CoreProgram) -> Result<String, String> {
        info!("Compiling core program for target {}", self.name());
        let (main_ops, function_defs) = program.clone().get_main_and_functions();
        let mut result = self.prelude(true).unwrap_or("".to_string());

        let mut matching_ops = vec![];
        let mut matching_funs = vec![];
        let mut current_fun: usize = 0;

        let tab = self.indentation().unwrap_or("".to_string());

        let mut indent = 0;

        result += &self
            .pre_funs(function_defs.keys().cloned().collect())
            .unwrap_or("".to_string());
        for i in 0..function_defs.len() as i32 {
            let f = &function_defs[&i];
            for op in f {
                result += &tab.repeat(indent);
                result += &self.build_op(
                    op,
                    &mut matching_ops,
                    &mut matching_funs,
                    &mut current_fun,
                    &mut indent,
                )?;
                result += &self.postop().unwrap_or("".to_string());
            }
        }
        result += &self
            .post_funs(function_defs.keys().cloned().collect())
            .unwrap_or("".to_string());
        indent = 1;
        for op in main_ops {
            result += &tab.repeat(indent);
            result += &self.build_op(
                &op,
                &mut matching_ops,
                &mut matching_funs,
                &mut current_fun,
                &mut indent,
            )?;
            result += &self.postop().unwrap_or("".to_string());
        }

        info!("Finished compiling core program for target {}", self.name());
        Ok(result + &tab + self.postlude(true).unwrap_or("".to_string()).as_str())
    }

    /// Compile the standard variant of the machine code (should be implemented for every target possible).
    fn build_std(&mut self, program: &vm::StandardProgram) -> Result<String, String> {
        info!("Compiling standard program for target {}", self.name());
        let (main_ops, function_defs) = program.clone().get_main_and_functions();
        let mut result = self.prelude(false).unwrap_or("".to_string());

        let mut matching_ops = vec![];
        let mut matching_funs = vec![];
        let mut current_fun: usize = 0;

        let tab = self.indentation().unwrap_or("".to_string());

        let mut indent = 0;
        result += &self
            .pre_funs(function_defs.keys().cloned().collect())
            .unwrap_or("".to_string());
        for i in 0..function_defs.len() as i32 {
            let f = &function_defs[&i];
            for op in f {
                result += &tab.repeat(indent);
                result += &self.build_std_op(
                    op,
                    &mut matching_ops,
                    &mut matching_funs,
                    &mut current_fun,
                    &mut indent,
                )?;
                result += &self.postop().unwrap_or("".to_string());
            }
        }
        result += &self
            .post_funs(function_defs.keys().cloned().collect())
            .unwrap_or("".to_string());
        indent = 1;
        for op in main_ops {
            result += &tab.repeat(indent);
            result += &self.build_std_op(
                &op,
                &mut matching_ops,
                &mut matching_funs,
                &mut current_fun,
                &mut indent,
            )?;
            result += &self.postop().unwrap_or("".to_string());
        }

        info!(
            "Finished compiling standard program for target {}",
            self.name()
        );
        Ok(result + &tab + self.postlude(false).unwrap_or("".to_string()).as_str())
    }
}
