//! # Assembly Module
//!
//! This module contains a small assembly language implemented
//! over the virtual machine. The assembly language, much like the virtual machine, is split
//! into two variants: the "core" variant and the "standard" variant. The assembly language
//! is not typed, and simply works by performing basic operations on one or more parameter addresses,
//! and automatically managing procedure pointers. It also gives the programmer the ability to
//! write inline assembly code for optimized performance or code size.
//!
//! ## Index
//!
//! 1. [Core Assembly](./core)
//! 2. [Standard Assembly](./std)
//! 3. [Assembly Memory Model](./location)
//!
//! ## The Core Variant
//!
//! The core variant is intended to be used with the core variant of
//! the virtual machine. It is very portable, and MUST be supported
//! on all implementations.
//!
//!
//! ## The Standard Variant
//!
//! The standard variant is intended to be used with the standard
//! variant of the virtual machine. It is very portable: it only adds
//! instructions for float operations, memory allocation, and I/O.
use ::std::collections::HashMap;

use log::{debug, trace, warn, error};

pub mod core;
pub mod location;
pub mod std;

pub use self::core::{CoreOp, CoreProgram};
pub use self::std::{StandardOp, StandardProgram};
pub use location::{REGISTERS, Globals, Location, A, B, C, D, E, F, FP, SP, GP};
pub(crate) use location::{TMP, FP_STACK};

/// A frontend to both the `CoreProgram` and `StandardProgram` types.
/// This allows the compiler to append `CoreOp`s to both programs
/// with guaranteed success, but optionally allows the compiler to
/// *attempt* to compile a `StandardOp`, which *may* fail depending
/// on the target's support for the instruction.
pub trait AssemblyProgram {
    /// Insert a core operation into the program.
    fn op(&mut self, op: CoreOp);
    /// Attempt to insert a standard operation into the program.
    /// This could fail depending on the backend's support for the
    /// instruction.
    fn std_op(&mut self, op: StandardOp) -> Result<(), Error>;
    /// Insert a comment into the program.
    fn comment(&mut self, comment: String) {
        self.op(CoreOp::Comment(comment))
    }
    /// Is the given label defined yet in the operations?
    /// I.E., has a `CoreOp::Fn` with this label been inserted
    /// into the program code yet?
    fn is_defined(&self, label: &str) -> bool;

    /// Get the current instruction number.
    fn current_instruction(&self) -> usize;

    /// Log all the instructions after the given instruction number.
    /// The `target` is an identifier for the instructions being logged. This is used
    /// to search for the log messages in the output.
    /// The `message` is a message to be logged before the instructions. This tells some
    /// context about the instructions being logged.
    /// The `i` is the instruction number to start logging at.
    /// This will log all instructions until the end of the program.
    fn log_instructions_after(&self, target: &str, message: &str, mut i: usize) {
        debug!(target: target, "{target}: {}", message);
        while let Some(op) = self.get_op(i) {
            match op {
                Ok(op) => debug!(target: target, "{:04x}: {}", i, op),
                Err(op) => debug!(target: target, "{:04x}: {}", i, op),
            }
            i += 1;
        }
    }

    /// Get the operation at the given instruction number.
    fn get_op(&self, start: usize) -> Option<Result<CoreOp, StandardOp>>;
}

/// An environment used to assemble a program.
/// This stores information about labels and their IDs in the virtual machine,
/// as well as information about matching instructions to their `End` statements.
#[derive(Default, Clone)]
struct Env {
    globals: Globals,
    labels: HashMap<String, usize>,
    label: usize,
    matching: Vec<(CoreOp, usize)>,
}

impl Env {
    /// Declare a new label
    fn declare_label(&mut self, name: &str) {
        trace!("Declared label {}", name);

        if self.labels.contains_key(name) {
            warn!("Label {} already declared", name);
        }
        self.labels.insert(name.to_string(), self.label);
        self.label += 1;
    }

    fn declare_global(&mut self, name: &str, size: usize) {
        trace!("Declared global {}", name);
        self.globals.add_global(name.to_owned(), size);
    }

    fn resolve(&mut self, loc: &Location) -> Result<Location, Error> {
        self.globals.resolve(loc)
    }

    fn get_size_of_globals(&self) -> usize {
        self.globals.get_size()
    }

    fn get_location_of_global(&mut self, name: &str) -> Result<Location, Error> {
        self.globals.get_global_location(name).ok_or_else(|| {
            error!("Undefined global {}", name);
            Error::UndefinedGlobal(name.to_string())
        })
    }

    /// Get the virtual machine ID of a label (which can be called as a function).
    fn get_label(&self, name: &str, current_instruction: usize) -> Result<usize, Error> {
        self.labels
            .get(name)
            .copied()
            .ok_or_else(|| {
                error!("Undefined label {} at instruction #{}", name, current_instruction);
                Error::UndefinedLabel(name.to_string(), current_instruction)
            })
    }

    /// Add a new instruction to be matched with `End`. For example,
    /// if the user declares an `If` statement, this will keep track
    /// of the `If` and help find its matching `End` statement.
    fn push_matching(&mut self, op: &CoreOp, current_instruction: usize) {
        self.matching.push((op.clone(), current_instruction));
    }

    /// Pops off the last instruction added with `push_matching`,
    /// and matches it with its `End` statement.
    fn pop_matching(&mut self, current_instruction: usize) -> Result<(CoreOp, usize), Error> {
        self.matching
            .pop()
            .ok_or(Error::Unmatched(CoreOp::End, current_instruction))
    }
}

/// An error generated by assembling some assembly language code.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Error {
    /// An error generated by the virtual machine.
    VirtualMachineError(crate::vm::Error),
    /// Is this standard assembly operation supported by the target?
    UnsupportedInstruction(StandardOp),
    /// The given label was not defined.
    UndefinedLabel(String, usize),
    /// The given global was not defined.
    UndefinedGlobal(String),
    /// The given instruction did not have a matching "end".
    /// This is used for `If`, `Else`, `While`, `For`, and `Fn` statements.
    Unmatched(CoreOp, usize),
    /// The given instruction was not expected, or cannot be used in this context.
    Unexpected(CoreOp, usize),
}

impl From<crate::vm::Error> for Error {
    fn from(e: crate::vm::Error) -> Self {
        Self::VirtualMachineError(e)
    }
}
