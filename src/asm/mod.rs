//! # Assembly Module
//!
//! This module contains a small assembly language implemented
//! over the virtual machine. The assembly language, much like the virtual machine, is split
//! into two variants: the "core" variant and the "standard" variant.
//!
//! ## The Core Variant
//!
//! The core variant is intended to be used with the core variant of
//! the virtual machine. It is very portable, and MUST be supported
//! on all implementations.
//!
//! ## The Standard Variant
//!
//! The standard variant is intended to be used with the standard
//! variant of the virtual machine. It is very portable: it only adds
//! instructions for float operations, memory allocation, and I/O.
use ::std::collections::HashMap;

pub mod std;
pub mod core;
pub mod location;

pub use self::std::{StandardOp, StandardProgram};
pub use self::core::{CoreOp, CoreProgram};
pub use location::{Location, A, B, C, D, E, F, FP, SP};

pub trait AssemblyProgram {
    fn op(&mut self, op: CoreOp);
    fn std_op(&mut self, op: StandardOp) -> Result<(), Error>;
    fn comment(&mut self, comment: String) {
        self.op(CoreOp::Comment(comment))
    }
}

/// An environment used to assemble a program.
/// This stores information about labels and their IDs in the virtual machine,
/// as well as information about matching instructions to their `End` statements.
#[derive(Default, Clone, Debug)]
struct Env {
    labels: HashMap<String, usize>,
    label: usize,
    matching: Vec<(CoreOp, usize)>,
}

impl Env {
    /// Declare a new label
    fn declare(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.label);
        self.label += 1;
    }
    /// Get the virtual machine ID of a label (which can be called as a function).
    fn get(&self, name: &str, current_instruction: usize) -> Result<usize, Error> {
        self.labels
            .get(name)
            .copied()
            .ok_or_else(|| Error::UndefinedLabel(name.to_string(), current_instruction))
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

/// An error returned by the assembly language.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Error {
    VirtualMachineError(crate::vm::Error),
    UnsupportedInstruction(StandardOp),
    UndefinedLabel(String, usize),
    Unmatched(CoreOp, usize),
    Unexpected(CoreOp, usize),
}

impl From<crate::vm::Error> for Error {
    fn from(e: crate::vm::Error) -> Self {
        Self::VirtualMachineError(e)
    }
}
