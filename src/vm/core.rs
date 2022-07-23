//! The core instructions of the virtual machine are defined here.
//!
//! Core instructions are instructions that **must** be implemented for
//! every target. Write programs in the core variant to guarantee ports
//! for ***every*** target.
use super::{Error, StandardOp, StandardProgram, VirtualMachineProgram};
use core::fmt;

impl VirtualMachineProgram for CoreProgram {
    fn op(&mut self, op: CoreOp) {
        self.0.push(op);
    }

    fn std_op(&mut self, op: StandardOp) -> Result<(), Error> {
        Err(Error::UnsupportedInstruction(op))
    }

    fn code(&self) -> Result<CoreProgram, StandardProgram> {
        Ok(self.clone())
    }
}

/// A program of only core virtual machine instructions.
#[derive(Default, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CoreProgram(pub Vec<CoreOp>);

impl fmt::Debug for CoreProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        for (i, op) in self.0.iter().enumerate() {
            writeln!(
                f,
                "{:04}: {}{:?}",
                i,
                match op {
                    CoreOp::Function | CoreOp::If | CoreOp::While => {
                        indent += 1;
                        "   ".repeat(indent - 1)
                    }
                    CoreOp::Else => {
                        "   ".repeat(indent - 1)
                    }
                    CoreOp::End => {
                        indent -= 1;
                        "   ".repeat(indent)
                    }
                    _ => "   ".repeat(indent),
                },
                op
            )?
        }
        Ok(())
    }
}

/// An individual core virtual machine instruction.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CoreOp {
    /// A comment in the machine code (not in the compiled output).
    Comment(String),

    /// Set the register equal to a constant value.
    Set(isize),

    /// Create a new function.
    Function,
    /// Calls the nth function defined in the program, where n is the value of the register.
    Call,
    /// Return from the current function.
    Return,

    /// Begin a "while the register is not zero" loop.
    While,
    /// Begin an "if the register is not zero" conditional.
    ///
    /// If statements are structured like so:
    /// ```hs
    /// if
    ///     ...
    /// end
    ///
    /// if
    ///     ...
    /// else
    ///     ...
    /// end
    /// ```
    If,
    /// Begin an "else" conditional.
    Else,
    /// End a conditional.
    End,

    /// Store the register to the value pointed to on the tape.
    Save,
    /// Store the value pointed to on the tape to the register.
    Restore,

    /// Move the pointer on the tape by a number of cells.
    Move(isize),

    /// Store the value of the pointer to the register.
    Where,
    /// The pointer is made equal to the value pointed to on the tape.
    Deref,
    /// The last "deref" operation is undone; the pointer is made equal to the value it was before the last "deref" operation.
    Refer,

    /// Increment the register by one.
    Inc,
    /// Decrement the register by one.
    Dec,
    /// Add the value pointed to on the tape to the register.
    Add,
    /// Subtract the value pointed to on the tape from the register.
    Sub,
    /// Multiply the register by the value pointed to on the tape.
    Mul,
    /// Divide the register by the value pointed to on the tape.
    Div,
    /// Store the remainder of the register and the value pointed to in the tape into the register.
    Rem,

    /// Make the register equal to 1 if the register is non-negative, otherwise make it equal to 0.
    IsNonNegative,

    /// Get a value from the input interface / device and store it in the register.
    /// This is intended to function something like system calls for using any external
    /// functionality that can't be implemented in the virtual machine, such as I/O or OS operations.
    ///
    /// The specific behavior of this instruction is purposefully not defined.
    Get,
    /// Write the value of the register to the output interface / device.
    /// This is intended to function something like system calls for using any external
    /// functionality that can't be implemented in the virtual machine, such as I/O or OS operations.
    ///
    /// The specific behavior of this instruction is purposefully not defined.
    Put,
}
