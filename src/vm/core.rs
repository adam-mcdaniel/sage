//! The core instructions of the virtual machine are defined here.
//! 
//! Core instructions are instructions that **must** be implemented for
//! every target. Write programs in the core variant to guarantee ports
//! for ***every*** target.
use super::{VirtualMachineProgram, StandardOp};

impl VirtualMachineProgram for CoreProgram {
    fn append_core_op(&mut self, op: CoreOp) {
        self.0.push(op);
    }

    fn append_standard_op(&mut self, op: StandardOp) {
        panic!("the core virtual machine does not support standard instructions: tried to run `{:?}`", op)
    }
}

/// A program of only core virtual machine instructions.
#[derive(Default, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CoreProgram(pub Vec<CoreOp>);

/// An individual core virtual machine instruction.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CoreOp {
    Comment(String),

    /// Set the register equal to a constant value.
    Constant(isize),

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
    /// ```
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

    /// Make the register equal to 1 if the register is a whole number, otherwise make it equal to 0.
    IsWhole,

    /// Get a character from the input stream and store it in the register.
    GetChar,
    /// Put the character value of the register to the output stream.
    PutChar,
}
