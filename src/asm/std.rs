//! # Standard Assembly Variant
//! 
//! This variant of the assembly language is intended to be used
//! with the standard variant of the virtual machine. It is very
//! portable, but probably not supported on older systems or
//! hardware implementations.
use super::{Location, FP, SP, BOTTOM_OF_STACK, Env, Error};
use crate::vm::{self, VirtualMachineProgram};


#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct StandardProgram(pub Vec<StandardOp>);

impl StandardProgram {
    pub fn assemble(&self) -> Result<vm::StandardProgram, Error> {
        let mut result = vm::StandardProgram(vec![]);
        let mut env = Env::default();
        BOTTOM_OF_STACK.copy_address_to(&SP, &mut result);
        SP.copy_to(&FP, &mut result);
        for (i, op) in self.0.iter().enumerate() {
            op.assemble(i, &mut env, &mut result)?
        }

        if let Ok((unmatched, last_instruction)) = env.pop_matching(self.0.len()-1) {
            return Err(Error::Unmatched(unmatched, last_instruction));
        }

        Ok(result)
    }
}

/// A core instruction of the assembly language. These are instructions
/// guaranteed to be implemented for every target possible.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum StandardOp {
    Comment(String),

    /// Set the value of a register, or any location in memory, to a given constant.
    Set(Location, isize),
    /// Set the value of a register, or any location in memory, to the value of a label's ID.
    SetLabel(Location, String),
    /// Get the address of a location, and store it in a destination
    GetAddress {
        addr: Location,
        dst: Location,
    },
    
    /// Get a value in memory and call it as a label ID.
    Call(Location),
    /// Call a function with a given label.
    CallLabel(String),
    /// Return from the current function.
    Return,
    
    /// Declare a new label.
    Fn(String),
    /// Begin a "while the value is not zero" loop over a given register or location in memory.
    While(Location),
    /// Begin an "if the value is not zero" statement over a given register or location in memory.
    If(Location),
    /// Add an "else" clause to an "if the value is not zero" statement.
    Else,
    /// Terminate a function declaration, a while loop, an if statement, or an else clause.
    End,

    /// Copy a value from a source location to a destination location.
    Move {
        src: Location,
        dst: Location
    },

    /// Swap the values of two locations.
    Swap(Location, Location),

    /// Make this pointer point to the next cell.
    Next(Location, Option<isize>),
    /// Make this pointer point to the previous cell.
    Prev(Location, Option<isize>),

    /// Increment the integer value of a location.
    Inc(Location),
    /// Decrement the integer value of a location.
    Dec(Location),
    /// Add an integer value from a source location to a destination location.
    Add {
        src: Location,
        dst: Location
    },
    /// Subtract a source integer value from a destination location.
    Sub {
        src: Location,
        dst: Location
    },
    /// Multiply a destination location by a source value.
    Mul {
        src: Location,
        dst: Location
    },
    /// Divide a destination location by a source value.
    Div {
        src: Location,
        dst: Location
    },
    /// Store the remainder of the destination modulus the source in the destination.
    Rem {
        src: Location,
        dst: Location
    },
    /// Divide a destination location by a source value.
    /// Store the quotient in the destination, and the remainder in the source.
    DivRem {
        src: Location,
        dst: Location
    },
    /// Negate an integer.
    Neg(Location),

    /// Replace a value in memory with its boolean complement.
    Not(Location),
    /// Logical "and" a destination with a source value.
    And {
        src: Location,
        dst: Location
    },
    /// Logical "or" a destination with a source value.
    Or {
        src: Location,
        dst: Location
    },

    /// Push an integer at a memory location on the stack.
    Push(Location),
    /// Pop an integer from the stack and store it in a memory location.
    Pop(Option<Location>),

    /// Store the comparison of "a" and "b" in a destination register.
    /// If "a" is less than "b", store -1. If "a" is greater than "b", store 1.
    /// If "a" is equal to "b", store 0.
    Compare {
        dst: Location,
        a: Location,
        b: Location
    },
    /// Perform dst = dst > src.
    IsGreater {
        dst: Location,
        src: Location
    },
    /// Perform dst = dst >= src.
    IsGreaterEqual {
        dst: Location,
        src: Location
    },
    /// Perform dst = dst < src.
    IsLess {
        dst: Location,
        src: Location
    },
    /// Perform dst = dst <= src.
    IsLessEqual {
        dst: Location,
        src: Location
    },
    /// Perform dst = dst == src.
    IsEqual {
        dst: Location,
        src: Location
    },
    /// Perform dst = dst != src.
    IsNotEqual {
        dst: Location,
        src: Location
    },

    /// Get a character from the input stream and store it in a destination register.
    GetChar(Location),
    /// Put a character from a source register to the output stream.
    PutChar(Location),
}


impl StandardOp {
    #[allow(unused_variables)]
    fn assemble(&self, current_instruction: usize, env: &mut Env, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        Ok(())
    }
}
