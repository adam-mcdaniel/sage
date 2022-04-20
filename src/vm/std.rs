//! The standard instructions of the virtual machine are defined here.
//! 
//! Standard instructions are instructions that *should* be implemented for
//! every target possible. Standard instructions should only not be implemented
//! for targets like physical hardware, where the program is executed on the
//! bare metal (a custom CPU or FPGA).

use super::{VirtualMachineProgram, CoreOp};

impl VirtualMachineProgram for StandardProgram {
    fn append_core_op(&mut self, op: CoreOp) {
        self.0.push(StandardOp::CoreOp(op));
    }

    fn append_standard_op(&mut self, op: StandardOp) {
        self.0.push(op);
    }
}

/// A program of core and standard instructions.
#[derive(Default, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct StandardProgram(pub Vec<StandardOp>);

/// A standard instruction.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum StandardOp {
    /// Execute a core instruction.
    CoreOp(CoreOp),

    /// Take the value of the register, and allocate that number of cells in memory.
    /// Set the register to the lowest address of the allocated memory.
    Alloc,

    /// Free the memory pointed to by the register.
    Free,
    
    /// Convert the register from a float to an integer.
    ToInt,
    /// Convert the register from an integer to a float.
    ToFloat,

    /// Swap the register with the value pointed to on the tape.
    Swap,

    /// Add the value pointed to on the tape to the register (as floats).
    Add,
    /// Subtract the value pointed to on the tape from the register (as floats).
    Sub,
    /// Multiply the register by the value pointed to on the tape (as floats).
    Mul,
    /// Divide the register by the value pointed to on the tape (as floats).
    Div,
    /// Store the remainder of the register and the value pointed to in the
    /// tape (as floats) into the register.
    Rem,

    /// Make the register equal to the integer 1 if the register (as a float)
    /// is a whole number, otherwise make it equal to 0.
    IsWhole,
    
    /// Store the sine of the register (as a float) into the register.
    Sin,
    /// Store the cosine of the register (as a float) into the register.
    Cos,
    /// Store the tangent of the register (as a float) into the register.
    Tan,
    /// Store the inverse-sine of the register (as a float) into the register.
    ASin,
    /// Store the inverse-cosine of the register (as a float) into the register.
    ACos,
    /// Store the inverse-tangent of the register (as a float) into the register.
    ATan,
    
    /// Get an integer from the input stream (like `scanf("%lld", &reg)`) and store it in the register.
    GetInt,
    /// Print the register as an integer to the output stream (like `printf("%lld", reg)`).
    PutInt,
    /// Get a float from the input stream (like `scanf("%f", &reg)`) and store it in the register.
    GetFloat,
    /// Print the register as a float to the output stream (like `printf("%f", reg)`).
    PutFloat,
}