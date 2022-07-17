//! # Standard Assembly Variant
//!
//! This variant of the assembly language is intended to be used
//! with the standard variant of the virtual machine. It is very
//! portable, but probably not supported on older systems or
//! hardware implementations.
use super::{location::FP_STACK, CoreOp, Env, Error, Location, F, FP, SP};
use crate::vm::{self, VirtualMachineProgram};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct StandardProgram(pub Vec<StandardOp>);

impl StandardProgram {
    pub fn assemble(&self, allowed_recursion_depth: usize) -> Result<vm::StandardProgram, Error> {
        let mut result = vm::StandardProgram(vec![]);
        let mut env = Env::default();
        // Create the stack of frame pointers starting directly after the last register
        F.copy_address_to(&FP_STACK, &mut result);
        // Copy the address just after the allocated space to the stack pointer.
        FP_STACK
            .deref()
            .offset(allowed_recursion_depth as isize)
            .copy_address_to(&SP, &mut result);

        SP.copy_to(&FP, &mut result);
        for (i, op) in self.0.iter().enumerate() {
            op.assemble(i, &mut env, &mut result)?
        }

        if let Ok((unmatched, last_instruction)) = env.pop_matching(self.0.len() - 1) {
            return Err(Error::Unmatched(unmatched, last_instruction));
        }

        Ok(result)
    }
}

/// A standard instruction of the assembly language. These are instructions
/// that should be implemented for every target possible. Standard instructions
/// should only not be implemented for targets like physical hardware, where the
/// program is executed on the bare metal (a custom CPU or FPGA).
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum StandardOp {
    /// Execute a core instruction.
    CoreOp(CoreOp),

    Set(Location, f64),
    PushLiteral(f64),

    Pow {
        src: Location,
        dst: Location,
    },
    Sqrt(Location),
    Abs(Location),

    Add {
        src: Location,
        dst: Location,
    },
    Sub {
        src: Location,
        dst: Location,
    },
    Mul {
        src: Location,
        dst: Location,
    },
    Div {
        src: Location,
        dst: Location,
    },
    Rem {
        src: Location,
        dst: Location,
    },
    Neg(Location),

    IsNonNegative(Location),

    Sin(Location),
    Cos(Location),
    Tan(Location),
    ASin(Location),
    ACos(Location),
    ATan(Location),

    IsLess {
        src: Location,
        dst: Location,
    },
    IsGreater {
        src: Location,
        dst: Location,
    },

    Alloc(Location),
    Free(Location),

    PutChar(Location),
    GetChar(Location),
    PutInt(Location),
    GetInt(Location),
    PutFloat(Location),
    GetFloat(Location),
}

impl StandardOp {
    #[allow(unused_variables)]
    pub(super) fn assemble(
        &self,
        current_instruction: usize,
        env: &mut Env,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        match self {
            Self::CoreOp(op) => op.assemble(current_instruction, env, result),

            _ => todo!(),
        }
    }
}
