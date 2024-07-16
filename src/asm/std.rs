//! # Standard Assembly Variant
//!
//! This variant of the assembly language is intended to be used
//! with the standard variant of the virtual machine. It is very
//! portable, but probably not supported on older systems or
//! hardware implementations.
//!
//! [***Click here to view opcodes!***](./enum.StandardOp.html)
use super::{
    location::*, AssemblyProgram, CoreOp, CoreProgram, Env, Error, Location, FP, GP, SP,
    START_OF_FP_STACK,
};
use crate::side_effects::ffi::FFIBinding;
use crate::vm::{self, VirtualMachineProgram};
use std::{collections::BTreeSet, fmt};

use log::info;
use serde_derive::{Serialize, Deserialize};

/// A program composed of standard instructions, which can be assembled
/// into the standard virtual machine instructions.
#[derive(Clone, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct StandardProgram {
    /// The list of standard assembly instructions in the program.
    pub code: Vec<StandardOp>,
    /// A set containining the labels for each function in the program
    /// that has been defined so far. This helps the LIR compiler
    /// determine if a function has been compiled yet or not.
    labels: BTreeSet<String>,
}

/// A default program is an empty program.
impl Default for StandardProgram {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl StandardProgram {
    /// Create a new program of core assembly instructions.
    pub fn new(code: Vec<StandardOp>) -> Self {
        let mut labels = BTreeSet::new();
        for op in &code {
            // If the operation is a function label, add its label to the set of defined labels.
            if let StandardOp::CoreOp(CoreOp::Fn(label)) = op {
                labels.insert(label.clone());
            }
        }
        Self { code, labels }
    }

    /// Get the size of the globals.
    fn get_size_of_globals(&self, env: &mut Env) -> Result<usize, Error> {
        for op in &self.code {
            // Go through all the operations and declare the globals.
            if let StandardOp::CoreOp(CoreOp::Global { name, size }) = op {
                env.declare_global(name, *size);
            }
        }

        // Get the size of the globals in the environment after the declarations.
        Ok(env.get_size_of_globals())
    }

    /// Assemble the program into a virtual machine program.
    ///
    /// The `allowed_recursion_depth` is the size of the frame pointer stack.
    /// The frame pointer stack is used to keep track of the frame pointers
    /// of each function call.
    pub fn assemble(&self, allowed_recursion_depth: usize) -> Result<vm::StandardProgram, Error> {
        let mut result = vm::StandardProgram(vec![]);
        let mut env = Env::default();

        // Get the size of the globals
        let size_of_globals = self.get_size_of_globals(&mut env)?;

        // Create the stack of frame pointers starting directly after the last register
        // let start_of_fp_stack = F.offset(1);
        START_OF_FP_STACK.copy_address_to(&FP_STACK, &mut result);
        info!(
            "Frame pointer stack begins at {FP_STACK:?}, and is {} cells long.",
            allowed_recursion_depth
        );
        let end_of_fp_stack = START_OF_FP_STACK.offset(allowed_recursion_depth as isize);

        // Copy the address just after the allocated space to the global pointer.
        let starting_gp_addr = end_of_fp_stack;
        starting_gp_addr.copy_address_to(&GP, &mut result);
        info!(
            "Global pointer is initialized to point to {starting_gp_addr:?}, and is {} cells long.",
            size_of_globals
        );

        // Allocate the global variables
        let starting_sp_addr = starting_gp_addr.offset(size_of_globals as isize);
        info!("Stack pointer is initialized to point to {starting_sp_addr:?}.");
        starting_sp_addr.copy_address_to(&SP, &mut result);
        starting_sp_addr.copy_address_to(&STACK_START, &mut result);

        SP.copy_to(&FP, &mut result);
        for (i, op) in self.code.iter().enumerate() {
            op.assemble(i, &mut env, &mut result)?
        }

        if let Ok((unmatched, last_instruction)) = env.pop_matching(self.code.len()) {
            return Err(Error::Unmatched(unmatched, last_instruction));
        }

        Ok(result.flatten())
    }
}

impl fmt::Display for StandardProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        let mut comment_count = 0;
        for (i, op) in self.code.iter().enumerate() {
            if f.alternate() {
                if let StandardOp::CoreOp(CoreOp::Comment(comment)) = op {
                    if f.alternate() {
                        write!(f, "{:4}  ", "")?;
                    }
                    comment_count += 1;
                    writeln!(f, "{}// {}", "   ".repeat(indent), comment,)?;
                    continue;
                }

                write!(f, "{:04x?}: ", i - comment_count)?;
            } else if let StandardOp::CoreOp(CoreOp::Comment(_)) = op {
                continue;
            }

            writeln!(
                f,
                "{}{}",
                match op {
                    StandardOp::CoreOp(CoreOp::Fn(_))
                    | StandardOp::CoreOp(CoreOp::If(_))
                    | StandardOp::CoreOp(CoreOp::While(_)) => {
                        indent += 1;
                        "   ".repeat(indent - 1)
                    }
                    StandardOp::CoreOp(CoreOp::Else) => {
                        "   ".repeat(indent - 1)
                    }
                    StandardOp::CoreOp(CoreOp::End) => {
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

impl AssemblyProgram for StandardProgram {
    /// Add a core operation to the program.
    fn op(&mut self, op: CoreOp) {
        // If the operation is a function label, add its label to the set of defined labels.
        if let CoreOp::Fn(label) = &op {
            self.labels.insert(label.clone());
        }

        if let Some(last_op) = self.code.last().cloned() {
            match (last_op, op) {
                (StandardOp::CoreOp(last_core_op), op) => match (last_core_op, op) {
                    (CoreOp::Push(src, 1), CoreOp::Pop(Some(dst), 1)) => {
                        self.code.pop();
                        self.op(CoreOp::Move {
                            src: src.clone(),
                            dst: dst.clone(),
                        })
                    }
                    (CoreOp::Next(SP, Some(1) | None), CoreOp::Set(dst, n))
                        if dst == SP.deref() =>
                    {
                        self.code.pop();
                        self.op(CoreOp::PushConst(vec![n]));
                    }
                    (CoreOp::Pop(None, n), CoreOp::Next(SP, Some(m))) if n as isize == m => {
                        self.code.pop();
                    }
                    (CoreOp::Next(SP, Some(n)), CoreOp::Pop(None, m)) if n == m as isize => {
                        self.code.pop();
                    }
                    (CoreOp::Pop(None, 1), CoreOp::Next(SP, None)) => {
                        self.code.pop();
                    }
                    (CoreOp::Next(SP, None), CoreOp::Pop(None, 1)) => {
                        self.code.pop();
                    }
                    (CoreOp::Pop(None, n), CoreOp::Pop(None, m)) => {
                        self.code.pop();
                        self.op(CoreOp::Pop(None, n + m));
                    }
                    (CoreOp::Push(_src, 1), CoreOp::Pop(None, 1)) => {
                        self.code.pop();
                    }
                    (CoreOp::Pop(None, 1), CoreOp::Push(src, 1)) => {
                        self.code.pop();
                        self.op(CoreOp::Move {
                            src,
                            dst: SP.deref(),
                        });
                    }
                    (CoreOp::Move { dst, .. }, CoreOp::Set(dst2, n)) if dst == dst2 => {
                        self.code.pop();
                        self.op(CoreOp::Set(dst, n));
                    }
                    (CoreOp::PushConst(vals), CoreOp::PushConst(vals2)) => {
                        self.code.pop();
                        self.op(CoreOp::PushConst(
                            vals.iter().chain(vals2.iter()).cloned().collect(),
                        ));
                    }
                    (_, CoreOp::Move { src, dst }) if src == dst => {}
                    (_, CoreOp::Copy { size: 0, .. }) => {}
                    (_, CoreOp::Copy { src, dst, .. }) if src == dst => {}
                    (_, op) => self.code.push(StandardOp::CoreOp(op)),
                },
                (_, op) => self.code.push(StandardOp::CoreOp(op)),
            }
        } else {
            match op {
                CoreOp::Move { src, dst } if src == dst => {}
                CoreOp::Copy { size: 0, .. } => {}
                CoreOp::Copy { src, dst, .. } if src == dst => {}
                op => self.code.push(StandardOp::CoreOp(op)),
            }
        }
        // self.code.push(StandardOp::CoreOp(op))
    }

    /// Add a standard operation to the program.
    fn std_op(&mut self, op: super::StandardOp) -> Result<(), Error> {
        // If the operation is a function label, add its label to the set of defined labels.
        if let StandardOp::CoreOp(CoreOp::Fn(label)) = &op {
            self.labels.insert(label.clone());
        }
        self.code.push(op);
        Ok(())
    }

    /// Is the given label defined yet in the operations?
    fn is_defined(&self, label: &str) -> bool {
        self.labels.contains(label)
    }

    /// Get the current instruction number.
    fn current_instruction(&self) -> usize {
        self.code.len()
    }

    /// Get the operation at the given instruction number.
    fn get_op(&self, start: usize) -> Option<Result<CoreOp, StandardOp>> {
        self.code.get(start).cloned().map(Err)
    }
}

impl From<CoreProgram> for StandardProgram {
    fn from(core: CoreProgram) -> Self {
        Self::new(core.code.into_iter().map(StandardOp::CoreOp).collect())
    }
}

/// A standard instruction of the assembly language. These are instructions
/// that should be implemented for every target possible. Standard instructions
/// should only not be implemented for targets like physical hardware, where the
/// program is executed on the bare metal (a custom CPU or FPGA).
#[derive(Clone, Debug, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum StandardOp {
    /// Execute a core instruction.
    CoreOp(CoreOp),

    /// Set the value of a cell to a constant float.
    Set(Location, f64),
    /// Set the vector values of a destination.
    VecSet(Location, Vec<f64>),
    /// Take the integer value stored in a cell and store the equivalent float
    /// value in the same cell.
    ToFloat(Location),
    /// Take the float value stored in a cell and store the equivalent integer
    /// value in the same cell.
    ToInt(Location),

    /// Write some constant values to a location on the tape
    Const {
        /// The beginning of the constant data.
        dst: Location,
        /// The constant data.
        vals: Vec<f64>,
    },
    /// Push some constant values to the stack.
    PushConst(Vec<f64>),

    /// Raise a cell (float) to the power of another cell (float).
    Pow {
        /// The source cell.
        src: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Take the square root of a cell (float).
    Sqrt(Location),

    /// Add the source cell (float) to the destination cell (float).
    Add {
        /// The source cell.
        src: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Subtract the source cell (float) from the destination cell (float).
    Sub {
        /// The source cell.
        src: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Multiply the source cell (float) by the destination cell (float).
    Mul {
        /// The source cell.
        src: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Divide the destination cell (float) by the source cell (float).
    Div {
        /// The source cell.
        src: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Perform the modulo operation on the destination cell (float) by the
    /// source cell (float).
    Rem {
        /// The source cell.
        src: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Negate the value of a cell (float) and store the result in the same cell.
    Neg(Location),

    /// Perform Sin on a cell (float) and store the result in the same cell.
    Sin(Location),
    /// Perform Cos on a cell (float) and store the result in the same cell.
    Cos(Location),
    /// Perform Tan on a cell (float) and store the result in the same cell.
    Tan(Location),
    /// Perform inverse Sin on a cell (float) and store the result in the same cell.
    ASin(Location),
    /// Perform inverse Cos on a cell (float) and store the result in the same cell.
    ACos(Location),
    /// Perform inverse Tan on a cell (float) and store the result in the same cell.
    ATan(Location),

    /// Perform dst = a > b.
    IsGreater {
        /// The first cell in the comparison (left hand side).
        a: Location,
        /// The second cell in the comparison (right hand side).
        b: Location,
        /// The destination cell.
        dst: Location,
    },
    /// Perform dst = a < b.
    IsLess {
        /// The first cell in the comparison (left hand side).
        a: Location,
        /// The second cell in the comparison (right hand side).
        b: Location,
        /// The destination cell.
        dst: Location,
    },

    /// Take the value in the operand cell. Allocate that number of cells.
    /// The address of the first cell is stored in the operand cell.
    Alloc(Location),
    /// Free the memory allocated at the address stored in the operand cell.
    Free(Location),

    /// Call a foreign function.
    Call(FFIBinding),

    /// Perform a SIMD floating point addition operation.
    VecAdd {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Perform a SIMD floating point subtraction operation.
    VecSub {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Perform a SIMD floating point multiplication operation.
    VecMul {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Perform a SIMD floating point division operation.
    VecDiv {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Perform a SIMD floating point remainder operation.
    VecRem {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Perform a SIMD floating point negation operation.
    VecNeg { dst: Location, size: usize },

    /// Perform a SIMD floating point power operation.
    VecPow {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Perform a SIMD floating point sine operation.
    VecSin { dst: Location, size: usize },

    /// Perform a SIMD floating point cosine operation.
    VecCos { dst: Location, size: usize },

    /// Perform a SIMD floating point tangent operation.
    VecTan { dst: Location, size: usize },

    /// Perform a SIMD floating point inverse sine operation.
    VecASin { dst: Location, size: usize },

    /// Perform a SIMD floating point inverse cosine operation.
    VecACos { dst: Location, size: usize },

    /// Perform a SIMD floating point inverse tangent operation.
    VecATan { dst: Location, size: usize },

    /// Perform a SIMD floating point greater than or equal to zero operation.
    VecGez {
        src: Location,
        dst: Location,
        size: usize,
    },
}

fn unsupported(op: StandardOp) -> Result<(), Error> {
    Err(Error::UnsupportedInstruction(op))
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
            Self::CoreOp(op) => op.assemble(current_instruction, env, result)?,
            Self::VecSet(dst, vals) => {
                let dst = env.resolve(dst)?;
                if result.set_float_vector(vals.clone()).is_err() {
                    unsupported(self.clone())?
                }
                dst.to(result);
                result.store_vector(vals.len());
                dst.from(result);
            }
            Self::VecAdd { src, dst, size } => {
                if dst.vec_float_add(src, *size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecSub { src, dst, size } => {
                if dst.vec_float_sub(src, *size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecMul { src, dst, size } => {
                if dst.vec_float_mul(src, *size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecDiv { src, dst, size } => {
                if dst.vec_float_div(src, *size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecRem { src, dst, size } => {
                if dst.vec_float_rem(src, *size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecNeg { dst, size } => {
                if dst.vec_float_neg(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecPow { src, dst, size } => {
                if dst.vec_float_pow(src, *size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecSin { dst, size } => {
                if dst.vec_float_sin(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecCos { dst, size } => {
                if dst.vec_float_cos(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecTan { dst, size } => {
                if dst.vec_float_tan(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecASin { dst, size } => {
                if dst.vec_float_asin(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecACos { dst, size } => {
                if dst.vec_float_acos(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecATan { dst, size } => {
                if dst.vec_float_atan(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::VecGez { src, dst, size } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;
                src.vec_copy_to(&dst, *size, result);
                if dst.vec_float_whole_int(*size, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Set(loc, val) => {
                if loc.set_float(*val, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Sin(loc) => {
                if loc.sin(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::Cos(loc) => {
                if loc.cos(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::Tan(loc) => {
                if loc.tan(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::ASin(loc) => {
                if loc.asin(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::ACos(loc) => {
                if loc.acos(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::ATan(loc) => {
                if loc.atan(result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::ToFloat(loc) => {
                if loc.to_float(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::ToInt(loc) => {
                if loc.to_int(result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Add { src, dst } => {
                if dst.add_float(src, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Sub { src, dst } => {
                if dst.sub_float(src, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Mul { src, dst } => {
                if dst.mul_float(src, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Div { src, dst } => {
                if dst.div_float(src, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Rem { src, dst } => {
                if dst.rem_float(src, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Neg(dst) => {
                if dst.neg_float(result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Pow { src, dst } => {
                if dst.pow_float(src, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::IsLess { a, b, dst } => {
                if a.is_less_than_float(b, dst, result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::IsGreater { a, b, dst } => {
                if a.is_greater_than_float(b, dst, result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::Alloc(loc) => {
                if loc.alloc(result).is_err() {
                    unsupported(self.clone())?
                }
            }
            Self::Free(loc) => {
                if loc.free(result).is_err() {
                    unsupported(self.clone())?
                }
            }

            Self::Call(binding) => {
                let input_cells = binding.input_cells;
                let output_cells = binding.output_cells;

                // `Poke` all the input cells to the FFI channel.
                // Start at the first input cell, which is located
                // at the address stored in the `SP` register minus
                // the number of input cells. The last input cell
                // is located at the address stored in the `SP`
                // register.

                // The address of the first input cell.
                let first_input_cell = SP.deref().offset(1 - (input_cells as isize));

                // The address of the first output cell.
                let first_output_cell = first_input_cell.clone();

                // Poke all the input cells to the FFI channel.
                // First, go to the first input cell.
                first_input_cell.to(result);
                for i in 0..input_cells {
                    // Get the input cell from the tape.
                    result.restore();
                    // Poke the input cell to the FFI channel.
                    result.poke()?;
                    if i < input_cells - 1 {
                        // If this is not the last input cell, go to the next input cell.
                        result.move_pointer(1);
                    }
                }
                first_input_cell
                    .offset(input_cells as isize - 1)
                    .from(result);

                // Call the foreign function.
                result.ffi_call(binding.clone())?;

                // Peek all the output cells from the FFI channel.
                // First, go to the first output cell.
                first_output_cell.to(result);
                for i in 0..output_cells {
                    // Peek the output cell from the FFI channel.
                    result.peek()?;
                    // Store to the output cell on the tape.
                    result.save();
                    if i < output_cells - 1 {
                        // If this is not the last output cell, go to the next output cell.
                        result.move_pointer(1);
                    }
                }
                SP.deref().from(result);
                SP.next(output_cells as isize - input_cells as isize, result);
            }

            Self::Const { vals, dst } => {
                let dst = env.resolve(dst)?;

                result.set_float_vector(vals.clone())?;
                dst.to(result);
                result.store_vector(vals.len());
                dst.from(result);
            }

            Self::PushConst(vals) => {
                SP.deref().offset(1).to(result);
                result.set_float_vector(vals.clone())?;
                result.store_vector(vals.len());
                SP.deref().offset(1).from(result);
                SP.next(vals.len() as isize, result);
            }

            _ => {
                panic!("unimplemented {}", self)
            }
        }
        Ok(())
    }
}

impl fmt::Display for StandardOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CoreOp(op) => write!(f, "{op}"),

            Self::Set(loc, n) => write!(f, "set-f {loc}, {n}"),
            Self::PushConst(items) => {
                write!(f, "push-const-f [")?;
                for (i, n) in items.iter().enumerate() {
                    write!(f, "{n}")?;
                    if i < items.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Self::Const { vals, dst } => {
                write!(f, "const-f {dst}, [")?;
                for (i, n) in vals.iter().enumerate() {
                    write!(f, "{n}")?;
                    if i < vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }

            Self::ToFloat(loc) => write!(f, "to-float {loc}"),
            Self::ToInt(loc) => write!(f, "to-int {loc}"),

            Self::Pow { src, dst } => write!(f, "pow {src}, {dst}"),
            Self::Sqrt(loc) => write!(f, "sqrt {loc}"),

            Self::Add { src, dst } => write!(f, "add-f {src}, {dst}"),
            Self::Sub { src, dst } => write!(f, "sub-f {src}, {dst}"),
            Self::Mul { src, dst } => write!(f, "mul-f {src}, {dst}"),
            Self::Div { src, dst } => write!(f, "div-f {src}, {dst}"),
            Self::Rem { src, dst } => write!(f, "rem-f {src}, {dst}"),
            Self::Neg(loc) => write!(f, "neg-f {loc}"),

            Self::Sin(loc) => write!(f, "sin {loc}"),
            Self::Cos(loc) => write!(f, "cos {loc}"),
            Self::Tan(loc) => write!(f, "tan {loc}"),
            Self::ASin(loc) => write!(f, "asin {loc}"),
            Self::ACos(loc) => write!(f, "acos {loc}"),
            Self::ATan(loc) => write!(f, "atan {loc}"),

            Self::IsGreater { a, b, dst } => write!(f, "gt-f {a}, {b}, {dst}"),
            Self::IsLess { a, b, dst } => write!(f, "lt-f {a}, {b}, {dst}"),

            Self::Alloc(loc) => write!(f, "alloc {loc}"),
            Self::Free(loc) => write!(f, "free {loc}"),

            // Self::Peek(loc) => write!(f, "peek {loc}"),
            // Self::Poke(loc) => write!(f, "poke {loc}"),
            Self::Call(binding) => write!(f, "call {:?}", binding),

            Self::VecSet(dst, vals) => {
                write!(f, "vset-f {dst}, {vals:?}")
            }
            Self::VecAdd { src, dst, size } => write!(f, "vadd-f {src}, {dst}, {size}"),
            Self::VecSub { src, dst, size } => write!(f, "vsub-f {src}, {dst}, {size}"),
            Self::VecMul { src, dst, size } => write!(f, "vmul-f {src}, {dst}, {size}"),
            Self::VecDiv { src, dst, size } => write!(f, "vdiv-f {src}, {dst}, {size}"),
            Self::VecRem { src, dst, size } => write!(f, "vrem-f {src}, {dst}, {size}"),
            Self::VecNeg { dst, size } => write!(f, "vneg-f {dst}, {size}"),
            Self::VecPow { src, dst, size } => write!(f, "vpow {src}, {dst}, {size}"),
            Self::VecSin { dst, size } => write!(f, "vsin {dst}, {size}"),
            Self::VecCos { dst, size } => write!(f, "vcos {dst}, {size}"),
            Self::VecTan { dst, size } => write!(f, "vtan {dst}, {size}"),
            Self::VecASin { dst, size } => write!(f, "vasin {dst}, {size}"),
            Self::VecACos { dst, size } => write!(f, "vacos {dst}, {size}"),
            Self::VecATan { dst, size } => write!(f, "vatan {dst}, {size}"),
            Self::VecGez { src, dst, size } => write!(f, "vgez-f {src}, {dst}, {size}"),
        }
    }
}
