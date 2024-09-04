//! # Core Assembly Variant
//!
//! This variant of the assembly language is intended to be used
//! with the core variant of the virtual machine. It is extremely
//! portable, but minimal.
//!
//! [***Click here to view opcodes!***](./enum.CoreOp.html)
//!
//! ## What kinds of instructions are supported by this variant?
//!
//! This variant attempts to support *as many instructions as possible
//! that can be implemented WITHOUT the standard virtual machine variant*.
//! This includes  instructions for operations like `Copy` (a `memcpy` clone),
//! static stack allocation, `Swap` (which uses a TMP register without the
//! more optimized standard `Swap` instruction), and `DivMod`, which
//! performs a division and modulo operation in a single instruction.
//!
//! ## What kinds of instructions are *not* supported by this variant?
//!
//! Mainly, this variant is lacking in I/O instructions and memory
//! allocation instructions. This is because of the bare bones
//! core virtual machine specification which only includes 2 I/O
//! instructions (Get and Put), and does not include any memory
//! allocation instructions.
//!
//! Standard instructions, like `PutInt`, can be implemented as
//! user defined functions in the core assembly language simply
//! using `Put`, and assuming-standard out, to display the integer in decimal.
use super::{
    location::{FP_STACK, TMP},
    AssemblyProgram, Env, Error, Location, StandardOp, FP, GP, SP, STACK_START, START_OF_FP_STACK,
};
use crate::{
    side_effects::{Input, InputMode, Output, OutputMode},
    vm::{self, VirtualMachineProgram},
};
use serde_derive::{Deserialize, Serialize};
use std::{collections::BTreeSet, fmt};

use log::{info, trace};

/// An assembly program composed of core instructions, which can be assembled
/// into the core virtual machine instructions.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct CoreProgram {
    /// The list of core assembly instructions in the program.
    pub code: Vec<CoreOp>,
    /// A set containining the labels for each function in the program
    /// that has been defined so far. This helps the LIR compiler
    /// determine if a function has been compiled yet or not.
    labels: BTreeSet<String>,
}

/// A default program is an empty program.
impl Default for CoreProgram {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl CoreProgram {
    /// Create a new program of core assembly instructions.
    pub fn new(code: Vec<CoreOp>) -> Self {
        let mut labels = BTreeSet::new();
        for op in &code {
            if let CoreOp::Fn(label) = op {
                labels.insert(label.clone());
            }
        }
        Self { code, labels }
    }

    /// Get the size of the globals in the program.
    fn get_size_of_globals(&self, env: &mut Env) -> Result<usize, Error> {
        trace!("Getting size of globals, this could be an expensive operation...");
        for op in &self.code {
            // Go through all the operations and declare the globals.
            if let CoreOp::Global { name, size } = op {
                env.declare_global(name, *size);
            }
        }

        // Get the size of the globals in the environment after the declarations.
        Ok(env.get_size_of_globals())
    }

    /// Assemble a program of core assembly instructions into the
    /// core virtual machine instructions.
    pub fn assemble(&self, allowed_recursion_depth: usize) -> Result<vm::CoreProgram, Error> {
        // Create the result program.
        let mut result = vm::CoreProgram(vec![]);
        // Create the environment in which to assemble the program.
        let mut env = Env::default();

        // Get the size of the globals
        let size_of_globals = self.get_size_of_globals(&mut env)?;

        // Create the bootstrap code.
        result.comment("BEGIN BOOTSTRAP");

        // Create the stack of frame pointers starting directly after the last register
        // let start_of_fp_stack = F.offset(1);
        START_OF_FP_STACK.copy_address_to(&FP_STACK, &mut result);
        info!(
            "Frame pointer stack begins at {START_OF_FP_STACK:?}, and is {} cells long.",
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
        starting_sp_addr.copy_address_to(&STACK_START, &mut result);
        starting_sp_addr.copy_address_to(&SP, &mut result);

        result.comment("END BOOTSTRAP");

        // Copy the stack pointer to the frame pointer
        SP.copy_to(&FP, &mut result);
        // For all the operations in the program, assemble them.
        for (i, op) in self.code.iter().enumerate() {
            op.assemble(i, &mut env, &mut result)?
        }

        // If there are any unmatched instructions, return an error.
        if let Ok((unmatched, last_instruction)) = env.pop_matching(self.code.len()) {
            return Err(Error::Unmatched(unmatched, last_instruction));
        }

        // Return the result.
        Ok(result.flatten())
    }
}

impl fmt::Display for CoreProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        let mut comment_count = 0;
        for (i, op) in self.code.iter().enumerate() {
            if f.alternate() {
                if let CoreOp::Comment(comment) = op {
                    if f.alternate() {
                        write!(f, "{:8}  ", "")?;
                    }
                    comment_count += 1;
                    writeln!(f, "{}// {}", "   ".repeat(indent), comment,)?;
                    continue;
                }
                write!(f, "{:04x?}: ", i - comment_count)?;
            } else if let CoreOp::Comment(_) = op {
                continue;
            }

            writeln!(
                f,
                "{}{}",
                match op {
                    CoreOp::Fn(_) | CoreOp::If(_) | CoreOp::While(_) => {
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

impl AssemblyProgram for CoreProgram {
    fn op(&mut self, op: CoreOp) {
        // If the operation is a function label, add its label to the set of defined labels.
        if let CoreOp::Fn(name) = &op {
            self.labels.insert(name.clone());
        }

        if let Some(last_core_op) = self.code.last().cloned() {
            match (last_core_op, op) {
                (CoreOp::Push(src, 1), CoreOp::Pop(Some(dst), 1)) => {
                    self.code.pop();
                    self.op(CoreOp::Move {
                        src: src.clone(),
                        dst: dst.clone(),
                    })
                }
                (CoreOp::Next(SP, Some(1) | None), CoreOp::Set(dst, n)) if dst == SP.deref() => {
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
                (_, op) => self.code.push(op),
            }
        } else {
            self.code.push(op)
        }
    }

    fn std_op(&mut self, op: super::StandardOp) -> Result<(), Error> {
        Err(Error::UnsupportedInstruction(op))
    }

    fn is_defined(&self, label: &str) -> bool {
        self.labels.contains(label)
    }

    fn current_instruction(&self) -> usize {
        self.code.len()
    }

    fn get_op(&self, start: usize) -> Option<Result<CoreOp, StandardOp>> {
        self.code.get(start).cloned().map(Ok)
    }
}

/// A core instruction of the assembly language. These are instructions
/// guaranteed to be implemented for every target possible.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum CoreOp {
    Comment(String),
    /// Many instructions to execute; conveniently grouped together.
    /// This is useful for code generation.
    Many(Vec<CoreOp>),

    /// Declare a global variable.
    ///
    /// This will allow the program to reference the global variable
    /// using `$name` in the assembly code, where `name` is the name
    /// of the global variable.
    ///
    /// To access the first element of the global variable, use `$name`.
    /// To access the second element of the global variable, use `$name + 1`.
    ///
    /// To get the address of the first element of the global variable,
    /// use `lea $name, $tmp`.
    Global {
        name: String,
        size: usize,
    },

    /// Set the value of a register, or any location in memory, to a given constant.
    Set(Location, i64),
    /// Set the vector values of a destination.
    VecSet(Location, Vec<i64>),

    /// Set the value of a register, or any location in memory, to the value of a label's ID.
    SetLabel(Location, String),
    /// Get the address of a location, and store it in a destination
    GetAddress {
        addr: Location,
        dst: Location,
    },
    /// Get the address of a location and push it to the stack.
    PushAddress(Location),

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
    ///
    /// `dst = src`
    Move {
        src: Location,
        dst: Location,
    },
    /// Copy a number of cells from a source referenced location to a destination
    /// referenced location.
    ///
    /// This will dereference `src`, copy its contents, and store them at the location pointed
    /// to by `dst`.
    ///
    /// `*dst = *src`
    Copy {
        src: Location,
        dst: Location,
        size: usize,
    },

    /// Swap the values of two locations.
    Swap(Location, Location),

    /// Make this pointer point to the next cell (or the nth next cell).
    Next(Location, Option<isize>),
    /// Make this pointer point to the previous cell (or the nth previous cell).
    Prev(Location, Option<isize>),

    /// Get the address of a location indexed by an offset stored at another location.
    /// Store the result in the destination register.
    ///
    /// This is essentially the runtime equivalent of `dst = addr.offset(offset)`
    Index {
        src: Location,
        offset: Location,
        dst: Location,
    },

    /// Increment the integer value of a location.
    Inc(Location),
    /// Decrement the integer value of a location.
    Dec(Location),
    /// Add an integer value from a source location to a destination location.
    Add {
        src: Location,
        dst: Location,
    },
    /// Subtract a source integer value from a destination location.
    Sub {
        src: Location,
        dst: Location,
    },
    /// Multiply a destination location by a source value.
    Mul {
        src: Location,
        dst: Location,
    },
    /// Divide a destination location by a source value.
    Div {
        src: Location,
        dst: Location,
    },
    /// Store the remainder of the destination modulus the source in the destination.
    Rem {
        src: Location,
        dst: Location,
    },
    /// Divide a destination location by a source value.
    /// Store the quotient in the destination, and the remainder in the source.
    DivRem {
        src: Location,
        dst: Location,
    },
    /// Negate an integer.
    Neg(Location),

    /// Perform a SIMD addition over a vector of integers.
    /// This will add the source vector to the destination vector.
    VecAdd {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD subtraction over a vector of integers.
    /// This will subtract the source vector from the destination vector.
    VecSub {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD multiplication over a vector of integers.
    /// This will multiply the source vector with the destination vector.
    VecMul {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD division over a vector of integers.
    /// This will divide the destination vector by the source vector.
    VecDiv {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD remainder over a vector of integers.
    /// This will store the remainder of the destination vector divided by the source vector.
    VecRem {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Replace a value in memory with its boolean complement.
    Not(Location),
    /// Logical "and" a destination with a source value.
    And {
        src: Location,
        dst: Location,
    },
    /// Logical "or" a destination with a source value.
    Or {
        src: Location,
        dst: Location,
    },

    /// Perform a vector "Not" operation.
    /// This will replace every value in the vector with its boolean complement.
    VecNot {
        size: usize,
        dst: Location,
    },

    /// Perform a vector "And" operation.
    /// This will perform a logical "and" operation on every value in the vectors.
    VecAnd {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a vector "Or" operation.
    /// This will perform a logical "or" operation on every value in the vectors.
    VecOr {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Increment a vector of integers.
    VecInc {
        size: usize,
        dst: Location,
    },
    /// Decrement a vector of integers.
    VecDec {
        size: usize,
        dst: Location,
    },

    /// Left shift a destination by a source value.
    LeftShift {
        src: Location,
        dst: Location,
    },
    /// Logical right shift a destination by a source value.
    /// This will fill the leftmost bits with zeroes.
    LogicalRightShift {
        src: Location,
        dst: Location,
    },
    /// Arithmetic right shift a destination by a source value.
    /// This will fill the leftmost bits with the sign bit.
    ArithmeticRightShift {
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD left shift operation over a vector of integers.
    VecLeftShift {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD logical right shift operation over a vector of integers.
    VecLogicalRightShift {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD arithmetic right shift operation over a vector of integers.
    VecArithmeticRightShift {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Perform a SIMD "is >= zero" operation over a vector of integers.
    /// Store the result in the destination vector.
    VecGez {
        size: usize,
        src: Location,
        dst: Location,
    },

    /// Negate a vector of integers using SIMD.
    VecNeg {
        size: usize,
        dst: Location,
    },

    /// Perform a SIMD pointer arithmetic operation over a vector of integers.
    /// This will add the "offset" to every value in the vector, so that each
    /// pointer in the vector is offset by the same amount.
    VecOffset {
        size: usize,
        dst: Location,
        offset: isize,
    },

    /// Perform a SIMD pointer index operation over a vector of integers.
    /// This will index every pointer in the vector by the integers in the
    /// source vector.
    VecIndex {
        size: usize,
        src: Location,
        offset: Location,
        dst: Location,
    },

    /// Push a number of cells starting at a memory location on the stack.
    Push(Location, usize),
    /// Pop a number of cells from the stack and store it in a memory location.
    Pop(Option<Location>, usize),

    /// Push a number of cells starting at a memory location onto a specified stack.
    /// This will increment the stack's pointer by the number of cells pushed.
    PushTo {
        src: Location,
        sp: Location,
        size: usize,
    },
    /// Pop a number of cells from a specified stack and store it in a memory location.
    /// This will decrement the stack's pointer by the number of cells popped.
    PopFrom {
        sp: Location,
        dst: Option<Location>,
        size: usize,
    },

    /// Store the comparison of "a" and "b" in a destination register.
    /// If "a" is less than "b", store -1. If "a" is greater than "b", store 1.
    /// If "a" is equal to "b", store 0.
    Compare {
        a: Location,
        b: Location,
        dst: Location,
    },
    /// Perform dst = a > b.
    IsGreater {
        a: Location,
        b: Location,
        dst: Location,
    },
    /// Perform dst = a >= b.
    IsGreaterEqual {
        a: Location,
        b: Location,
        dst: Location,
    },
    /// Perform dst = a < b.
    IsLess {
        a: Location,
        b: Location,
        dst: Location,
    },
    /// Perform dst = a <= b.
    IsLessEqual {
        a: Location,
        b: Location,
        dst: Location,
    },
    /// Perform dst = a == b.
    IsEqual {
        a: Location,
        b: Location,
        dst: Location,
    },
    /// Perform dst = a != b.
    IsNotEqual {
        a: Location,
        b: Location,
        dst: Location,
    },

    /// Get a value from the input device / interface and store it in a destination register.
    Get(Location, Input),
    /// Put a value from a source register to the output device / interface.
    Put(Location, Output),
    /// Store a list of values at a source location. Then, store the address past the
    /// last value into the destination location.
    Array {
        src: Location,
        dst: Location,
        vals: Vec<i64>,
    },

    /// Write a set of values to a memory location.
    Const {
        dst: Location,
        vals: Vec<i64>,
    },
    /// Push a const to the stack.
    PushConst(Vec<i64>),

    BitwiseNand {
        src: Location,
        dst: Location,
    },
    BitwiseXor {
        src: Location,
        dst: Location,
    },
    BitwiseOr {
        src: Location,
        dst: Location,
    },
    BitwiseNor {
        src: Location,
        dst: Location,
    },
    BitwiseAnd {
        src: Location,
        dst: Location,
    },
    BitwiseNot(Location),

    // Perform a SIMD bitwise "and" operation over a vector of integers.
    VecBitwiseAnd {
        size: usize,
        src: Location,
        dst: Location,
    },
    // Perform a SIMD bitwise "or" operation over a vector of integers.
    VecBitwiseOr {
        size: usize,
        src: Location,
        dst: Location,
    },
    // Perform a SIMD bitwise "xor" operation over a vector of integers.
    VecBitwiseXor {
        size: usize,
        src: Location,
        dst: Location,
    },
    // Perform a SIMD bitwise "nor" operation over a vector of integers.
    VecBitwiseNor {
        size: usize,
        src: Location,
        dst: Location,
    },
    // Perform a SIMD bitwise "nand" operation over a vector of integers.
    VecBitwiseNand {
        size: usize,
        src: Location,
        dst: Location,
    },
    // Perform a SIMD bitwise "not" operation over a vector of integers.
    VecBitwiseNot {
        size: usize,
        dst: Location,
    },
}

impl CoreOp {
    /// Put a string literal as UTF-8 to the output device.
    pub fn put_string(msg: impl ToString, dst: Output) -> Self {
        Self::Many(
            msg.to_string()
                // For every character
                .chars()
                // Set the TMP register to the character,
                // and Put the TMP register.
                .map(|ch| Self::Many(vec![Self::Set(TMP, ch as i64), Self::Put(TMP, dst.clone())]))
                .collect(),
        )
    }

    /// Push a string literal as UTF-8 to the stack.
    pub fn push_string(msg: impl ToString) -> Self {
        let mut vals: Vec<i64> = msg.to_string().chars().map(|c| c as i64).collect();
        vals.push(0);
        Self::Many(vec![
            Self::Array {
                src: SP.deref().offset(1),
                vals,
                dst: SP,
            },
            Self::Prev(SP, None),
        ])
    }

    /// Allocate a string on the stack, and store its address in a destination register.
    pub fn stack_alloc_string(dst: Location, text: impl ToString) -> Self {
        let mut vals: Vec<i64> = text.to_string().chars().map(|c| c as i64).collect();
        vals.push(0);
        Self::Many(vec![
            Self::GetAddress {
                addr: SP.deref().offset(1),
                dst,
            },
            Self::Array {
                src: SP.deref().offset(1),
                vals,
                dst: SP,
            },
            Self::Prev(SP, None),
        ])
    }

    pub(super) fn assemble(
        &self,
        current_instruction: usize,
        env: &mut Env,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        match self {
            CoreOp::VecLeftShift { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_left_shift(&src, *size, result);
            }

            CoreOp::VecLogicalRightShift { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_logical_right_shift(&src, *size, result);
            }

            CoreOp::VecArithmeticRightShift { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_arithmetic_right_shift(&src, *size, result);
            }

            CoreOp::VecSet(dst, vals) => {
                let dst = env.resolve(dst)?;
                result.set_vector(vals.clone());
                dst.to(result);
                result.store_vector(vals.len());
                dst.from(result);
            }

            CoreOp::VecAdd { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_add(&src, *size, result);
            }

            CoreOp::VecSub { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_sub(&src, *size, result);
            }

            CoreOp::VecMul { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_mul(&src, *size, result);
            }

            CoreOp::VecDiv { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_div(&src, *size, result);
            }

            CoreOp::VecRem { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_rem(&src, *size, result);
            }

            CoreOp::VecNot { size, dst } => {
                let dst = env.resolve(dst)?;

                dst.vec_not(*size, result);
            }

            CoreOp::VecAnd { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_and(&src, *size, result);
            }

            CoreOp::VecOr { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_or(&src, *size, result);
            }

            CoreOp::VecIndex {
                size,
                offset,
                src,
                dst,
            } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;
                src.vec_copy_to(&dst, *size, result);
                dst.vec_index(offset, *size, result);
            }

            CoreOp::VecOffset { size, dst, offset } => {
                let dst = env.resolve(dst)?;

                dst.vec_offset(*offset, *size, result);
            }

            CoreOp::VecGez { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;
                src.vec_copy_to(&dst, *size, result);
                dst.vec_whole_int(*size, result);
            }

            CoreOp::VecInc { size, dst } => {
                let dst = env.resolve(dst)?;

                dst.vec_inc(*size, result);
            }

            CoreOp::VecDec { size, dst } => {
                let dst = env.resolve(dst)?;

                dst.vec_dec(*size, result);
            }

            CoreOp::VecNeg { size, dst } => {
                let dst = env.resolve(dst)?;

                dst.vec_neg(*size, result);
            }

            CoreOp::VecBitwiseAnd { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_bitwise_and(&src, *size, result);
            }

            CoreOp::VecBitwiseOr { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_bitwise_or(&src, *size, result);
            }

            CoreOp::VecBitwiseXor { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_bitwise_xor(&src, *size, result);
            }

            CoreOp::VecBitwiseNor { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_bitwise_nor(&src, *size, result);
            }

            CoreOp::VecBitwiseNand { size, src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.vec_bitwise_nand(&src, *size, result);
            }

            CoreOp::VecBitwiseNot { size, dst } => {
                let dst = env.resolve(dst)?;

                dst.vec_bitwise_not(*size, result);
            }

            CoreOp::Many(many) => {
                for op in many {
                    op.assemble(current_instruction, env, result)?
                }
            }

            CoreOp::Comment(comment) => result.comment(comment),
            CoreOp::Global { name, size } => {
                // Declare the global in the environment.
                env.declare_global(name, *size);
            }

            CoreOp::Array { src, vals, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                // For every character in the message
                // Go to the top of the stack, and push the ASCII value of the character
                src.to(result);
                result.set_vector(vals.clone());
                result.store_vector(vals.len());
                result.move_pointer(vals.len() as isize);
                // Save where we ended up
                result.where_is_pointer();
                // Move the pointer back where we came from
                src.offset(vals.len() as isize).from(result);
                // Save where we ended up to the destination
                dst.to(result);
                result.save();
                dst.from(result);
            }

            CoreOp::Const { dst, vals } => {
                let dst = env.resolve(dst)?;

                result.set_vector(vals.clone());
                dst.to(result);
                result.store_vector(vals.len());
                dst.from(result);
            }

            CoreOp::PushConst(vals) => {
                SP.deref().offset(1).to(result);
                result.set_vector(vals.clone());
                result.store_vector(vals.len());
                SP.deref().offset(1).from(result);
                SP.next(vals.len() as isize, result);
            }
            CoreOp::PushAddress(addr) => {
                let addr = env.resolve(addr)?;
                SP.next(1, result);
                addr.copy_address_to(&SP.deref(), result);
            }
            CoreOp::GetAddress { addr, dst } => {
                let addr = env.resolve(addr)?;
                let dst = env.resolve(dst)?;

                addr.copy_address_to(&dst, result)
            }
            CoreOp::Next(dst, count) => {
                let dst = env.resolve(dst)?;

                dst.next(count.unwrap_or(1), result)
            }
            CoreOp::Prev(dst, count) => {
                let dst = env.resolve(dst)?;

                dst.prev(count.unwrap_or(1), result)
            }
            CoreOp::Index { src, offset, dst } => {
                let src = env.resolve(src)?;
                let offset = env.resolve(offset)?;
                let dst = env.resolve(dst)?;

                // Store the address to index in the register
                src.restore_from(result);
                // Goto the offset
                offset.to(result);
                // Index the address in the register with the offset
                result.index();
                // Go back to the default position
                offset.from(result);
                // Save the index'd address in the register to the destination
                dst.save_to(result);
            }

            CoreOp::Set(dst, value) => {
                let dst = env.resolve(dst)?;
                dst.set(*value, result)
            }
            CoreOp::SetLabel(dst, name) => {
                let dst = env.resolve(dst)?;
                dst.set(env.get_label(name, current_instruction)? as i64, result)
            }

            CoreOp::Call(src) => {
                let src = env.resolve(src)?;
                src.restore_from(result);
                result.call();
            }

            CoreOp::CallLabel(name) => {
                result.set_register(env.get_label(name, current_instruction)? as i64);
                result.call();
            }

            CoreOp::Return => {
                FP.pop_from(&FP_STACK, result);
                result.ret();
            }

            CoreOp::Fn(name) => {
                // Declare the function in the environment.
                env.declare_label(name);
                // Push this instruction to the stack of instructions
                // matched with `End`.
                env.push_matching(self, current_instruction);
                // Start the function
                result.begin_function();
                // Push the frame pointer to the frame pointer stack
                FP.push_to(&FP_STACK, result);
                // Overwrite the old frame pointer with the stack pointer
                SP.copy_to(&FP, result);
            }
            CoreOp::While(src) => {
                let src = env.resolve(src)?;

                // Read the condition
                src.restore_from(result);
                // Begin the while loop
                result.begin_while();
                // Push this instruction to the stack of instructions
                // matched with `End`.
                env.push_matching(self, current_instruction);
            }
            CoreOp::If(src) => {
                let src = env.resolve(src)?;

                // Read the condition
                src.restore_from(result);
                // Begin the if statement
                result.begin_if();
                // Push this instruction to the stack of instructions
                // matched with `End`.
                env.push_matching(self, current_instruction);
            }
            CoreOp::Else => {
                if let Ok((CoreOp::If(_), _)) = env.pop_matching(current_instruction) {
                    // If the last block-creating instruction was an `If`,
                    // begin the else.
                    result.begin_else();
                    env.push_matching(self, current_instruction);
                } else {
                    // Otherwise, we've encountered invalid syntax.
                    return Err(Error::Unexpected(CoreOp::Else, current_instruction));
                }
            }
            CoreOp::End => {
                // Get the matching instruction for this `End` declaration.
                match env.pop_matching(current_instruction) {
                    Ok((CoreOp::Fn(_), _)) => {
                        // If it's the end of a function, return from the function.
                        CoreOp::Return.assemble(current_instruction, env, result)?
                    }
                    Ok((CoreOp::While(src), _)) => {
                        // If it's the end of a loop, reread the condition.
                        src.restore_from(result);
                    }
                    // If it's an if or else statement, there's no setup we need to do.
                    Ok((CoreOp::If(_), _)) | Ok((CoreOp::Else, _)) => {}
                    // If there was no matching instruction, or a non-block-creating
                    // instruction, then the syntax is invalid.
                    Ok(_) | Err(_) => {
                        return Err(Error::Unmatched(CoreOp::End, current_instruction))
                    }
                }
                // Terminate the block.
                result.end();
            }

            CoreOp::Move { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;
                src.copy_to(&dst, result)
            }

            CoreOp::Swap(a, b) => {
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.copy_to(&TMP, result);
                b.copy_to(&a, result);
                TMP.copy_to(&b, result);
            }

            CoreOp::Inc(dst) => env.resolve(dst)?.inc(result),
            CoreOp::Dec(dst) => env.resolve(dst)?.dec(result),

            CoreOp::Add { src, dst } => env.resolve(dst)?.add(src, result),
            CoreOp::Sub { src, dst } => env.resolve(dst)?.sub(src, result),
            CoreOp::Mul { src, dst } => env.resolve(dst)?.mul(src, result),
            CoreOp::Div { src, dst } => env.resolve(dst)?.div(src, result),
            CoreOp::Rem { src, dst } => env.resolve(dst)?.rem(src, result),
            CoreOp::LeftShift { src, dst } => env.resolve(dst)?.left_shift(src, result),
            CoreOp::LogicalRightShift { src, dst } => {
                env.resolve(dst)?.logical_right_shift(src, result)
            }
            CoreOp::ArithmeticRightShift { src, dst } => {
                env.resolve(dst)?.arithmetic_right_shift(src, result)
            }

            CoreOp::DivRem { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                src.copy_to(&TMP, result);
                dst.copy_to(&src, result);
                dst.div(&TMP, result);
                src.rem(&TMP, result);
            }
            CoreOp::Neg(dst) => {
                let dst = env.resolve(dst)?;
                dst.neg(result);
            }

            Self::BitwiseNand { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.bitwise_nand(&src, result);
            }
            Self::BitwiseXor { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.bitwise_xor(&src, result);
            }
            Self::BitwiseOr { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.bitwise_or(&src, result);
            }
            Self::BitwiseNor { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.bitwise_nor(&src, result);
            }
            Self::BitwiseAnd { src, dst } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                dst.bitwise_and(&src, result);
            }
            Self::BitwiseNot(dst) => {
                let dst = env.resolve(dst)?;

                dst.bitwise_not(result);
            }

            CoreOp::Not(dst) => env.resolve(dst)?.not(result),
            CoreOp::And { src, dst } => env.resolve(dst)?.and(&env.resolve(src)?, result),
            CoreOp::Or { src, dst } => env.resolve(dst)?.or(&env.resolve(src)?, result),

            CoreOp::PushTo { sp, src, size } => {
                let sp = env.resolve(sp)?;
                let src = env.resolve(src)?;
                let size = *size;

                src.to(result);
                result.load_vector(size);
                src.from(result);

                sp.deref().offset(1).to(result);
                result.store_vector(size);
                sp.deref().offset(1).from(result);

                sp.next(size as isize, result);
            }

            CoreOp::PopFrom { sp, dst, size } => {
                let sp = env.resolve(sp)?;
                let size = *size as isize;

                if size == 0 {
                    return Ok(());
                }

                if let Some(dst) = dst {
                    let dst = env.resolve(dst)?;
                    sp.prev(size, result);
                    sp.deref().offset(1).to(result);
                    result.load_vector(size as usize);
                    sp.deref().offset(1).from(result);
                    dst.to(result);
                    result.store_vector(size as usize);
                    dst.from(result);
                } else {
                    sp.prev(size, result)
                }
            }
            CoreOp::Push(src, size) => {
                let src = env.resolve(src)?;
                if *size == 0 {
                    return Ok(());
                }

                CoreOp::PushTo {
                    sp: SP,
                    src,
                    size: *size,
                }
                .assemble(current_instruction, env, result)?
            }
            CoreOp::Pop(dst, size) => {
                // If the destination is None, then we're just popping the stack into oblivion.
                // Otherwise, we're popping the stack into `Some` destination.
                // If we have a destination, resolve it. If we can't resolve it, return an error.
                let dst = dst.as_ref().map(|dst| env.resolve(dst)).transpose()?;

                CoreOp::PopFrom {
                    sp: SP,
                    dst,
                    size: *size,
                }
                .assemble(current_instruction, env, result)?
            }

            CoreOp::IsGreater { dst, a, b } => {
                let dst = env.resolve(dst)?;
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.is_greater_than(&b, &dst, result)
            }
            CoreOp::IsGreaterEqual { dst, a, b } => {
                let dst = env.resolve(dst)?;
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.is_greater_or_equal_to(&b, &dst, result)
            }
            CoreOp::IsLess { dst, a, b } => {
                let dst = env.resolve(dst)?;
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.is_less_than(&b, &dst, result)
            }
            CoreOp::IsLessEqual { dst, a, b } => {
                let dst = env.resolve(dst)?;
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.is_less_or_equal_to(&b, &dst, result)
            }
            CoreOp::IsEqual { dst, a, b } => {
                let dst = env.resolve(dst)?;
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.is_equal(&b, &dst, result)
            }
            CoreOp::IsNotEqual { dst, a, b } => {
                let dst = env.resolve(dst)?;
                let a = env.resolve(a)?;
                let b = env.resolve(b)?;
                a.is_not_equal(&b, &dst, result)
            }

            CoreOp::Compare { dst, a, b } => {
                let dst = &env.resolve(dst)?;
                let a = &env.resolve(a)?;
                let b = &env.resolve(b)?;
                a.is_greater_than(b, dst, result);
                dst.restore_from(result);
                result.begin_if();
                result.set_register(1);
                result.begin_else();
                a.is_less_than(b, dst, result);
                dst.restore_from(result);
                result.begin_if();
                result.set_register(-1);
                result.begin_else();
                result.set_register(0);
                result.end();
                result.end();
                dst.save_to(result);
            }

            CoreOp::Get(dst, input) => {
                let dst = env.resolve(dst)?;
                // result.get(input.clone());
                // dst.save_to(result)
                dst.get(input.clone(), result)
            }
            CoreOp::Put(src, output) => {
                let src = env.resolve(src)?;
                // dst.restore_from(result);
                // result.put(output.clone())
                src.put(output.clone(), result)
            }

            CoreOp::Copy { src, dst, size } => {
                let src = env.resolve(src)?;
                let dst = env.resolve(dst)?;

                if src == dst {
                    return Ok(());
                }

                src.to(result);
                result.load_vector(*size);
                src.from(result);
                dst.to(result);
                result.store_vector(*size);
                dst.from(result);
            }
        }

        Ok(())
    }
}

impl fmt::Display for CoreOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Many(ops) => {
                for op in ops {
                    write!(f, "{} ", op)?;
                }
                Ok(())
            }
            Self::Comment(comment) => write!(f, "// {comment}"),
            Self::Global { name, size } => write!(f, "global ${name}, {size}"),

            Self::VecSet(dst, vals) => {
                write!(f, "vset {dst}, {vals:?}")
            }
            Self::VecLeftShift { size, src, dst } => {
                write!(f, "vlsh {src}, {dst}, {size}")
            }

            Self::VecLogicalRightShift { size, src, dst } => {
                write!(f, "vlrsh {src}, {dst}, {size}")
            }

            Self::VecArithmeticRightShift { size, src, dst } => {
                write!(f, "varsh {src}, {dst}, {size}")
            }

            Self::VecAdd { size, src, dst } => {
                write!(f, "vadd {src}, {dst}, {size}")
            }

            Self::VecSub { size, src, dst } => {
                write!(f, "vsub {src}, {dst}, {size}")
            }

            Self::VecMul { size, src, dst } => {
                write!(f, "vmul {src}, {dst}, {size}")
            }

            Self::VecDiv { size, src, dst } => {
                write!(f, "vdiv {src}, {dst}, {size}")
            }

            Self::VecRem { size, src, dst } => {
                write!(f, "vrem {src}, {dst}, {size}")
            }

            Self::VecNot { size, dst } => {
                write!(f, "vnot {dst}, {size}")
            }

            Self::VecAnd { size, src, dst } => {
                write!(f, "vand {src}, {dst}, {size}")
            }

            Self::VecOr { size, src, dst } => {
                write!(f, "vor {src}, {dst}, {size}")
            }

            Self::VecInc { size, dst } => {
                write!(f, "vinc {dst}, {size}")
            }

            Self::VecDec { size, dst } => {
                write!(f, "vdec {dst}, {size}")
            }

            Self::VecBitwiseAnd { size, src, dst } => {
                write!(f, "vband {src}, {dst}, {size}")
            }

            Self::VecBitwiseOr { size, src, dst } => {
                write!(f, "vbor {src}, {dst}, {size}")
            }

            Self::VecBitwiseXor { size, src, dst } => {
                write!(f, "vbxor {src}, {dst}, {size}")
            }

            Self::VecBitwiseNor { size, src, dst } => {
                write!(f, "vbnor {src}, {dst}, {size}")
            }

            Self::VecBitwiseNand { size, src, dst } => {
                write!(f, "vbnand {src}, {dst}, {size}")
            }

            Self::VecBitwiseNot { size, dst } => {
                write!(f, "vbnot {dst}, {size}")
            }

            Self::VecGez { size, src, dst } => {
                write!(f, "vgez {src}, {dst}, {size}")
            }

            Self::VecNeg { size, dst } => {
                write!(f, "vneg {dst}, {size}")
            }

            Self::VecOffset { size, dst, offset } => {
                write!(f, "voffset {dst}, {offset}, {size}")
            }

            Self::VecIndex {
                size,
                offset,
                src,
                dst,
            } => {
                write!(f, "vindex {src}, {offset}, {dst}, {size}")
            }

            Self::LeftShift { src, dst } => {
                write!(f, "lsh {src}, {dst}")
            }

            Self::LogicalRightShift { src, dst } => {
                write!(f, "lrsh {src}, {dst}")
            }

            Self::ArithmeticRightShift { src, dst } => {
                write!(f, "arsh {src}, {dst}")
            }

            Self::PushTo { src, sp, size } => {
                write!(f, "push-to {src}, {sp}, {size}")
            }
            Self::PopFrom { sp, dst, size } => {
                write!(f, "pop {sp}")?;
                if let Some(dst) = dst {
                    write!(f, ", {dst}")?
                }
                write!(f, ", {size}")
            }

            Self::Push(loc, size) => {
                write!(f, "push {loc}")?;
                if *size != 1 {
                    write!(f, ", {size}")?
                }
                Ok(())
            }
            Self::Const { dst, vals } => {
                write!(f, "const {dst}, [")?;
                for (i, val) in vals.iter().enumerate() {
                    if i < vals.len() - 1 {
                        write!(f, "{val}, ")?;
                    } else {
                        write!(f, "{val}")?;
                    }
                }
                write!(f, "]")
            }
            Self::PushConst(vals) => {
                write!(f, "push-const [")?;
                for (i, val) in vals.iter().enumerate() {
                    if i < vals.len() - 1 {
                        write!(f, "{val}, ")?;
                    } else {
                        write!(f, "{val}")?;
                    }
                }
                write!(f, "]")
            }
            Self::Pop(loc, size) => {
                write!(f, "pop")?;
                if let Some(dst) = loc {
                    write!(f, " {dst}")?;
                    if *size != 1 {
                        write!(f, ", {size}")?
                    }
                } else if *size != 1 {
                    write!(f, " {size}")?
                }
                Ok(())
            }

            Self::Call(loc) => write!(f, "call {loc}"),
            Self::CallLabel(label) => write!(f, "call @{label}"),

            Self::PushAddress(addr) => write!(f, "lea-push {addr}"),
            Self::GetAddress { addr, dst } => write!(f, "lea {addr}, {dst}"),
            Self::Return => write!(f, "ret"),

            Self::Fn(label) => write!(f, "fun @{label}"),
            Self::While(cond) => write!(f, "while {cond}"),
            Self::If(cond) => write!(f, "if {cond}"),
            Self::Else => write!(f, "else"),
            Self::End => write!(f, "end"),

            Self::Move { src, dst } => write!(f, "mov {src}, {dst}"),
            Self::Copy { src, dst, size } => write!(f, "copy {src}, {dst}, {size}"),
            Self::Swap(a, b) => write!(f, "swap {a}, {b}"),
            Self::Next(loc, size) => {
                write!(f, "next {loc}")?;
                if let Some(n) = size {
                    write!(f, ", {n}")?
                }
                Ok(())
            }
            Self::Prev(loc, size) => {
                write!(f, "prev {loc}")?;
                if let Some(n) = size {
                    write!(f, ", {n}")?
                }
                Ok(())
            }
            Self::Index { src, offset, dst } => {
                write!(f, "index {src}, {offset}, {dst}")
            }
            Self::Inc(loc) => write!(f, "inc {loc}"),
            Self::Dec(loc) => write!(f, "dec {loc}"),

            Self::Set(loc, n) => write!(f, "set {loc}, {n}"),
            Self::SetLabel(loc, label) => write!(f, "set {loc}, @{label}"),

            Self::BitwiseNand { src, dst } => write!(f, "bitwise-nand {src}, {dst}"),
            Self::BitwiseAnd { src, dst } => write!(f, "bitwise-and {src}, {dst}"),
            Self::BitwiseXor { src, dst } => write!(f, "bitwise-xor {src}, {dst}"),
            Self::BitwiseOr { src, dst } => write!(f, "bitwise-or {src}, {dst}"),
            Self::BitwiseNor { src, dst } => write!(f, "bitwise-nor {src}, {dst}"),
            Self::BitwiseNot(loc) => write!(f, "bitwise-not {loc}"),

            Self::And { src, dst } => write!(f, "and {src}, {dst}"),
            Self::Or { src, dst } => write!(f, "or {src}, {dst}"),
            Self::Not(loc) => write!(f, "not {loc}"),

            Self::Add { src, dst } => write!(f, "add {src}, {dst}"),
            Self::Sub { src, dst } => write!(f, "sub {src}, {dst}"),
            Self::Mul { src, dst } => write!(f, "mul {src}, {dst}"),
            Self::Div { src, dst } => write!(f, "div {src}, {dst}"),
            Self::Rem { src, dst } => write!(f, "rem {src}, {dst}"),
            Self::DivRem { src, dst } => write!(f, "div-rem {src}, {dst}"),
            Self::Neg(loc) => write!(f, "neg {loc}"),

            Self::Array { src, vals, dst } => write!(f, "array {src}, {vals:?}, {dst}"),

            Self::Compare { a, b, dst } => write!(f, "cmp {a}, {b}, {dst}"),
            Self::IsGreater { a, b, dst } => write!(f, "gt {a}, {b}, {dst}"),
            Self::IsGreaterEqual { a, b, dst } => write!(f, "gte {a}, {b}, {dst}"),
            Self::IsLess { a, b, dst } => write!(f, "lt {a}, {b}, {dst}"),
            Self::IsLessEqual { a, b, dst } => write!(f, "lte {a}, {b}, {dst}"),
            Self::IsEqual { a, b, dst } => write!(f, "eq {a}, {b}, {dst}"),
            Self::IsNotEqual { a, b, dst } => write!(f, "neq {a}, {b}, {dst}"),

            Self::Get(
                loc,
                Input {
                    mode: InputMode::StdinChar,
                    ..
                },
            ) => write!(f, "get-char {loc}"),
            Self::Get(
                loc,
                Input {
                    mode: InputMode::StdinInt,
                    ..
                },
            ) => write!(f, "get-int {loc}"),
            Self::Get(
                loc,
                Input {
                    mode: InputMode::StdinFloat,
                    ..
                },
            ) => write!(f, "get-float {loc}"),
            Self::Get(loc, i) => write!(f, "get {loc}, {i}"),
            Self::Put(
                loc,
                Output {
                    mode: OutputMode::StdoutChar,
                    ..
                },
            ) => write!(f, "put-char {loc}"),
            Self::Put(
                loc,
                Output {
                    mode: OutputMode::StdoutInt,
                    ..
                },
            ) => write!(f, "put-int {loc}"),
            Self::Put(
                loc,
                Output {
                    mode: OutputMode::StdoutFloat,
                    ..
                },
            ) => write!(f, "put-float {loc}"),
            Self::Put(loc, o) => write!(f, "put {loc}, {o}"),
        }
    }
}
