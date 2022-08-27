//! # Core Assembly Variant
//!
//! This variant of the assembly language is intended to be used
//! with the core variant of the virtual machine. It is extremely
//! portable, but minimal.
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
    AssemblyProgram, Env, Error, Location, F, FP, SP,
};
use crate::vm::{self, VirtualMachineProgram};
use core::fmt;

/// A program composed of core instructions, which can be assembled
/// into the core virtual machine instructions.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CoreProgram(pub Vec<CoreOp>);

impl CoreProgram {
    /// Assemble a program of core assembly instructions into the
    /// core virtual machine instructions.
    pub fn assemble(&self, allowed_recursion_depth: usize) -> Result<vm::CoreProgram, Error> {
        let mut result = vm::CoreProgram(vec![]);
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

        if let Ok((unmatched, last_instruction)) = env.pop_matching(self.0.len()) {
            return Err(Error::Unmatched(unmatched, last_instruction));
        }

        Ok(result)
    }
}

impl fmt::Debug for CoreProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut indent = 0;
        for (i, op) in self.0.iter().enumerate() {
            if let CoreOp::Comment(comment) = op {
                writeln!(
                    f,
                    "{:04x?}: {}// {}",
                    i,
                    "   ".repeat(indent),
                    comment,
                )?;
                continue;
            }

            writeln!(
                f,
                "{:04x?}: {}{:?}",
                i,
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
        self.0.push(op)
    }

    fn std_op(&mut self, op: super::StandardOp) -> Result<(), Error> {
        Err(Error::UnsupportedInstruction(op))
    }
}

/// A core instruction of the assembly language. These are instructions
/// guaranteed to be implemented for every target possible.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CoreOp {
    Comment(String),
    Many(Vec<CoreOp>),

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
    Get(Location),
    /// Put a value from a source register to the output device / interface.
    Put(Location),
    /// Store a list of values at a source location. Then, store the address past the
    /// last value into the destination location.
    Array {
        src: Location,
        dst: Location,
        vals: Vec<isize>,
    },
}

impl CoreOp {
    /// Put a string literal as UTF-8 to the output device.
    pub fn put_string(msg: impl ToString) -> Self {
        Self::Many(
            msg.to_string()
                // For every character
                .chars()
                // Set the TMP register to the character,
                // and Put the TMP register.
                .map(|ch| Self::Many(vec![Self::Set(TMP, ch as isize), Self::Put(TMP)]))
                .collect(),
        )
    }

    pub fn push_string(msg: impl ToString) -> Self {
        let mut vals: Vec<isize> = msg.to_string().chars().map(|c| c as isize).collect();
        vals.push(0);
        Self::Many(vec![
            Self::Array {
                src: SP.deref().offset(1),
                vals,
                dst: SP,
            },
            Self::Prev(SP, None)
        ])
    }

    pub fn stack_alloc_cells(dst: Location, vals: Vec<isize>) -> Self {
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
            Self::Prev(SP, None)
        ])
    }

    pub fn stack_alloc_string(dst: Location, text: impl ToString) -> Self {
        let mut vals: Vec<isize> = text.to_string().chars().map(|c| c as isize).collect();
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
            Self::Prev(SP, None)
        ])
    }

    pub(super) fn assemble(
        &self,
        current_instruction: usize,
        env: &mut Env,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        match self {
            CoreOp::Many(many) => {
                for op in many {
                    op.assemble(current_instruction, env, result)?
                }
            }
            CoreOp::Comment(comment) => result.comment(comment),

            CoreOp::Array { src, vals, dst } => {
                // For every character in the message
                // Go to the top of the stack, and push the ASCII value of the character
                src.to(result);
                for val in vals {
                    // Set the register to the ASCII value
                    result.set_register(*val);
                    // Save the register to the memory location
                    result.save();
                    // Move to the next cell
                    result.move_pointer(1);
                }
                // Save where we ended up
                result.where_is_pointer();
                // Move the pointer back where we came from
                src.offset(vals.len() as isize).from(result);
                // Save where we ended up to the destination
                dst.to(result);
                result.save();
                dst.from(result);
            }

            CoreOp::GetAddress { addr, dst } => addr.copy_address_to(dst, result),
            CoreOp::Next(dst, count) => dst.next(count.unwrap_or(1), result),
            CoreOp::Prev(dst, count) => dst.prev(count.unwrap_or(1), result),
            CoreOp::Index { src, offset, dst } => {
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

            CoreOp::Set(dst, value) => dst.set(*value, result),
            CoreOp::SetLabel(dst, name) => {
                dst.set(env.get(name, current_instruction)? as isize, result)
            }

            CoreOp::Call(src) => {
                src.restore_from(result);
                result.call();
            }

            CoreOp::CallLabel(name) => {
                result.set_register(env.get(name, current_instruction)? as isize);
                result.call();
            }

            CoreOp::Return => {
                FP.pop_from(&FP_STACK, result);
                result.ret();
            }

            CoreOp::Fn(name) => {
                // Declare the function in the environment.
                env.declare(name);
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
                // Read the condition
                src.restore_from(result);
                // Begin the while loop
                result.begin_while();
                // Push this instruction to the stack of instructions
                // matched with `End`.
                env.push_matching(self, current_instruction);
            }
            CoreOp::If(src) => {
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

            CoreOp::Move { src, dst } => src.copy_to(dst, result),

            CoreOp::Swap(a, b) => {
                // Get the value in `a`
                a.restore_from(result);
                // Go to `b`
                b.to(result);
                // Swap the value in `b` with `a`
                result.swap();
                // Go back to `a`
                b.from(result);
                a.to(result);
                // Swap the value from `b` back into `a`
                result.swap();
                // Return to the default position.
                a.from(result);
            }

            CoreOp::Inc(dst) => dst.inc(result),
            CoreOp::Dec(dst) => dst.dec(result),

            CoreOp::Add { src, dst } => dst.add(src, result),
            CoreOp::Sub { src, dst } => dst.sub(src, result),
            CoreOp::Mul { src, dst } => dst.mul(src, result),
            CoreOp::Div { src, dst } => dst.div(src, result),
            CoreOp::Rem { src, dst } => dst.rem(src, result),
            CoreOp::DivRem { src, dst } => {
                src.copy_to(&TMP, result);
                dst.copy_to(src, result);
                dst.div(&TMP, result);
                src.rem(&TMP, result);
            }
            CoreOp::Neg(dst) => {
                result.set_register(-1);
                dst.to(result);
                result.op(vm::CoreOp::Mul);
                result.save();
                dst.from(result)
            }

            CoreOp::Not(dst) => dst.not(result),
            CoreOp::And { src, dst } => dst.and(src, result),
            CoreOp::Or { src, dst } => dst.or(src, result),

            CoreOp::PushTo { sp, src, size } => {
                for i in 0..*size {
                    src.offset(i as isize)
                        .copy_to(&sp.deref().offset(i as isize + 1), result);
                }
                sp.next(*size as isize, result);
            }

            CoreOp::PopFrom { sp, dst, size } => {
                if let Some(dst) = dst {
                    for i in 1..=*size {
                        dst.offset((*size - i) as isize).pop_from(sp, result)
                    }
                } else {
                    sp.prev(*size as isize, result)
                }
            }
            CoreOp::Push(src, size) => CoreOp::PushTo {
                sp: SP,
                src: src.clone(),
                size: *size,
            }
            .assemble(current_instruction, env, result)?,
            CoreOp::Pop(dst, size) => CoreOp::PopFrom {
                sp: SP,
                dst: dst.clone(),
                size: *size,
            }
            .assemble(current_instruction, env, result)?,

            CoreOp::IsGreater { dst, a, b } => a.is_greater_than(b, dst, result),
            CoreOp::IsGreaterEqual { dst, a, b } => a.is_greater_or_equal_to(b, dst, result),
            CoreOp::IsLess { dst, a, b } => a.is_less_than(b, dst, result),
            CoreOp::IsLessEqual { dst, a, b } => a.is_less_or_equal_to(b, dst, result),
            CoreOp::IsNotEqual { dst, a, b } => a.is_not_equal(b, dst, result),
            CoreOp::IsEqual { dst, a, b } => a.is_equal(b, dst, result),

            CoreOp::Compare { dst, a, b } => {
                a.copy_to(dst, result);
                a.is_greater_than(b, dst, result);
                result.begin_if();
                result.set_register(1);
                result.begin_else();
                a.copy_to(dst, result);
                a.is_less_than(b, dst, result);
                result.begin_if();
                result.set_register(-1);
                result.begin_else();
                result.set_register(0);
                result.end();
                result.end();
                dst.save_to(result);
            }

            CoreOp::Get(dst) => {
                result.get();
                dst.save_to(result)
            }
            CoreOp::Put(dst) => {
                dst.restore_from(result);
                result.put()
            }

            CoreOp::Copy { src, dst, size } => {
                for i in 0..*size {
                    src.offset(i as isize)
                        .copy_to(&dst.offset(i as isize), result);
                }
            }
        }

        Ok(())
    }
}
