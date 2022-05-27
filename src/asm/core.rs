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
//! instructions (GetChar and PutChar), and does not include
//! any memory allocation instructions.
//! 
//! Standard instructions, like `PutInt`, can be implemented as
//! user defined functions in the core assembly language simply
//! using `PutChar` to display the integer in decimal.
use super::{Location, TMP, FP, SP, BOTTOM_OF_STACK, Env, Error};
use crate::vm::{self, VirtualMachineProgram};

/// A program composed of core instructions, which can be assembled
/// into the core virtual machine instructions.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CoreProgram(pub Vec<CoreOp>);

impl CoreProgram {
    /// Assemble a program of core assembly instructions into the
    /// core virtual machine instructions.
    pub fn assemble(&self) -> Result<vm::CoreProgram, Error> {
        let mut result = vm::CoreProgram(vec![]);
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
pub enum CoreOp {
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

    /// Make this pointer point to the next cell (or the nth next cell).
    Next(Location, Option<isize>),
    /// Make this pointer point to the previous cell (or the nth previous cell).
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
    /// Perform dst = a > b.
    IsGreater {
        dst: Location,
        a: Location,
        b: Location
    },
    /// Perform dst = a >= b.
    IsGreaterEqual {
        dst: Location,
        a: Location,
        b: Location
    },
    /// Perform dst = a < b.
    IsLess {
        dst: Location,
        a: Location,
        b: Location
    },
    /// Perform dst = a <= b.
    IsLessEqual {
        dst: Location,
        a: Location,
        b: Location
    },
    /// Perform dst = a == b.
    IsEqual {
        dst: Location,
        a: Location,
        b: Location
    },
    /// Perform dst = a != b.
    IsNotEqual {
        dst: Location,
        a: Location,
        b: Location
    },

    /// Load a number of cells from a source location onto the stack.
    Load(Location, usize),
    /// Store a number of cells to a destination location from the stack.
    Store(Location, usize),
    /// Copy a number of cells from a source location to a destination location.
    Copy {
        src: Location,
        dst: Location,
        size: usize
    },

    /// Get a character from the input stream and store it in a destination register.
    GetChar(Location),
    /// Put a character from a source register to the output stream.
    PutChar(Location),

    /// Put a list of values to the output stream.
    PutLiteral(Vec<isize>),
    /// Push a list of values (each stored in consecutive cells) onto the stack.
    PushLiteral(Vec<isize>),

    /// Push a list of characters (each stored in consecutive cells) onto the stack,
    /// and store their address in a destination register.
    StackAllocateLiteral(Location, Vec<isize>),
}


impl CoreOp {
    pub fn put_string(msg: impl ToString) -> Self {
        Self::PutLiteral(msg.to_string().chars().map(|c| c as isize).collect())
    }

    pub fn push_string(msg: impl ToString) -> Self {
        let mut vals: Vec<isize> = msg.to_string().chars().map(|c| c as isize).collect();
        vals.push(0);
        Self::PushLiteral(vals)
    }

    pub fn stack_alloc_string(dst: Location, msg: impl ToString) -> Self {
        let mut vals: Vec<isize> = msg.to_string().chars().map(|c| c as isize).collect();
        vals.push(0);
        Self::StackAllocateLiteral(dst, vals)
    }

    fn assemble(&self, current_instruction: usize, env: &mut Env, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        match self {
            CoreOp::Comment(comment) => result.comment(comment),
            CoreOp::PutLiteral(vals) => {
                // For every character in the message
                for val in vals {
                    // Set the register to the ASCII value
                    result.set_register(*val);
                    // Put the register
                    result.putchar();
                }
            },
            CoreOp::PushLiteral(vals) => {
                // For every character in the message
                // Go to the top of the stack, and push the ASCII value of the character
                SP.deref().to(result);
                for val in vals {
                    result.move_pointer(1);
                    // Goto the pushed character's address above the top of the stack
                    // Set the register to the ASCII value
                    result.set_register(*val);
                    // Save the register to the memory location
                    result.save();
                    // SP.deref().offset(i as isize + 1).from(result);
                }
                // Move the pointer back where we came from
                result.where_is_pointer();
                SP.deref().offset(vals.len() as isize).from(result);
                SP.to(result);
                result.save();
                SP.from(result);
            },

            CoreOp::StackAllocateLiteral(dst, vals) => {
                // Copy the address of the future literal on the stack
                SP.deref().offset(1).copy_address_to(dst, result);
                CoreOp::PushLiteral(vals.clone()).assemble(current_instruction, env, result)?;
            },


            CoreOp::GetAddress { addr, dst } => addr.copy_address_to(dst, result),
            CoreOp::Next(dst, count) => dst.next(count.unwrap_or(1), result),
            CoreOp::Prev(dst, count) => dst.prev(count.unwrap_or(1), result),

            CoreOp::Set(dst, value) => dst.set(*value, result),
            CoreOp::SetLabel(dst, name) => dst.set(env.get(name, current_instruction)? as isize, result),

            CoreOp::Call(src) => {
                src.restore_from(result);
                result.call();
            }

            CoreOp::CallLabel(name) => {
                result.set_register(env.get(name, current_instruction)? as isize);
                result.call();
            }

            CoreOp::Return => {
                FP.pop(result);
                result.ret();
            }

            CoreOp::Fn(name) => {
                env.declare(name);
                env.push_matching(self, current_instruction);
                result.begin_function();
                FP.push(result);
                SP.copy_to(&FP, result);
            },
            CoreOp::While(src) => {
                src.restore_from(result);
                result.begin_while();
                env.push_matching(self, current_instruction);
            },
            CoreOp::If(src) => {
                src.restore_from(result);
                result.begin_if();
                env.push_matching(self, current_instruction);
            },
            CoreOp::Else => {
                if let Ok((CoreOp::If(_), _)) = env.pop_matching(current_instruction) {
                    result.begin_else();
                    env.push_matching(self, current_instruction);
                } else {
                    return Err(Error::Unexpected(CoreOp::Else, current_instruction));
                }
            },
            CoreOp::End => {
                match env.pop_matching(current_instruction) {
                    Ok((CoreOp::Fn(_), _)) => {
                        FP.pop(result);
                        result.end();
                    }
                    Ok((CoreOp::While(src), _)) => {
                        src.restore_from(result);
                        result.end();
                    },
                    Ok(_) => result.end(),
                    Err(_) => return Err(Error::Unmatched(CoreOp::End, current_instruction))
                }
            }

            CoreOp::Move { src, dst } => src.copy_to(dst, result),

            CoreOp::Swap(a, b) => {
                a.copy_to(&TMP, result);
                b.copy_to(a, result);
                TMP.copy_to(b, result);
            },

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
            },
            CoreOp::Neg(dst) => {
                result.set_register(-1);
                dst.to(result);
                result.append_core_op(vm::CoreOp::Mul);
                result.save();
                dst.from(result)
            },
            
            CoreOp::Not(dst) => dst.not(result),
            CoreOp::And { src, dst } => dst.and(src, result),
            CoreOp::Or  { src, dst } => dst.or(src, result),

            CoreOp::Push(src) => src.push(result),
            CoreOp::Pop(Some(dst)) => dst.pop(result),
            CoreOp::Pop(None) => SP.prev(1, result),

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
            },

            CoreOp::GetChar(dst) => {
                result.getchar();
                dst.save_to(result)
            }
            CoreOp::PutChar(dst) => {
                dst.restore_from(result);
                result.putchar()
            }

            CoreOp::Load(src, size) => {
                for i in 0..*size {
                    src.deref().offset(i as isize)
                        .copy_to(&SP.deref().offset(i as isize + 1), result);
                }
                SP.next(*size as isize, result);
            }
            CoreOp::Store(dst, size) => {
                for i in 0..*size {
                    SP.deref().offset(-(i as isize))
                        .copy_to(&dst.deref().offset((*size - i - 1) as isize), result);
                }
                SP.prev(*size as isize, result);
            }
            CoreOp::Copy{ src, dst, size } => {
                for i in 0..*size {
                    src.deref().offset(i as isize)
                        .copy_to(&dst.deref().offset(i as isize), result);
                }
            }
        }

        Ok(())
    }
}
