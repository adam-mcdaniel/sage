//! # Core Assembly Variant
//! 
//! This variant of the assembly language is intended to be used
//! with the core variant of the virtual machine. It is extremely
//! portable, but minimal.
use std::collections::HashMap;
use super::Error;
use crate::vm::{self, VirtualMachineProgram};

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CoreProgram(pub Vec<CoreOp>);

impl CoreProgram {
    pub fn assemble(&self) -> Result<vm::CoreProgram, Error> {
        let mut result = vm::CoreProgram(vec![]);
        let mut env = Env::default();
        SP.offset(BOTTOM_OF_STACK).copy_address_to(&SP, &mut result);
        SP.copy_to(&FP, &mut result);
        for op in &self.0 {
            op.assemble(&mut env, &mut result)?
        }
        Ok(result)
    }
}

/// The stack pointer register.
pub const SP: Location = Location::Address(0);
/// A temporary register. It can be used as a trash can.
const TMP: Location = Location::Address(1);
/// The frame pointer register.
pub const FP: Location = Location::Address(2);
/// The "A" general purpose register.
pub const A: Location = Location::Address(3);
/// The "B" general purpose register.
pub const B: Location = Location::Address(4);
/// The "C" general purpose register.
pub const C: Location = Location::Address(5);
/// The "D" general purpose register.
pub const D: Location = Location::Address(6);
/// The "E" general purpose register.
pub const E: Location = Location::Address(7);
/// The "F" general purpose register.
pub const F: Location = Location::Address(8);
/// The offset of the bottom of the stack.
const BOTTOM_OF_STACK: isize = 9;

/// A location in memory (on the tape of the virtual machine).
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Location {
    /// A fixed position in the tape (a constant address known at compile time).
    Address(usize),
    /// Use the value of a cell on the tape as an address.
    /// For example, Indirect(SP) is the location of the top item on the stack.
    Indirect(Box<Self>),
    /// Go to a position in memory, and then move the pointer according to an offset.
    /// For example, `Offset(Address(8), -2)` is equivalent to `Address(6)`.
    Offset(Box<Self>, isize),
}

impl Location {
    pub fn offset(&self, offset: isize) -> Self {
        Location::Offset(Box::new(self.clone()), offset)
    }

    pub fn deref(&self) -> Self {
        Location::Indirect(Box::new(self.clone()))
    }

    pub fn push(&self, result: &mut dyn VirtualMachineProgram) {
        SP.deref().offset(1).copy_address_to(&SP, result);
        self.copy_to(&SP.deref(), result)
    }

    pub fn pop(&self, result: &mut dyn VirtualMachineProgram) {
        SP.deref().copy_to(self, result);
        SP.deref().offset(-1).copy_address_to(&SP, result)
    }

    pub fn copy_address_to(&self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.where_is_pointer();
        self.from(result);
        dst.save_to(result);
    }

    pub fn to(&self, result: &mut dyn VirtualMachineProgram) {
        match self {
            Location::Address(addr) => result.move_pointer(*addr as isize),
            Location::Indirect(loc) => {
                loc.to(result);
                result.deref();
            },
            Location::Offset(loc, offset) => {
                loc.to(result);
                result.move_pointer(*offset);
            },
        }
    }

    pub fn from(&self, result: &mut dyn VirtualMachineProgram) {
        match self {
            Location::Address(addr) => result.move_pointer(-(*addr as isize)),
            Location::Indirect(loc) => {
                result.refer();
                loc.from(result);
            },

            Location::Offset(loc, offset) => {
                result.move_pointer(-*offset);
                loc.from(result);
            },
        }
    }


    pub fn whole_int(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.whole();
        result.save();
        self.from(result);
    }

    pub fn save_to(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.save();
        self.from(result);
    }

    pub fn restore_from(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        self.from(result);
    }

    pub fn inc(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.inc();
        result.save();
        self.from(result);
    }

    pub fn dec(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.dec();
        result.save();
        self.from(result);
    }

    fn binop(&self, op: vm::CoreOp, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        src.to(result);
        result.append_core_op(op);
        src.from(result);
        self.save_to(result);
    }

    pub fn not(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.begin_if();
            result.set_register(0);
        result.begin_else();
            result.set_register(1);
        result.end();
        result.save();
        self.from(result);
    }

    pub fn and(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.begin_if();
            self.from(result);
            other.restore_from(result);
            self.to(result);
        result.begin_else();
            result.set_register(0);
        result.end();
        result.save();
        self.from(result);
    }

    pub fn or(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.begin_if();
            result.set_register(1);
        result.begin_else();
            self.from(result);
            other.restore_from(result);
            self.to(result);
        result.end();
        result.save();
        self.from(result);
    }

    pub fn is_greater_than(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.copy_to(&TMP, result);
        TMP.sub(other, result);
        TMP.dec(result);
        TMP.whole_int(result);
        self.save_to(result);
    }

    pub fn is_greater_or_equal_to(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.copy_to(&TMP, result);
        TMP.sub(other, result);
        TMP.whole_int(result);
        self.save_to(result);
    }

    pub fn is_less_than(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        other.copy_to(&TMP, result);
        TMP.sub(self, result);
        TMP.dec(result);
        TMP.whole_int(result);
        self.save_to(result);
    }

    pub fn is_less_or_equal_to(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        other.copy_to(&TMP, result);
        TMP.sub(self, result);
        TMP.whole_int(result);
        self.save_to(result);
    }

    pub fn add(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Add, other, result);
    }

    pub fn sub(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Sub, other, result);
    }

    pub fn mul(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Mul, other, result);
    }

    pub fn div(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Div, other, result);
    }

    pub fn rem(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Rem, other, result);
    }

    pub fn set(&self, val: isize, result: &mut dyn VirtualMachineProgram) {
        result.set_register(val);
        self.save_to(result)
    }

    pub fn copy_to(&self, other: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        other.save_to(result);
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

    /// Print a string to the output stream.
    PutLiteral(String),
    /// Push a list of characters (each stored in consecutive cells) onto the stack.
    PushLiteral(String),

    /// Push a list of characters (each stored in consecutive cells) onto the stack,
    /// and store their address in a destination register.
    StackAllocateLiteral(Location, String),
}


#[derive(Default, Clone, Debug)]
struct Env {
    labels: HashMap<String, usize>,
    label: usize,
    matching: Vec<CoreOp>,
}

impl Env {
    fn define(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.label);
        self.label += 1;
    }
    fn get(&self, name: &str) -> Result<usize, Error> {
        self.labels.get(name).copied().ok_or_else(|| Error::UndefinedLabel(name.to_string()))
    }
    fn push_matching(&mut self, op: &CoreOp) {
        self.matching.push(op.clone());
    }
    fn pop_matching(&mut self) -> Result<CoreOp, Error> {
        self.matching.pop().ok_or(Error::UnmatchedEnd)
    }
}

impl CoreOp {
    fn assemble(&self, env: &mut Env, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        match self {
            CoreOp::Comment(comment) => result.comment(comment),
            CoreOp::PutLiteral(msg) => {
                for ch in msg.chars() {
                    result.set_register(ch as isize);
                    result.putchar();
                }
            },
            CoreOp::PushLiteral(msg) => {
                for ch in msg.chars() {
                    SP.deref().offset(1).to(result);
                    result.set_register(ch as isize);
                    result.save();
                    result.where_is_pointer();
                    SP.deref().offset(1).from(result);
                    SP.save_to(result);
                }
            },
            CoreOp::StackAllocateLiteral(dst, msg) => {
                SP.deref().offset(1).copy_address_to(dst, result);
                for ch in msg.chars() {
                    SP.deref().offset(1).to(result);
                    result.set_register(ch as isize);
                    result.save();
                    result.where_is_pointer();
                    SP.deref().offset(1).from(result);
                    SP.save_to(result);
                }
            },


            CoreOp::GetAddress { addr, dst } => addr.copy_address_to(dst, result),
            CoreOp::Next(dst, Some(count)) => dst.deref().offset(*count).copy_address_to(dst, result),
            CoreOp::Prev(dst, Some(count)) => dst.deref().offset(-count).copy_address_to(dst, result),
            CoreOp::Next(dst, None) => dst.deref().offset(1).copy_address_to(dst, result),
            CoreOp::Prev(dst, None) => dst.deref().offset(-1).copy_address_to(dst, result),

            CoreOp::Set(dst, value) => {
                dst.set(*value, result);
            },
            CoreOp::SetLabel(dst, name) => {
                dst.set(env.get(name)? as isize, result);
            },

            CoreOp::Call(src) => {
                // Call function
                src.restore_from(result);
                result.call();
            }

            CoreOp::CallLabel(name) => {
                // Call function
                result.set_register(env.get(name)? as isize);
                result.call();
            }

            CoreOp::Return => {
                // Pop FP
                FP.pop(result);
                // Return
                result.ret();
            }

            CoreOp::Fn(name) => {
                env.define(name);
                env.push_matching(self);
                // Begin function
                result.begin_function();
                // Push FP
                FP.push(result);
                // Set FP to SP
                SP.copy_to(&FP, result);
            },
            CoreOp::While(src) => {
                src.restore_from(result);
                result.begin_while();
                env.push_matching(self);
            },
            CoreOp::If(src) => {
                src.restore_from(result);
                result.begin_if();
                env.push_matching(self);
            },
            CoreOp::Else => {
                if let Ok(CoreOp::If(_)) = env.pop_matching() {
                    result.begin_else();
                    env.push_matching(self);
                } else {
                    return Err(Error::UnexpectedElse);
                }
            },
            CoreOp::End => {
                match env.pop_matching() {
                    Ok(CoreOp::Fn(_)) => {
                        // Pop FP
                        FP.pop(result);
                        // End
                        result.end();
                    }
                    Ok(CoreOp::While(src)) => {
                        src.restore_from(result);
                        result.end();
                    },
                    Ok(_) => result.end(),
                    _ => return Err(Error::UnmatchedEnd)
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
                TMP.set(-1, result);
                dst.mul(&TMP, result)
            },
            
            CoreOp::Not(dst) => dst.not(result),
            CoreOp::And { src, dst } => dst.and(src, result),
            CoreOp::Or  { src, dst } => dst.or(src, result),

            CoreOp::Push(src) => {
                src.push(result);
            },
            CoreOp::Pop(Some(dst)) => {
                // SP.deref().copy_to(dst, result);
                // SP.dec(result);
                dst.pop(result);
            },
            CoreOp::Pop(None) => {
                SP.deref().offset(-1).copy_address_to(&SP, result);
            },

            CoreOp::IsGreater { src, dst } => dst.is_greater_than(src, result),
            CoreOp::IsGreaterEqual { src, dst } => dst.is_greater_or_equal_to(src, result),
            CoreOp::IsLess { src, dst } => dst.is_less_than(src, result),
            CoreOp::IsLessEqual { src, dst } => dst.is_less_or_equal_to(src, result),
            CoreOp::IsNotEqual { src, dst } => dst.sub(src, result),
            CoreOp::IsEqual { src, dst } => {
                dst.sub(src, result);
                dst.not(result);
            },

            CoreOp::Compare { dst, a, b } => {
                a.copy_to(dst, result);
                dst.is_greater_than(b, result);
                result.begin_if();
                    result.set_register(1);
                result.begin_else();
                    a.copy_to(dst, result);
                    dst.is_less_than(b, result);
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
                dst.save_to(result);
            }
            CoreOp::PutChar(dst) => {
                dst.restore_from(result);
                result.putchar();
            }

            CoreOp::Load(src, size) => {
                for i in 0..*size {
                    SP.deref().offset(1).copy_address_to(&SP, result);
                    src.deref().offset(i as isize).copy_to(&SP.deref(), result);
                }
            }
            CoreOp::Store(dst, size) => {
                for i in 0..*size {
                    SP.deref().copy_to(&dst.deref().offset((*size - i - 1) as isize), result);
                    SP.deref().offset(-1).copy_address_to(&SP, result)
                }
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
