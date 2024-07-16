//! # Assembly Memory Location
//!
//! This module contains the `Location` type, which represents a memory
//! location on the virtual machine's tape in the assembly language.
//!
//! ## What should I know first?
//!
//! You should ***NOT*** use pointers as if they are integers.
//! Think about pointers and integers as two completely separate types.
//!
//! #### Why?
//!
//! This is because virtual machine implementations are **bound** to vary.
//! For example: my C implementation uses *real pointers* (which are retrieved
//! through virtual machine instructions `Where` and `Alloc`, and allows the
//! implementation to be used with valgrind, gprof, a custom allocater, or
//! potentially garbage collection!), but an implementation in a language
//! like Python might use integer indices in a list instead.
//!
//! If the backend implementation uses pointers, *using `Inc` to move a pointer
//! to the next cell **will not work***. This is because pointers need to be
//! incremented by the size of the data type they point to. Because the virtual
//! machine's cell size is undefined (purposely, to make this backend as flexible
//! as possible), ***you cannot know this size***. Therefore you cannot use `Inc`
//! to move a pointer to the next cell unless *you want your code to be unportable*.
//!
//! ***DO NOT USE `Inc` AND `Dec` TO MOVE POINTERS! USE `Next` AND `Prev` INSTEAD!
//! OR YOUR CODE WILL NOT PORT TO ALL VIRTUAL MACHINE IMPLEMENTATIONS!!***
//!
//! ## What locations are available?
//!
//! There are several constant locations to use:
//!
//! * `BOTTOM_OF_STACK`: The bottom of the stack.
//! * `TMP`: A volatile register. Essentially a trashcan.
//! * `SP`: The stack pointer. `SP.deref()` is the location of the top item
//!   on the stack.
//! * `FP`: The frame pointer. Automatically updated by `Call` and `Return`.
//!   The `FP` register points to the top of the stack when the function was called.
//! * `FP_STACK`: The stack of frame pointers. Whenever the program starts,
//!   a frame pointer stack is initialized. Whenever a function is called,
//!   the old frame pointer is pushed to the `FP_STACK`. Whenever a function
//!   returns, it pops the frame pointer from the `FP_STACK`.
//! * `A`, `B`, `C`, `D`, `E`, `F`: General purpose registers.
//!
//! ## What kinds of locations are there?
//!
//! There are three kinds of locations:
//!
//! * Constant addresses: these are addresses that are known at compile time.
//!   They're simply just offsets from the base of the tape.
//! * Offset addresses: these are addresses that are relative to other addresses.
//!   You might want to dereference a pointer at an address, and then move
//!   the pointer by an offset. This would be represented like:
//!   ```rs
//!   Offset(Indirect(Address(6)), -2) // go the address stored at the 6th cell
//!                                    // from the start of the tape, and move 2 cells back
//!   ```
//! * Indirect addresses: these are dereferenced addresses. To dereference a
//!   value stored at another location, use the `Indirect` constructor.
//!   ```rs
//!   Indirect(Address(6)) // go the address pointed to by the value in the 6th cell of the tape
//!   ```
use serde_derive::{Serialize, Deserialize};

use crate::{
    side_effects::{Input, Output},
    vm::{self, Error, VirtualMachineProgram},
    NULL,
};

use core::fmt;

/// The stack pointer register.
pub const SP: Location = Location::Address(0);
/// A volatile register. This register may be silently overwritten by
/// some assembly instructions.
pub(crate) const TMP: Location = Location::Address(1);
/// The frame pointer register.
pub const FP: Location = Location::Address(2);
/// The stack pointer register for the stack of frames.
/// This always points to the parent frame's saved frame pointer.
/// At the beginning of the program, this is allocated with a specified number of cells.
pub(crate) const FP_STACK: Location = Location::Address(3);
/// The Global Pointer register. This is used to access global variables.
pub const GP: Location = Location::Address(4);
/// The "A" general purpose register.
pub const A: Location = Location::Address(5);
/// The "B" general purpose register.
pub const B: Location = Location::Address(6);
/// The "C" general purpose register.
pub const C: Location = Location::Address(7);
/// The "D" general purpose register.
pub const D: Location = Location::Address(8);
/// The "E" general purpose register.
pub const E: Location = Location::Address(9);
/// The "F" general purpose register.
pub const F: Location = Location::Address(10);
/// A register that points to the bottom of the stack.
pub(crate) const STACK_START: Location = Location::Address(11);
/// The bottom of the frame pointer stack.
pub(crate) const START_OF_FP_STACK: Location = Location::Address(12);

pub const REGISTERS: [Location; 12] = [SP, TMP, FP, FP_STACK, GP, A, B, C, D, E, F, STACK_START];

/// A location in memory (on the tape of the virtual machine).
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum Location {
    /// A fixed position in the tape (a constant address known at compile time).
    Address(usize),
    /// Use the value of a cell on the tape as an address.
    /// For example, Indirect(SP) is the location of the top item on the stack.
    Indirect(Box<Self>),
    /// Go to a position in memory, and then move the pointer according to an offset.
    /// For example, `Offset(Address(8), -2)` is equivalent to `Address(6)`.
    Offset(Box<Self>, isize),
    /// A global variable.
    Global(String),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Location::Address(addr) => match addr {
                _ if self == &SP => write!(f, "SP"),
                _ if self == &FP => write!(f, "FP"),
                _ if self == &FP_STACK => write!(f, "FP_STACK"),
                _ if self == &GP => write!(f, "GP"),
                _ if self == &A => write!(f, "A"),
                _ if self == &B => write!(f, "B"),
                _ if self == &C => write!(f, "C"),
                _ if self == &D => write!(f, "D"),
                _ if self == &E => write!(f, "E"),
                _ if self == &F => write!(f, "F"),
                _ if self == &TMP => write!(f, "TMP"),
                _ if self == &STACK_START => write!(f, "STACK_START"),
                other => write!(f, "{}", other),
            },
            Location::Indirect(loc) => write!(f, "[{}]", loc),
            Location::Offset(loc, offset) => {
                let offset = *offset;
                if let Location::Indirect(ref addr) = **loc {
                    write!(
                        f,
                        "[{} {} {}]",
                        addr,
                        if offset < 0 { "-" } else { "+" },
                        if offset < 0 { -offset } else { offset }
                    )
                } else {
                    write!(
                        f,
                        "{} {} {}",
                        loc,
                        if offset < 0 { "-" } else { "+" },
                        if offset < 0 { -offset } else { offset }
                    )
                }
            }
            Location::Global(name) => write!(f, "${name}"),
        }
    }
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Location::Global(name) => write!(f, "{name}"),
            Location::Address(addr) if *addr <= 10 => write!(f, "{self} ({addr})"),
            Location::Address(addr) => write!(f, "{addr}"),
            Location::Indirect(loc) => write!(f, "[{loc:?}]"),
            Location::Offset(loc, offset) => {
                let offset = *offset;
                if let Location::Indirect(ref addr) = **loc {
                    write!(
                        f,
                        "[{:?} {} {}]",
                        addr,
                        if offset < 0 { "-" } else { "+" },
                        if offset < 0 { -offset } else { offset }
                    )
                } else {
                    write!(
                        f,
                        "{:?} {} {}",
                        loc,
                        if offset < 0 { "-" } else { "+" },
                        if offset < 0 { -offset } else { offset }
                    )
                }
            }
        }
    }
}

impl Location {
    /// Get the location offset by a constant number of cells from a starting location.
    /// For example, `Offset(Address(8), -2)` is equivalent to `Address(6)`.
    pub fn offset(&self, offset: isize) -> Self {
        if offset == 0 {
            return self.clone();
        }

        match self {
            // If we are offsetting from another offset, then we can just add the offsets together.
            Location::Offset(loc, x) => Location::Offset(loc.clone(), *x + offset),
            // If we are offsetting from a constant address, then we can just add the offset to the address.
            Location::Address(addr) => Location::Address((*addr as isize + offset) as usize),
            // Offsetting from a dereferenced pointer.
            Location::Indirect(_) => Location::Offset(Box::new(self.clone()), offset),
            // Offsetting from a global variable.
            Location::Global(_) => Location::Offset(Box::new(self.clone()), offset),
        }
    }

    /// Get the location of the value pointed to by this location.
    pub fn deref(&self) -> Self {
        Location::Indirect(Box::new(self.clone()))
    }

    /// Push the value of this location to a given stack.
    pub(crate) fn push_to(&self, sp: &Location, result: &mut dyn VirtualMachineProgram) {
        sp.deref().offset(1).copy_address_to(sp, result);
        self.copy_to(&sp.deref(), result)
    }

    /// Pop the top item off a given stack and store it in this location.
    pub(crate) fn pop_from(&self, sp: &Location, result: &mut dyn VirtualMachineProgram) {
        sp.deref().copy_to(self, result);
        sp.deref().offset(-1).copy_address_to(sp, result)
    }

    /// Restore the address of this location to the register.
    pub(crate) fn restore_address(&self, result: &mut dyn VirtualMachineProgram) {
        match self {
            Location::Address(addr) => {
                result.where_is_pointer();
                result.op(vm::CoreOp::Offset(*addr as isize, 1));
            }
            Location::Indirect(loc) => {
                loc.restore_from(result);
            }
            Location::Offset(loc, offset) => {
                loc.restore_address(result);
                result.op(vm::CoreOp::Offset(*offset, 1));
            }
            Location::Global(name) => {
                panic!("Cannot restore address of global variable {}", name);
            }
        }
    }

    /// Copy the address of this location to another location.
    pub(crate) fn copy_address_to(&self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_address(result);
        dst.save_to(result);
    }

    /// Move the pointer to this location.
    pub(crate) fn to(&self, result: &mut dyn VirtualMachineProgram) {
        match self {
            Location::Address(addr) => result.move_pointer(*addr as isize),
            Location::Indirect(loc) => {
                loc.to(result);
                result.deref();
            }
            Location::Offset(loc, offset) => {
                loc.to(result);
                result.move_pointer(*offset);
            }
            Location::Global(name) => {
                panic!("Cannot move pointer to global variable {}", name);
            }
        }
    }

    /// Move the pointer from this location.
    pub(crate) fn from(&self, result: &mut dyn VirtualMachineProgram) {
        match self {
            Location::Address(addr) => result.move_pointer(-(*addr as isize)),
            Location::Indirect(loc) => {
                result.refer();
                loc.from(result);
            }
            Location::Offset(loc, offset) => {
                // If the offset is from a dereferenced pointer, then moving back before
                // reversing the dereference does nothing, so we can skip it.
                if !matches!(**loc, Location::Indirect(_)) {
                    result.move_pointer(-*offset);
                }
                loc.from(result);
            }
            Location::Global(name) => {
                panic!("Cannot move pointer from global variable {}", name);
            }
        }
    }

    /// Take the pointer value of this location, and make it point
    /// `count` number of cells to the right of its original position.
    pub(crate) fn next(&self, count: isize, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Offset(count, 1));
        result.save();
        self.from(result);
    }

    /// Take the pointer value of this location, and make it point
    /// `count` number of cells to the left of its original position.
    pub(crate) fn prev(&self, count: isize, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Offset(-count, 1));
        result.save();
        self.from(result);
    }

    /// Take the value at this location. If it is a whole number (>= 0),
    /// then the value of this location is now 1. Otherwise, the value is 0.
    pub(crate) fn whole_int(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.is_non_negative();
        result.save();
        self.from(result);
    }

    /// Save the value of the virtual machine's register to this location.
    pub(crate) fn save_to(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.save();
        self.from(result);
    }

    /// Restore the value from this location into the virtual machine's register.
    pub(crate) fn restore_from(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        self.from(result);
    }

    /// Increment the value of this location.
    pub(crate) fn inc(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Inc(1));
        result.save();
        self.from(result);
    }

    /// Decrement the value of this location.
    pub(crate) fn dec(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Dec(1));
        result.save();
        self.from(result);
    }

    fn vec_restore_from(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.load_vector(size);
        self.from(result);
    }

    fn vec_save_to(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.store_vector(size);
        self.from(result);
    }

    pub(crate) fn vec_add(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::Add(size), src, size, result);
    }

    pub(crate) fn vec_sub(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::Sub(size), src, size, result);
    }

    pub(crate) fn vec_mul(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::Mul(size), src, size, result);
    }

    pub(crate) fn vec_div(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::Div(size), src, size, result);
    }

    pub(crate) fn vec_rem(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::Rem(size), src, size, result);
    }

    pub(crate) fn vec_left_shift(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::LeftShift(size), src, size, result);
    }

    pub(crate) fn vec_arithmetic_right_shift(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::ArithmeticRightShift(size), src, size, result);
    }

    pub(crate) fn vec_logical_right_shift(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::LogicalRightShift(size), src, size, result);
    }

    pub(crate) fn vec_bitwise_nand(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::BitwiseNand(size), src, size, result);
    }

    pub(crate) fn vec_bitwise_and(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::BitwiseAnd(size), src, size, result);
    }

    pub(crate) fn vec_bitwise_or(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::BitwiseOr(size), src, size, result);
    }
    pub(crate) fn vec_bitwise_nor(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_restore_from(size, result);
        src.to(result);
        result.op(vm::CoreOp::BitwiseOr(size));
        result.op(vm::CoreOp::BitwiseNot(size));
        src.from(result);
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_bitwise_xor(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::BitwiseXor(size), src, size, result);
    }

    pub(crate) fn vec_bitwise_not(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::BitwiseNot(size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_not(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::Not(size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_and(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::And(size), src, size, result);
    }

    pub(crate) fn vec_or(&self, src: &Self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_binop(vm::CoreOp::Or(size), src, size, result);
    }

    pub(crate) fn vec_whole_int(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::IsNonNegative(size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_inc(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::Inc(size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_dec(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::Dec(size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_neg(&self, size: usize, result: &mut dyn VirtualMachineProgram) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::Neg(size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_offset(
        &self,
        offset: isize,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_restore_from(size, result);
        result.op(vm::CoreOp::Offset(offset, size));
        self.vec_save_to(size, result);
    }

    pub(crate) fn vec_index(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_binop(vm::CoreOp::Index(size), src, size, result);
    }

    pub(crate) fn vec_copy_to(
        &self,
        dst: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_restore_from(size, result);
        dst.vec_save_to(size, result);
    }

    pub(crate) fn vec_float_add(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_std_binop(vm::StandardOp::Add(size), src, size, result)
    }

    pub(crate) fn vec_float_sub(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_std_binop(vm::StandardOp::Sub(size), src, size, result)
    }

    pub(crate) fn vec_float_mul(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_std_binop(vm::StandardOp::Mul(size), src, size, result)
    }

    pub(crate) fn vec_float_div(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_std_binop(vm::StandardOp::Div(size), src, size, result)
    }

    pub(crate) fn vec_float_rem(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_std_binop(vm::StandardOp::Rem(size), src, size, result)
    }

    pub(crate) fn vec_float_neg(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::Neg(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_whole_int(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::IsNonNegative(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_pow(
        &self,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_std_binop(vm::StandardOp::Pow(size), src, size, result)
    }

    pub(crate) fn vec_float_sin(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::Sin(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_cos(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::Cos(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_tan(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::Tan(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_asin(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::ASin(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_acos(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::ACos(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    pub(crate) fn vec_float_atan(
        &self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        result.std_op(vm::StandardOp::ATan(size))?;
        self.vec_save_to(size, result);
        Ok(())
    }

    fn vec_binop(
        &self,
        op: vm::CoreOp,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.vec_restore_from(size, result);
        src.to(result);
        result.op(op);
        src.from(result);
        self.vec_save_to(size, result);
    }

    fn vec_std_binop(
        &self,
        op: vm::StandardOp,
        src: &Self,
        size: usize,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.vec_restore_from(size, result);
        src.to(result);
        result.std_op(op)?;
        src.from(result);
        self.vec_save_to(size, result);
        Ok(())
    }

    /// Perform a `CoreOp` as an abstract binary operation.
    /// Essentially, if you pass an instruction such as `Add`, `Sub`, etc.,
    /// then the corresponding operation will be performed such that:
    /// `self` is the destination, and `src` is the source.
    fn binop(&self, op: vm::CoreOp, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        src.to(result);
        result.op(op);
        src.from(result);
        self.save_to(result);
    }

    /// Perform a `StandardOp` as an abstract binary operation.
    /// Essentially, if you pass an instruction such as `Add`, `Sub`, etc.,
    /// then the corresponding operation will be performed such that:
    /// `self` is the destination, and `src` is the source.
    fn std_binop(
        &self,
        op: vm::StandardOp,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.restore_from(result);
        src.to(result);
        result.std_op(op)?;
        src.from(result);
        self.save_to(result);
        Ok(())
    }

    /// Perform bitwise-nand on this cell and a source cell.
    pub(crate) fn bitwise_nand(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::BitwiseNand(1), src, result);
    }
    /// Perform bitwise-and on this cell and a source cell.
    pub(crate) fn bitwise_and(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::BitwiseAnd(1), src, result);
    }

    /// Perform bitwise-or on this cell and a source cell.
    pub(crate) fn bitwise_or(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::BitwiseOr(1), src, result);
    }

    /// Perform bitwise-xor on this cell and a source cell.
    pub(crate) fn bitwise_xor(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::BitwiseXor(1), src, result);
    }
    /// Perform bitwise-xor on this cell and a source cell.
    pub(crate) fn bitwise_not(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::BitwiseNot(1));
        result.save();
        self.from(result);
    }

    /// Perform bitwise-nor on this cell and a source cell.
    pub(crate) fn bitwise_nor(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        src.to(result);
        result.op(vm::CoreOp::BitwiseOr(1));
        result.op(vm::CoreOp::BitwiseNot(1));
        src.from(result);
        self.save_to(result);
    }

    /// If this cell is non-zero, then the value of this location is now 0.
    /// Otherwise, the value of this location is now 1.
    ///
    /// Perform boolean not on the value of this cell
    pub(crate) fn not(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Not(1));
        result.save();
        self.from(result);
    }

    /// Perform boolean and on the value of this cell and a source cell.
    pub(crate) fn and(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::And(1), src, result);
    }

    /// Perform boolean or on the value of this cell and a source cell.
    pub(crate) fn or(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Or(1), src, result);
    }

    /// dst = this cell > source cell.
    pub(crate) fn is_greater_than(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.is_less_or_equal_to(src, dst, result);
        dst.not(result)
    }

    /// dst = this cell >= source cell.
    pub(crate) fn is_greater_than_float(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.copy_to(dst, result);
        dst.sub_float(src, result)?;
        dst.std_op(vm::StandardOp::IsNonNegative(1), result)
    }

    /// dst = this cell >= source cell.
    pub(crate) fn is_greater_or_equal_to(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        src.copy_to(dst, result);
        dst.sub(self, result);
        dst.dec(result);
        dst.whole_int(result);
        dst.not(result);
    }

    /// dst = this cell < source cell.
    pub(crate) fn is_less_than(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        src.copy_to(dst, result);
        dst.sub(self, result);
        dst.dec(result);
        dst.whole_int(result);
    }

    /// dst = this cell < source cell.
    pub(crate) fn is_less_than_float(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        src.copy_to(dst, result);
        dst.sub_float(self, result)?;
        dst.std_op(vm::StandardOp::IsNonNegative(1), result)
    }
    /// dst = this cell <= source cell.
    pub(crate) fn is_less_or_equal_to(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        src.copy_to(dst, result);
        dst.sub(self, result);
        dst.whole_int(result);
    }

    pub(crate) fn is_not_equal(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        src.copy_to(dst, result);
        dst.sub(self, result);
    }

    pub(crate) fn is_equal(&self, src: &Self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.is_not_equal(src, dst, result);
        dst.not(result);
    }

    /// This cell += source cell.
    pub(crate) fn add(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Add(1), src, result);
    }

    /// This cell -= source cell.
    pub(crate) fn sub(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Sub(1), src, result);
    }

    /// This cell *= source cell.
    pub(crate) fn mul(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Mul(1), src, result);
    }

    /// This cell /= source cell.
    pub(crate) fn div(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Div(1), src, result);
    }

    /// This cell %= source cell.
    pub(crate) fn rem(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Rem(1), src, result);
    }

    /// This cell <<= source cell.
    pub(crate) fn left_shift(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::LeftShift(1), src, result);
    }

    /// This cell >>= source cell.
    /// This is an arithmetic shift right.
    pub(crate) fn arithmetic_right_shift(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.binop(vm::CoreOp::ArithmeticRightShift(1), src, result);
    }

    /// This cell >>= source cell.
    /// This is a logical shift right.
    pub(crate) fn logical_right_shift(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::LogicalRightShift(1), src, result);
    }
    /// Negate the cell
    pub(crate) fn neg(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Neg(1));
        result.save();
        self.from(result);
    }

    /// This cell += source cell.
    pub(crate) fn add_float(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.std_binop(vm::StandardOp::Add(1), src, result)
    }

    /// This cell -= source cell.
    pub(crate) fn sub_float(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.std_binop(vm::StandardOp::Sub(1), src, result)
    }

    /// This cell *= source cell.
    pub(crate) fn mul_float(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.std_binop(vm::StandardOp::Mul(1), src, result)
    }

    /// This cell /= source cell.
    pub(crate) fn div_float(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.std_binop(vm::StandardOp::Div(1), src, result)
    }

    /// This cell %= source cell.
    pub(crate) fn rem_float(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.std_binop(vm::StandardOp::Rem(1), src, result)
    }

    /// Negate this cell
    pub(crate) fn neg_float(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.to(result);
        result.restore();
        result.std_op(vm::StandardOp::Neg(1))?;
        result.save();
        self.from(result);
        Ok(())
    }

    /// This cell **= source cell.
    pub(crate) fn pow_float(
        &self,
        src: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.std_binop(vm::StandardOp::Pow(1), src, result)
    }

    /// This cell = a constant value.
    pub(crate) fn set(&self, val: i64, result: &mut dyn VirtualMachineProgram) {
        result.set_register(val);
        self.save_to(result)
    }

    /// This cell = a constant floating point value. This requires
    /// the standard  instruction.
    pub(crate) fn set_float(
        &self,
        val: f64,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        result.std_op(vm::StandardOp::Set(vec![val]))?;
        self.save_to(result);
        Ok(())
    }

    fn std_op(
        &self,
        op: vm::StandardOp,
        result: &mut dyn VirtualMachineProgram,
    ) -> Result<(), Error> {
        self.to(result);
        result.restore();
        result.std_op(op)?;
        result.save();
        self.from(result);
        Ok(())
    }

    /// Read the value of this cell, allocate that number of cells, store the address
    /// of the first cell in this cell.
    pub(crate) fn alloc(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::Alloc, result)
    }

    /// Free the pointer stored in this cell, and set the value to -1000 (to prevent)
    /// accidental use of the freed memory.
    pub(crate) fn free(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.to(result);
        result.restore();
        result.std_op(vm::StandardOp::Free)?;
        result.set_register(NULL);
        result.save();
        self.from(result);
        Ok(())
    }

    pub(crate) fn sin(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::Sin(1), result)
    }
    pub(crate) fn cos(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::Cos(1), result)
    }
    pub(crate) fn tan(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::Tan(1), result)
    }

    pub(crate) fn asin(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::ASin(1), result)
    }
    pub(crate) fn acos(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::ACos(1), result)
    }
    pub(crate) fn atan(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::ATan(1), result)
    }

    pub(crate) fn to_float(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::ToFloat(1), result)
    }

    pub(crate) fn to_int(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.std_op(vm::StandardOp::ToInt(1), result)
    }

    pub(crate) fn get(&self, src: Input, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.op(vm::CoreOp::Get(src));
        result.save();
        self.from(result);
    }

    pub(crate) fn put(&self, dst: Output, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.op(vm::CoreOp::Put(dst));
        self.from(result);
    }

    #[allow(dead_code)]
    pub(crate) fn peek(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.to(result);
        result.std_op(vm::StandardOp::Peek)?;
        result.save();
        self.from(result);
        Ok(())
    }

    #[allow(dead_code)]
    pub(crate) fn poke(&self, result: &mut dyn VirtualMachineProgram) -> Result<(), Error> {
        self.to(result);
        result.restore();
        result.std_op(vm::StandardOp::Poke)?;
        self.from(result);
        Ok(())
    }

    /// Store the value of this cell into another cell.
    pub(crate) fn copy_to(&self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        dst.save_to(result);
    }
}
