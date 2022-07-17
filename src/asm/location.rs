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

use crate::vm::{self, VirtualMachineProgram};

/// The stack pointer register.
pub const SP: Location = Location::Address(0);
/// A volatile register. This register may be silently overwritten by
/// some assembly instructions.
pub(super) const TMP: Location = Location::Address(1);
/// The frame pointer register.
pub const FP: Location = Location::Address(2);
/// The stack pointer register for the stack of frames.
/// This always points to the parent frame's saved frame pointer.
/// At the beginning of the program, this is allocated with a specified number of cells.
pub(super) const FP_STACK: Location = Location::Address(3);
/// The "A" general purpose register.
pub const A: Location = Location::Address(4);
/// The "B" general purpose register.
pub const B: Location = Location::Address(5);
/// The "C" general purpose register.
pub const C: Location = Location::Address(6);
/// The "D" general purpose register.
pub const D: Location = Location::Address(7);
/// The "E" general purpose register.
pub const E: Location = Location::Address(8);
/// The "F" general purpose register.
pub const F: Location = Location::Address(9);

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

pub fn var(offset: usize) -> Location {
    FP.deref().offset(-(offset as isize))
}

impl Location {
    /// Get the location offset by a constant number of cells from a starting location.
    /// For example, `Offset(Address(8), -2)` is equivalent to `Address(6)`.
    pub fn offset(&self, offset: isize) -> Self {
        if offset == 0 {
            self.clone()
        } else if let Self::Offset(addr, x) = self {
            Location::Offset(addr.clone(), *x + offset)
        } else {
            Location::Offset(Box::new(self.clone()), offset)
        }
    }

    /// Get the location of the value pointed to by this location.
    pub fn deref(&self) -> Self {
        Location::Indirect(Box::new(self.clone()))
    }

    /// Push the value of this location to the stack.
    pub fn push(&self, result: &mut dyn VirtualMachineProgram) {
        self.push_to(&SP, result)
    }

    /// Pop the top item off the stack and store it in this location.
    pub fn pop(&self, result: &mut dyn VirtualMachineProgram) {
        self.pop_from(&SP, result)
    }

    /// Push the value of this location to a given stack.
    pub fn push_to(&self, sp: &Location, result: &mut dyn VirtualMachineProgram) {
        sp.deref().offset(1).copy_address_to(sp, result);
        self.copy_to(&sp.deref(), result)
    }

    /// Pop the top item off a given stack and store it in this location.
    pub fn pop_from(&self, sp: &Location, result: &mut dyn VirtualMachineProgram) {
        sp.deref().copy_to(self, result);
        sp.deref().offset(-1).copy_address_to(sp, result)
    }

    /// Copy the address of this location to another location.
    pub fn copy_address_to(&self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.where_is_pointer();
        self.from(result);
        dst.save_to(result);
    }

    /// Move the pointer to this location.
    pub fn to(&self, result: &mut dyn VirtualMachineProgram) {
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
        }
    }

    /// Move the pointer from this location.
    pub fn from(&self, result: &mut dyn VirtualMachineProgram) {
        match self {
            Location::Address(addr) => result.move_pointer(-(*addr as isize)),
            Location::Indirect(loc) => {
                result.refer();
                loc.from(result);
            }

            Location::Offset(loc, offset) => {
                result.move_pointer(-*offset);
                loc.from(result);
            }
        }
    }

    /// Take the pointer value of this location, and make it point
    /// `count` number of cells to the right of its original position.
    pub fn next(&self, count: isize, result: &mut dyn VirtualMachineProgram) {
        self.deref().offset(count).copy_address_to(self, result);
    }

    /// Take the pointer value of this location, and make it point
    /// `count` number of cells to the left of its original position.
    pub fn prev(&self, count: isize, result: &mut dyn VirtualMachineProgram) {
        self.deref().offset(-count).copy_address_to(self, result);
    }

    /// Take the value at this location. If it is a whole number (>= 0),
    /// then the value of this location is now 1. Otherwise, the value is 0.
    pub fn whole_int(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.is_non_negative();
        result.save();
        self.from(result);
    }

    /// Save the value of the virtual machine's register to this location.
    pub fn save_to(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.save();
        self.from(result);
    }

    /// Restore the value from this location into the virtual machine's register.
    pub fn restore_from(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        self.from(result);
    }

    /// Increment the value of this location.
    pub fn inc(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.inc();
        result.save();
        self.from(result);
    }

    /// Decrement the value of this location.
    pub fn dec(&self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.dec();
        result.save();
        self.from(result);
    }

    /// Perform a `CoreOp` as an abstract binary operation.
    /// Essentially, if you pass an instruction such as `Add`, `Sub`, etc.,
    /// then the corresponding operation will be performed such that:
    /// `self` is the destination, and `src` is the source.
    fn binop(&self, op: vm::CoreOp, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        src.to(result);
        result.append_core_op(op);
        src.from(result);
        self.save_to(result);
    }

    /// If this cell is non-zero, then the value of this location is now 0.
    /// Otherwise, the value of this location is now 1.
    ///
    /// Perform boolean not on the value of this cell
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

    /// Perform boolean and on the value of this cell and a source cell.
    pub fn and(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.begin_if();
        self.from(result);
        src.restore_from(result);
        self.to(result);
        result.begin_else();
        result.set_register(0);
        result.end();
        result.save();
        self.from(result);
    }

    /// Perform boolean or on the value of this cell and a source cell.
    pub fn or(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.to(result);
        result.restore();
        result.begin_if();
        result.set_register(1);
        result.begin_else();
        self.from(result);
        src.restore_from(result);
        self.to(result);
        result.end();
        result.save();
        self.from(result);
    }

    /// dst = this cell > source cell.
    pub fn is_greater_than(&self, src: &Self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.copy_to(dst, result);
        dst.sub(src, result);
        dst.dec(result);
        dst.whole_int(result);
    }

    /// dst = this cell >= source cell.
    pub fn is_greater_or_equal_to(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        self.copy_to(dst, result);
        dst.sub(src, result);
        dst.whole_int(result);
    }

    /// dst = this cell < source cell.
    pub fn is_less_than(&self, src: &Self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        src.copy_to(dst, result);
        dst.sub(self, result);
        dst.dec(result);
        dst.whole_int(result);
    }

    /// dst = this cell <= source cell.
    pub fn is_less_or_equal_to(
        &self,
        src: &Self,
        dst: &Self,
        result: &mut dyn VirtualMachineProgram,
    ) {
        src.copy_to(dst, result);
        dst.sub(self, result);
        dst.whole_int(result);
    }

    pub fn is_not_equal(&self, src: &Self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        src.copy_to(dst, result);
        dst.sub(self, result);
    }

    pub fn is_equal(&self, src: &Self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.is_not_equal(src, dst, result);
        dst.not(result);
    }

    /// This cell += source cell.
    pub fn add(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Add, src, result);
    }

    /// This cell -= source cell.
    pub fn sub(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Sub, src, result);
    }

    /// This cell *= source cell.
    pub fn mul(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Mul, src, result);
    }

    /// This cell /= source cell.
    pub fn div(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Div, src, result);
    }

    /// This cell %= source cell.
    pub fn rem(&self, src: &Self, result: &mut dyn VirtualMachineProgram) {
        self.binop(vm::CoreOp::Rem, src, result);
    }

    /// This cell = a constant value.
    pub fn set(&self, val: isize, result: &mut dyn VirtualMachineProgram) {
        result.set_register(val);
        self.save_to(result)
    }

    /// Store the value of this cell into another cell.
    pub fn copy_to(&self, dst: &Self, result: &mut dyn VirtualMachineProgram) {
        self.restore_from(result);
        dst.save_to(result);
    }
}
