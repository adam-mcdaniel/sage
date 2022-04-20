//! # Virtual Machine Module
//! 
//! This module contains all things related to the virtual machine.
//! 
//! ### What is this machine?
//! 
//! This virtual machine is a simple turing tape machine.
//! There is one register, and a tape of cells. Cells are restricted
//! to integers in the core variant of the machine code, but floats
//! are supported in the standard variant.
//! 
//! ### What data can it use?
//! 
//! The tape on this virtual machine is an array of 64 bit integers, which
//! are interpreted as 64 bit floats when used with standard instructions.
//! 
//! An implementation of the virtual machine can be of any reasonable
//! bit width like 32 or 64 bits. For each implementation, the bits
//! of the integer and floats supported should be identical. The 
//! default implementation should be with 64 bit ints and floats, but
//! 8 bit ints and no floats for a hardware implementation would suffice.

mod core;
pub use self::core::*;

mod std;
pub use self::std::*;


pub trait VirtualMachineProgram {
    fn append_core_op(&mut self, op: CoreOp);
    fn append_standard_op(&mut self, op: StandardOp);

    fn comment(&mut self, comment: &str) {
        self.append_core_op(CoreOp::Comment(comment.to_string()));
    }

    fn restore(&mut self) {
        self.append_core_op(CoreOp::Restore);
    }

    fn save(&mut self) {
        self.append_core_op(CoreOp::Save);
    }

    fn ret(&mut self) {
        self.append_core_op(CoreOp::Return);
    }

    fn where_is_pointer(&mut self) {
        self.append_core_op(CoreOp::Where);
    }

    fn deref(&mut self) {
        self.append_core_op(CoreOp::Deref);
    }

    fn refer(&mut self) {
        self.append_core_op(CoreOp::Refer);
    }

    fn move_pointer(&mut self, cells: isize) {
        self.append_core_op(CoreOp::Move(cells));
    }

    fn set_register(&mut self, val: isize) {
        self.append_core_op(CoreOp::Constant(val))
    }

    fn begin_while(&mut self) {
        self.append_core_op(CoreOp::While)
    }

    fn begin_if(&mut self) {
        self.append_core_op(CoreOp::If)
    }

    fn begin_else(&mut self) {
        self.append_core_op(CoreOp::Else)
    }

    fn begin_function(&mut self) {
        self.append_core_op(CoreOp::Function)
    }

    fn end(&mut self) {
        self.append_core_op(CoreOp::End)
    }

    fn call(&mut self) {
        self.append_core_op(CoreOp::Call)
    }

    fn inc(&mut self) {
        self.append_core_op(CoreOp::Inc)
    }

    fn dec(&mut self) {
        self.append_core_op(CoreOp::Dec)
    }

    fn getchar(&mut self) {
        self.append_core_op(CoreOp::GetChar)
    }

    fn putchar(&mut self) {
        self.append_core_op(CoreOp::PutChar)
    }

    fn is_whole_int(&mut self) {
        self.append_core_op(CoreOp::IsWhole)
    }
}
