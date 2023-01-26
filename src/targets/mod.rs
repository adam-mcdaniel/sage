//! # Targets Module
//!
//! This module contains the code dealing with building the virtual
//! machine code to various targets, such as C source code.
//!
//! ## Current Structure
//!
//! Right now, this module is a bit empty, only implementing C (GCC only)
//! as a compiler target. This is due to the fact that it has been much
//! simpler to build the language on top of the virtual machine when there
//! are fewer implementations to change.
//!
//! ## Future Structure
//!
//! In the future, this module will be *much* more featured.
//! The frontend will use `Put` and `Get` to interact with the
//! virtual machine's I/O device, and this module will provide hooks
//! to add "system calls" to implement actual hardware-specific behaviors
//! using these instructions, **but with portability in mind**.
//!
//! Consider the `fork` system call on Unix systems. A backend might
//! implement a `fork` frontend for this virtual machine by executing
//! `fork` when a correct series of `Put` values are given to the I/O device.
//!
//! Other backends might not implement `fork` though, and so "catch all"
//! code can be implemented to *allow a backend to compile **as if** `fork`
//! is provided*. This might work by providing some frontend code which takes
//! two functions and runs them serially instead of in parallel, but still
//! accomplishing a simulated `fork`'s state.
//!
//! Other hardware specific instructions can be implemented this way very
//! nicely. Consider a VGA device which displays a screen. A hardware
//! specific implementation can be written for each supported target,
//! and a "catch all" implementation can work by trying to draw the screen
//! as ASCII/UNICODE with `PutChar. A hardware specific implementation may
//! also *choose* to fail under unsupported targets to prevent use where
//! not intended.

pub mod c;
pub use c::*;

use crate::{Input, Output, vm};

pub trait CompiledDevice {
    /// Get the next input (from a given input source).
    fn get(&mut self, src: Input) -> Result<String, String>;
    /// Put the given value to the given output destination.
    fn put(&mut self, dst: Output) -> Result<String, String>;
}

/// Implement a compiler for the given target.
pub trait CompiledTarget {
    /// Compile the core variant of the machine code (must be implemented for every target).
    fn build_core(&self, program: &vm::CoreProgram) -> Result<String, String>;
    /// Compile the standard variant of the machine code (should be implemented for every target possible).
    fn build_std(&self, program: &vm::StandardProgram) -> Result<String, String>;
}
