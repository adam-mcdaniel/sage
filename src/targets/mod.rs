pub mod c;
pub use c::*;

use crate::vm;

/// Implement a compiler for the given target.
pub trait Target {
    /// Compile the core variant of the machine code (must be implemented for every target).
    fn build_core(&self, program: &vm::CoreProgram) -> Result<String, String>;
    /// Compile the standard variant of the machine code (should be implemented for every target possible).
    fn build_std(&self, program: &vm::StandardProgram) -> Result<String, String>;
}
