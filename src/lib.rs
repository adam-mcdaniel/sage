pub mod asm;
pub mod ir;
pub mod targets;
pub mod vm;

/// Implement a compiler for the given target.
pub trait CompilerTarget {
    /// Compile the core variant of the machine code (must be implemented for every target).
    fn compile_core(&self, program: &vm::CoreProgram) -> Result<String, String>;
    /// Compile the standard variant of the machine code (should be implemented for every target possible).
    fn compile_standard(&self, program: &vm::StandardProgram) -> Result<String, String>;
}
