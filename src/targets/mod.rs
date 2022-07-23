pub mod c;
pub use c::*;

use crate::{asm, ir, vm};

/// Implement a compiler for the given target.
pub trait Target {
    /// Compile the core variant of the machine code (must be implemented for every target).
    fn compile_core(&self, program: &vm::CoreProgram) -> Result<String, String>;
    /// Compile the standard variant of the machine code (should be implemented for every target possible).
    fn compile_std(&self, program: &vm::StandardProgram) -> Result<String, String>;

    fn assemble(
        &self,
        allowed_recursion_depth: usize,
        program: &dyn asm::AssemblyProgram,
    ) -> Result<String, String> {
        match program.assemble(allowed_recursion_depth) {
            Ok(core_or_std) => match core_or_std {
                Ok(core) => self.compile_core(&core),
                Err(std) => self.compile_std(&std),
            },
            Err(e) => Err(format!("{:?}", e)),
        }
    }

    fn compile(
        &self,
        expr: ir::Expr,
        mut env: ir::Env,
        allowed_recursion_depth: usize,
    ) -> Result<String, String> {
        let mut core_program = asm::CoreProgram(vec![]);
        use ir::Compile;
        match expr.clone().compile(&mut env, &mut core_program) {
            Ok(()) => self.assemble(allowed_recursion_depth, &core_program),
            Err(_) => {
                let mut std_program = asm::StandardProgram(vec![]);
                match expr.compile(&mut env, &mut std_program) {
                    Ok(()) => self.assemble(allowed_recursion_depth, &std_program),
                    Err(e) => Err(format!("{:?}", e)),
                }
            }
        }
    }
}
