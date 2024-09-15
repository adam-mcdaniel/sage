//! # C Target
//!
//! An implementation of the virtual machine for the C language.
//!
//! This allows the virtual machine to target C programs.
use super::{Architecture, CompiledTarget};
use crate::{
    side_effects::{Input, Output},
    vm::{CoreOp, StandardOp},
};

/// The type for the C target which implements the `Target` trait.
/// This allows the compiler to target the C language.
#[derive(Default)]
pub struct SageLisp(sage_lisp::Env);

impl SageLisp {
    pub fn new(env: sage_lisp::Env) -> Self {
        Self(env)
    }
}

impl Architecture for SageLisp {
    fn supports_input(&self, _i: &Input) -> bool {
        true
    }

    fn supports_output(&self, _o: &Output) -> bool {
        true
    }

    fn op(&mut self, _op: &CoreOp) -> String {
        String::new()
    }

    fn std_op(&mut self, _op: &StandardOp) -> Result<String, String> {
        Ok(String::new())
    }

    fn end(&mut self, _matching: &CoreOp, _fun: Option<usize>) -> String {
        String::new()
    }

    fn declare_proc(&mut self, _label_id: usize) -> String {
        String::new()
    }

    fn name(&self) -> &str {
        "Sage Lisp"
    }
    fn version(&self) -> &str {
        "1.0"
    }

    fn supports_floats(&self) -> bool {
        true
    }

    fn get(&mut self, _src: &Input) -> Result<String, String> {
        Ok(Default::default())
    }

    fn put(&mut self, _dst: &Output) -> Result<String, String> {
        Ok(Default::default())
    }
    fn peek(&mut self) -> Result<String, String> {
        Ok(Default::default())
    }
    fn poke(&mut self) -> Result<String, String> {
        Ok(Default::default())
    }
    fn prelude(&self, _is_core: bool) -> Option<String> {
        Default::default()
    }

    fn post_funs(&self, _funs: Vec<i32>) -> Option<String> {
        Default::default()
    }

    fn postop(&self) -> Option<String> {
        Default::default()
    }

    fn postlude(&self, _is_core: bool) -> Option<String> {
        Default::default()
    }
}

impl CompiledTarget for SageLisp {
    fn build_core(&mut self, program: &crate::vm::CoreProgram) -> Result<String, String> {
        // Try to evaluate the `build-core` function from the environment on the input.
        let arg = sage_lisp::Expr::serialize(program);
        let c = sage_lisp::Expr::symbol("build-core");
        let s = sage_lisp::Expr::symbol("build-std");
        let result = if self.0.get(&c).is_some() {
            self.0.eval(c.apply(&[arg.quote()]))
        } else {
            self.0.eval(s.apply(&[sage_lisp::Expr::serialize(crate::vm::StandardProgram::from(program.clone())).quote()]))
        };

        Ok(result.to_string())
    }

    fn build_std(&mut self, program: &crate::vm::StandardProgram) -> Result<String, String> {
        // Try to evaluate the `build-core` function from the environment on the input.
        let arg = sage_lisp::Expr::serialize(program);
        let call = sage_lisp::Expr::symbol("build-std").apply(&[arg.quote()]);

        let result = self.0.eval(call);

        Ok(result.to_string())
    }
}
