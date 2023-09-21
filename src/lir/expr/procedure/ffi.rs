//! # FFI-Procedure
//!
//! A typed procedure which calls a foreign function.
//! This is compiled down to a standard assembly `Call` instruction.
//! The label is the name of the foreign function. The types determine the
//! size of the cells for the arguments and return value.
use crate::asm::{AssemblyProgram, StandardOp};
use crate::lir::{Compile, Env, Error, GetSize, GetType, Type, TypeCheck};
use crate::side_effects::FFIBinding;
use core::fmt::{Display, Formatter, Result as FmtResult};

use log::debug;

/// A typed procedure which calls a foreign function.
/// This is compiled down to a standard assembly `Call` instruction.
/// The label is the name of the foreign function. The types determine the
/// size of the cells for the arguments and return value.
#[derive(Clone, Debug, PartialEq)]
pub struct FFIProcedure {
    /// The name of the foreign function.
    name: String,
    /// The types of the arguments.
    args: Vec<Type>,
    /// The return type of the foreign function.
    ret: Type,
}

impl FFIProcedure {
    /// Create a new FFI procedure.
    pub fn new(name: String, args: Vec<Type>, ret: Type) -> Self {
        Self { name, args, ret }
    }
}

impl TypeCheck for FFIProcedure {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        for ty in &self.args {
            ty.type_check(env)?;
        }
        self.ret.type_check(env)
    }
}

impl GetType for FFIProcedure {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Proc(
            self.args.iter().map(|t| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }

    fn substitute(&mut self, name: &str, substitution: &Type) {
        self.args
            .iter_mut()
            .for_each(|arg_ty| *arg_ty = arg_ty.substitute(name, substitution));
        self.ret = self.ret.substitute(name, substitution);
    }
}

impl Compile for FFIProcedure {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        debug!("Compiling FFI procedure: {}", self);
        let mut args_size = 0;
        for arg in &self.args {
            args_size += arg.get_size(env)?;
        }
        let ret_size = self.ret.get_size(env)?;

        output.std_op(StandardOp::Call(FFIBinding::new(
            self.name, args_size, ret_size,
        )))?;

        Ok(())
    }
}

impl Display for FFIProcedure {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ") -> {}", self.ret)
    }
}
