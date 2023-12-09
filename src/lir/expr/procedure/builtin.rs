//! # Builtin Procedures
//!
//! `CoreBuiltin` and `StandardBuiltin` procedures which are implemented in core or standard assembly respectively.
//!
//! These are not executed like genuine procedures, but instead, their code is inlined into the assembly code.

use crate::asm::{AssemblyProgram, CoreOp, StandardOp};
use crate::lir::{Compile, Env, Error, GetType, Type, TypeCheck};
use core::fmt;
use std::hash::{Hash, Hasher};

use log::trace;

/// A builtin pseudo-procedure implemented in the core assembly variant.
///
/// This is not actually executed like a legitimate procedure, but
/// instead the assembly code is directly pasted where the procedure is called.
///
/// The arguments of the procedure are pushed to the stack in the order they are given.
/// The builtin must pop its arguments. The return value is left on the stack in their place by the builtin.
#[derive(Clone, Debug, PartialEq)]
pub struct CoreBuiltin {
    /// The name of the builtin. This isn't used in compilation, just for debugging.
    pub name: String,
    /// The arguments of the builtin. These will be typechecked when the builtin is called.
    pub args: Vec<(String, Type)>,
    /// The return value the builtin will leave on the stack after popping its arguments.
    pub ret: Type,
    /// The list of assembly instructions to be pasted into the assembly code.
    pub body: Vec<CoreOp>,
}

impl GetType for CoreBuiltin {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        trace!("Getting type of builtin: {}", self);
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }

    fn substitute(&mut self, name: &str, ty: &Type) {
        self.args
            .iter_mut()
            .for_each(|(_, t)| *t = t.substitute(name, ty));
        self.ret = self.ret.substitute(name, ty);
    }
}

impl TypeCheck for CoreBuiltin {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        trace!("Type checking builtin: {}", self);
        for (_, ty) in &self.args {
            ty.type_check(env)?;
        }
        self.ret.type_check(env)
    }
}

impl Compile for CoreBuiltin {
    fn compile_expr(self, _env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        trace!("Compiling builtin: {}", self);
        self.body.into_iter().for_each(|op| output.op(op));
        Ok(())
    }
}

impl Hash for CoreBuiltin {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.args.hash(state);
        self.ret.hash(state);
    }
}

impl Eq for CoreBuiltin {}

/// A builtin pseudo-procedure implemented in the standard assembly variant.
///
/// This is not actually executed like a legitimate procedure, but
/// instead the assembly code is directly pasted where the procedure is called.
///
/// The arguments of the procedure are pushed to the stack in the order they are given.
/// The builtin must pop its arguments. The return value is left on the stack in their place by the builtin.
///
/// This should be used to implement important standard library functions, like `alloc` and `free`.
#[derive(Clone, Debug, PartialEq)]
pub struct StandardBuiltin {
    /// The name of the builtin. This isn't used in compilation, just for debugging.
    pub name: String,
    /// The arguments of the builtin. These will be typechecked when the builtin is called.
    pub args: Vec<(String, Type)>,
    /// The return value the builtin will leave on the stack after popping its arguments.
    pub ret: Type,
    /// The list of assembly instructions to be pasted into the assembly code.
    pub body: Vec<StandardOp>,
}

impl TypeCheck for StandardBuiltin {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        trace!("Type checking builtin: {}", self);
        for (_, ty) in &self.args {
            ty.type_check(env)?;
        }
        self.ret.type_check(env)
    }
}

impl GetType for StandardBuiltin {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        trace!("Getting type of builtin: {}", self);
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }

    fn substitute(&mut self, name: &str, ty: &Type) {
        self.args
            .iter_mut()
            .for_each(|(_, t)| *t = t.substitute(name, ty));
        self.ret = self.ret.substitute(name, ty);
    }
}

impl Compile for StandardBuiltin {
    fn compile_expr(self, _env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        trace!("Compiling builtin: {}", self);
        for op in self.body {
            output.std_op(op)?
        }
        Ok(())
    }
}

impl fmt::Display for CoreBuiltin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "proc(")?;
        for (i, op) in self.args.iter().enumerate() {
            write!(f, "{}: {}", op.0, op.1)?;
            if i < self.body.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, ") -> {} = core {{", self.ret)?;
        for (i, op) in self.body.iter().enumerate() {
            op.fmt(f)?;
            if i < self.body.len() - 1 {
                write!(f, " ")?
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for StandardBuiltin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "proc(")?;
        for (i, op) in self.args.iter().enumerate() {
            write!(f, "{}: {}", op.0, op.1)?;
            if i < self.body.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, ") -> {} = std {{", self.ret)?;
        for (i, op) in self.body.iter().enumerate() {
            op.fmt(f)?;
            if i < self.body.len() - 1 {
                write!(f, " ")?
            }
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl Eq for StandardBuiltin {}

impl Hash for StandardBuiltin {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.args.hash(state);
        self.ret.hash(state);
    }
}
