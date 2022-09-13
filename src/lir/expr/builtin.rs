use super::{AssemblyProgram, Compile, Env, Error, GetType, Type, TypeCheck};
use crate::asm::{CoreOp, StandardOp};
use core::fmt;

/// A builtin pseudo-procedure implemented in the core assembly variant.
/// 
/// This is not actually executed like a legitimate procedure, but 
/// instead the assembly code is directly pasted where the procedure is called.
/// 
/// The arguments of the procedure are pushed to the stack in the order they are given.
/// The builtin must pop its arguments. The return value is left on the stack in their place by the builtin.
#[derive(Clone, PartialEq)]
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
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }
}

impl TypeCheck for CoreBuiltin {
    fn type_check(&self, _env: &Env) -> Result<(), Error> {
        Ok(())
    }
}

impl Compile for CoreBuiltin {
    fn compile_expr(self, _env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        self.body.into_iter().for_each(|op| output.op(op));
        Ok(())
    }
}

/// A builtin pseudo-procedure implemented in the standard assembly variant.
/// 
/// This is not actually executed like a legitimate procedure, but 
/// instead the assembly code is directly pasted where the procedure is called.
/// 
/// The arguments of the procedure are pushed to the stack in the order they are given.
/// The builtin must pop its arguments. The return value is left on the stack in their place by the builtin.
/// 
/// This should be used to implement important standard library functions, like `alloc` and `free`.
#[derive(Clone, PartialEq)]
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
    fn type_check(&self, _env: &Env) -> Result<(), Error> {
        Ok(())
    }
}

impl GetType for StandardBuiltin {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }
}

impl Compile for StandardBuiltin {
    fn compile_expr(self, _env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        for op in self.body {
            output.std_op(op)?
        }
        Ok(())
    }
}


impl fmt::Debug for CoreBuiltin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "core {{")?;
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


impl fmt::Debug for StandardBuiltin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, op) in self.body.iter().enumerate() {
            op.fmt(f)?;
            if i < self.body.len() - 1 {
                write!(f, " ")?
            }
        }
        Ok(())
    }
}