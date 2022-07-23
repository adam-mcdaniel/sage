mod const_expr;
mod expression;

pub use const_expr::*;
pub use expression::*;

mod procedure;
pub use procedure::*;

mod builtin;
pub use builtin::*;

use super::{Env, Error, GetSize, Type};
use crate::asm::AssemblyProgram;

pub trait GetType {
    fn get_type(&self, env: &Env) -> Result<Type, Error> {
        self.get_type_checked(env, 0)
    }

    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error>;
}

impl<T> GetSize for T
where
    T: GetType,
{
    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error> {
        self.get_type(env)?.get_size_checked(env, i)
    }
}

pub trait Compile {
    fn compile(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error>;
}
