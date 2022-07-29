mod const_expr;
mod expression;

pub use const_expr::*;
pub use expression::*;

mod procedure;
pub use procedure::*;

mod builtin;
pub use builtin::*;

use super::{Env, Error, GetSize, Type, TypeCheck};
use crate::asm::{AssemblyProgram, CoreProgram, StandardProgram};

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

pub trait Compile: TypeCheck {
    fn compile(self) -> Result<Result<CoreProgram, StandardProgram>, Error>
    where
        Self: Sized + Clone,
    {
        self.type_check(&Env::default())?;
        let mut core_asm = CoreProgram(vec![]);
        if self
            .clone()
            .compile_expr(&mut Env::default(), &mut core_asm)
            .is_err()
        {
            let mut std_asm = StandardProgram(vec![]);
            self.compile_expr(&mut Env::default(), &mut std_asm)?;
            Ok(Err(std_asm))
        } else {
            Ok(Ok(core_asm))
        }
    }
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error>;
}
