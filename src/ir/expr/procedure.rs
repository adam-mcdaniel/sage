use crate::asm::{AssemblyProgram, CoreOp, A, FP, SP};
use crate::ir::{Compile, Env, Error, Expr, GetSize, GetType, Type, TypeCheck};
use std::sync::Mutex;

use lazy_static::lazy_static;
lazy_static! {
    static ref LAMBDA_COUNT: Mutex<usize> = Mutex::new(0);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Procedure {
    name: String,
    args: Vec<(String, Type)>,
    ret: Type,
    body: Box<Expr>,
}

impl Procedure {
    pub fn new(args: Vec<(String, Type)>, ret: Type, body: impl Into<Expr>) -> Self {
        let mut lambda_count = LAMBDA_COUNT.lock().unwrap();
        *lambda_count += 1;
        Self {
            name: format!("__LAMBDA_{lambda_count}"),
            args,
            ret,
            body: Box::new(body.into()),
        }
    }
}

impl TypeCheck for Procedure {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        let mut new_env = Env::default();
        new_env.def_args(self.args.clone())?;

        self.body.get_type(&new_env)?.equals(&self.ret, env)?;
        self.body.type_check(&new_env)
    }
}

impl GetType for Procedure {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }
}

impl Compile for Procedure {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        // Compile the contents of the procedure under a new environment
        let mut new_env = Env::default();
        // Declare the arguments and get their size
        let args_size = new_env.def_args(self.args)?;
        // Get the size of the return value to leave on the stack
        let ret_size = self.ret.get_size(env)?;
        // Declare the function body
        output.op(CoreOp::Fn(self.name.clone()));
        // Execute the body to leave the return value
        self.body.compile_expr(&mut new_env, output)?;

        // Overwrite the arguments with the return value
        output.op(CoreOp::Copy {
            dst: FP.deref().offset(1 - args_size as isize),
            src: SP.deref().offset(1 - ret_size as isize),
            size: ret_size,
        });
        // Decrement the stack pointer by the difference between the size of the
        // arguments and return value, to leave the return value on the stack.
        output.op(CoreOp::Pop(None, args_size));
        // End the function body
        output.op(CoreOp::End);

        // Push the procedure label address onto the stack
        output.op(CoreOp::SetLabel(A, self.name));
        output.op(CoreOp::Push(A, 1));
        Ok(())
    }
}
