use super::{AssemblyProgram, Compile, Env, Error, GetType, Type};
use crate::asm::{CoreOp, StandardOp};

#[derive(Clone, Debug, PartialEq)]
pub struct CoreBuiltin {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret: Type,
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

impl Compile for CoreBuiltin {
    fn compile(self, _env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        self.body.into_iter().for_each(|op| output.op(op));
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StandardBuiltin {
    name: String,
    args: Vec<(String, Type)>,
    ret: Type,
    body: Vec<StandardOp>,
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
    fn compile(self, _env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        for op in self.body {
            output.std_op(op)?
        }
        Ok(())
    }
}
