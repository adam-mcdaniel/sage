use super::{Type, GetType, Env, Error};
use crate::asm::{CoreOp, StandardOp};

#[derive(Clone, Debug)]
pub struct CoreBuiltin {
    name: String,
    args: Vec<(String, Type)>,
    ret: Type,
    body: Vec<CoreOp>,
}

impl GetType for CoreBuiltin {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }
}


#[derive(Clone, Debug)]
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
