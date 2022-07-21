use super::{Env, Error, Expr, GetType, Type};

#[derive(Clone, Debug)]
pub struct Procedure {
    name: String,
    args: Vec<(String, Type)>,
    ret: Type,
    body: Box<Expr>,
}

impl GetType for Procedure {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Proc(
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }
}
