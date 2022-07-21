use super::{ConstExpr, Type, GetSize, Error};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Env {
    pub types: HashMap<String, Type>,
    pub consts: HashMap<String, ConstExpr>,
    vars: HashMap<String, (Type, isize)>,
    fp_offset: usize,
}


impl Env {
    pub fn def_var(&mut self, var: String, t: Type) -> Result<(), Error> {
        let size = t.get_size(self)?;
        self.vars.insert(var, (t, self.fp_offset as isize));
        self.fp_offset += size;
        Ok(())
    }

    pub fn get_var(&self, var: &str) -> Option<&(Type, isize)> {
        self.vars.get(var)
    }
}