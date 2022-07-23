use super::{ConstExpr, Error, GetSize, Type};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Env {
    pub types: HashMap<String, Type>,
    pub consts: HashMap<String, ConstExpr>,
    vars: HashMap<String, (Type, isize)>,
    fp_offset: isize,
    args_size: usize,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            types: HashMap::new(),
            consts: HashMap::new(),
            vars: HashMap::new(),
            fp_offset: 1,
            args_size: 0,
        }
    }
}

impl Env {
    pub fn def_var(&mut self, var: String, t: Type) -> Result<isize, Error> {
        let size = t.get_size(self)? as isize;
        let offset = self.fp_offset;
        self.fp_offset += size;
        self.vars.insert(var, (t, offset));
        Ok(offset)
    }

    pub fn get_args_size(&self) -> usize {
        self.args_size
    }

    pub fn get_var(&self, var: &str) -> Option<&(Type, isize)> {
        self.vars.get(var)
    }

    pub fn def_args(&mut self, args: Vec<(String, Type)>) -> Result<usize, Error> {
        self.fp_offset = 1;
        self.args_size = 0;

        for (name, t) in args.into_iter().rev() {
            let size = t.get_size(self)?;
            self.args_size += size;
            self.fp_offset -= size as isize;
            self.vars.insert(name, (t, self.fp_offset));
        }
        self.fp_offset = 1;

        Ok(self.args_size)
    }
}
