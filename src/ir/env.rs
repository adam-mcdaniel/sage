use super::{ConstExpr, Error, GetSize, Type};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Env {
    /// The types defined under the environment.
    pub types: HashMap<String, Type>,
    /// The constants defined under the environment.
    pub consts: HashMap<String, ConstExpr>,
    /// The variables defined under the environment.
    vars: HashMap<String, (Type, isize)>,
    /// The current offset of the frame pointer to assign to the next variable.
    /// This is incremented by the size of each variable as it is defined.
    fp_offset: isize,
    /// The size of the arguments supplied to the function, in cells.
    /// This is incremented by the size of each argument defined (for a procedure).
    /// This is unaffected by defining *variables* in the scope of the function.
    args_size: usize,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            types: HashMap::new(),
            consts: HashMap::new(),
            vars: HashMap::new(),
            // The last argument is stored at `[FP]`, so our first variable must be at `[FP + 1]`.
            fp_offset: 1,
            args_size: 0,
        }
    }
}

impl Env {
    /// Get a variable's size, in cells.
    pub fn get_args_size(&self) -> usize {
        self.args_size
    }

    /// Get a variable's type and its offset from the frame pointer in the current scope.
    pub fn get_var(&self, var: &str) -> Option<&(Type, isize)> {
        self.vars.get(var)
    }

    /// Define the arguments for the current scope (if this is a procedure).
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

    /// Define a variable in the current scope.
    /// This will increment the scope's frame pointer offset by the size of the variable.
    pub fn def_var(&mut self, var: String, t: Type) -> Result<isize, Error> {
        let size = t.get_size(self)? as isize;
        let offset = self.fp_offset;
        self.fp_offset += size;
        self.vars.insert(var, (t, offset));
        Ok(offset)
    }
}
