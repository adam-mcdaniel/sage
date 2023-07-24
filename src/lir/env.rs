//! # Environment
//!
//! This module defines the `Env` type, which is used to store the types, constants, and procedures
//! defined in a given scope. It also stores the variables defined in the scope, and the their offsets
//! with respect to the frame pointer.

use super::{Compile, ConstExpr, Error, GetSize, Procedure, Type};
use crate::asm::AssemblyProgram;
use std::{collections::HashMap, rc::Rc};

/// An environment under which expressions and types are compiled and typechecked.
/// This is essentially the scope of an expression.
#[derive(Clone, Debug)]
pub struct Env {
    /// The types (and also their sizes) defined under the environment.
    types: Rc<HashMap<String, Type>>,
    /// The constants defined under the environment.
    consts: Rc<HashMap<String, ConstExpr>>,
    /// The procedures defined under the environment.
    procs: Rc<HashMap<String, Procedure>>,
    /// The variables defined under the environment.
    vars: Rc<HashMap<String, (Type, isize)>>,
    /// The current offset of the frame pointer to assign to the next variable.
    /// This is incremented by the size of each variable as it is defined.
    fp_offset: isize,
    /// The size of the arguments supplied to the function, in cells.
    /// This is incremented by the size of each argument defined (for a procedure).
    /// This is unaffected by defining *variables* in the scope of the function.
    args_size: usize,
    /// Expected return type of the current function.
    /// This is `None` if we are not currently compiling a function.
    expected_ret: Option<Type>,

    type_sizes: HashMap<Type, usize>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            // It is important that we use reference counting for the tables because the environment
            // will be copied many times during the compilation process to create new scopes.
            types: Rc::new(HashMap::new()),
            consts: Rc::new(HashMap::new()),
            procs: Rc::new(HashMap::new()),
            vars: Rc::new(HashMap::new()),
            type_sizes: HashMap::new(),
            // The last argument is stored at `[FP]`, so our first variable must be at `[FP + 1]`.
            fp_offset: 1,
            args_size: 0,
            expected_ret: None,
        }
    }
}

impl Env {
    /// Create a copy of the current environment but without any variables or arguments defined.
    pub fn new_scope(&self) -> Self {
        Self {
            // Only keep the types, constants, and procedures defined.
            types: self.types.clone(),
            consts: self.consts.clone(),
            procs: self.procs.clone(),
            type_sizes: self.type_sizes.clone(),

            // The rest are the same as a new environment.
            ..Env::default()
        }
    }

    /// Define a type with a given name under this environment.
    pub fn define_type(&mut self, name: impl ToString, ty: Type) {
        if Type::Symbol(name.to_string()) != ty {
            if let Ok(size) = ty.get_size(self) {
                self.type_sizes.insert(ty.clone(), size);
            }

            Rc::make_mut(&mut self.types).insert(name.to_string(), ty);
        }
    }

    /// Get a type definition from this environment.
    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    /// Define a constant with a given name under this environment.
    pub fn define_const(&mut self, name: impl ToString, e: ConstExpr) {
        Rc::make_mut(&mut self.consts).insert(name.to_string(), e);
    }

    /// Get a constant definition from this environment.
    pub fn get_const(&self, name: &str) -> Option<&ConstExpr> {
        self.consts.get(name)
    }

    /// Define a procedure with a given name under this environment.
    pub fn define_proc(&mut self, name: impl ToString, proc: Procedure) {
        Rc::make_mut(&mut self.procs).insert(name.to_string(), proc);
    }

    /// Get a procedure definition from this environment.
    pub fn get_proc(&self, name: &str) -> Option<&Procedure> {
        self.procs.get(name)
    }

    /// Does this environment have a procedure with the given name?
    pub fn has_proc(&self, name: &str) -> bool {
        self.procs.contains_key(name)
    }

    /// Push a procedure defined in the environment onto the stack.
    pub fn push_proc(&mut self, name: &str, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        // Check if the procedure is defined.
        if let Some(proc) = Rc::make_mut(&mut self.procs).get_mut(name) {
            // Compile the procedure.
            proc.clone().compile_expr(self, output)
        } else {
            // If not, the symbol isn't defined.
            Err(Error::SymbolNotDefined(name.to_string()))
        }
    }

    /// Get a variable's size, in cells.
    pub fn get_args_size(&self) -> usize {
        self.args_size
    }

    /// Get a variable's type and its offset from the frame pointer in the current scope.
    pub fn get_var(&self, var: &str) -> Option<&(Type, isize)> {
        self.vars.get(var)
    }

    /// Define the arguments for the current scope (if this is a procedure).
    pub fn define_args(&mut self, args: Vec<(String, Type)>) -> Result<usize, Error> {
        self.fp_offset = 1;
        self.args_size = 0;

        // For each argument in reverse order (starting from the last argument)
        for (name, t) in args.into_iter().rev() {
            // Get the size of the argument we're defining.
            let size = t.get_size(self)?;
            // Add the size of the argument to the total number of cells taken up by the arguments.
            self.args_size += size;
            // Decrement the frame pointer offset by the size of the argument
            // so that the FP + the offset is the address of the argument.
            self.fp_offset -= size as isize;
            // Store the argument's type and offset in the environment.
            Rc::make_mut(&mut self.vars).insert(name, (t, self.fp_offset));
        }
        // Set the frame pointer offset to `1` so that the first variable defined under the scope is at `[FP + 1]`.
        self.fp_offset = 1;

        // Return the size of the arguments for the procedure in cells,
        // so that the compiler can deallocate the arguments after compiling the procedure.
        Ok(self.args_size)
    }

    /// Define a variable in the current scope.
    /// This will increment the scope's frame pointer offset by the size of the variable.
    /// This method returns the offset of the variable from the frame pointer under this scope.
    pub fn define_var(&mut self, var: impl ToString, t: Type) -> Result<isize, Error> {
        // Get the size of the variable we're defining.
        let size = t.get_size(self)? as isize;
        // Remember the offset of the variable under the current scope.
        let offset = self.fp_offset;
        // Increment the frame pointer offset by the size of the variable
        // so that the next variable is allocated directly after this variable.
        self.fp_offset += size;
        // Store the variable's type and offset in the environment.
        Rc::make_mut(&mut self.vars).insert(var.to_string(), (t, offset));
        // Return the offset of the variable from the frame pointer.
        Ok(offset)
    }

    /// Get the expected return type of the current function.
    /// This is used to check if the returned value of a function matches the expected return type.
    /// This method returns `None` if the current scope is not a function.
    pub fn get_expected_return_type(&self) -> Option<&Type> {
        self.expected_ret.as_ref()
    }

    /// Set the expected return type of the current function.
    /// If we're in a function, this will be the type of the function.
    /// If we're not in a function, this will be `None`.
    pub fn set_expected_return_type(&mut self, t: Type) {
        self.expected_ret = Some(t);
    }

    pub fn define_type_size(&mut self, ty: Type, size: usize) {
        self.type_sizes.insert(ty, size);
    }

    pub fn get_type_size(&self, ty: &Type) -> Option<usize> {
        self.type_sizes.get(ty).copied()
    }
}
