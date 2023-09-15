//! # Environment
//!
//! This module defines the `Env` type, which is used to store the types, constants, and procedures
//! defined in a given scope. It also stores the variables defined in the scope, and the their offsets
//! with respect to the frame pointer.

use super::{Compile, ConstExpr, Error, GetSize, Procedure, Type};
use crate::asm::AssemblyProgram;
use core::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::{collections::HashMap, rc::Rc, sync::RwLock};


use log::{debug, warn, error};

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

    /// Memoized type sizes.
    type_sizes: Rc<RwLock<HashMap<Type, usize>>>,
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
            type_sizes: Rc::new(RwLock::new(HashMap::new())),
            // The last argument is stored at `[FP]`, so our first variable must be at `[FP + 1]`.
            fp_offset: 1,
            args_size: 0,
            expected_ret: None,
        }
    }
}

impl Env {
    /// Create a copy of the current environment but without any variables or arguments defined.
    pub(super) fn new_scope(&self) -> Self {
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
    pub(super) fn define_type(&mut self, name: impl ToString, ty: Type) {
        let name = name.to_string();
        match &ty {
            Type::Symbol(sym) if sym == &name => {
                warn!("Defining type {ty} to itself as {name}");
            }
            _ => {
                if let Ok(size) = ty.get_size(self) {
                    self.set_precalculated_size(ty.clone(), size);
                }
    
                Rc::make_mut(&mut self.types).insert(name, ty);
            }
        }
    }

    /// Get a type definition from this environment.
    pub(super) fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    /// Define a constant with a given name under this environment.
    pub(super) fn define_const(&mut self, name: impl ToString, e: ConstExpr) {
        Rc::make_mut(&mut self.consts).insert(name.to_string(), e);
    }

    /// Get a constant definition from this environment.
    pub(super) fn get_const(&self, name: &str) -> Option<&ConstExpr> {
        self.consts.get(name)
    }

    /// Define a procedure with a given name under this environment.
    pub(super) fn define_proc(&mut self, name: impl ToString, proc: Procedure) {
        Rc::make_mut(&mut self.procs).insert(name.to_string(), proc);
    }

    /// Get a procedure definition from this environment.
    pub(super) fn get_proc(&self, name: &str) -> Option<&Procedure> {
        self.procs.get(name)
    }

    /// Does this environment have a procedure with the given name?
    pub(super) fn has_proc(&self, name: &str) -> bool {
        self.procs.contains_key(name)
    }

    /// Push a procedure defined in the environment onto the stack.
    pub(super) fn push_proc(&mut self, name: &str, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        // Check if the procedure is defined.
        if let Some(proc) = Rc::make_mut(&mut self.procs).get_mut(name) {
            debug!("Pushing procedure {} onto the stack", name);
            // Compile the procedure.
            proc.clone().compile_expr(self, output)
        } else {
            error!("Undefined procedure {}", name);
            // If not, the symbol isn't defined.
            Err(Error::SymbolNotDefined(name.to_string()))
        }
    }

    /// Get a variable's size, in cells.
    pub(super) fn get_args_size(&self) -> usize {
        self.args_size
    }

    /// Get a variable's type and its offset from the frame pointer in the current scope.
    pub(super) fn get_var(&self, var: &str) -> Option<&(Type, isize)> {
        self.vars.get(var)
    }

    /// Define the arguments for the current scope (if this is a procedure).
    pub(super) fn define_args(&mut self, args: Vec<(String, Type)>) -> Result<usize, Error> {
        debug!("Defining arguments {args:?} in\n{self}");
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
            debug!("Defined argument {name} of type {t} at offset {} in\n{self}", self.fp_offset);
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
    pub(super) fn define_var(&mut self, var: impl ToString, t: Type) -> Result<isize, Error> {
        let var = var.to_string();
        // Get the size of the variable we're defining.
        let size = t.get_size(self)? as isize;
        // Remember the offset of the variable under the current scope.
        let offset = self.fp_offset;
        // Increment the frame pointer offset by the size of the variable
        // so that the next variable is allocated directly after this variable.
        debug!("Defining variable {var} of type {t} at {offset} in\n{self}");
        self.fp_offset += size;
        // Store the variable's type and offset in the environment.
        Rc::make_mut(&mut self.vars).insert(var, (t, offset));
        // Return the offset of the variable from the frame pointer.
        Ok(offset)
    }

    /// Get the expected return type of the current function.
    /// This is used to check if the returned value of a function matches the expected return type.
    /// This method returns `None` if the current scope is not a function.
    pub(super) fn get_expected_return_type(&self) -> Option<&Type> {
        self.expected_ret.as_ref()
    }

    /// Set the expected return type of the current function.
    /// If we're in a function, this will be the type of the function.
    /// If we're not in a function, this will be `None`.
    pub(super) fn set_expected_return_type(&mut self, t: Type) {
        self.expected_ret = Some(t);
    }

    /// Does the environment have some precalculated size for the given type?
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn has_precalculated_size(&self, ty: &Type) -> bool {
        self.type_sizes.read().unwrap().contains_key(ty)
    }

    /// Get the precalculated size of the given type.
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn get_precalculated_size(&self, ty: &Type) -> Option<usize> {
        self.type_sizes.read().unwrap().get(ty).copied()
    }

    /// Set the precalculated size of the given type.
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn set_precalculated_size(&self, ty: Type, size: usize) {
        debug!("Memoizing type size {ty} with size {size}");
        self.type_sizes.write().unwrap().insert(ty, size);
    }
}

impl Display for Env {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "Env")?;
        writeln!(f, "   Types:")?;
        for (name, ty) in self.types.iter() {
            writeln!(f, "      {}: {}", name, ty)?;
        }
        writeln!(f, "   Constants:")?;
        for (name, e) in self.consts.iter() {
            writeln!(f, "      {}: {}", name, e)?;
        }
        writeln!(f, "   Procedures:")?;
        for (name, proc) in self.procs.iter() {
            writeln!(f, "      {}: {}", name, proc)?;
        }
        writeln!(f, "   Variables:")?;
        for (name, (ty, offset)) in self.vars.iter() {
            writeln!(f, "      {}: {} (frame-offset {})", name, ty, offset)?;
        }
        Ok(())
    }
}