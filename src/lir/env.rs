//! # Environment
//!
//! This module defines the `Env` type, which is used to store the types, constants, and procedures
//! defined in a given scope. It also stores the variables defined in the scope, and the their offsets
//! with respect to the frame pointer.

use super::{Compile, ConstExpr, Declaration, Error, GetType, GetSize, Mutability, FFIProcedure, PolyProcedure, Procedure, Type};
use crate::asm::{AssemblyProgram, Globals, Location};
use core::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::{
    collections::HashMap,
    rc::Rc,
    sync::{Mutex, RwLock},
};

use log::{debug, error, trace, warn};

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
    vars: Rc<HashMap<String, (Mutability, Type, isize)>>,
    /// The static variables defined under the environment.
    static_vars: Rc<HashMap<String, (Mutability, Type, Location)>>,
    /// A lookup for the offsets of global variables.
    globals: Rc<Mutex<Globals>>,
    /// Associated constants for types.
    associated_constants: Rc<RwLock<HashMap<Type, HashMap<String, (ConstExpr, Type)>>>>,

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
            type_sizes: Rc::new(RwLock::new(HashMap::new())),
            consts: Rc::new(HashMap::new()),
            procs: Rc::new(HashMap::new()),
            vars: Rc::new(HashMap::new()),
            static_vars: Rc::new(HashMap::new()),
            globals: Rc::new(Mutex::new(Globals::new())),
            associated_constants: Rc::new(RwLock::new(HashMap::new())),

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
            static_vars: self.static_vars.clone(),
            type_sizes: self.type_sizes.clone(),
            globals: self.globals.clone(),
            associated_constants: self.associated_constants.clone(),

            // The rest are the same as a new environment.
            ..Env::default()
        }
    }

    /// Get the type of an associated constant of a type.
    pub fn get_type_of_associated_const(&self, ty: &Type, name: &str) -> Option<Type> {
        trace!("Getting type of associated const {name} of type {ty} in {self}");
        let associated_constants = self.associated_constants.read().unwrap();

        if let Some((_, expr_ty)) = associated_constants.get(ty).and_then(|consts| consts.get(name)) {
            trace!("Found memoized type of associated const {name} of type {ty} in {self}");
            return Some(expr_ty.clone());
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.iter() {
            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                trace!("Type {other_ty} does not equal {ty}");
                continue;
            }
            if let Some((constant, expr_ty)) = consts.get(name) {
                let constant = constant.clone();
                let expr_ty = expr_ty.clone();
                drop(associated_constants);
                self.memoize_associated_const(ty, name, constant, expr_ty.clone()).ok()?;
                return Some(expr_ty);
            }
        }
        trace!("Could not find associated const {name} of type {ty} in {self}");
        drop(associated_constants);

        if let Type::Type(inner_ty) = ty {
            if let Some(ty) = self.get_type_of_associated_const(inner_ty, name) {
                return Some(ty);
            }
        }
        if let Type::Pointer(_, inner_ty) = ty {
            if let Some(ty) = self.get_type_of_associated_const(inner_ty, name) {
                return Some(ty);
            }
        }
        if let Type::Unit(_, inner_ty) = ty {
            if let Some(ty) = self.get_type_of_associated_const(inner_ty, name) {
                return Some(ty);
            }
        }
        None
    }

    pub fn get_associated_const(&self, ty: &Type, name: &str) -> Option<ConstExpr> {
        trace!("Getting associated const {name} of type {ty} in {self}");
        let associated_constants = self.associated_constants.read().unwrap();
        
        if let Some((constant, _)) = associated_constants.get(ty).and_then(|consts| consts.get(name)) {
            trace!("Found associated const {name} of type {ty} in {self}");
            return Some(constant.clone());
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.iter() {
            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                trace!("Type {other_ty} does not equal {ty}");
                continue;
            }
            trace!("Found eligible type {other_ty} for {ty}");
            if let Some((const_expr, expr_ty)) = consts.get(name) {
                let expr_ty = expr_ty.clone();
                let const_expr = const_expr.clone();
                drop(associated_constants);
                self.memoize_associated_const(ty, name, const_expr.clone(), expr_ty).ok()?;
                return Some(const_expr);
            }
        }
        trace!("Could not find associated const {name} of type {ty} in {self}");
        drop(associated_constants);

        if let Type::Type(inner_ty) = ty {
            if let Some(constant) = self.get_associated_const(inner_ty, name) {
                let expr_ty = constant.get_type(self).ok()?;
                self.memoize_associated_const(ty, name, constant.clone(), expr_ty).ok()?;
                return Some(constant);
            }
        }
        if let Type::Pointer(_mutability, inner_ty) = ty {
            if let Some(constant) = self.get_associated_const(inner_ty, name) {
                // Memoize the associated constant.
                let expr_ty = constant.get_type(self).ok()?;
                self.memoize_associated_const(ty, name, constant.clone(), expr_ty).ok()?;
                return Some(constant);
            }
        }
        if let Type::Unit(_unit_name, inner_ty) = ty {
            if let Some(constant) = self.get_associated_const(inner_ty, name) {
                // Memoize the associated constant.
                let expr_ty = constant.get_type(self).ok()?;
                self.memoize_associated_const(ty, name, constant.clone(), expr_ty).ok()?;
                return Some(constant);
            }
        }
        None
    }

    fn memoize_associated_const(&self, ty: &Type, name: &str, constant: ConstExpr, expr_ty: Type) -> Result<(), Error> {
        trace!("Memoizing associated const {name} of type {ty} in {self}");
        let mut associated_constants = self.associated_constants.write().unwrap();
        // Does the type already have the associated constant?
        if let Some(consts) = associated_constants.get(ty) {
            if let Some((_, _)) = consts.get(name) {
                // If so, we don't need to memoize it.
                return Ok(());
            }
        }

        let consts = associated_constants.entry(ty.clone()).or_default();
        if consts.contains_key(name) {
            return Ok(());
        }
        consts.insert(name.to_owned(), (constant, expr_ty));
        Ok(())
    }

    pub fn has_associated_const(&self, ty: &Type, name: &str) -> bool {
        self.get_associated_const(ty, name).is_some()
    }

    pub fn get_all_associated_consts(&self, ty: &Type) -> Vec<(String, ConstExpr)> {
        trace!("Getting all associated constants of type {ty}");
        let associated_constants = self.associated_constants.read().unwrap();
        let mut result = Vec::new();
        if let Some(consts) = associated_constants.get(ty) {
            for (name, (const_expr, _)) in consts.iter() {
                result.push((name.to_owned(), const_expr.clone()));
            }
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.iter() {
            if ty == other_ty {
                continue;
            }
            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                trace!("Type {other_ty} does not equal {ty}");
                continue;
            }
            trace!("Found eligible type {other_ty} for {ty}");
            for (name, (const_expr, _)) in consts.iter() {
                result.push((name.to_owned(), const_expr.clone()));
            }
        }
        result
    }

    pub fn add_associated_const(
        &mut self,
        ty: Type,
        associated_const_name: impl ToString,
        expr: ConstExpr,
    ) -> Result<(), Error> {
        let associated_const_name = associated_const_name.to_string();
        trace!("Defining associated const {associated_const_name} as {expr} to type {ty}");
        let expr_ty = expr.get_type(self)?;
        // // Rc::make_mut(&mut self.associated_constants)
        //     .entry(ty)
        //     .or_default()
        //     .insert(associated_const_name, (expr, expr_ty));
        let mut associated_constants = self.associated_constants.write().unwrap();
        associated_constants
            .entry(ty)
            .or_default()
            .insert(associated_const_name, (expr, expr_ty));

        Ok(())
    }

    /// Add all the declarations to this environment.
    pub(super) fn add_declaration(&mut self, declaration: &Declaration) -> Result<(), Error> {
        self.add_compile_time_declaration(declaration)?;
        self.add_local_variable_declaration(declaration)?;
        Ok(())
    }

    /// Add all the compile-time declarations to this environment. These are declarations
    /// for types, constants, and procedures that are defined at compile-time. Variables
    /// are not included because they are defined at runtime.
    pub(super) fn add_compile_time_declaration(&mut self, declaration: &Declaration) -> Result<(), Error> {
        match declaration {
            Declaration::Type(name, ty) => {
                self.define_type(name, ty.clone());
            }
            Declaration::Const(name, e) => {
                self.define_const(name, e.clone());
            }
            Declaration::Proc(name, proc) => {
                self.define_proc(name, proc.clone());
            }
            Declaration::PolyProc(name, proc) => {
                self.define_poly_proc(name, proc.clone());
            }
            Declaration::ExternProc(name, proc) => {
                self.define_ffi_proc(name, proc.clone());
            }
            Declaration::StaticVar(name, mutability, ty, _expr) => {
                self.define_static_var(name, *mutability, ty.clone())?;
            }
            Declaration::Impl(ty, impls) => {
                for (name, associated_const) in impls {
                    self.add_associated_const(ty.clone(), name, associated_const.clone())?;
                }
            }
            Declaration::Var(_, _, _, _) => {
                // Variables are not defined at compile-time.
            }
            Declaration::VarPat(_, _) => {
                // Variables are not defined at compile-time.
            }
            Declaration::Many(decls) => {
                for decl in decls {
                    self.add_compile_time_declaration(decl)?;
                }
                for decl in decls {
                    if let Declaration::Type(name, ty) = decl {
                        if let Ok(size) = ty.get_size(self) {
                            self.set_precalculated_size(ty.clone(), size);
                        } else {
                            warn!("Failed to memoize type size for {name} = {ty}");
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Add a single variable declaration to this environment. These are declarations
    /// for variables that are defined at runtime. Types, constants, and procedures
    /// are not included because they are defined at compile-time.
    pub(super) fn add_local_variable_declaration(&mut self, declaration: &Declaration) -> Result<(), Error> {
        match declaration {
            Declaration::Type(_, _) => {
                // Types are not defined at runtime.
            }
            Declaration::Const(_, _) => {
                // Constants are not defined at runtime.
            }
            Declaration::Proc(_, _) => {
                // Procedures are not defined at runtime.
            }
            Declaration::PolyProc(_, _) => {
                // Polymorphic procedures are not defined at runtime.
            }
            Declaration::ExternProc(_, _) => {
                // FFI procedures are not defined at runtime.
            }
            Declaration::StaticVar(_, _, _, _) => {
                // Static variables are not defined at runtime.
            }
            Declaration::Impl(_, _) => {
                // Implementations are not defined at runtime.
            }
            Declaration::Var(name, mutability, ty, expr) => {
                let ty = match ty {
                    Some(ty) => ty.clone(),
                    None => expr.get_type(self)?,
                };
                self.define_var(name, *mutability, ty)?;
            }
            Declaration::VarPat(pat, expr) => {
                let ty = expr.get_type(self)?;
                pat.declare_let_bind(expr, &ty, self)?;
            }
            Declaration::Many(decls) => {
                for decl in decls {
                    self.add_local_variable_declaration(decl)?;
                }
            }
        }
        Ok(())
    }

    /// Define a static variable with a given name under this environment.
    pub(super) fn define_static_var(
        &mut self,
        name: impl ToString,
        mutability: Mutability,
        ty: Type,
    ) -> Result<Location, Error> {
        let name = name.to_string();
        let size = ty.get_size(self)?;
        let mut globals = self.globals.lock().unwrap();
        let location = globals.add_global(name.clone(), size);

        trace!("Defining static variable {name} of type {ty} at {location}");
        Rc::make_mut(&mut self.static_vars).insert(name, (mutability, ty, location.clone()));
        Ok(location)
    }

    /// Get a static variable definition from this environment.
    /// This contains:
    /// 1. The mutability of the variable.
    /// 2. The type of the variable.
    /// 3. The location of the variable in memory.
    pub(super) fn get_static_var(&self, name: &str) -> Option<&(Mutability, Type, Location)> {
        self.static_vars.get(name)
    }

    /// Define a type with a given name under this environment.
    pub(super) fn define_type(&mut self, name: impl ToString, ty: Type) {
        let name = name.to_string();
        match &ty {
            Type::Symbol(sym) if sym == &name => {
                warn!("Defining type {ty} to itself as {name}");
            }
            _ => {
                trace!("Defining type {name} as {ty}");
                Rc::make_mut(&mut self.types).insert(name, ty.clone());

                if let Ok(size) = ty.get_size(self) {
                    self.set_precalculated_size(ty.clone(), size);
                }
            }
        }
    }

    /// Define multiple types with the given names under this environment.
    ///
    /// This must be used in situations where the different types depend on each other.
    /// This is because the sizes of types are memoized, and this will interfere with
    /// the memoization process if the types are defined separately. It will lead to
    /// typechecking errors if the environment does not already have a memoized size
    /// for the type of a subexpression.
    pub fn define_types(&mut self, types: Vec<(String, Type)>) {
        for (name, ty) in &types {
            match &ty {
                Type::Symbol(sym) if sym == name => {
                    warn!("Defining type {ty} to itself as {name}");
                }
                _ => {
                    trace!("Defining type {name} as {ty}");
                    Rc::make_mut(&mut self.types).insert(name.clone(), ty.clone());
                }
            }
        }

        for (_, ty) in types {
            if let Ok(size) = ty.get_size(self) {
                self.set_precalculated_size(ty, size);
            } else {
                warn!("Failed to memoize type size for {ty}");
            }
        }
    }

    /// Get a type definition from this environment.
    pub(super) fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    /// Define a constant with a given name under this environment.
    pub(super) fn define_const(&mut self, name: impl ToString, e: ConstExpr) {
        let name = name.to_string();
        trace!("Defining constant {name} as {e}");
        Rc::make_mut(&mut self.consts).insert(name, e);
    }

    /// Get a constant definition from this environment.
    pub(super) fn get_const(&self, name: &str) -> Option<&ConstExpr> {
        self.consts.get(name)
    }

    /// Define a procedure with a given name under this environment.
    pub(super) fn define_proc(&mut self, name: impl ToString, proc: Procedure) {
        let name = name.to_string();
        trace!("Defining procedure {name} as {proc}");
        Rc::make_mut(&mut self.procs).insert(name, proc);
    }

    /// Define a polymorphic procedure with a given name under this environment.
    pub(super) fn define_poly_proc(&mut self, name: impl ToString, proc: PolyProcedure) {
        let name = name.to_string();
        trace!("Defining polymorphic procedure {name} as {proc}");
        Rc::make_mut(&mut self.consts).insert(name, ConstExpr::PolyProc(proc));
    }

    /// Define an FFI procedure with a given name under this environment.
    pub(super) fn define_ffi_proc(&mut self, name: impl ToString, proc: FFIProcedure) {
        let name = name.to_string();
        trace!("Defining FFI procedure {name} as {proc}");
        Rc::make_mut(&mut self.consts).insert(name, ConstExpr::FFIProcedure(proc));
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
    pub(super) fn push_proc(
        &mut self,
        name: &str,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
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
    pub(super) fn get_var(&self, var: &str) -> Option<&(Mutability, Type, isize)> {
        self.vars.get(var)
    }

    /// Is the variable defined in scope as mutable?
    pub(super) fn is_defined_as_mutable(&self, var: &str) -> bool {
        if let Some((mutability, _, _)) = self.vars.get(var) {
            mutability.is_mutable()
        } else if let Some((mutability, _, _)) = self.static_vars.get(var) {
            mutability.is_mutable()
        } else {
            false
        }
    }

    /// Define the arguments for the current scope (if this is a procedure).
    pub(super) fn define_args(
        &mut self,
        args: Vec<(String, Mutability, Type)>,
    ) -> Result<usize, Error> {
        debug!("Defining arguments {args:?} in\n{self}");
        self.fp_offset = 1;
        self.args_size = 0;

        // For each argument in reverse order (starting from the last argument)
        for (name, mutability, ty) in args.into_iter().rev() {
            // Get the size of the argument we're defining.
            let size = ty.get_size(self)?;
            // Add the size of the argument to the total number of cells taken up by the arguments.
            self.args_size += size;
            // Decrement the frame pointer offset by the size of the argument
            // so that the FP + the offset is the address of the argument.
            self.fp_offset -= size as isize;
            // Store the argument's type and offset in the environment.
            debug!(
                "Defined argument {name} of type {ty} at offset {} in\n{self}",
                self.fp_offset
            );
            Rc::make_mut(&mut self.vars).insert(name, (mutability, ty, self.fp_offset));
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
    pub fn define_var(
        &mut self,
        var: impl ToString,
        mutability: Mutability,
        ty: Type,
    ) -> Result<isize, Error> {
        let var = var.to_string();
        // Get the size of the variable we're defining.
        let size = ty.get_size(self)? as isize;
        // Remember the offset of the variable under the current scope.
        let offset = self.fp_offset;
        // Increment the frame pointer offset by the size of the variable
        // so that the next variable is allocated directly after this variable.
        debug!("Defining variable {var} of type {ty} at {offset} in\n{self}");
        self.fp_offset += size;
        // Store the variable's type and offset in the environment.
        Rc::make_mut(&mut self.vars).insert(var, (mutability, ty, offset));
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
        // Get the precalculated size of the given type.
        // let size = self.type_sizes.get(ty).copied()?;
        let size = self.type_sizes.read().unwrap().get(ty).copied()?;
        // Log the size of the type.
        debug!(target: "size", "Getting memoized type size for {ty} => {size}");
        // Return the size of the type.
        Some(size)
    }

    /// Set the precalculated size of the given type.
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn set_precalculated_size(&mut self, ty: Type, size: usize) {
        debug!(target: "size", "Memoizing type size {ty} with size {size}");
        if let Some(old_size) = self.get_precalculated_size(&ty) {
            if old_size == size {
                debug!(target: "size", "Type size {ty} was already memoized with size {size}");
                return;
            } else {
                warn!(target: "size", "Type size {ty} was already memoized with size {old_size}, but we memoized it with size {size}");
            }
        }
        self.type_sizes.write().unwrap().insert(ty, size);
    }
}

impl Display for Env {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        // writeln!(f, "Env")?;
        // writeln!(f, "   Types:")?;
        // for (name, ty) in self.types.iter() {
        //     writeln!(f, "      {}: {}", name, ty)?;
        //     let constants = self.get_all_associated_consts(ty);
        //     if constants.is_empty() {
        //         continue;
        //     }
        //     writeln!(f, "         Associated constants:")?;
        //     for (name, cexpr) in constants {
        //         writeln!(f, "            {}: {}", name, cexpr)?;
        //     }
        // }
        // writeln!(f, "   Constants:")?;
        // for (name, e) in self.consts.iter() {
        //     writeln!(f, "      {}: {}", name, e)?;
        // }
        // writeln!(f, "   Procedures:")?;
        // for (name, proc) in self.procs.iter() {
        //     writeln!(f, "      {}: {}", name, proc)?;
        // }
        // writeln!(f, "   Globals:")?;
        // for (name, (mutability, ty, location)) in self.static_vars.iter() {
        //     writeln!(f, "      {mutability} {name}: {ty} (location {location})")?;
        // }
        // writeln!(f, "   Variables:")?;
        // for (name, (mutability, ty, offset)) in self.vars.iter() {
        //     writeln!(f, "      {mutability} {name}: {ty} (frame-offset {offset})")?;
        // }
        Ok(())
    }
}
