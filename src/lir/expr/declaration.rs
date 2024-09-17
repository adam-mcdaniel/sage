use super::{PolyProcedure, Procedure};
use crate::{
    asm::{AssemblyProgram, CoreOp, Location, SP},
    lir::{
        Compile, ConstExpr, Env, Error, Expr, FFIProcedure, GetSize, GetType, Mutability, Pattern,
        Type, TypeCheck,
    },
};
use core::{
    fmt::{Display, Formatter, Result as FmtResult},
    ops::{Add, AddAssign},
};
use log::*;
use rayon::prelude::*;
use serde_derive::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// A declaration of a variable, function, type, etc.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Declaration {
    /// A static variable declaration.
    StaticVar(String, Mutability, Type, Expr),
    /// A variable declaration.
    Var(String, Mutability, Option<Type>, Expr),
    /// A procedure declaration.
    Proc(String, Procedure),
    /// A polymorphic procedure declaration.
    PolyProc(String, PolyProcedure),
    /// A type declaration.
    Type(String, Type),
    /// A constant expression.
    Const(String, ConstExpr),
    /// A variable declaration with a pattern.
    VarPat(Pattern, Expr),
    /// A foreign function declaration.
    ExternProc(String, FFIProcedure),
    /// Declare associated constants and procedures for a type.
    Impl(Type, Vec<(String, ConstExpr)>),
    /// Many declarations.
    Many(Arc<Vec<Declaration>>),
    /// Declare a module
    ///
    /// Do NOT instantiate this directly:
    /// use the `Declaration::module` method.
    /// This will redistribute the declarations to make sure
    /// everything is in-order internally to be imported/exported.
    Module(String, Arc<Vec<Declaration>>, bool, usize),
    /// Import an element from a module.
    FromImport {
        module: ConstExpr,
        names: Vec<(String, Option<String>)>,
    },
    FromImportAll(ConstExpr),
}

impl Declaration {
    pub(crate) fn static_var(
        name: impl Into<String>,
        mutability: Mutability,
        ty: Type,
        expr: impl Into<Expr>,
    ) -> Self {
        Self::StaticVar(name.into(), mutability, ty, expr.into())
    }

    /// Create a collection of declarations
    pub fn many(decls: impl Into<Vec<Self>>) -> Self {
        Self::Many(Arc::new(decls.into()))
    }

    /// Create a module with a given name and a list of declarations, and whether or not it is checked.
    pub fn module(name: impl ToString, decls: impl Into<Vec<Self>>, checked: bool) -> Self {
        lazy_static::lazy_static! {
            static ref MODULE_COUNT: std::sync::Mutex<usize> = std::sync::Mutex::new(0);
        }
        let module_count = {
            let mut count = MODULE_COUNT.lock().unwrap();
            *count += 1;
            *count
        };

        let mut decls = decls.into();
        let mut import = Self::many(decls.clone());
        if !checked {
            import.mark_no_checking();
        }
        import.filter(&|decl| {
            decl.is_compile_time_declaration() && !matches!(decl, Declaration::Impl(..))
        });

        for decl in &mut decls {
            decl.distribute_decls(&import.clone());
        }

        let mut result = Self::Module(name.to_string(), Arc::new(decls), checked, module_count);
        if !checked {
            result.mark_no_checking();
        }
        result
    }

    fn mark_no_checking(&mut self) {
        match self {
            Self::Module(_, decls, checked, ..) => {
                *checked = false;
                for decl in Arc::make_mut(decls).iter_mut() {
                    decl.mark_no_checking();
                }
            }
            Self::Many(decls) => {
                for decl in Arc::make_mut(decls).iter_mut() {
                    decl.mark_no_checking();
                }
            }
            _ => {}
        }
    }

    /// Filter all the subdeclarations to satisfy some condition.
    fn filter(&mut self, f: &impl Fn(&Declaration) -> bool) {
        match self {
            Self::Many(decls) => {
                let decls = Arc::make_mut(decls);
                decls.retain(|decl| f(decl));
                decls.iter_mut().for_each(|decl| decl.filter(f));
            }
            Self::Module(_name, decls, ..) => {
                let decls = Arc::make_mut(decls);
                decls.retain(|decl| f(decl));
                decls.iter_mut().for_each(|decl| decl.filter(f));
            }
            _ => {}
        }
    }

    /// Make this declaration only have compile time subdeclarations
    fn filter_for_compile_time_only(&mut self) {
        self.filter(&|decl| decl.is_compile_time_declaration());
    }

    /// Distribute a declaration amongst several other declarations.
    /// This makes sure the declaration is in scope for all the subexpressions
    /// in the other declarations.
    fn distribute_decls(&mut self, distributed: &Self) {
        match self {
            Self::Many(decls) => {
                let decls = Arc::make_mut(decls);
                for decl in decls.iter_mut() {
                    decl.distribute_decls(distributed);
                }
            }
            Self::Module(_name, decls, ..) => {
                let decls = Arc::make_mut(decls);
                for decl in decls.iter_mut() {
                    decl.distribute_decls(distributed);
                }
            }
            Self::Impl(_ty, impls) => {
                let mut distributed = distributed.clone();
                distributed.filter_for_compile_time_only();
                for (_name, expr) in impls {
                    *expr = expr.with(distributed.clone());
                }
            }
            Self::Proc(_name, proc) => {
                *proc = proc.with(distributed.clone());
            }
            Self::PolyProc(_name, proc) => {
                *proc = proc.with(distributed.clone());
            }
            _ => {}
        }
    }

    /// Flatten a multi-declaration into a single-dimensional vector of declarations.
    pub(crate) fn flatten(self) -> Vec<Self> {
        match self {
            Self::Many(decls) => {
                let mut flattened = Vec::new();
                for decl in decls.iter() {
                    flattened.append(&mut decl.clone().flatten());
                }
                flattened
            }
            decl => vec![decl],
        }
    }

    /// Does this declaration include a local variable declaration?
    pub(crate) fn has_local_variable_declaration(&self) -> bool {
        match self {
            Self::Var(..) => true,
            Self::VarPat(..) => true,
            Self::Many(decls) => decls
                .iter()
                .any(|decl| decl.has_local_variable_declaration()),
            _ => false,
        }
    }

    /// Is this a compile time declaration?
    pub(crate) fn is_compile_time_declaration(&self) -> bool {
        match self {
            Self::Type(..) => true,
            Self::Const(..) => true,
            Self::Proc(..) => true,
            Self::PolyProc(..) => true,
            Self::ExternProc(..) => true,
            Self::Module(..) => true,
            Self::Impl(..) => true,
            Self::FromImport { .. } => true,
            Self::FromImportAll(..) => true,
            Self::StaticVar(..) => true,
            Self::Many(decls) => decls
                .par_iter()
                .all(|decl| decl.is_compile_time_declaration()),
            _ => false,
        }
    }

    /// Compile a declaration with a body in a new scope. This will copy the old environment,
    /// and add the declaration to the new environment.
    pub(crate) fn compile(
        &self,
        body: Expr,
        env: &Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        self.compile_helper(Some(body), &mut env.clone(), output)
            .map(|_| ())
    }

    /// Compile a declaration, and return how many cells were allocated for variables.
    /// This will modify the environment to add the declaration. If there is a body,
    /// it will be compiled under a new scope, and will be popped off the stack when
    /// the declaration is finished.
    fn compile_helper(
        &self,
        body: Option<Expr>,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<usize, Error> {
        // Add all the compile time declarations to the environment so that they can be used
        // in these declarations.
        env.add_compile_time_declaration(self, true)?;
        // The size of the variables declared.
        // This is used to pop the stack when we're done.
        let mut var_size = 0;
        match self {
            Declaration::Var(name, _mutability, specifier, expr) => {
                // Get the current instruction (for logging)
                let current_instruction = output.current_instruction();
                // A log message
                let log_message = format!("Initializing '{name}' with expression '{expr}' in {self}");

                // Get the type of the variable using its specifier,
                // or by deducing the type ourselves.
                let var_ty = if let Some(specified_ty) = specifier {
                    specified_ty.clone()
                } else {
                    expr.get_type(env)?
                };
                // Get the size of the variables for the body of the declaration.
                var_size = var_ty.get_size(env)?;
                println!("Var {name} has type {var_ty:?} is {var_size}");
                let cur = output.current_instruction();
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;
                output.log_instructions_after("var", &log_message, cur);

                // Add the variable to the environment, so that it can be used in the body.
                env.add_local_variable_declaration(self, true)?;
                // Log the instructions for the declaration.
                output.log_instructions_after(name, &log_message, current_instruction);
            }
            Declaration::VarPat(pat, expr) => {
                // Get the type of the expression being assigned to the pattern.
                let expr_ty = expr.get_type(env)?;
                // The size of all the variables is the size of the expression.
                var_size = expr.get_size(env)?;
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;
                // Add the variable to the environment, so that it can be used in the body.
                pat.declare_let_bind(expr, &expr_ty, env)?;
            }
            Declaration::StaticVar(name, _mutability, ty, expr) => {
                // Get the current instruction (for logging)
                let current_instruction = output.current_instruction();
                // A log message
                let log_message =
                    format!("Initializing static variable '{name}' with expression '{expr}'");

                // Get the type of the variable.
                let var_ty = ty.clone();
                // Get the size of the variable.
                let static_var_size = var_ty.get_size(env)?;

                let name = name.clone();
                // Allocate the global variable.
                output.op(CoreOp::Global {
                    name: name.clone(),
                    size: static_var_size,
                });
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;
                // Write the value of the expression to the global variable.
                output.op(CoreOp::Pop(
                    Some(Location::Global(name.clone())),
                    static_var_size,
                ));
                // Log the instructions for the declaration.
                output.log_instructions_after(&name, &log_message, current_instruction);
            }
            Declaration::Many(decls) => {
                for decl in decls.iter() {
                    // Compile all the sub-declarations,
                    // and leave their variables on the stack.
                    // Add their variable sizes to the total variable size.
                    // This will be used to pop the stack when we're done.
                    var_size += decl.compile_helper(None, env, output)?;
                }
            }
            _ => {}
        }

        // If there is some body to execute under the new scope,
        // then compile it and pop the stack when we're done.
        // Otherwise, leave the stack unpopped with all the variables
        // still on the stack.
        if let Some(body) = body {
            // The type and size of the result expression of the declaration block.
            let result_type = body.get_type(env)?;
            let result_size = result_type.get_size(env)?;

            // Compile the body under the new scope
            body.compile_expr(env, output)?;
            if var_size != 0 {
                // Copy the return value over where the arguments were stored,
                // so that when we pop the stack, it's as if we popped the variables
                // and arguments, and pushed our return value.
                debug!(
                    "Destroying {var_size} cells of stack, leaving {result_size} cells of stack"
                );
                output.op(CoreOp::Copy {
                    // The address of the return value (the values we pushed)
                    src: SP.deref().offset(1 - result_size as isize),
                    // The address of the arguments (the values we want to write over)
                    dst: SP
                        .deref()
                        .offset(1 - var_size as isize - result_size as isize),
                    // The size of the return value
                    size: result_size,
                });
                output.op(CoreOp::Pop(None, var_size));
            }
        }

        Ok(var_size)
    }

    /// Detect duplicate modules
    fn detect_duplicate_modules(&self, modules: &mut HashSet<String>) -> Result<(), Error> {
        match self {
            Self::Module(name, _decls, ..) => {
                if modules.contains(name) {
                    return Err(Error::ModuleRedefined(name.clone()));
                }
                modules.insert(name.clone());
            }
            Self::Many(decls) => {
                for decl in decls.iter() {
                    decl.detect_duplicate_modules(modules)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Merge two declarations into one, while preserving the order of the declarations.
    /// This will not mutate the declaration, but will return a new declaration.
    pub(crate) fn join(&self, other: impl Into<Self>) -> Self {
        let mut result = self.clone();
        result.append(other.into());
        result
    }

    /// Merge two declarations into one, while preserving the order of the declarations.
    /// This will merge the declarations into a multi-declaration.
    pub(crate) fn append(&mut self, other: impl Into<Self>) {
        match (self, other.into()) {
            (Self::Many(decls), Self::Many(mut other_decls)) => {
                Arc::make_mut(decls).append(Arc::make_mut(&mut other_decls));
            }
            (Self::Many(decls), other) => {
                Arc::make_mut(decls).append(&mut other.flatten());
            }
            (self_, Self::Many(mut other_decls)) => {
                let mut result = self_.clone().flatten();
                result.append(Arc::make_mut(&mut other_decls));
                *self_ = Self::many(result)
            }
            (self_, other) => *self_ = Self::many(vec![self_.clone(), other]),
        }
    }

    /// Substitute a type symbol for a type.
    pub(crate) fn substitute(&mut self, substitution_name: &str, substitution_ty: &Type) {
        match self {
            Self::StaticVar(_name, _mutability, expected_ty, expr) => {
                expr.substitute(substitution_name, substitution_ty);
                expected_ty.substitute(substitution_name, substitution_ty);
            }
            Self::Var(_name, _mutability, expected_ty, expr) => {
                expr.substitute(substitution_name, substitution_ty);
                if let Some(expected_ty) = expected_ty {
                    *expected_ty = expected_ty.substitute(substitution_name, substitution_ty);
                }
            }
            Self::Proc(_name, proc) => {
                proc.substitute(substitution_name, substitution_ty);
            }
            Self::PolyProc(_name, proc) => {
                proc.substitute(substitution_name, substitution_ty);
            }
            Self::Type(_name , ty) => {
                *ty = ty.substitute(substitution_name, substitution_ty);
            }
            Self::Const(_name, expr) => {
                expr.substitute(substitution_name, substitution_ty);
            }
            Self::VarPat(_pat, expr) => {
                expr.substitute(substitution_name, substitution_ty);
            }
            Self::ExternProc(_name, proc) => {
                proc.substitute(substitution_name, substitution_ty);
            }
            Self::Impl(_name, impls) => {
                // for (_name, expr) in impls {
                //     expr.substitute(substitution_name, substitution_ty);
                // }
                impls.par_iter_mut().for_each(|(_name, expr)| {
                    expr.substitute(substitution_name, substitution_ty);
                });
            }
            Self::Many(decls) => {
                // for decl in decls {
                //     decl.substitute(substitution_name, substitution_ty);
                // }
                Arc::make_mut(decls).par_iter_mut().for_each(|decl| {
                    decl.substitute(substitution_name, substitution_ty);
                });
            }
            Self::Module(_name, decls, ..) => {
                Arc::make_mut(decls).par_iter_mut().for_each(|decl| {
                    decl.substitute(substitution_name, substitution_ty);
                });
            }
            Self::FromImport { module, .. } => {
                module.substitute(substitution_name, substitution_ty);
            }
            Self::FromImportAll(module) => module.substitute(substitution_name, substitution_ty),
        }
    }
}

impl TypeCheck for Declaration {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        match self {
            // Typecheck a variable declaration.
            Self::Var(_name, _mutability, expected_ty, expr) => {
                // Get the type of the expression.
                let found_ty = expr.get_type(env)?;
                // If there is a type specified, then make sure the type of the expression
                // can decay to the specified type.
                if let Some(expected_ty) = expected_ty {
                    // Typecheck the specified type.
                    expected_ty.type_check(env)?;
                    // Make sure the type of the expression can decay to the specified type.
                    if !found_ty.can_decay_to(expected_ty, env)? {
                        error!(
                            "Invalid declaration {}: {found_ty:?} != {expected_ty:?}",
                            self.clone()
                        );
                        return Err(Error::MismatchedTypes {
                            expected: expected_ty.clone(),
                            found: found_ty.clone(),
                            expr: Expr::NONE.with(self.clone()),
                        });
                    }
                }

                expr.type_check(env)?;
            }
            // Typecheck a procedure declaration.
            Self::Proc(name, proc) => {
                let mut new_env = env.clone();
                new_env.define_proc(name, proc.clone());
                proc.type_check(&new_env)?;
            }
            // Typecheck a polymorphic procedure declaration.
            Self::PolyProc(name, proc) => {
                let mut new_env = env.clone();
                new_env.define_poly_proc(name, proc.clone());
                proc.type_check(&new_env)?;
            }
            // Typecheck a type declaration.
            Self::Type(name, ty) => {
                let mut new_env = env.clone();
                new_env.define_type(name, ty.clone());
                // ty.add_monomorphized_associated_consts(env)?;
                ty.type_check(&new_env)?;
            }
            // Typecheck a constant expression.
            Self::Const(name, expr) => {
                let mut new_env = env.clone();
                new_env.define_const(name, expr.clone());
                expr.type_check(&new_env)?;
            }
            // Typecheck a static variable declaration.
            Self::StaticVar(name, mutability, expected_ty, expr) => {
                // Create a new environment with the static variable declared.
                let mut new_env = env.clone();
                new_env.define_static_var(name, *mutability, expected_ty.clone())?;

                // Typecheck the specified type of the variable.
                expected_ty.type_check(&new_env)?;
                // Typecheck the expression assigned to the variable.
                expr.type_check(&new_env)?;
                let found_ty = expr.get_type(&new_env)?;

                // Make sure the type of the expression matches the type of the variable.
                if !found_ty.can_decay_to(expected_ty, &new_env)? {
                    // If it does not, then we throw an error.
                    error!("Static variable {name} has type {found_ty} and cannot coerce to {expected_ty}");
                    return Err(Error::MismatchedTypes {
                        expected: expected_ty.clone(),
                        found: found_ty.clone(),
                        expr: Expr::NONE.with(self.clone()),
                    });
                }
            }
            // Typecheck a variable declaration with a pattern.
            Self::VarPat(pat, expr) => {
                // Typecheck the pattern in the environment.
                pat.type_check(expr, &Expr::NONE, env)?;
                // Typecheck the expression assigned to the pattern in the environment.
                expr.type_check(env)?;

                // Get the type of the expression.
                let ty = expr.get_type(env)?;
                // ty.add_monomorphized_associated_consts(env)?;
                // Get the size of the expression.
                let size = ty.get_size(env)?;
                if !pat.is_exhaustive(expr, &ty, env)? {
                    // Make sure the pattern is exhaustive.
                    // If it is not, then we throw an error.
                    return Err(Error::NonExhaustivePatterns {
                        patterns: vec![pat.clone()],
                        expr: Expr::NONE.with(self.clone()),
                    });
                }
                // Get the bindings of the variables under the pattern
                let bindings = pat.get_bindings(expr, &ty, env)?;
                // Get the size of all the bindings
                let size_of_bindings = bindings
                    .values()
                    .collect::<Vec<_>>()
                    .par_iter()
                    .map(|(_mut, ty)| ty.get_size(env).unwrap_or(0))
                    .sum::<usize>();
                // Make sure the size of the bindings is the same as the size of the expression.
                if size_of_bindings != size {
                    return Err(Error::InvalidPatternForExpr(
                        Expr::NONE.with(self.clone()),
                        pat.clone(),
                    ));
                }
            }
            // Typecheck a foreign function declaration.
            Self::ExternProc(_name, proc) => {
                proc.type_check(env)?;
            }
            Self::Impl(ty, impls) => {
                let mut new_env = env.clone();
                // Add all the compile-time declarations to the environment.
                new_env.add_compile_time_declaration(&self.clone(), false)?;

                if let Type::Apply(template, supplied_params) = ty {
                    // If this is an implementation for a template type, we need to
                    // get the template parameters and add them to each associated constant.
                    let template_params = template.get_template_params(&new_env);

                    if template_params.len() != supplied_params.len() {
                        error!("Invalid impl for {template}");
                        return Err(Error::MismatchedTypes {
                            expected: *template.clone(),
                            found: Type::Apply(template.clone(), supplied_params.clone()),
                            expr: Expr::NONE.with(self.clone()),
                        });
                    }

                    let supplied_param_symbols = supplied_params
                        .iter()
                        .map(|ty| {
                            if let Type::Symbol(sym) = ty {
                                Ok(sym.clone())
                            } else {
                                Err(Error::MismatchedTypes {
                                    expected: Type::Symbol(
                                        "A symbol, not a concrete type".to_owned(),
                                    ),
                                    found: ty.clone(),
                                    expr: Expr::NONE.with(self.clone()),
                                })
                            }
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut templated_consts = vec![];

                    for (name, associated_const) in impls {
                        let templated_const =
                            associated_const.template(template_params.clone().into_iter().zip(supplied_param_symbols.clone().into_iter()).map(|((_, ty), param)| (param, ty)).collect());
                        new_env.add_associated_const(
                            *template.clone(),
                            name,
                            templated_const.clone(),
                        )?;

                        // new_env.add_associated_const(simplified_template.clone(), name, templated_const.clone())?;
                        templated_consts.push(templated_const);
                    }

                    // If we are at the limit of parallel recursion, then we should
                    // type check the associated constants sequentially.
                    templated_consts
                        .par_iter()
                        .try_for_each(|templated_const| templated_const.type_check(&new_env))?;
                } else {
                    // ty.add_monomorphized_associated_consts(env)?;
                    for (name, associated_const) in impls {
                        new_env.add_associated_const(ty.clone(), name, associated_const.clone())?;
                    }

                    impls.par_iter().try_for_each(|(_name, associated_const)| {
                        associated_const.type_check(&new_env)
                    })?;
                }
            }
            // Typecheck a multi-declaration.
            Self::Many(decls) => {
                let mut new_env = env.clone();

                // Add all the compile-time declarations to the environment.
                self.detect_duplicate_modules(&mut HashSet::new())?;
                new_env.add_declaration(&self.clone(), false)?;

                // Get all the compile time declarations so we can type check them in parallel.
                // let (comp_time_decls, run_time_decls): (Vec<_>, Vec<_>) = self.clone()
                //     .flatten()
                //     .into_iter()
                //     .partition(|decl| decl.is_compile_time_declaration());
                let (comp_time_decls, run_time_decls): (Vec<_>, Vec<_>) = decls
                    .iter()
                    .partition(|decl| decl.is_compile_time_declaration());

                if !comp_time_decls.is_empty() {
                    // Type check all the compile time declarations in parallel.
                    comp_time_decls.par_iter().try_for_each(|decl| {
                        debug!("Typechecking decl: {decl}");
                        decl.type_check(&new_env)
                        // Ok::<(), Error>(())
                    })?;
                }

                run_time_decls
                    .iter()
                    // .map(|decl| decl.type_check(&new_env))
                    .try_for_each(|decl| {
                        decl.type_check(&new_env)?;
                        new_env.add_declaration(decl, false)
                    })?;
            }

            Self::Module(name, decls, checked, ..) => {
                Self::Many(decls.clone()).detect_duplicate_modules(&mut HashSet::new())?;
                if *checked {
                    let mut new_env = env.clone();
                    // Add all the compile-time declarations to the environment.
                    new_env.add_compile_time_declaration(&Self::Many(decls.clone()), false)?;
                    trace!("Typechecking module {}", name);
                    // Get all the compile time declarations so we can type check them in parallel.
                    let (comp_time_decls, _run_time_decls): (Vec<_>, Vec<_>) = decls
                        .iter()
                        .partition(|decl| decl.is_compile_time_declaration());
    
                    if !comp_time_decls.is_empty() {
                        // Type check all the compile time declarations in parallel.
                        comp_time_decls
                            .par_iter()
                            .try_for_each(|decl| decl.type_check(&new_env))?;
                    }
                } else {
                    env.save_type_checked_const(ConstExpr::Symbol(name.clone()))
                }
            }

            Self::FromImport { module, names } => {
                module.type_check(env)?;
                for (name, _) in names {
                    let access = module.clone().field(ConstExpr::var(name));
                    access.type_check(env)?;
                }
            }
            Self::FromImportAll(module) => {
                module.type_check(env)?;
            },
        }
        Ok(())
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::StaticVar(name, mutability, ty, expr) => {
                write!(f, "static {mutability} {name}: {ty} = {expr}")?;
            }
            Self::Var(name, _mutability, ty, expr) => {
                write!(f, "{} = {}", name, expr)?;
                if let Some(ty) = ty {
                    write!(f, ": {}", ty)?;
                }
            }
            Self::Proc(name, proc) => {
                write!(f, "{}", proc)?;
                write!(f, " {}", name)?;
            }
            Self::PolyProc(name, proc) => {
                write!(f, "{}", proc)?;
                write!(f, " {}", name)?;
            }
            Self::Type(name, ty) => {
                write!(f, "type {}", name)?;
                write!(f, " = {}", ty)?;
            }
            Self::Const(name, expr) => {
                write!(f, "const {}: {}", name, expr)?;
            }
            Self::VarPat(pat, expr) => {
                write!(f, "{} = {}", pat, expr)?;
            }
            Self::ExternProc(name, proc) => {
                write!(f, "extern {}", proc)?;
                write!(f, " {}", name)?;
            }
            Self::Impl(name, impls) => {
                write!(f, "impl {}", name)?;
                write!(f, " {{")?;
                for (name, expr) in impls {
                    write!(f, "{} = {}", name, expr)?;
                }
                write!(f, "}}")?;
            }
            Self::Many(decls) => {
                for decl in decls.iter() {
                    writeln!(f, "{}", decl)?;
                }
            }
            Self::Module(name, _decls, ..) => {
                write!(f, "module {} {{..}}", name)?;
            }
            Self::FromImport { module, names } => {
                write!(f, "from {} import", module)?;
                for (name, alias) in names {
                    write!(f, " {}", name)?;
                    if let Some(alias) = alias {
                        write!(f, " as {}", alias)?;
                    }
                }
            }
            Self::FromImportAll(module) => write!(f, "from {module} import *")?,
        }
        Ok(())
    }
}

impl From<(String, Type)> for Declaration {
    fn from((name, ty): (String, Type)) -> Self {
        Self::Type(name, ty)
    }
}

impl From<(&str, Type)> for Declaration {
    fn from((name, expr): (&str, Type)) -> Self {
        Self::Type(name.to_string(), expr)
    }
}

impl From<(String, Expr)> for Declaration {
    fn from((name, expr): (String, Expr)) -> Self {
        Self::Var(name, Mutability::Immutable, None, expr)
    }
}

impl From<(&str, Expr)> for Declaration {
    fn from((name, expr): (&str, Expr)) -> Self {
        Self::Var(name.to_string(), Mutability::Immutable, None, expr)
    }
}

impl From<(String, Mutability, Expr)> for Declaration {
    fn from((name, mutability, expr): (String, Mutability, Expr)) -> Self {
        Self::Var(name, mutability, None, expr)
    }
}

impl From<(&str, Mutability, Expr)> for Declaration {
    fn from((name, mutability, expr): (&str, Mutability, Expr)) -> Self {
        Self::Var(name.to_string(), mutability, None, expr)
    }
}

impl From<(String, Mutability, Type, Expr)> for Declaration {
    fn from((name, mutability, ty, expr): (String, Mutability, Type, Expr)) -> Self {
        Self::Var(name, mutability, Some(ty), expr)
    }
}

impl From<(&str, Mutability, Type, Expr)> for Declaration {
    fn from((name, mutability, ty, expr): (&str, Mutability, Type, Expr)) -> Self {
        Self::Var(name.to_string(), mutability, Some(ty), expr)
    }
}

impl From<(String, Mutability, Option<Type>, Expr)> for Declaration {
    fn from((name, mutability, ty, expr): (String, Mutability, Option<Type>, Expr)) -> Self {
        Self::Var(name, mutability, ty, expr)
    }
}

impl From<(&str, Mutability, Option<Type>, Expr)> for Declaration {
    fn from((name, mutability, ty, expr): (&str, Mutability, Option<Type>, Expr)) -> Self {
        Self::Var(name.to_string(), mutability, ty, expr)
    }
}

impl From<(String, Procedure)> for Declaration {
    fn from((name, proc): (String, Procedure)) -> Self {
        Self::Proc(name, proc)
    }
}

impl From<(&str, Procedure)> for Declaration {
    fn from((name, proc): (&str, Procedure)) -> Self {
        Self::Proc(name.to_string(), proc)
    }
}

impl From<(String, PolyProcedure)> for Declaration {
    fn from((name, proc): (String, PolyProcedure)) -> Self {
        Self::PolyProc(name, proc)
    }
}

impl From<(&str, PolyProcedure)> for Declaration {
    fn from((name, proc): (&str, PolyProcedure)) -> Self {
        Self::PolyProc(name.to_string(), proc)
    }
}

impl From<(String, ConstExpr)> for Declaration {
    fn from((name, expr): (String, ConstExpr)) -> Self {
        Self::Const(name, expr)
    }
}

impl From<(&str, ConstExpr)> for Declaration {
    fn from((name, proc): (&str, ConstExpr)) -> Self {
        Self::Const(name.to_string(), proc)
    }
}

impl From<(Pattern, Expr)> for Declaration {
    fn from((pat, expr): (Pattern, Expr)) -> Self {
        Self::VarPat(pat, expr)
    }
}

impl From<(String, FFIProcedure)> for Declaration {
    fn from((name, proc): (String, FFIProcedure)) -> Self {
        Self::ExternProc(name, proc)
    }
}

impl From<Box<Declaration>> for Declaration {
    fn from(x: Box<Self>) -> Self {
        *x
    }
}

impl<K, V> From<BTreeMap<K, V>> for Declaration
where
    (K, V): Into<Declaration>,
{
    fn from(bt: BTreeMap<K, V>) -> Self {
        Self::Many(
            bt.into_iter()
                .map(|(k, v)| (k, v).into())
                .collect::<Vec<_>>()
                .into(),
        )
    }
}

impl From<(&str, FFIProcedure)> for Declaration {
    fn from((name, proc): (&str, FFIProcedure)) -> Self {
        Self::ExternProc(name.to_string(), proc)
    }
}

impl<T> From<Vec<T>> for Declaration
where
    T: Into<Declaration>,
{
    fn from(decls: Vec<T>) -> Self {
        Self::Many(
            decls
                .into_iter()
                .map(|decl| decl.into())
                .collect::<Vec<_>>()
                .into(),
        )
    }
}

impl<T> Add<T> for Declaration
where
    T: Into<Declaration>,
{
    type Output = Self;

    fn add(self, other: T) -> Self::Output {
        self.join(other)
    }
}

impl<T> AddAssign<T> for Declaration
where
    T: Into<Declaration>,
{
    fn add_assign(&mut self, other: T) {
        self.append(other.into());
    }
}

impl Hash for Declaration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::StaticVar(name, mutability, ty, expr) => {
                state.write_u8(0);
                name.hash(state);
                mutability.hash(state);
                ty.hash(state);
                expr.hash(state);
            }
            Self::Var(name, mutability, ty, expr) => {
                state.write_u8(1);
                name.hash(state);
                mutability.hash(state);
                ty.hash(state);
                expr.hash(state);
            }
            Self::Proc(name, proc) => {
                state.write_u8(2);
                name.hash(state);
                proc.hash(state);
            }
            Self::PolyProc(name, proc) => {
                state.write_u8(3);
                name.hash(state);
                proc.hash(state);
            }
            Self::Type(name, ty) => {
                state.write_u8(4);
                name.hash(state);
                ty.hash(state);
            }
            Self::Const(name, expr) => {
                state.write_u8(5);
                name.hash(state);
                expr.hash(state);
            }
            Self::VarPat(pat, expr) => {
                state.write_u8(6);
                pat.hash(state);
                expr.hash(state);
            }
            Self::ExternProc(name, proc) => {
                state.write_u8(7);
                name.hash(state);
                proc.hash(state);
            }
            Self::Impl(name, impls) => {
                state.write_u8(8);
                name.hash(state);
                impls.hash(state);
            }
            Self::Many(decls) => {
                state.write_u8(9);
                decls.hash(state);
            }
            Self::Module(name, decls, _checked, id) => {
                state.write_u8(10);
                name.hash(state);
                decls.hash(state);
                id.hash(state);
            }
            Self::FromImport { module, names } => {
                state.write_u8(11);
                module.hash(state);
                names.hash(state);
            }
            Self::FromImportAll(module) => {
                state.write_u8(12);
                module.hash(state);
            }
        }
    }
}
