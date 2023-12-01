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
use std::collections::BTreeMap;

/// A declaration of a variable, function, type, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// A static variable declaration.
    StaticVar(String, Mutability, Type, ConstExpr),
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
    Many(Vec<Declaration>),
}

impl Declaration {
    pub(crate) fn static_var(
        name: impl Into<String>,
        mutability: Mutability,
        ty: Type,
        expr: ConstExpr,
    ) -> Self {
        Self::StaticVar(name.into(), mutability, ty, expr)
    }

    /// Flatten a multi-declaration into a single-dimensional vector of declarations.
    pub(crate) fn flatten(self) -> Vec<Self> {
        match self {
            Self::Many(decls) => {
                let mut flattened = Vec::new();
                for decl in decls {
                    flattened.append(&mut decl.flatten());
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
        env.add_compile_time_declaration(self)?;
        // The size of the variables declared.
        // This is used to pop the stack when we're done.
        let mut var_size = 0;
        match self {
            Declaration::Var(name, _mutability, specifier, expr) => {
                // Get the current instruction (for logging)
                let current_instruction = output.current_instruction();
                // A log message
                let log_message = format!("Initializing '{name}' with expression '{expr}'");

                // Get the type of the variable using its specifier,
                // or by deducing the type ourselves.
                let var_ty = if let Some(specified_ty) = specifier {
                    specified_ty.clone()
                } else {
                    expr.get_type(env)?
                };
                // Get the size of the variables for the body of the declaration.
                var_size = var_ty.get_size(env)?;
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;

                // Add the variable to the environment, so that it can be used in the body.
                env.add_local_variable_declaration(self)?;
                // Log the instructions for the declaration.
                output.log_instructions_after(&name, &log_message, current_instruction);
            }
            Declaration::VarPat(pat, expr) => {
                // Get the type of the expression being assigned to the pattern.
                let expr_ty = expr.get_type(env)?;
                // The size of all the variables is the size of the expression.
                var_size = expr.get_size(env)?;
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;
                // Add the variable to the environment, so that it can be used in the body.
                pat.declare_let_bind(&expr, &expr_ty, env)?;
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

                // let name = name.clone().to_uppercase();
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
                for decl in decls {
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
                decls.append(&mut other_decls);
            }
            (Self::Many(decls), other) => {
                decls.append(&mut other.flatten());
            }
            (self_, Self::Many(mut other_decls)) => {
                let mut result = self_.clone().flatten();
                result.append(&mut other_decls);
                *self_ = Self::Many(result)
            }
            (self_, other) => *self_ = Self::Many(vec![self_.clone(), other]),
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
            Self::Type(_name, ty) => {
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
                for (_name, expr) in impls {
                    expr.substitute(substitution_name, substitution_ty);
                }
            }
            Self::Many(decls) => {
                for decl in decls {
                    decl.substitute(substitution_name, substitution_ty);
                }
            }
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
                pat.type_check(&expr, &Expr::NONE, env)?;
                // Typecheck the expression assigned to the pattern in the environment.
                expr.type_check(env)?;

                // Get the type of the expression.
                let ty = expr.get_type(env)?;
                // ty.add_monomorphized_associated_consts(env)?;
                // Get the size of the expression.
                let size = ty.get_size(env)?;
                if !pat.is_exhaustive(&expr, &ty, env)? {
                    // Make sure the pattern is exhaustive.
                    // If it is not, then we throw an error.
                    return Err(Error::NonExhaustivePatterns {
                        patterns: vec![pat.clone()],
                        expr: Expr::NONE.with(self.clone()),
                    });
                }
                // Get the bindings of the variables under the pattern
                let bindings = pat.get_bindings(&expr, &ty, env)?;
                // Get the size of all the bindings
                let size_of_bindings = bindings
                    .iter()
                    .map(|(_name, (_mut, ty))| ty.get_size(env).unwrap_or(0))
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
                new_env.add_compile_time_declaration(&self.clone())?;

                if let Type::Apply(template, supplied_params) = ty {
                    // If this is an implementation for a template type, we need to
                    // get the template parameters and add them to each associated constant.
                    let template_params = template.get_template_params(&new_env);

                    if template_params.len() != supplied_params.len() {
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
                            associated_const.template(supplied_param_symbols.clone());
                        new_env.add_associated_const(
                            *template.clone(),
                            name,
                            templated_const.clone(),
                        )?;

                        // new_env.add_associated_const(simplified_template.clone(), name, templated_const.clone())?;
                        templated_consts.push(templated_const);
                    }

                    for templated_const in templated_consts {
                        debug!(
                            "About to type check templated const: {templated_const}",
                            templated_const = templated_const
                        );
                        templated_const.type_check(&new_env)?;
                    }
                } else {
                    ty.add_monomorphized_associated_consts(env)?;
                    for (name, associated_const) in impls {
                        new_env.add_associated_const(ty.clone(), name, associated_const.clone())?;
                    }

                    for (_name, associated_const) in impls {
                        associated_const.type_check(&new_env)?;
                    }
                }
                // for (_name, expr) in impls {
                //     expr.type_check(&new_env)?;
                // }
            }
            // Typecheck a multi-declaration.
            Self::Many(decls) => {
                let mut new_env = env.clone();
                // Add all the compile-time declarations to the environment.
                new_env.add_compile_time_declaration(&self.clone())?;
                for decl in decls {
                    // Typecheck any variable declarations in the old scope
                    decl.type_check(&new_env)?;
                    // Add them to the new scope.
                    new_env.add_declaration(decl)?;
                }
            }
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
                for decl in decls {
                    writeln!(f, "{}", decl)?;
                }
            }
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
        Self::Many(bt.into_iter().map(|(k, v)| (k, v).into()).collect())
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
        Self::Many(decls.into_iter().map(|decl| decl.into()).collect())
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
