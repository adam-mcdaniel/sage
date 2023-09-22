use crate::{lir::{Expr, Env, Compile, Error, GetType, Type, TypeCheck, Mutability, ConstExpr, FFIProcedure, Pattern, GetSize}, asm::{AssemblyProgram, SP, CoreOp, Location}};
use super::{Procedure, PolyProcedure};

use std::collections::BTreeMap;
use core::ops::Add;
use core::fmt::{Display, Formatter, Result as FmtResult};
use log::debug;

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
    /// Many declarations.
    Many(Vec<Declaration>),
}

impl Declaration {
    /// Flatten a multi-declaration into a single-dimensional vector of declarations.
    pub(crate) fn flatten(self) -> Vec<Self> {
        match self {
            Self::Many(decls) => {
                let mut flattened = Vec::new();
                for decl in decls {
                    flattened.extend(decl.flatten());
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
            Self::Many(decls) => decls.iter().any(|decl| decl.has_local_variable_declaration()),
            _ => false,
        }
    }

    /// Compile a declaration with a body in a new scope. This will copy the old environment,
    /// and add the declaration to the new environment.
    pub(crate) fn compile(&self, body: Expr, env: &Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        self.compile_helper(Some(body), &mut env.clone(), output).map(|_| ())
    }

    /// Compile a declaration, and return how many cells were allocated for variables.
    /// This will modify the environment to add the declaration. If there is a body,
    /// it will be compiled under a new scope, and will be popped off the stack when
    /// the declaration is finished.
    fn compile_helper(&self, body: Option<Expr>, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<usize, Error> {
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
                let log_message = format!("Initializing static variable '{name}' with expression '{expr}'");

                // Get the type of the variable.
                let var_ty = ty.clone();
                // Get the size of the variable.
                var_size = var_ty.get_size(env)?;
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;
                // Allocate the global variable.
                output.op(CoreOp::Global {
                    name: name.clone(),
                    size: var_size,
                });
                // Write the value of the expression to the global variable.
                output.op(CoreOp::Pop(Some(Location::Global(name.clone())), var_size));
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

            // Copy the return value over where the arguments were stored,
            // so that when we pop the stack, it's as if we popped the variables
            // and arguments, and pushed our return value.
            debug!("Destroying {var_size} cells of stack, leaving {result_size} cells of stack");
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

        Ok(var_size)
    }

    /// Merge two declarations into one, while preserving the order of the declarations.
    pub(crate) fn join(mut self, other: impl Into<Self>) -> Self {
        match (&mut self, other.into()) {
            (Self::Many(decls), Self::Many(other_decls)) => {
                decls.extend(other_decls);
                self
            }
            (Self::Many(decls), other) => {
                decls.extend(other.flatten());
                self
            }
            (self_, Self::Many(mut other_decls)) => {
                other_decls.extend(self_.clone().flatten());
                Self::Many(other_decls)
            }
            (self_, other) => {
                Self::Many(vec![self_.clone(), other])
            }
        }
    }

    /// Substitute a type symbol for a type.
    pub(crate) fn substitute(&mut self, name: &str, ty: &Type) {
        match self {
            Self::StaticVar(name, _mutability, expected_ty, expr) => {
                expr.substitute(name, ty);
                expected_ty.substitute(name, ty);
            }
            Self::Var(name, _mutability, expected_ty, expr) => {
                expr.substitute(name, ty);
                if let Some(expected_ty) = expected_ty {
                    expected_ty.substitute(name, ty);
                }
            }
            Self::Proc(name, proc) => {
                proc.substitute(name, ty);
            }
            Self::PolyProc(name, proc) => {
                proc.substitute(name, ty);
            }
            Self::Type(name, ty) => {
                ty.substitute(name, ty);
            }
            Self::Const(name, expr) => {
                expr.substitute(name, ty);
            }
            Self::VarPat(_pat, expr) => {
                expr.substitute(name, ty);
            }
            Self::ExternProc(name, proc) => {
                proc.substitute(name, ty);
            }
            Self::Many(decls) => {
                for decl in decls {
                    decl.substitute(name, ty);
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
                        return Err(Error::MismatchedTypes {
                            expected: expected_ty.clone(),
                            found: found_ty.clone(),
                            expr: Expr::NONE.with(self.clone())
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
                        expr: Expr::NONE.with(self.clone())
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
                let size_of_bindings = bindings.iter().map(|(_name, (_mut, ty))| ty.get_size(env).unwrap_or(0)).sum::<usize>();
                // Make sure the size of the bindings is the same as the size of the expression.
                if size_of_bindings != size {
                    return Err(Error::InvalidPatternForExpr(Expr::NONE.with(self.clone()), pat.clone()));
                }
            }
            // Typecheck a foreign function declaration.
            Self::ExternProc(_name, proc) => {
                proc.type_check(env)?;
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
            },
            Self::Var(name, mutability, ty, expr) => {
                write!(f, "{} {} = {}", mutability, name, expr)?;
                if let Some(ty) = ty {
                    write!(f, ": {}", ty)?;
                }
            },
            Self::Proc(name, proc) => {
                write!(f, "{}", proc)?;
                write!(f, " {}", name)?;
            },
            Self::PolyProc(name, proc) => {
                write!(f, "{}", proc)?;
                write!(f, " {}", name)?;
            },
            Self::Type(name, ty) => {
                write!(f, "type {}", name)?;
                write!(f, " = {}", ty)?;
            },
            Self::Const(name, expr) => {
                write!(f, "const {}: {}", name, expr)?;
            },
            Self::VarPat(pat, expr) => {
                write!(f, "{} = {}", pat, expr)?;
            },
            Self::ExternProc(name, proc) => {
                write!(f, "extern {}", proc)?;
                write!(f, " {}", name)?;
            },
            Self::Many(decls) => {
                for decl in decls {
                    writeln!(f, "{}", decl)?;
                }
            },
        }
        Ok(())
    }
}

impl From<(String, Type)> for Declaration {
    fn from((name, ty): (String, Type)) -> Self {
        Self::Type(name, ty)
    }
}

impl From<(String, Expr)> for Declaration {
    fn from((name, expr): (String, Expr)) -> Self {
        Self::Var(name, Mutability::Immutable, None, expr)
    }
}

impl From<(String, Mutability, Expr)> for Declaration {
    fn from((name, mutability, expr): (String, Mutability, Expr)) -> Self {
        Self::Var(name, mutability, None, expr)
    }
}

impl From<(String, Mutability, Type, Expr)> for Declaration {
    fn from((name, mutability, ty, expr): (String, Mutability, Type, Expr)) -> Self {
        Self::Var(name, mutability, Some(ty), expr)
    }
}

impl From<(String, Mutability, Option<Type>, Expr)> for Declaration {
    fn from((name, mutability, ty, expr): (String, Mutability, Option<Type>, Expr)) -> Self {
        Self::Var(name, mutability, ty, expr)
    }
}

impl From<(String, Procedure)> for Declaration {
    fn from((name, proc): (String, Procedure)) -> Self {
        Self::Proc(name, proc)
    }
}

impl From<(String, PolyProcedure)> for Declaration {
    fn from((name, proc): (String, PolyProcedure)) -> Self {
        Self::PolyProc(name, proc)
    }
}

impl From<(String, ConstExpr)> for Declaration {
    fn from((name, expr): (String, ConstExpr)) -> Self {
        Self::Const(name, expr)
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

impl<K, V> From<BTreeMap<K, V>> for Declaration where (K, V): Into<Declaration> {
    fn from(bt: BTreeMap<K, V>) -> Self {
        Self::Many(bt.into_iter().map(|(k, v)| (k, v).into()).collect())
    }
}

impl<T> From<Vec<T>> for Declaration where T: Into<Declaration> {
    fn from(decls: Vec<T>) -> Self {
        Self::Many(decls.into_iter().map(|decl| decl.into()).collect())
    }
}

impl<T> Add<T> for Declaration where T: Into<Declaration> {
    type Output = Self;

    fn add(self, other: T) -> Self::Output {
        self.join(other.into())
    }
}