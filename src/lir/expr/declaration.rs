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

    /// Compile a declaration, and return how many cells were allocated for variables.
    pub(crate) fn compile(&self, body: &Expr, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<usize, Error> {
        env.add_compile_time_declaration(self)?;
        let mut var_size = 0;
        match self {
            Declaration::Var(name, _mutability, specifier, expr) => {
                let current_instruction = output.current_instruction();
                let message = format!("Initializing '{name}' with expression '{expr}'");
                // Get the type of the variable using its specifier,
                // or by deducing the type ourselves.
                let var_ty = if let Some(specified_ty) = specifier {
                    specified_ty.clone()
                } else {
                    expr.get_type(env)?
                };
                var_size = var_ty.get_size(env)?;
                // Compile the expression to leave the value on the stack.
                expr.clone().compile_expr(env, output)?;

                // Get the size of the variable we are writing to.
                env.add_variable_declaration(self)?;
                output.log_instructions_after(&name, &message, current_instruction);
            }
            Declaration::VarPat(pat, expr) => {
                let expr_ty = expr.get_type(env)?;
                var_size = expr_ty.get_size(env)?;
                expr.clone().compile_expr(env, output)?;
                pat.declare_let_bind(&expr, &expr_ty, env)?;
            }
            Declaration::StaticVar(name, _mutability, ty, expr) => {
                let current_instruction = output.current_instruction();
                let message = format!("Initializing static variable '{name}' with expression '{expr}'");
                let var_ty = ty.clone();
                var_size = var_ty.get_size(env)?;
                expr.clone().compile_expr(env, output)?;
                let size = var_ty.get_size(env)?;
                // Store the value in the variable.
                output.op(CoreOp::Global {
                    name: name.clone(),
                    size,
                });
                output.op(CoreOp::Pop(Some(Location::Global(name.clone())), size));
                output.log_instructions_after(&name, &message, current_instruction);
            }
            Declaration::Many(decls) => {
                for decl in decls {
                    var_size += decl.compile(body, env, output)?;
                }
            }
            _ => {}
        }
        Ok(var_size)
    }

    pub(crate) fn join(mut self, other: Self) -> Self {
        match (&mut self, other) {
            (Self::Many(decls), Self::Many(other_decls)) => {
                decls.extend(other_decls);
                self
            }
            (Self::Many(decls), other) => {
                decls.push(other);
                self
            }
            (self_, Self::Many(mut other_decls)) => {
                other_decls.push(self_.clone());
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
            Self::Var(_name, _mutability, expected_ty, expr) => {
                let found_ty = expr.get_type(env)?;
                if let Some(expected_ty) = expected_ty {
                    expected_ty.type_check(env)?;
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
            Self::Proc(name, proc) => {
                let mut new_env = env.clone();
                new_env.define_proc(name, proc.clone());
                proc.type_check(&new_env)?;
            }
            Self::PolyProc(name, proc) => {
                let mut new_env = env.clone();
                new_env.define_poly_proc(name, proc.clone());
                proc.type_check(&new_env)?;
            }
            Self::Type(name, ty) => {
                let mut new_env = env.clone();
                new_env.define_type(name, ty.clone());
                ty.type_check(&new_env)?;
            }
            Self::Const(name, expr) => {
                let mut new_env = env.clone();
                new_env.define_const(name, expr.clone());
                expr.type_check(&new_env)?;
            }
            Self::StaticVar(name, mutability, expected_ty, expr) => {
                let mut new_env = env.clone();
                new_env.define_static_var(name, *mutability, expected_ty.clone())?;
                let found_ty = expr.get_type(&new_env)?;
                expected_ty.type_check(&new_env)?;
                expr.type_check(&new_env)?;

                if !found_ty.can_decay_to(expected_ty, &new_env)? {
                    return Err(Error::MismatchedTypes {
                        expected: expected_ty.clone(),
                        found: found_ty.clone(),
                        expr: Expr::NONE.with(self.clone())
                    });
                }
            }
            Self::VarPat(pat, expr) => {
                let ty = expr.get_type(env)?;
                let size = ty.get_size(env)?;
                if !pat.is_exhaustive(&expr, &ty, env)? {
                    return Err(Error::NonExhaustivePatterns {
                        patterns: vec![pat.clone()],
                        expr: Expr::NONE.with(self.clone()),
                    });
                }
                let bindings = pat.get_bindings(&expr, &ty, env)?;
                let size_of_bindings = bindings.iter().map(|(_name, (_mut, ty))| ty.get_size(env).unwrap_or(0)).sum::<usize>();
                if size_of_bindings != size {
                    return Err(Error::InvalidPatternForExpr(Expr::NONE.with(self.clone()), pat.clone()));
                }
                
                pat.type_check(&expr, &Expr::NONE, env)?;
                expr.type_check(env)?;
            }
            Self::ExternProc(_name, proc) => {
                proc.type_check(env)?;
            }
            Self::Many(decls) => {
                let mut new_env = env.clone();
                new_env.add_compile_time_declaration(&self.clone())?;
                for decl in decls {
                    decl.type_check(&new_env)?;
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