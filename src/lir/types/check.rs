//! # Type Checking
//!
//! This module contains the type checking logic for the Lower Intermediate Representation.
//! Type checking is the process of ensuring that the types of expressions are sound.
//! This performs a number of checks, including:
//! - Ensuring that all types are defined.
//! - Ensuring that all constants are defined.
//! - Ensuring that all procedures are defined.
//! - Ensuring that all variables are defined.
//! - Ensuring that all array lengths are non-negative.
//! - Ensuring that you don't attempt to access a variable that is out of scope.
use super::*;
use crate::lir::Pattern;

use rayon::prelude::*;

use log::{error, trace};
/// A trait used to enforce type checking.
///
/// Whenever this is applied, it will return `Ok(())`
/// if the typing is sound, and `Err(...)` if it is not.
pub trait TypeCheck {
    /// Type check the expression.
    fn type_check(&self, env: &Env) -> Result<(), Error>;
}

/// Check the soundness of a given type in the environment.
impl TypeCheck for Type {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        trace!("Type checking type: {self}");
        // TODO: Also add checks for infinitely sized types.
        match self {
            Self::Any
            | Self::Never
            | Self::None
            | Self::Cell
            | Self::Int
            | Self::Float
            | Self::Bool
            | Self::Char
            | Self::Enum(_) => Ok(()),

            Self::ConstParam(cexpr) => cexpr.type_check(env),
            Self::Type(t) => t.type_check(env),

            // Units are sound if their inner type is sound.
            Self::Unit(_unit_name, t) => t.type_check(env),

            // Symbols are sound if they are defined in the environment
            Self::Symbol(name) => {
                if env.get_type(name).is_some() {
                    Ok(())
                } else {
                    debug!("Type {name} not defined in environment {env}");
                    Err(Error::TypeNotDefined(name.clone()))
                }
            }
            // Let bindings are sound if their inner types are sound.
            Self::Let(name, t, ret) => {
                // Create a new environment with the type defined.
                let mut new_env = env.clone();
                // Define the type in the environment.
                new_env.define_type(name, *t.clone());
                // Check the inner type and the return type.
                t.type_check(&new_env)?;
                ret.type_check(&new_env)
            }
            // Arrays are sound if their inner type is sound.
            Self::Array(t, len) => {
                // Check the inner type and the length constant-expression.
                t.type_check(env)?;
                len.clone().type_check(env)?;

                // Check that the length is non-negative.
                debug!("About to convert {len} to int");
                match len.clone().as_int(env) {
                    Ok(n) if n < 0 => {
                        // If it is negative, return an error.
                        error!("Negative array length detected in type {self} in environment {env}");
                        return Err(Error::NegativeArrayLength(Expr::ConstExpr(*len.clone())));
                    }
                    _ => {}
                }
                // Otherwise, return success.
                Ok(())
            }
            Self::Tuple(ts) => {
                // Check each inner type.
                /*
                for t in ts {
                    // Check the inner type.
                    t.type_check(env)?;
                }
                */
                ts.into_par_iter().try_for_each(|t| t.type_check(env))?;
                // Return success if all the types are sound.
                Ok(())
            }
            Self::Struct(fields) | Self::Union(fields) | Self::EnumUnion(fields) => {
                // Check each inner type.
                /*
                for t in fields.values() {
                    // Check the inner type.
                    t.type_check(env)?;
                }
                */
                fields
                    .values()
                    .collect::<Vec<&Type>>()
                    .into_par_iter()
                    .try_for_each(|t| t.type_check(env))?;
                // Return success if all the types are sound.
                Ok(())
            }

            Self::Proc(args, ret) => {
                // Check each argument type.
                /*
                for t in args {
                    // Check the argument type.
                    t.type_check(env)?;
                }
                */
                args.into_par_iter().try_for_each(|t| t.type_check(env))?;

                // Check the return type.
                ret.type_check(env)
            }

            Self::Poly(ty_params, template) => {
                // Create a new environment with the type parameters defined.
                let mut new_env = env.clone();
                // Define the type parameters in the environment.
                new_env.define_types(
                    ty_params
                        .clone()
                        .into_iter()
                        .map(|p| (p.0.clone(), Type::Unit(p.0, Box::new(Type::Any))))
                        .collect(),
                );
                // Check the template type.
                template.type_check(&new_env)
            }

            Self::Apply(poly, ty_args) => {
                // Check the polymorphic type.
                poly.type_check(env)?;

                // Check each type argument.
                /*
                for t in ty_args {
                    // Check the type argument.
                    t.type_check(env)?;
                }
                */
                ty_args
                    .into_par_iter()
                    .try_for_each(|t| t.type_check(env))?;

                // Try to confirm that the polymorphic type is a template.
                match poly.simplify_until_poly(env, true)? {
                    Type::Symbol(name) => {
                        // Get the type definition.
                        let ty = env
                            .get_type(&name)
                            .ok_or(Error::TypeNotDefined(name.clone()))?;
                        // Check that the type is a template.
                        match ty.simplify_until_poly(env, true)? {
                            Type::Poly(ty_params, _) => {
                                // Check that the number of type arguments matches the number of type parameters.
                                if ty_args.len() != ty_params.len() {
                                    error!("Expected {} type arguments for type {name}, but found {} in environment {env}", ty_params.len(), ty_args.len());
                                    Err(Error::InvalidTemplateArgs(self.clone()))?;
                                }
                            }
                            _ => {
                                error!("Type {name} is not a template in environment {env}");
                                Err(Error::ApplyNonTemplate(self.clone()))?
                            }
                        }
                    }
                    Type::Poly(ty_params, _) => {
                        // Check that the number of type arguments matches the number of type parameters.
                        if ty_params.len() != ty_args.len() {
                            error!("Expected {} type arguments for type {self}, but found {} in environment {env}", ty_params.len(), ty_args.len());
                            Err(Error::InvalidTemplateArgs(self.clone()))?;
                        }
                    }
                    _ => {
                        error!("Type {self} is not a template in environment {env}");
                        Err(Error::ApplyNonTemplate(self.clone()))?
                    }
                }
                // Return success if all the types are sound.
                Ok(())
            }

            // Pointers are sound if their inner type is sound.
            Self::Pointer(_, t) => t.type_check(env),
        }
    }
}

/// Check the type-soundness of a given expression.
impl TypeCheck for Expr {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        trace!("Type checking expression: {self}");
        let ty = self.get_type(env)?;
        ty.type_check(env)?;

        match self {
            Self::Annotated(expr, metadata) => {
                // Check the inner expression.
                expr.type_check(env)
                    .map_err(|e| e.annotate(metadata.clone()))
            }

            Self::Declare(declaration, body) => {
                // Create a new environment with the declarations defined.
                let mut new_env = env.clone();
                // Check the declaration.
                declaration.type_check(&new_env)?;
                // Add the declarations to the environment.
                new_env.add_declaration(declaration)?;
                // Check the body with the declarations defined.
                body.type_check(&new_env)
            }

            Self::UnaryOp(unop, expr) => {
                let unop = env
                    .get_unop(unop)
                    .ok_or(Error::UnimplementedOperator(unop.clone()))?;
                if let Self::Annotated(expr, metadata) = &**expr {
                    return unop
                        .type_check(expr, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                unop.type_check(expr, env)
            }
            Self::BinaryOp(binop, lhs, rhs) => {
                let binop = env
                    .get_binop(binop)
                    .ok_or(Error::UnimplementedOperator(binop.clone()))?;
                if let Self::Annotated(lhs, metadata) = &**lhs {
                    return binop
                        .type_check(lhs, rhs, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(rhs, metadata) = &**rhs {
                    return binop
                        .type_check(lhs, rhs, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }

                binop.type_check(lhs, rhs, env)
            }
            Self::TernaryOp(ternop, a, b, c) => {
                let ternop = env
                    .get_ternop(ternop)
                    .ok_or(Error::UnimplementedOperator(ternop.clone()))?;
                if let Self::Annotated(a, metadata) = &**a {
                    return ternop
                        .type_check(a, b, c, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(b, metadata) = &**b {
                    return ternop
                        .type_check(a, b, c, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(c, metadata) = &**c {
                    return ternop
                        .type_check(a, b, c, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                ternop.type_check(a, b, c, env)
            }
            Self::AssignOp(op, dst, src) => {
                let op = env
                    .get_assignop(op)
                    .ok_or(Error::UnimplementedOperator(op.clone()))?;
                if let Self::Annotated(src, metadata) = &**src {
                    return op
                        .type_check(dst, src, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(dst, metadata) = &**dst {
                    return op
                        .type_check(dst, src, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }

                // Check if the assignment operator is sound with
                // the given expressions.
                op.type_check(dst, src, env)
            }

            Self::Match(expr, branches) => {
                // Check the expression we're matching on.
                expr.type_check(env)?;
                let ty = expr.get_type(env)?;

                if ty == Type::Never || ty == Type::Any {
                    // If the expression is an opaque type like `Any` or `Never`, then
                    // throw an `InvalidMatchExpr` error. We do this
                    // because `Never` is an opaque type, and we can't
                    // match on it.
                    return Err(Error::InvalidMatchExpr(*expr.clone()));
                }

                let mut result_ty: Option<Type> = None;

                // Check each branch.
                for (pat, branch) in branches {
                    // Create a new environment with the bindings defined.
                    let mut new_env = env.clone();
                    // Get the bindings from the pattern.
                    let bindings = pat.get_bindings(expr, &ty, env)?;
                    // Define the bindings in the environment.
                    for (name, (mutability, ty)) in bindings {
                        new_env.define_var(name, mutability, ty)?;
                    }
                    // Check the branch under the new environment.
                    pat.type_check(expr, branch, env)?;

                    // Check that the branch has the same type as the others.
                    // Get the type of the branch.
                    let branch_ty = branch.get_type(&new_env)?;
                    // If we haven't found a type yet, set it.
                    if let Some(result_ty) = &mut result_ty {
                        // Check that the branch type matches the result type.
                        if !branch_ty.can_decay_to(result_ty, &new_env)? {
                            // If it doesn't, return an error.
                            error!("Branch of match has unexpected return type");
                            return Err(Error::MismatchedTypes {
                                found: branch_ty,
                                expected: result_ty.clone(),
                                expr: branch.clone(),
                            });
                        }
                    } else {
                        // Set the result type.
                        result_ty = Some(branch_ty);
                    }
                }

                // Now collect patterns into a list and check if they're exhaustive with Pattern::are_patterns_exhaustive.
                let patterns = branches
                    .iter()
                    .map(|(pat, _)| pat.clone())
                    .collect::<Vec<Pattern>>();
                // If they're not exhaustive, return an error.
                if !Pattern::are_patterns_exhaustive(self, &patterns, &ty, env)? {
                    return Err(Error::NonExhaustivePatterns {
                        patterns,
                        expr: self.clone(),
                    });
                }

                // Return success if all the branches are sound.
                Ok(())
            }

            // Typecheck an if-let expression.
            Self::IfLet(pat, expr, then, otherwise) => {
                // Check the expression we're matching on.
                expr.type_check(env)?;
                // Typecheck the else branch.
                otherwise.type_check(env)?;
                // Check the then and otherwise branches.
                pat.type_check(expr, then, env)?;

                let then_type = pat.get_branch_result_type(expr, then, env)?;
                let otherwise_type = otherwise.get_type(env)?;
                if !otherwise_type.can_decay_to(&then_type, env)? {
                    error!("The else branch of the if statement has an unexpected type");
                    return Err(Error::MismatchedTypes {
                        expected: then_type,
                        found: otherwise_type,
                        expr: self.clone(),
                    });
                }
                // Return success if all the branches are sound.
                Ok(())
            }

            // Typecheck the inner constant expression.
            Self::ConstExpr(c) => c.type_check(env),

            // Typecheck a block of expressions.
            Self::Many(exprs) => {
                // Typecheck each expression.
                /*
                for (i, expr) in exprs.iter().enumerate() {
                    // Check the inner expression.
                    expr.type_check(env)?;
                    if i < exprs.len() - 1 {
                        // If it's not the last expression, confirm that it's of type `None`.
                        // Otherwise, return an error.
                        let ty = expr.get_type(env)?;
                        if !ty.can_decay_to(&Type::None, env)? {
                            error!("Expected type {} for expression {expr}, but found type {ty} in environment {env}", Type::None);
                            // If it's not, return an error.
                            return Err(Error::UnusedExpr(expr.clone(), ty));
                        }
                    }
                }
                */

                let count = exprs.len();
                exprs.into_par_iter()
                    .enumerate()
                    .try_for_each(|(i, expr)| {
                        expr.type_check(env)?;
                        if i < count - 1 {
                            // If it's not the last expression, confirm that it's of type `None`.
                            // Otherwise, return an error.
                            let ty = expr.get_type(env)?;
                            if !ty.can_decay_to(&Type::None, env)? {
                                error!("Expected type {} for expression {expr}, but found type {ty} in environment {env}", Type::None);
                                // If it's not, return an error.
                                return Err(Error::UnusedExpr(expr.clone(), ty));
                            }
                        }
                        Ok(())
                    })?;

                // Return success if all the expressions are sound.
                Ok(())
            }

            Self::While(cond, body) => {
                // Typecheck the condition.
                cond.type_check(env)?;
                // Typecheck the body.
                body.type_check(env)
            }

            Self::If(cond, t, e) => {
                // Typecheck the condition.
                cond.type_check(env)?;
                // Typecheck the then and else branches.
                t.type_check(env)?;
                e.type_check(env)?;

                // Get the types of the then and else branches.
                let t_type = t.get_type(env)?;
                let e_type = e.get_type(env)?;
                // Check that the types of the then and else branches are compatible.
                if !e_type.can_decay_to(&t_type, env)? {
                    // If they're not, return an error.
                    return Err(Error::MismatchedTypes {
                        expected: t_type,
                        found: e_type,
                        expr: self.clone(),
                    });
                }
                Ok(())
            }

            Self::When(cond, t, e) => {
                // Typecheck the condition.
                cond.type_check(env)?;
                if cond.clone().as_bool(env)? {
                    // Typecheck the then branch.
                    t.type_check(env)?;
                } else {
                    // Typecheck the else branch.
                    e.type_check(env)?;
                }
                // Since `when` expressions are computed at compile time,
                // we don't have to care about matching the types of the then and else branches.
                Ok(())
            }

            // Typecheck a reference to a value.
            Self::Refer(expected_mutability, e) => match *e.clone() {
                Expr::Annotated(expr, metadata) => Self::Refer(*expected_mutability, expr)
                    .type_check(env)
                    .map_err(|e| e.annotate(metadata)),
                Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                    // Check if the symbol is defined as mutable
                    if env.is_defined_as_mutable(&name) || expected_mutability.is_constant() {
                        // If it is, then return success. (We can reference it however we want.)
                        // If the symbol is not defined as mutable, but we expect it to be constant,
                        // then return success. (We can reference it as a constant.)
                        Ok(())
                    } else {
                        error!("Expected mutability {expected_mutability} for expression {self}, but found incompatible mutability in environment {env}");
                        // If it isn't, then return an error.
                        Err(Error::InvalidRefer(self.clone()))
                    }
                }
                Expr::ConstExpr(_cexpr) => Ok(()),
                Expr::Deref(inner) | Expr::Index(inner, _) => {
                    // Confirm that the inner expression can be referenced.
                    match inner.get_type(env)? {
                        // If we are dereferencing/indexing a pointer,
                        // confirm that the inner pointer has the expected mutability.
                        Type::Pointer(found_mutability, _) => {
                            if !found_mutability.can_decay_to(expected_mutability) {
                                // If if doesn't, then return an error.
                                error!("Expected mutability {expected_mutability} for expression {self}, but found mutability {found_mutability} in environment {env}");
                                return Err(Error::MismatchedMutability {
                                    expected: *expected_mutability,
                                    found: found_mutability,
                                    expr: self.clone(),
                                });
                            }
                        }

                        // If we are indexing an array, confirm that the inner array can be referenced with the expected mutability.
                        Type::Array(_, _) => {
                            inner.refer(*expected_mutability).type_check(env)?;
                        }

                        _ => {}
                    }

                    e.type_check(env)
                }
                Expr::Member(inner, _) => {
                    // // Confirm that the inner expression can be referenced.
                    match inner.get_type(env)? {
                        // If we are getting a member of a struct/union/tuple,
                        // check if we can reference the inner expression with the expected mutability.
                        Type::Struct(_) | Type::Union(_) | Type::Tuple(_) => {
                            inner.refer(*expected_mutability).type_check(env)?;
                        }
                        // If we are getting a member of a pointer,
                        // confirm that the inner pointer has the expected mutability.
                        Type::Pointer(found_mutability, _) => {
                            if !found_mutability.can_decay_to(expected_mutability) {
                                // If if doesn't, then return an error.
                                error!("Expected mutability {expected_mutability} for expression {self}, but found mutability {found_mutability} in environment {env}");
                                return Err(Error::MismatchedMutability {
                                    expected: *expected_mutability,
                                    found: found_mutability,
                                    expr: self.clone(),
                                });
                            }
                        }
                        _ => {}
                    }

                    // If so, then make sure the expression being accessed is also sound.
                    e.type_check(env)
                }
                other => Err(Error::InvalidRefer(other)),
            },
            // Typecheck a dereference of a pointer.
            Self::Deref(e) => {
                // Typecheck the expression which evaluates
                // to the address we will dereference.
                e.type_check(env)?;
                // Get the type of the expression.
                let t = e.get_type(env)?;
                // Check that the type is a pointer.
                if let Type::Pointer(_, _) = t {
                    // If it is, return success.
                    Ok(())
                } else {
                    // If it isn't, return an error.
                    Err(Error::MismatchedTypes {
                        // The expected type is a pointer.
                        expected: Type::Pointer(Mutability::Any, Box::new(Type::Any)),
                        found: t,
                        expr: self.clone(),
                    })
                }
            }

            // Typecheck an assignment of a value to the data stored at
            // a given pointer.
            Self::DerefMut(ptr, val) => {
                // Typecheck the pointer and the value we want to assign.
                ptr.type_check(env)?;
                val.type_check(env)?;
                // Get the types of the pointer and the value.
                let ptr_type = ptr.get_type(env)?;
                let val_type = val.get_type(env)?;
                // Check that the pointer is a pointer.
                if let Type::Pointer(mutability, ptr_elem_ty) = ptr_type {
                    // Check that the type of the value is compatible
                    // with the type of data stored at the pointer's
                    // address.
                    if ptr_elem_ty.can_decay_to(&val_type, env)? {
                        // If it is, return success.
                        if mutability.is_mutable() {
                            Ok(())
                        } else {
                            Err(Error::MismatchedMutability {
                                expected: Mutability::Mutable,
                                found: mutability,
                                expr: self.clone(),
                            })
                        }
                    } else {
                        error!("Could not assign to pointer of different type");
                        // If it isn't, return an error.
                        Err(Error::MismatchedTypes {
                            expected: val_type,
                            found: *ptr_elem_ty,
                            expr: self.clone(),
                        })
                    }
                } else {
                    // If the destination to store isn't a pointer, return an error.
                    error!("Could not assign to non-pointer");
                    Err(Error::MismatchedTypes {
                        expected: Type::Pointer(Mutability::Mutable, Box::new(Type::Any)),
                        found: ptr_type,
                        expr: self.clone(),
                    })
                }
            }

            // Typecheck a function application.
            Self::Apply(f, args) => {
                if self.is_method_call(env)? {
                    // Get the type of the object we're calling the method on.
                    let method_call = self.transform_method_call(env)?;
                    debug!("Transformed method call: {method_call}");

                    if let Self::Apply(f, args) = method_call.clone() {
                        // Typecheck the supplied arguments.
                        for arg in &args {
                            arg.type_check(env)?;
                        }

                        f.type_check(env)?;

                        // Get the type of the function.
                        let f_type = f.get_type(env)?.simplify_until_concrete(env, true)?;
                        // Infer the types of the supplied arguments.
                        let mut found_arg_tys = vec![];
                        for arg in args {
                            found_arg_tys.push(arg.get_type(env)?);
                        }
                        match f_type {
                            Type::Proc(expected_arg_tys, ret_ty) => {
                                // If the number of arguments is incorrect, then return an error.
                                if expected_arg_tys.len() != found_arg_tys.len() {
                                    error!("Unexpected number of arguments");
                                    return Err(Error::MismatchedTypes {
                                        expected: Type::Proc(expected_arg_tys, ret_ty.clone()),
                                        found: Type::Proc(found_arg_tys, ret_ty),
                                        expr: self.clone(),
                                    });
                                }
                                // If the function is a procedure, confirm that the type of each
                                // argument matches the the type of the supplied value.
                                for (expected, found) in
                                    expected_arg_tys.into_iter().zip(found_arg_tys.into_iter())
                                {
                                    // If the types don't match, return an error.
                                    if !found.can_decay_to(&expected, env)? {
                                        error!("Procedure argument type mismatch");
                                        return Err(Error::MismatchedTypes {
                                            expected,
                                            found,
                                            expr: self.clone(),
                                        });
                                    }
                                }
                                return Ok(());
                            }
                            // If the function is not a procedure, return an error.
                            _ => {
                                error!("Called non-procedure");
                                return Err(Error::MismatchedTypes {
                                    expected: Type::Proc(found_arg_tys, Box::new(Type::Any)),
                                    found: f_type,
                                    expr: self.clone(),
                                });
                            }
                        }
                    }
                }

                // Typecheck the expression we want to call as a procedure.
                f.type_check(env)?;
                // Typecheck the supplied arguments.
                for arg in args {
                    arg.type_check(env)?;
                }

                // Get the type of the function.
                let f_type = f.get_type(env)?.simplify_until_concrete(env, true)?;
                // Infer the types of the supplied arguments.
                let mut found_arg_tys = vec![];
                for arg in args {
                    found_arg_tys.push(arg.get_type(env)?);
                }
                match f_type {
                    Type::Proc(expected_arg_tys, ret_ty) => {
                        // If the number of arguments is incorrect, then return an error.
                        if expected_arg_tys.len() != found_arg_tys.len() {
                            return Err(Error::MismatchedTypes {
                                expected: Type::Proc(expected_arg_tys, ret_ty.clone()),
                                found: Type::Proc(found_arg_tys, ret_ty),
                                expr: self.clone(),
                            });
                        }
                        // If the function is a procedure, confirm that the type of each
                        // argument matches the the type of the supplied value.
                        for (expected, found) in
                            expected_arg_tys.into_iter().zip(found_arg_tys.into_iter())
                        {
                            // If the types don't match, return an error.
                            if !found.can_decay_to(&expected, env)? {
                                error!("Procedure argument type mismatch");
                                return Err(Error::MismatchedTypes {
                                    expected,
                                    found,
                                    expr: self.clone(),
                                });
                            }
                        }
                        Ok(())
                    }
                    // If the function is not a procedure, return an error.
                    _ => {
                        error!("Called non-function {self}");
                        Err(Error::MismatchedTypes {
                            expected: Type::Proc(found_arg_tys, Box::new(Type::Any)),
                            found: f_type,
                            expr: self.clone(),
                        })
                    }
                }
            }

            // Typecheck a return statement.
            Self::Return(e) => {
                e.type_check(env)?;
                let found = e.get_type(env)?;
                let expected = env
                    .get_expected_return_type()
                    .cloned()
                    .unwrap_or(Type::None).simplify(env)?;
                if !found.can_decay_to(&expected, env)? {
                    error!("The found return type {found} does not match expected type {expected} in {env}");
                    return Err(Error::MismatchedTypes {
                        expected,
                        found,
                        expr: self.clone(),
                    });
                }
                Ok(())
            }

            // Typecheck an array or tuple literal.
            Self::Array(items) => {
                let last_type = items[0].get_type(env)?;
                items.into_par_iter().try_for_each(|item| {
                    item.type_check(env)?;
                    let item_type = item.get_type(env)?;
                    if !last_type.can_decay_to(&item_type, env)? {
                        error!("Mismatched types: {last_type} != {item_type} in environment {env}");
                        return Err(Error::MismatchedTypes {
                            expected: last_type.clone(),
                            found: item_type,
                            expr: self.clone(),
                        });
                    }
                    Ok(())
                })?;

                // Return success.
                Ok(())
            }
            Self::Tuple(elems) => {
                for elem in elems {
                    elem.type_check(env)?;
                }
                Ok(())
            }

            // Typecheck a struct literal.
            Self::Struct(fields) => {
                for field_expr in fields.values() {
                    field_expr.type_check(env)?;
                }
                Ok(())
            }

            // Typecheck a union literal.
            Self::Union(t, field, val) => {
                // Typecheck the type.
                t.type_check(env)?;
                let t = t.simplify_until_union(env, true)?;
                match t {
                    Type::Union(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(expected_ty) = fields.get(field) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !found.can_decay_to(expected_ty, env)? {
                                return Err(Error::MismatchedTypes {
                                    expected: expected_ty.clone(),
                                    found,
                                    expr: self.clone(),
                                });
                            }
                            Ok(())
                        } else {
                            Err(Error::MemberNotFound(
                                self.clone(),
                                ConstExpr::Symbol(field.clone()),
                            ))
                        }
                    }
                    _ => Err(Error::MemberNotFound(
                        self.clone(),
                        ConstExpr::Symbol(field.clone()),
                    )),
                }
            }

            // Typecheck a tagged union literal.
            Self::EnumUnion(t, variant, val) => {
                // Typecheck the type
                t.type_check(env)?;
                let t = t.simplify_until_union(env, true)?;

                match t {
                    Type::EnumUnion(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(expected_ty) = fields.get(variant) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !found.can_decay_to(expected_ty, env)? {
                                error!("Could not verify variant of enum {self}");
                                return Err(Error::MismatchedTypes {
                                    expected: expected_ty.clone(),
                                    found,
                                    expr: self.clone(),
                                });
                            }
                            Ok(())
                        } else {
                            Err(Error::VariantNotFound(
                                Type::EnumUnion(fields),
                                variant.clone(),
                            ))
                        }
                    }
                    t => Err(Error::VariantNotFound(t, variant.clone())),
                }
            }

            // Typecheck a type-cast.
            Self::As(e, desired_ty) => {
                // Typecheck the expression we want to cast.
                e.type_check(env)?;
                // Get the actual type of the expression.
                let found_ty = e.get_type(env)?;

                // Check that the cast is valid.
                if found_ty.can_cast_to(desired_ty, env)? {
                    // If it is, return success.
                    Ok(())
                } else {
                    // Otherwise, it isn't a valid cast, so return an error.
                    Err(Error::InvalidAs(self.clone(), found_ty, desired_ty.clone()))
                }
            }

            // Typecheck a member access.
            Self::Member(e, field) => {
                trace!("Typechecking regular member access {e}.{field}");
                // Typecheck the expression we want to access a member of.
                e.type_check(env)?;
                // Get the type of the expression.
                let e_type = e.get_type(env)?;
                // e_type.add_monomorphized_associated_consts(env)?;

                // Typecheck the member we want to access.
                match e_type.type_check_member(field, e, env) {
                    Ok(_) => Ok(()),
                    Err(e) => {
                        debug!("Type {e_type} doesn't have member {field} in environment {env}");
                        match field
                            .clone()
                            .as_symbol(env)
                            .map(|name| env.get_associated_const(&e_type, &name))
                        {
                            Ok(_) => Ok(()),
                            Err(_) => {
                                error!("Could not find member {field} in type {e_type} in environment {env}");
                                Err(e)
                            }
                        }
                    }
                }
            }

            // Typecheck an index access.
            Self::Index(val, idx) => {
                // Typecheck the expression we want to index.
                val.type_check(env)?;
                // Typecheck the index we want to access.
                idx.type_check(env)?;
                // Get the type of the expression we want to index.
                let val_type = val.get_type(env)?;
                // Get the type of the index.
                let idx_type = idx.get_type(env)?;
                // Confirm that the type is an array or pointer.
                match val_type {
                    Type::Array(_, _) | Type::Pointer(_, _) => {}
                    // If it isn't, return an error.
                    _ => return Err(Error::InvalidIndex(self.clone())),
                }

                // Confirm that the index is an integer.
                if let Type::Int = idx_type {
                    // If it is, return success.
                    Ok(())
                } else {
                    // Otherwise, return an error.
                    Err(Error::InvalidIndex(self.clone()))
                }
            }
        }
    }
}

// Typecheck a constant expression.
impl TypeCheck for ConstExpr {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        if env.has_type_checked_const(self) {
            return Ok(());
        }

        debug!("Typechecking constant expression: {}", self);
        match self {
            Self::Any => Ok(()),
            Self::Template(_ty_params, _template) => {
                // Create a new environment with the type parameters defined.
                // let mut new_env = env.clone();
                // // Define the type parameters in the environment.
                // new_env.define_types(
                //     ty_params
                //         .clone()
                //         .into_iter()
                //         .map(|p| (p.clone(), Type::Unit(p, Box::new(Type::None))))
                //         .collect(),
                // );
                // Check the template type.
                // template.type_check(&new_env)
                Ok(())
            }

            Self::Annotated(expr, metadata) => expr
                .type_check(env)
                .map_err(|e| e.annotate(metadata.clone())),

            // Typecheck a type expression.
            Self::Type(t) => t.type_check(env),

            // Typecheck a member access.
            Self::Member(e, field) => {
                // Typecheck the expression we want to access a member of.
                e.type_check(env)?;
                // Get the type of the expression.
                let e_type = e.get_type(env)?;
                // e_type.add_monomorphized_associated_consts(env)?;
                // Typecheck the member we want to access.
                match e_type.type_check_member(field, &Expr::ConstExpr(*e.clone()), env) {
                    Ok(_) => Ok(()),
                    Err(_err) => {
                        debug!("Member {field} not found in type {e_type} in environment {env}");
                        match field
                            .clone()
                            .as_symbol(env)
                            .map(|name| env.get_associated_const(&e_type, &name))
                        {
                            Ok(_) => {
                                debug!("Associated constant {field} found in type {e_type} in environment {env}");
                                Ok(())
                            }
                            // Err(_) => Err(e),
                            Err(_) => {
                                // Try to perform the member op as a regular member op.
                                debug!("Associated constant {field} not found in type {e_type} in environment {env}");
                                debug!("Falling back on regular member access");
                                Expr::Member(Box::new(Expr::ConstExpr(*e.clone())), *field.clone())
                                    .type_check(env)
                            }
                        }
                    }
                }
            }

            // These are all guaranteed to be valid, or
            // to fail at compile time.
            Self::None
            | Self::Null
            | Self::Cell(_)
            | Self::Int(_)
            | Self::Float(_)
            | Self::Char(_)
            | Self::Bool(_) => Ok(()),

            Self::SizeOfType(t) => t.type_check(env),

            Self::Declare(bindings, expr) => {
                // Create a new environment with the declarations defined.
                let mut new_env = env.clone();
                // If this binding declares a local variable,
                // throw an error.
                if bindings.has_local_variable_declaration() {
                    // Cannot declare local variables in a constant expression.
                    return Err(Error::InvalidConstExpr(self.clone()));
                }
                // Add all the bindings to the environment.
                new_env.add_compile_time_declaration(bindings)?;
                // Typecheck the bindings
                bindings.type_check(&new_env)?;
                // Typecheck the expression with the bindings defined.
                expr.type_check(&new_env)
            }
            Self::Monomorphize(expr, ty_args) => {
                debug!(
                    "Monomorphizing {expr} with type arguments {ty_args:?} in environment {env}"
                );
                match **expr {
                    Self::Template(ref ty_params, ref template) => {
                        // Create a new environment with the type parameters defined.
                        // Define the type parameters in the environment.
                        // new_env.define_types(
                        //     ty_params.clone().into_iter().map(|x| x.0).zip(ty_args.clone()).collect(),
                        // );

                        let mut ret = template.clone();
                        let mut new_env = env.clone();
                        for ((param, ty), ty_arg) in ty_params.iter().zip(ty_args.iter()) {
                            if let Type::ConstParam(cexpr) = ty_arg {
                                if let Some(expected_ty) = ty {
                                    let expected = expected_ty.clone();
                                    let found = cexpr.get_type(env)?;
                                    if !found.equals(expected_ty, env)? {
                                        error!("Mismatch in expected type for constant parameter");
                                        return Err(Error::MismatchedTypes { expected, found, expr: (*cexpr.clone()).into() })
                                    }
                                    ret.substitute(param, ty_arg)
                                }
                                ret.substitute(param, ty_arg);
                                new_env.define_const(param, *cexpr.clone());
                            } else {
                                ret.substitute(param, ty_arg);
                                new_env.define_type(param, ty_arg.clone());
                            }
                        }
                        debug!("Result: {ret}");
                        let ret = ret.get_type(&new_env)?
                            .simplify_until_poly(&new_env, true)?;
                        // Check the template type.
                        ret.type_check(&new_env)
                        // Ok(())
                    }
                    Self::PolyProc(ref poly) => {
                        // poly.monomorphize(ty_args.clone(), env)?.type_check(env)
                        // Create a new environment with the type parameters defined.
                        // let mut new_env = env.clone();
                        // // Define the type parameters in the environment.
                        // new_env.define_types(
                        //     poly.ty_params
                        //         .clone()
                        //         .into_iter()
                        //         .zip(ty_args.clone())
                        //         .collect(),
                        // );
                        // Check the template type.
                        poly.type_check(env)
                        // Ok(())
                    }
                    _ => {
                        self.get_type(env)?.type_check(env)?;
                        expr.type_check(env)?;
                        // if let Self::PolyProc(poly) = *expr.clone() {
                        //     poly.type_check(env)?
                        // }
                        /*
                        for ty in ty_args {
                            ty.type_check(env)?;
                        }
                        */
                        ty_args
                            .into_par_iter()
                            .try_for_each(|ty| ty.type_check(env))?;
                        Ok(())
                    }
                }
            }

            Self::TypeOf(expr) => expr.type_check(env),

            // Typecheck a constant type-cast.
            Self::As(expr, cast_ty) => {
                // Calculate the inferred type of the expression.
                let found = expr.get_type(env)?;
                // Confirm that the cast is valid.
                if !found.can_cast_to(cast_ty, env)? {
                    error!("Invalid cast: {found} as {cast_ty} in environment {env}");
                    // If it isn't, return an error.
                    return Err(Error::InvalidAs(
                        Expr::ConstExpr(*expr.clone()),
                        found,
                        cast_ty.clone(),
                    ));
                }
                // If it is, return the result of the inner expression's typechecking result.
                expr.type_check(env)
            }

            // Get the size of an expression in cells.
            Self::SizeOfExpr(e) => e.type_check(env),

            // Typecheck a core-builtin inline assembly procedure.
            Self::CoreBuiltin(builtin) => builtin.type_check(env),
            // Typecheck a standard-builtin inline assembly procedure.
            Self::StandardBuiltin(builtin) => builtin.type_check(env),
            // Typecheck a FFI procedure call
            Self::FFIProcedure(ffi) => ffi.type_check(env),
            // Typecheck a procedure.
            Self::Proc(proc) => proc.type_check(env),
            Self::PolyProc(proc) => proc.type_check(env),

            // Typecheck a symbol.
            Self::Symbol(name) => {
                // If there is some binding for the symbol, return success.
                if env.get_const(name).is_some()
                    || env.get_proc(name).is_some()
                    || env.get_var(name).is_some()
                    || env.get_static_var(name).is_some()
                    || env.get_type(name).is_some()
                {
                    // Return success.
                    Ok(())
                } else {
                    error!("Symbol {name} not defined in environment {env}");
                    // If there is no binding for the symbol, return an error.
                    Err(Error::SymbolNotDefined(name.clone()))
                }
            }

            // Typecheck a variant of an enum.
            Self::Of(t, variant) => {
                let t = t
                    .simplify_until_has_variants(env, true)
                    .map_err(|_| Error::VariantNotFound(t.clone(), variant.clone()))?;

                match t {
                    Type::Enum(variants) => {
                        // If the enum contains the variant, return success.
                        if variants.contains(variant) {
                            // Return success.
                            Ok(())
                        } else {
                            // Otherwise, the variant isn't contained in the enum,
                            // so return an error.
                            Err(Error::VariantNotFound(
                                Type::Enum(variants),
                                variant.clone(),
                            ))
                        }
                    }
                    Type::EnumUnion(variants) if variants.get(variant) == Some(&Type::None) => {
                        // If the enum union contains the variant, and the variant is empty, return success.
                        if variants.contains_key(variant)
                            && variants.get(variant) == Some(&Type::None)
                        {
                            // Return success.
                            Ok(())
                        } else {
                            // Otherwise, the variant isn't contained in the enum,
                            // so return an error.
                            Err(Error::VariantNotFound(
                                Type::EnumUnion(variants),
                                variant.clone(),
                            ))
                        }
                    }
                    _ => Err(Error::VariantNotFound(t.clone(), variant.clone())),
                }
            }

            // Typecheck a tuple literal.
            Self::Tuple(items) => {
                // Typecheck each item in the tuple.
                /*
                for item in items {
                    // Typecheck the item.
                    item.type_check(env)?;
                }
                */
                items
                    .into_par_iter()
                    .try_for_each(|item| item.type_check(env))?;
                // Return success.
                Ok(())
            }

            // Typecheck an array literal.
            Self::Array(items) => {
                let last_type = items[0].get_type(env)?;
                // Typecheck each item in the array.
                /*
                for item in items {
                    // Typecheck the item.
                    item.type_check(env)?;
                    // Get the type of the item.
                    let item_type = item.get_type(env)?;
                    // If the type of the item is different from the last item,
                    if let Some(last_type) = last_type {
                        // Confirm that the type of the item is the same as the
                        // last item.
                        if !last_type.can_decay_to(&item_type, env)? {
                            error!(
                                "Mismatched types: {last_type} != {item_type} in environment {env}"
                            );
                            // If it isn't, return an error.
                            return Err(Error::MismatchedTypes {
                                expected: last_type,
                                found: item_type,
                                expr: Expr::ConstExpr(self.clone()),
                            });
                        }
                    }
                    last_type = Some(item_type);
                }
                 */
                items.into_par_iter().try_for_each(|item| {
                    item.type_check(env)?;
                    let item_type = item.get_type(env)?;
                    if !last_type.can_decay_to(&item_type, env)? {
                        error!("Mismatched types: {last_type} != {item_type} in environment {env}");
                        return Err(Error::MismatchedTypes {
                            expected: last_type.clone(),
                            found: item_type,
                            expr: Expr::ConstExpr(self.clone()),
                        });
                    }
                    Ok(())
                })?;

                // Return success.
                Ok(())
            }

            // Typecheck a struct literal.
            Self::Struct(fields) => {
                // Typecheck each field in the struct.
                /*
                for item in fields.values() {
                    // Typecheck the item.
                    item.type_check(env)?;
                }
                */
                fields
                    .values()
                    .collect::<Vec<&ConstExpr>>()
                    .into_par_iter()
                    .try_for_each(|item| item.type_check(env))?;
                // Return success.
                Ok(())
            }

            // Typecheck a union literal.
            Self::Union(t, field, val) => {
                // Confirm the type supplied is a union.
                let t = t.simplify_until_union(env, true)?;

                match t {
                    Type::Union(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(expected_ty) = fields.get(field) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !found.can_decay_to(expected_ty, env)? {
                                error!("Mismatched types: {expected_ty} != {found} in environment {env}");
                                return Err(Error::MismatchedTypes {
                                    expected: expected_ty.clone(),
                                    found,
                                    expr: Expr::ConstExpr(self.clone()),
                                });
                            }
                            Ok(())
                        } else {
                            error!("Member {field} not found in type {self} in environment {env}");
                            Err(Error::MemberNotFound(
                                Expr::ConstExpr(self.clone()),
                                ConstExpr::Symbol(field.clone()),
                            ))
                        }
                    }
                    _ => {
                        error!("Member {field} not found in type {self} in environment {env}");
                        Err(Error::MemberNotFound(
                            Expr::ConstExpr(self.clone()),
                            ConstExpr::Symbol(field.clone()),
                        ))
                    }
                }
            }

            // Typecheck a tagged union literal.
            Self::EnumUnion(t, variant, val) => {
                // Confirm the type supplied is a union.
                let t = t.simplify_until_union(env, true)?;
                match t {
                    Type::EnumUnion(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(expected_ty) = fields.get(variant) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !found.can_decay_to(expected_ty, env)? {
                                error!("Mismatched types: {found} != {expected_ty} in environment {env}");
                                return Err(Error::MismatchedTypes {
                                    expected: expected_ty.clone(),
                                    found,
                                    expr: Expr::ConstExpr(self.clone()),
                                });
                            }
                            Ok(())
                        } else {
                            error!(
                                "Variant {variant} not found in type {self} in environment {env}"
                            );
                            Err(Error::VariantNotFound(
                                Type::EnumUnion(fields),
                                variant.clone(),
                            ))
                        }
                    }
                    _ => Err(Error::VariantNotFound(t.clone(), variant.clone())),
                }
            }
        }?;
        env.save_type_checked_const(self.clone());
        Ok(())
    }
}
