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

            // Units are sound if their inner type is sound.
            Self::Unit(_unit_name, t) => t.type_check(env),

            // Symbols are sound if they are defined in the environment
            Self::Symbol(name) => {
                if env.get_type(name).is_some() {
                    Ok(())
                } else {
                    Err(Error::TypeNotDefined(name.clone()))
                }
            }
            // Let bindings are sound if their inner types are sound.
            Self::Let(name, t, ret) => {
                // Create a new environment with the type defined.
                let mut new_env = env.clone();
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
                if len.clone().as_int(env)? < 0 {
                    // If it is negative, return an error.
                    return Err(Error::NegativeArrayLength(Expr::ConstExpr(*len.clone())));
                }
                // Otherwise, return success.
                Ok(())
            }
            Self::Tuple(ts) => {
                // Check each inner type.
                for t in ts {
                    // Check the inner type.
                    t.type_check(env)?;
                }
                // Return success if all the types are sound.
                Ok(())
            }
            Self::Struct(fields) | Self::Union(fields) | Self::EnumUnion(fields) => {
                // Check each inner type.
                for t in fields.values() {
                    // Check the inner type.
                    t.type_check(env)?;
                }
                // Return success if all the types are sound.
                Ok(())
            }

            Self::Proc(args, ret) => {
                // Check each argument type.
                for t in args {
                    // Check the argument type.
                    t.type_check(env)?;
                }
                // Check the return type.
                ret.type_check(env)
            }

            Self::Poly(ty_params, template) => {
                // Create a new environment with the type parameters defined.
                let mut new_env = env.clone();
                for param in ty_params {
                    new_env.define_type(param, Type::Unit(param.clone(), Box::new(Type::Any)));
                }
                // Check the template type.
                template.type_check(&new_env)
            }

            Self::Apply(poly, ty_args) => {
                // Check the polymorphic type.
                poly.type_check(env)?;

                // Check each type argument.
                for t in ty_args {
                    // Check the type argument.
                    t.type_check(env)?;
                }

                // Try to confirm that the polymorphic type is a template.
                match poly.clone().simplify_until_matches(
                    env,
                    Type::Poly(vec![], Box::new(Type::Any)),
                    |t, env| Ok(matches!(t, Type::Poly(_, _))),
                )? {
                    Type::Symbol(name) => {
                        // Get the type definition.
                        let ty = env
                            .get_type(&name)
                            .ok_or(Error::TypeNotDefined(name.clone()))?;
                        // Check that the type is a template.
                        match ty.clone().simplify_until_matches(
                            env,
                            Type::Poly(vec![], Box::new(Type::Any)),
                            |t, env| Ok(matches!(t, Type::Poly(_, _))),
                        )? {
                            Type::Poly(ty_params, _) => {
                                // Check that the number of type arguments matches the number of type parameters.
                                if ty_args.len() != ty_params.len() {
                                    Err(Error::InvalidTemplateArgs(self.clone()))?;
                                }
                            }
                            _ => Err(Error::ApplyNonTemplate(self.clone()))?,
                        }
                    }
                    Type::Poly(ty_params, _) => {
                        // Check that the number of type arguments matches the number of type parameters.
                        if ty_params.len() != ty_args.len() {
                            Err(Error::InvalidTemplateArgs(self.clone()))?;
                        }
                    }
                    _ => Err(Error::ApplyNonTemplate(self.clone()))?,
                }
                // Return success if all the types are sound.
                Ok(())
            }

            // Pointers are sound if their inner type is sound.
            Self::Pointer(t) => t.type_check(env),
        }
    }
}

/// Check the type-soundness of a given expression.
impl TypeCheck for Expr {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        match self {
            Self::AnnotatedWithSource { expr, loc } => {
                // Check the inner expression.
                expr.type_check(env).map_err(|e| e.with_loc(loc))
            }

            Self::UnaryOp(unop, expr) => {
                // Check if the unary operator is sound with
                // the given expression.
                unop.type_check(expr, env)
            }
            Self::BinaryOp(binop, lhs, rhs) => {
                // Check if the binary operator is sound with
                // the given expressions.
                binop.type_check(lhs, rhs, env)
            }
            Self::TernaryOp(ternop, a, b, c) => {
                // Check if the ternary operator is sound with
                // the given expressions.
                ternop.type_check(a, b, c, env)
            }
            Self::AssignOp(op, dst, src) => {
                // Check if the assignment operator is sound with
                // the given expressions.
                op.type_check(dst, src, env)
            }

            Self::Match(expr, branches) => {
                // Check the expression we're matching on.
                expr.type_check(env)?;
                let ty = expr.get_type(env)?;

                if ty == Type::Never {
                    // If the expression is of type `Never`, then
                    // throw an `InvalidMatchExpr` error. We do this
                    // because `Never` is an opaque type, and we can't
                    // match on it.
                    return Err(Error::InvalidMatchExpr(*expr.clone()));
                } else if ty == Type::Any {
                    // If the expression is an opaque `Any` type,
                    // then throw an `InvalidMatchExpr` error. We do this
                    // because `Any` is an opaque type, and we can't
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
                    for (name, t) in bindings {
                        new_env.define_var(name, t)?;
                    }
                    // Check the branch under the new environment.
                    pat.type_check(expr, branch, env)?;

                    // Check that the branch has the same type as the others.
                    // Get the type of the branch.
                    let branch_ty = branch.get_type(&new_env)?;
                    // If we haven't found a type yet, set it.
                    if let Some(result_ty) = &mut result_ty {
                        // Check that the branch type matches the result type.
                        if !result_ty.equals(&branch_ty, &new_env)? {
                            // If it doesn't, return an error.
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
                let mut patterns = Vec::new();
                for (pat, _) in branches {
                    patterns.push(pat.clone());
                }
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
                if !then_type.equals(&otherwise_type, env)? {
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
                for (i, expr) in exprs.iter().enumerate() {
                    // Check the inner expression.
                    expr.type_check(env)?;
                    if i < exprs.len() - 1 {
                        // If it's not the last expression, confirm that it's of type `None`.
                        // Otherwise, return an error.
                        let ty = expr.get_type(env)?;
                        if !ty.equals(&Type::None, env)? {
                            // If it's not, return an error.
                            return Err(Error::UnusedExpr(expr.clone(), ty.clone()));
                        }
                    }
                }
                // Return success if all the expressions are sound.
                Ok(())
            }

            // Typecheck a declaration of a constant.
            Self::LetConst(name, e, ret) => {
                // Typecheck the constant expression we're assigning to the variable.
                let mut new_env = env.clone();
                new_env.define_const(name.clone(), e.clone());
                e.type_check(&new_env)?;
                // Typecheck the resulting expression with the constant
                // defined in the environment.
                ret.type_check(&new_env)
            }

            // Typecheck a declaration of multiple constants.
            Self::LetConsts(constants, ret) => {
                // Add all the constants to the scope.
                let mut new_env = env.clone();
                for (name, c) in constants {
                    // Define the constant in the environment.
                    new_env.define_const(name, c.clone());
                }
                // Typecheck the constant expression we're assigning to each name.
                for c in constants.values() {
                    // Typecheck the constant expression in the new environment.
                    c.type_check(&new_env)?;
                }
                // Typecheck the resulting expression with the constants
                // defined in the environment.
                ret.type_check(&new_env)
            }

            // Typecheck a declaration of a procedure.
            Self::LetProc(var, proc, ret) => {
                // Create a new environment with the procedure defined.
                let mut new_env = env.clone();
                new_env.define_proc(var.clone(), proc.clone());
                // Typecheck the procedure we're defining.
                proc.type_check(&new_env)?;
                // Typecheck the resulting expression with the procedure
                // defined in the environment.
                ret.type_check(&new_env)
            }

            Self::LetProcs(procs, ret) => {
                // Create a new environment with the procedures defined.
                let mut new_env = env.clone();
                for (name, proc) in procs {
                    // Define the procedure in the environment.
                    new_env.define_proc(name, proc.clone());
                }
                // Typecheck the procedures we're defining.
                for (_, proc) in procs {
                    // Typecheck the procedure in the new environment.
                    proc.type_check(&new_env)?;
                }
                // Typecheck the resulting expression with the procedures
                // defined in the environment.
                ret.type_check(&new_env)
            }

            // Typecheck a declaration of a type.
            Self::LetType(name, t, ret) => {
                // Create a new environment with the type defined.
                let mut new_env = env.clone();
                new_env.define_type(name.clone(), t.clone());
                // Typecheck the type we're defining.
                t.type_check(&new_env)?;
                // Typecheck the resulting expression with the type
                // defined in the environment.
                ret.type_check(&new_env)
            }

            // Typecheck a declaration of multiple types.
            Self::LetTypes(types, ret) => {
                // Create a new environment with the types defined.
                let mut new_env = env.clone();
                for (name, ty) in types {
                    // Define the type in the environment.
                    new_env.define_type(name, ty.clone());
                }
                // Typecheck the types we're defining.
                for (_, t) in types {
                    // Typecheck the type in the new environment.
                    t.type_check(&new_env)?;
                }
                // Typecheck the resulting expression with the types
                // defined in the environment.
                ret.type_check(&new_env)
            }

            // Typecheck a declaration of a variable.
            Self::LetVar(var, t, e, ret) => {
                // Typecheck the expression we're assigning to the variable.
                e.type_check(env)?;
                // Get the inferred type of the expression.
                let inferred_t = e.get_type(env)?;
                // If there's a type specification for the variable, check it.
                if let Some(t) = t {
                    // Typecheck the type.
                    t.type_check(env)?;

                    // Check that the inferred type is compatible with the type specified.
                    if !inferred_t.equals(t, env)? {
                        return Err(Error::MismatchedTypes {
                            expected: t.clone(),
                            found: inferred_t,
                            expr: self.clone(),
                        });
                    }
                }

                // Create a new environment with the variable defined.
                let mut new_env = env.clone();
                new_env.define_var(var, t.clone().unwrap_or(inferred_t))?;
                // Typecheck the resulting expression with the variable
                // defined in the environment.
                ret.type_check(&new_env)
            }

            // Typecheck a declaration of multiple variables.
            Self::LetVars(vars, ret) => {
                let mut new_env = env.clone();
                for (var, t, e) in vars {
                    // Typecheck the expression we're assigning to the variable.
                    e.type_check(&new_env)?;
                    // Get the inferred type of the expression.
                    let inferred_t = e.get_type(&new_env)?;
                    // If there's a type specification for the variable, check it.
                    if let Some(t) = t {
                        // Typecheck the type.
                        t.type_check(env)?;

                        // Check that the inferred type is compatible with the type specified.
                        if !inferred_t.equals(t, env)? {
                            return Err(Error::MismatchedTypes {
                                expected: t.clone(),
                                found: inferred_t,
                                expr: self.clone(),
                            });
                        }
                    }
                    // Define the variable in the environment.
                    new_env.define_var(var, t.clone().unwrap_or(inferred_t))?;
                }
                // Typecheck the resulting expression with the variables
                // defined in the environment.
                ret.type_check(&new_env)
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
                if !t_type.equals(&e_type, env)? {
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
                // Typecheck the then and else branches.
                t.type_check(env)?;
                e.type_check(env)
                // Since `when` expressions are computed at compile time,
                // we don't have to care about matching the types of the then and else branches.
            }

            // Typecheck a reference to a value.
            Self::Refer(e) => match *e.clone() {
                Expr::AnnotatedWithSource { expr, loc } => {
                    Self::Refer(expr).type_check(env).map_err(|e| e.with_loc(&loc))
                }
                Expr::ConstExpr(ConstExpr::Symbol(_))
                | Expr::Deref(_)
                | Expr::Index(_, _) => e.type_check(env),
                Expr::Member(inner, _) => {
                    // Confirm that the inner expression can be referenced.
                    inner.refer().type_check(env)?;
                    // If so, then make sure the expression being accessed is also sound.
                    e.type_check(env)
                },
                other => Err(Error::InvalidRefer(other.clone())),
            },
            // Typecheck a dereference of a pointer.
            Self::Deref(e) => {
                // Typecheck the expression which evaluates
                // to the address we will dereference.
                e.type_check(env)?;
                // Get the type of the expression.
                let t = e.get_type(env)?;
                // Check that the type is a pointer.
                if let Type::Pointer(_) = t {
                    // If it is, return success.
                    Ok(())
                } else {
                    // If it isn't, return an error.
                    Err(Error::MismatchedTypes {
                        // The expected type is a pointer.
                        expected: Type::Pointer(Box::new(Type::Any)),
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
                if let Type::Pointer(t) = ptr_type {
                    // Check that the type of the value is compatible
                    // with the type of data stored at the pointer's
                    // address.
                    if t.equals(&val_type, env)? {
                        // If it is, return success.
                        Ok(())
                    } else {
                        // If it isn't, return an error.
                        Err(Error::MismatchedTypes {
                            expected: val_type,
                            found: *t,
                            expr: self.clone(),
                        })
                    }
                } else {
                    // If the destination to store isn't a pointer, return an error.
                    Err(Error::MismatchedTypes {
                        expected: Type::Pointer(Box::new(Type::Any)),
                        found: ptr_type,
                        expr: self.clone(),
                    })
                }
            }

            // Typecheck a function application.
            Self::Apply(f, args) => {
                // Typecheck the expression we want to call as a procedure.
                f.type_check(env)?;
                // Typecheck the supplied arguments.
                for arg in args {
                    arg.type_check(env)?;
                }
                // Get the type of the function.
                let mut f_type = f.get_type(env)?;
                f_type =
                    f_type.simplify_until_matches(env, Type::Any, |t, env| Ok(t.is_simple()))?;
                // Infer the types of the supplied arguments.
                let mut args_inferred = vec![];
                for arg in args {
                    args_inferred.push(arg.get_type(env)?);
                }
                match f_type {
                    Type::Proc(args_t, ret_t) => {
                        // If the number of arguments is incorrect, then return an error.
                        if args_t.len() != args_inferred.len() {
                            return Err(Error::MismatchedTypes {
                                expected: Type::Proc(args_t, ret_t.clone()),
                                found: Type::Proc(args_inferred, ret_t),
                                expr: self.clone(),
                            });
                        }
                        // If the function is a procedure, confirm that the type of each
                        // argument matches the the type of the supplied value.
                        for (arg_t, arg) in args_t.into_iter().zip(args_inferred.into_iter()) {
                            // If the types don't match, return an error.
                            if !arg_t.equals(&arg, env)? {
                                return Err(Error::MismatchedTypes {
                                    expected: arg_t,
                                    found: arg,
                                    expr: self.clone(),
                                });
                            }
                        }
                        Ok(())
                    }
                    // If the function is not a procedure, return an error.
                    _ => Err(Error::MismatchedTypes {
                        expected: Type::Proc(args_inferred, Box::new(Type::Any)),
                        found: f_type,
                        expr: self.clone(),
                    }),
                }
            }

            // Typecheck a return statement.
            Self::Return(e) => {
                e.type_check(env)?;
                let ty = e.get_type(env)?;
                let expected_ty = env
                    .get_expected_return_type()
                    .cloned()
                    .unwrap_or(Type::None);
                if !expected_ty.equals(&ty, env)? {
                    return Err(Error::MismatchedTypes {
                        expected: expected_ty,
                        found: ty,
                        expr: self.clone(),
                    });
                }
                Ok(())
            }

            // Typecheck an array or tuple literal.
            Self::Array(items) => {
                let mut last_type: Option<Type> = None;
                // Typecheck each item in the array.
                for item in items {
                    // Typecheck the item.
                    item.type_check(env)?;
                    // Get the type of the item.
                    let item_type = item.get_type(env)?;
                    // If the type of the item is different from the last item,
                    if let Some(last_type) = last_type {
                        // Confirm that the type of the item is the same as the
                        // last item.
                        if !last_type.equals(&item_type, env)? {
                            // If it isn't, return an error.
                            return Err(Error::MismatchedTypes {
                                expected: last_type,
                                found: item_type,
                                expr: self.clone(),
                            });
                        }
                    }
                    last_type = Some(item_type);
                }
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
                let mut t = t.clone();
                t = t.simplify_until_matches(env, Type::Union(BTreeMap::new()), |t, env| {
                    Ok(matches!(t, Type::Union(_)))
                })?;
                match t {
                    Type::Union(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(ty) = fields.get(field) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !ty.equals(&found, env)? {
                                return Err(Error::MismatchedTypes {
                                    expected: ty.clone(),
                                    found,
                                    expr: self.clone(),
                                });
                            }
                            return Ok(());
                        } else {
                            return Err(Error::MemberNotFound(
                                self.clone(),
                                ConstExpr::Symbol(field.clone()),
                            ));
                        }
                    }
                    _ => {
                        return Err(Error::MemberNotFound(
                            self.clone(),
                            ConstExpr::Symbol(field.clone()),
                        ))
                    }
                }
                // for _ in 0..Type::SIMPLIFY_RECURSION_LIMIT {
                //     t = t.clone().simplify(env)?;
                //     match t {
                //         Type::Union(fields) => {
                //             // Confirm that the variant is a valid variant.
                //             if let Some(ty) = fields.get(field) {
                //                 // Typecheck the value assigned to the variant.
                //                 val.type_check(env)?;
                //                 let found = val.get_type(env)?;
                //                 if !ty.equals(&found, env)? {
                //                     return Err(Error::MismatchedTypes {
                //                         expected: ty.clone(),
                //                         found,
                //                         expr: self.clone(),
                //                     });
                //                 }
                //                 return Ok(());
                //             } else {
                //                 return Err(Error::MemberNotFound(
                //                     self.clone(),
                //                     ConstExpr::Symbol(field.clone()),
                //                 ));
                //             }
                //         }
                //         Type::Symbol(_) | Type::Let(_, _, _) | Type::Apply(_, _) => continue,
                //         _ => {
                //             return Err(Error::MemberNotFound(
                //                 self.clone(),
                //                 ConstExpr::Symbol(field.clone()),
                //             ))
                //         }
                //     }
                // }
                // Err(Error::MemberNotFound(
                //     self.clone(),
                //     ConstExpr::Symbol(field.clone()),
                // ))
            }

            // Typecheck a tagged union literal.
            Self::EnumUnion(t, variant, val) => {
                // Typecheck the type
                t.type_check(env)?;
                let mut t = t.clone();
                t = t.simplify_until_matches(env, Type::EnumUnion(BTreeMap::new()), |t, env| {
                    Ok(matches!(t, Type::EnumUnion(_)))
                })?;

                match t {
                    Type::EnumUnion(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(ty) = fields.get(variant) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !ty.equals(&found, env)? {
                                return Err(Error::MismatchedTypes {
                                    expected: ty.clone(),
                                    found,
                                    expr: self.clone(),
                                });
                            }
                            return Ok(());
                        } else {
                            return Err(Error::VariantNotFound(
                                Type::EnumUnion(fields),
                                variant.clone(),
                            ));
                        }
                    }
                    t => return Err(Error::VariantNotFound(t, variant.clone())),
                }

                // for _ in 0..Type::SIMPLIFY_RECURSION_LIMIT {
                //     t = t.clone().simplify(env)?;
                //     match t {
                //         Type::EnumUnion(fields) => {
                //             // Confirm that the variant is a valid variant.
                //             if let Some(ty) = fields.get(variant) {
                //                 // Typecheck the value assigned to the variant.
                //                 val.type_check(env)?;
                //                 let found = val.get_type(env)?;
                //                 if !ty.equals(&found, env)? {
                //                     return Err(Error::MismatchedTypes {
                //                         expected: ty.clone(),
                //                         found,
                //                         expr: self.clone(),
                //                     });
                //                 }
                //                 return Ok(());
                //             } else {
                //                 return Err(Error::VariantNotFound(
                //                     Type::EnumUnion(fields),
                //                     variant.clone(),
                //                 ));
                //             }
                //         }
                //         Type::Symbol(_) | Type::Let(_, _, _) | Type::Apply(_, _) => continue,
                //         _ => return Err(Error::VariantNotFound(t.clone(), variant.clone())),
                //     }
                // }
                // Err(Error::VariantNotFound(t.clone(), variant.clone()))
            }

            // Typecheck a type-cast.
            Self::As(e, t) => {
                // Typecheck the expression we want to cast.
                e.type_check(env)?;
                // Get the actual type of the expression.
                let original_t = e.get_type(env)?;

                // Check that the cast is valid.
                if original_t.can_cast_to(t, env)? {
                    // If it is, return success.
                    Ok(())
                } else {
                    // Otherwise, it isn't a valid cast, so return an error.
                    Err(Error::InvalidAs(self.clone(), original_t, t.clone()))
                }
            }

            // Typecheck a member access.
            Self::Member(e, field) => {
                // Typecheck the expression we want to access a member of.
                e.type_check(env)?;
                // Get the type of the expression.
                let e_type = e.get_type(env)?;
                // Typecheck the member we want to access.
                e_type.type_check_member(field, e, env)
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
                    Type::Array(_, _) | Type::Pointer(_) => {}
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
        match self {
            Self::AnnotatedWithSource { expr, loc } => {
                expr.type_check(env).map_err(|e| e.with_loc(loc))
            }

            // These are all guaranteed to be valid, or
            // to fail at compile time.
            Self::None
            | Self::Null
            | Self::Cell(_)
            | Self::Int(_)
            | Self::Float(_)
            | Self::Char(_)
            | Self::Bool(_)
            | Self::SizeOfType(_) => Ok(()),

            Self::LetTypes(bindings, expr) => {
                let mut new_env = env.clone();
                for (name, ty) in bindings {
                    new_env.define_type(name.clone(), ty.clone());
                }
                for (_, ty) in bindings {
                    ty.type_check(&mut new_env)?;
                }
                expr.type_check(&mut new_env)
            }
            Self::Monomorphize(expr, ty_args) => {
                self.get_type(env)?.type_check(env)?;
                if let Self::PolyProc(poly) = *expr.clone() {
                    poly.type_check(env)?
                }
                for ty in ty_args {
                    ty.type_check(env)?;
                }
                Ok(())
            }

            Self::TypeOf(expr) => expr.type_check(env),

            // Typecheck a constant type-cast.
            Self::As(expr, cast_ty) => {
                // Calculate the inferred type of the expression.
                let found = expr.get_type(env)?;
                // Confirm that the cast is valid.
                if !found.can_cast_to(&cast_ty, env)? {
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
            // Typecheck a procedure.
            Self::Proc(proc) => proc.type_check(env),
            Self::PolyProc(proc) => proc.type_check(env),

            // Typecheck a symbol.
            Self::Symbol(name) => {
                // If there is some binding for the symbol, return success.
                if env.get_const(name).is_some()
                    || env.get_proc(name).is_some()
                    || env.get_var(name).is_some()
                {
                    // Return success.
                    Ok(())
                } else {
                    // If there is no binding for the symbol, return an error.
                    Err(Error::SymbolNotDefined(name.clone()))
                }
            }

            // Typecheck a variant of an enum.
            Self::Of(t, variant) => {
                let mut t = t.clone();

                t =
                    t.simplify_until_matches(env, Type::Enum(vec![variant.clone()]), |t, env| {
                        Ok(matches!(t, Type::Enum(_) | Type::EnumUnion(_)))
                    })?;

                match t {
                    Type::Enum(variants) => {
                        // If the enum contains the variant, return success.
                        if variants.contains(variant) {
                            // Return success.
                            return Ok(());
                        } else {
                            // Otherwise, the variant isn't contained in the enum,
                            // so return an error.
                            return Err(Error::VariantNotFound(
                                Type::Enum(variants),
                                variant.clone(),
                            ));
                        }
                    }
                    Type::EnumUnion(variants) if variants.get(variant) == Some(&Type::None) => {
                        // If the enum union contains the variant, and the variant is empty, return success.
                        if variants.contains_key(variant)
                            && variants.get(variant) == Some(&Type::None)
                        {
                            // Return success.
                            return Ok(());
                        } else {
                            // Otherwise, the variant isn't contained in the enum,
                            // so return an error.
                            return Err(Error::VariantNotFound(
                                Type::EnumUnion(variants),
                                variant.clone(),
                            ));
                        }
                    }
                    _ => return Err(Error::VariantNotFound(t.clone(), variant.clone())),
                }
                // // Check that the variant is contained in the enum.
                // // Only check 50 levels deep to keep recursion under control.
                // for _ in 0..Type::SIMPLIFY_RECURSION_LIMIT {
                //     // Simplify the type.
                //     t = t.simplify(env)?;
                //     // Check if the type is an enum or an enum union.
                //     match t.clone() {
                //         Type::Enum(variants) => {
                //             // If the enum contains the variant, return success.
                //             if variants.contains(variant) {
                //                 // Return success.
                //                 return Ok(());
                //             } else {
                //                 // Otherwise, the variant isn't contained in the enum,
                //                 // so return an error.
                //                 return Err(Error::VariantNotFound(t.clone(), variant.clone()));
                //             }
                //         }
                //         Type::EnumUnion(variants) if variants.get(variant) == Some(&Type::None) => {
                //             // If the enum union contains the variant, and the variant is empty, return success.
                //             if variants.contains_key(variant)
                //                 && variants.get(variant) == Some(&Type::None)
                //             {
                //                 // Return success.
                //                 return Ok(());
                //             } else {
                //                 // Otherwise, the variant isn't contained in the enum,
                //                 // so return an error.
                //                 return Err(Error::VariantNotFound(t.clone(), variant.clone()));
                //             }
                //         }
                //         // If the type is a let binding or a symbol, simplify it again.
                //         Type::Let(_, _, _) | Type::Symbol(_) | Type::Apply(_, _) => continue,
                //         // If the type isn't an enum, return an error.
                //         _ => return Err(Error::VariantNotFound(t.clone(), variant.clone())),
                //     }
                // }
                // If we've recursed too deep, return an error.
                // return Err(Error::VariantNotFound(t.clone(), variant.clone()));
            }

            // Typecheck a tuple literal.
            Self::Tuple(items) => {
                // Typecheck each item in the tuple.
                for item in items {
                    // Typecheck the item.
                    item.type_check(env)?;
                }
                // Return success.
                Ok(())
            }

            // Typecheck an array literal.
            Self::Array(items) => {
                let mut last_type: Option<Type> = None;
                // Typecheck each item in the array.
                for item in items {
                    // Typecheck the item.
                    item.type_check(env)?;
                    // Get the type of the item.
                    let item_type = item.get_type(env)?;
                    // If the type of the item is different from the last item,
                    if let Some(last_type) = last_type {
                        // Confirm that the type of the item is the same as the
                        // last item.
                        if !last_type.equals(&item_type, env)? {
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
                // Return success.
                Ok(())
            }

            // Typecheck a struct literal.
            Self::Struct(fields) => {
                // Typecheck each field in the struct.
                for item in fields.values() {
                    // Typecheck the item.
                    item.type_check(env)?;
                }
                // Return success.
                Ok(())
            }

            // Typecheck a union literal.
            Self::Union(t, field, val) => {
                // Confirm the type supplied is a union.
                let mut t = t.clone();

                t = t.simplify_until_matches(env, Type::Union(BTreeMap::new()), |t, env| {
                    Ok(matches!(t, Type::Union(_)))
                })?;

                match t {
                    Type::Union(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(ty) = fields.get(field) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !ty.equals(&found, env)? {
                                return Err(Error::MismatchedTypes {
                                    expected: ty.clone(),
                                    found,
                                    expr: Expr::ConstExpr(self.clone()),
                                });
                            }
                            return Ok(());
                        } else {
                            return Err(Error::MemberNotFound(
                                Expr::ConstExpr(self.clone()),
                                ConstExpr::Symbol(field.clone()),
                            ));
                        }
                    }
                    _ => {
                        return Err(Error::MemberNotFound(
                            Expr::ConstExpr(self.clone()),
                            ConstExpr::Symbol(field.clone()),
                        ))
                    }
                }
            }

            // Typecheck a tagged union literal.
            Self::EnumUnion(t, variant, val) => {
                // Confirm the type supplied is a union.
                let mut t = t.clone();
                t =
                    t.simplify_until_matches(env, Type::Enum(vec![variant.clone()]), |t, env| {
                        Ok(matches!(t, Type::EnumUnion(_) | Type::Enum(_)))
                    })?;
                match t {
                    Type::EnumUnion(fields) => {
                        // Confirm that the variant is a valid variant.
                        if let Some(ty) = fields.get(variant) {
                            // Typecheck the value assigned to the variant.
                            val.type_check(env)?;
                            let found = val.get_type(env)?;
                            if !ty.equals(&found, env)? {
                                return Err(Error::MismatchedTypes {
                                    expected: ty.clone(),
                                    found,
                                    expr: Expr::ConstExpr(self.clone()),
                                });
                            }
                            return Ok(());
                        } else {
                            return Err(Error::VariantNotFound(
                                Type::EnumUnion(fields),
                                variant.clone(),
                            ));
                        }
                    }
                    _ => return Err(Error::VariantNotFound(t.clone(), variant.clone())),
                }
            }
        }
    }
}
