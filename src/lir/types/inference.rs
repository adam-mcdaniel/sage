//! # Type Inference
//!
//! This module contains the type inference algorithm for the language.
//!
//! Type inference is done with one trait: `GetType`. This trait allows
//! the type of any expression, constant expression, or other type to be
//! inferred under a given environment.
//!
//! The algorithm should ***always*** halt, and any infinite recursion
//! or stack overflow is a [bug that should be reported](https://github.com/adam-mcdaniel/sage/issues/new).

use super::*;

use log::trace;
use rayon::prelude::*;

/// Get the type associated with a value under a given environment.
pub trait GetType {
    /// Get the type associated with a value under a given environment.
    fn get_type(&self, env: &Env) -> Result<Type, Error> {
        self.get_type_checked(env, 0)?.simplify(env)
    }

    /// Get the type of a value under a given environment and check
    /// recursion depth to prevent a possible stack overflow.
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error>;

    /// Substitute a type for a given name in the environment.
    fn substitute(&mut self, name: &str, ty: &Type);

    fn substitute_types(&mut self, names: &[String], types: &[Type]) {
        for (name, ty) in names.iter().zip(types.iter()) {
            self.substitute(name, ty)
        }
    }
}

/// Infer the type associated with an expression under a given environment.
impl GetType for Expr {
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error> {
        trace!("Getting type of expression {self} in env {env}");
        let i = i + 1;
        Ok(match self {
            Self::Annotated(expr, annotation) => {
                // Get the type of the inner expression.
                expr.get_type_checked(env, i)
                    .map_err(|e| e.annotate(annotation.clone()))?
            }

            Self::Declare(declaration, body) => {
                if let Ok(ty) = body.get_type_checked(env, i) {
                    // If the body returns a value, then the declaration
                    // returns the type of the body.
                    return Ok(ty);
                }

                // Create a new environment with the declarations.
                let mut new_env = env.clone();
                // Add the declarations to the environment.
                new_env.add_declaration(declaration)?;
                // Get the type of the body in the new environment.
                body.get_type_checked(&new_env, i)?
            }

            Self::Match(expr, branches) => {
                for (pat, branch) in branches {
                    let ty = pat.get_branch_result_type(expr, branch, env)?;
                    if ty != Type::Never {
                        return Ok(ty);
                    }
                }
                // If the match expression is empty, then it returns the None value.
                if branches.is_empty() {
                    Type::None
                } else {
                    // All the branches never return a value, so the match expression
                    // returns the Never type.
                    Type::Never
                }
            }

            Self::IfLet(_pat, _expr, _a, b) => {
                // We could get the type of the then branch,
                // but the else branch should always be the same type.
                // (and if it isn't, the type checker will catch it)
                // return pat.get_branch_result_type(&expr, a, env)
                b.get_type_checked(env, i)?
            }

            Self::UnaryOp(unop, expr) => {
                let unop = env
                    .get_unop(unop)
                    .ok_or(Error::UnimplementedOperator(unop.clone()))?;

                if let Self::Annotated(expr, metadata) = &**expr {
                    return unop
                        .return_type(expr, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                // Infer the type of the unary operation
                // on the expression.
                unop.return_type(expr, env)?
            }
            Self::BinaryOp(binop, lhs, rhs) => {
                let binop = env
                    .get_binop(binop)
                    .ok_or(Error::UnimplementedOperator(binop.clone()))?;
                // Infer the type of the binary operation
                // on the two expressions.
                if let Self::Annotated(lhs, metadata) = &**lhs {
                    return binop
                        .return_type(lhs, rhs, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(rhs, metadata) = &**rhs {
                    return binop
                        .return_type(lhs, rhs, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }

                binop.return_type(lhs, rhs, env)?
            }
            Self::TernaryOp(ternop, a, b, c) => {
                let ternop = env
                    .get_ternop(ternop)
                    .ok_or(Error::UnimplementedOperator(ternop.clone()))?;

                if let Self::Annotated(a, metadata) = &**a {
                    return ternop
                        .return_type(a, b, c, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(b, metadata) = &**b {
                    return ternop
                        .return_type(a, b, c, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                if let Self::Annotated(c, metadata) = &**c {
                    return ternop
                        .return_type(a, b, c, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                // Infer the type of the ternary operation
                // on the three expressions.
                ternop.return_type(a, b, c, env)?
            }
            Self::AssignOp(op, dst, src) => {
                let op = env
                    .get_assignop(op)
                    .ok_or(Error::UnimplementedOperator(op.clone()))?;

                if let Self::Annotated(dst, metadata) = &**dst {
                    return op
                        .return_type(dst, src, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }

                if let Self::Annotated(src, metadata) = &**src {
                    return op
                        .return_type(dst, src, env)
                        .map_err(|e| e.annotate(metadata.clone()));
                }
                // Infer the type of the assignment operation
                // on the two expressions.
                op.return_type(dst, src, env)?
            }

            // Get the type of the inner constant expression.
            Self::ConstExpr(c) => c.get_type_checked(env, i)?,
            // Get the type of the result of the block of expressions.
            Self::Many(exprs) => {
                // Get the type of the last expression in the block.
                if let Some(expr) = exprs.last() {
                    // If the last expression returns a value,
                    // then the block returns that value.
                    expr.get_type_checked(env, i)?
                } else {
                    // If the block is empty, then it returns
                    // the None value.
                    Type::None
                }
            }

            // The resulting type of a type cast is the type being cast to.
            Self::As(_, t) => t.clone(),

            // A while loop returns the None value.
            Self::While(cond, _) => {
                let mut cond = *cond.clone();
                while let Expr::Annotated(expr, _) = cond {
                    cond = *expr;
                }

                match cond {
                    Self::ConstExpr(ConstExpr::Bool(true)) => Type::Never,
                    Self::ConstExpr(ref c) => {
                        if let Ok(true) = c.clone().simplify(env)?.as_bool(env) {
                            Type::Never
                        } else {
                            Type::None
                        }
                    }
                    _ => Type::None,
                }
            }

            // An if statement returns the type of the expression
            // that is evaluated if the condition is true (which must
            // be type-equal with the else branch).
            Self::If(_, t, e) => {
                let ty = t.get_type_checked(env, i)?;
                if ty == Type::Never {
                    e.get_type_checked(env, i)?
                } else {
                    ty
                }
            }
            // When statements return either the type of the expression
            // that is evaluated if the condition is true or the else branch.
            Self::When(c, t, e) => {
                // Check the condition, and return the type of the branch
                // that is evaluated.
                if c.clone().as_bool(env)? { t } else { e }.get_type_checked(env, i)?
            }

            // Return the type of a reference to the expression.
            Self::Refer(mutability, expr) => {
                Type::Pointer(*mutability, Box::new(expr.get_type_checked(env, i)?))
            }
            // Return the type of the expression being dereferenced.
            Self::Deref(expr) => {
                // Get the type of the expression.
                let t = expr
                    .get_type_checked(env, i)?
                    .simplify_until_concrete(env)?;
                // If the type is a pointer, return the inner type of the pointer.
                if let Type::Pointer(_, inner) = t {
                    // If the type *evaluates* to a pointer, return that inner type.
                    *inner
                } else {
                    // Otherwise, we cannot prove that we can dereference this type.
                    return Err(Error::DerefNonPointer(self.clone()));
                }
            }
            // The result of an assignment to a pointer is None.
            Self::DerefMut(_, _) => Type::None,

            // The type of a return is "Never" - it never allows
            // the following instructions to be executed.
            Self::Return(_) => Type::Never,
            // Get the type of a procedure call.
            Self::Apply(func, _) => {
                // Get the type of the function.
                let ty = func
                    .get_type_checked(env, i)?
                    .simplify_until_concrete(env)?;
                match ty {
                    Type::Proc(_, ret) => *ret,
                    _ => return Err(Error::ApplyNonProc(self.clone())),
                }
            }

            // Get the type of a tuple literal.
            Self::Tuple(items) => Type::Tuple(
                // Get the type of each item in the tuple.
                items
                    .clone()
                    // Make an iterator over the items.
                    .into_par_iter()
                    // Get the type of each item.
                    .map(|c| c.get_type_checked(env, i))
                    // Collect the results into a vector.
                    .collect::<Result<Vec<Type>, Error>>()?,
            ),
            // Get the type of an array literal.
            Self::Array(items) => Type::Array(
                // Get the type of the first item in the array.
                Box::new(if !items.is_empty() {
                    // If the array is not empty, get the type of the first item.
                    items[0].get_type_checked(env, i)?
                } else {
                    // Otherwise, allow any type.
                    Type::Any
                }),
                // Get the length of the array.
                Box::new(ConstExpr::Int(items.len() as i64)),
            ),
            // Get the type of a struct literal.
            Self::Struct(fields) => Type::Struct(
                // Get the type of each field in the struct.
                fields
                    .clone()
                    // Make an iterator over the fields.
                    .into_par_iter()
                    // Get the type of each field.
                    .map(|(k, c)| Ok((k, c.get_type_checked(env, i)?)))
                    // Collect the results into a map.
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            // Get the type of a union literal.
            Self::Union(t, _, _) => t.clone(),

            // Get the type of a tagged union literal.
            Self::EnumUnion(t, _, _) => t.clone(),

            // Get the type of a member access.
            Self::Member(val, field) => {
                // Get the field to access (as a symbol)
                let as_symbol = field.clone().as_symbol(env);
                // Get the field to access (as an integer)
                let as_int = field.clone().as_int(env);
                // Get the type of the value to get the member of.
                let val_type = val.get_type_checked(env, i)?;
                // val_type.add_monomorphized_associated_consts(env)?;
                let val_type = val_type.simplify_until_concrete(env)?;
                // val_type.add_monomorphized_associated_consts(env)?;
                match val_type {
                    Type::Type(ty) => {
                        // ty.add_monomorphized_associated_consts(env)?;

                        // Get the associated constant expression's type.
                        env.get_type_of_associated_const(&ty, &as_symbol?)
                            .ok_or(Error::MemberNotFound(*val.clone(), field.clone()))?
                    }
                    Type::Unit(_unit_name, inner_ty) => {
                        // Get the associated constant expression's type.
                        env.get_type_of_associated_const(&inner_ty, &as_symbol?)
                            .ok_or(Error::MemberNotFound(*val.clone(), field.clone()))?
                    }
                    Type::Pointer(found_mutability, t) => {
                        match t.get_member_offset(field, val, env) {
                            Ok((t, _)) => t,
                            Err(_) => {
                                return env
                                    .get_type_of_associated_const(
                                        &Type::Pointer(found_mutability, t),
                                        &as_symbol?,
                                    )
                                    .ok_or(Error::MemberNotFound(*val.clone(), field.clone()));
                            }
                        }
                    }
                    // If we're accessing a member of a tuple,
                    // we use the `as_int` interpretation of the field.
                    // This is because tuples are accesed by integer index.
                    Type::Tuple(items) => {
                        if as_symbol.is_ok() {
                            return env
                                .get_type_of_associated_const(&Type::Tuple(items), &as_symbol?)
                                .ok_or(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                        // Get the index of the field.
                        let n = as_int? as usize;
                        // If the index is in range, return the type of the field.
                        if n < items.len() {
                            // Return the type of the field.
                            items[n].clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    // If we're accessing a member of a struct,
                    // we use the `as_symbol` interpretation of the field.
                    // This is because struct members are accessed by name.
                    Type::Struct(fields) => {
                        // Get the type of the field.
                        let name = as_symbol?;
                        if let Some(t) = fields.get(&name) {
                            // Return the type of the field.
                            t.clone()
                        } else {
                            // If the field is not in the struct, return an error.
                            return env
                                .get_type_of_associated_const(&Type::Struct(fields), &name)
                                .ok_or(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    // If we're accessing a member of a union,
                    // we use the `as_symbol` interpretation of the field.
                    // This is because union members are accessed by name.
                    Type::Union(types) => {
                        // Get the type of the field.
                        let name = as_symbol?;
                        if let Some(t) = types.get(&name) {
                            // Return the type of the field.
                            t.clone()
                        } else {
                            // If the field is not in the union, return an error.
                            return env
                                .get_type_of_associated_const(&Type::Union(types), &name)
                                .ok_or(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }

                    // If we're accessing a member of a type that is not a tuple,
                    // struct, union, or pointer, we cannot access a member.
                    val_type => {
                        // error!("BING BONG");
                        // Try to get the member of the underlying type.
                        if let Ok((t, _)) = val_type.get_member_offset(field, val, env) {
                            return Ok(t);
                        }

                        // Try to get the member of the underlying type.
                        return env
                            .get_type_of_associated_const(&val_type, &as_symbol?)
                            .ok_or(Error::MemberNotFound(*val.clone(), field.clone()));
                    } // Err(e) => {
                      //     // Try to get the member of the underlying type.
                      //     // return Err(e);
                      //     return env.get_type_of_associated_const(&val_type, &as_symbol?)
                      //         .ok_or(Error::MemberNotFound(*val.clone(), field.clone()));
                      // }
                }
            }

            // Get the type of an index access.
            Self::Index(val, _) => match val.get_type_checked(env, i)?.simplify(env)? {
                // Only arrays and pointers can be indexed.
                Type::Array(item, _) => *item,
                Type::Pointer(_, item) => *item,

                // If we're accessing an index of a type that is not an array or pointer,
                // we cannot access an index.
                _ => return Err(Error::InvalidIndex(self.clone())),
            },
        })
    }

    /// Substitute a type in a given expression.
    fn substitute(&mut self, name: &str, ty: &Type) {
        trace!("Substituting {name} for {ty} in {self}");
        match self {
            Self::Annotated(expr, _) => {
                expr.substitute(name, ty);
            }

            Self::Declare(declaration, body) => {
                declaration.substitute(name, ty);
                body.substitute(name, ty);
            }

            Self::ConstExpr(cexpr) => cexpr.substitute(name, ty),

            Self::If(cond, then, els) => {
                cond.substitute(name, ty);
                then.substitute(name, ty);
                els.substitute(name, ty)
            }
            Self::While(cond, body) => {
                cond.substitute(name, ty);
                body.substitute(name, ty)
            }

            Self::Many(exprs) => {
                // for expr in exprs.iter_mut() {
                //     expr.substitute(name, ty);
                // }
                exprs
                    .par_iter_mut()
                    .for_each(|expr| expr.substitute(name, ty));
            }
            Self::When(cond, then_body, else_body) => {
                cond.substitute(name, ty);
                then_body.substitute(name, ty);
                else_body.substitute(name, ty)
            }

            Self::Match(expr, arms) => {
                expr.substitute(name, ty);
                // for (_, arm) in arms.iter_mut() {
                //     arm.substitute(name, ty);
                // }
                arms.par_iter_mut()
                    .for_each(|(_, arm)| arm.substitute(name, ty));
            }

            Self::IfLet(_pat, expr, then_body, else_body) => {
                expr.substitute(name, ty);
                then_body.substitute(name, ty);
                else_body.substitute(name, ty)
            }

            Self::UnaryOp(_op, expr) => expr.substitute(name, ty),

            Self::BinaryOp(_op, lhs, rhs) => {
                lhs.substitute(name, ty);
                rhs.substitute(name, ty)
            }

            Self::TernaryOp(_op, cond, then, els) => {
                cond.substitute(name, ty);
                then.substitute(name, ty);
                els.substitute(name, ty)
            }

            Self::AssignOp(_op, lhs, rhs) => {
                lhs.substitute(name, ty);
                rhs.substitute(name, ty)
            }

            Self::Refer(_, expr) => expr.substitute(name, ty),

            Self::Deref(expr) => expr.substitute(name, ty),

            Self::DerefMut(expr, val) => {
                expr.substitute(name, ty);
                val.substitute(name, ty)
            }

            Self::Apply(expr, args) => {
                expr.substitute(name, ty);
                // for arg in args.iter_mut() {
                //     arg.substitute(name, ty);
                // }
                args.par_iter_mut().for_each(|arg| arg.substitute(name, ty));
            }

            Self::Return(expr) => expr.substitute(name, ty),

            Self::Array(exprs) | Self::Tuple(exprs) => {
                // for expr in exprs.iter_mut() {
                //     expr.substitute(name, ty);
                // }
                exprs
                    .par_iter_mut()
                    .for_each(|expr| expr.substitute(name, ty));
            }

            Self::Union(t, _, expr) => {
                *t = t.substitute(name, ty);
                expr.substitute(name, ty)
            }

            Self::EnumUnion(t, _, expr) => {
                *t = t.substitute(name, ty);
                expr.substitute(name, ty)
            }

            Self::Struct(fields) => {
                // for (_, expr) in fields.iter_mut() {
                //     expr.substitute(name, ty);
                // }
                fields
                    .par_iter_mut()
                    .for_each(|(_, expr)| expr.substitute(name, ty));
            }

            Self::As(expr, t) => {
                expr.substitute(name, ty);
                *t = t.substitute(name, ty)
            }

            Self::Member(expr, cexpr) => {
                expr.substitute(name, ty);
                cexpr.substitute(name, ty)
            }

            Self::Index(expr, cexpr) => {
                expr.substitute(name, ty);
                cexpr.substitute(name, ty)
            }
        }
    }
}
