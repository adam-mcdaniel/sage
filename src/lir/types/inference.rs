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
        let i = i + 1;
        Ok(match self {
            Self::AnnotatedWithSource { expr, loc } => {
                // Get the type of the inner expression.
                expr.get_type_checked(env, i).map_err(|e| e.with_loc(loc))?
            }

            Self::Match(expr, branches) => {
                for (pat, branch) in branches {
                    return pat.get_branch_result_type(&expr, branch, env);
                }
                Type::None
            }

            Self::IfLet(_pat, _expr, _a, b) => {
                // We could get the type of the then branch,
                // but the else branch should always be the same type.
                // (and if it isn't, the type checker will catch it)
                // return pat.get_branch_result_type(&expr, a, env)
                b.get_type_checked(env, i)?
            }

            Self::UnaryOp(unop, expr) => {
                // Infer the type of the unary operation
                // on the expression.
                unop.return_type(expr, env)?
            }
            Self::BinaryOp(binop, lhs, rhs) => {
                // Infer the type of the binary operation
                // on the two expressions.
                binop.return_type(lhs, rhs, env)?
            }
            Self::TernaryOp(ternop, a, b, c) => {
                // Infer the type of the ternary operation
                // on the three expressions.
                ternop.return_type(a, b, c, env)?
            }
            Self::AssignOp(op, dst, src) => {
                // Infer the type of the assignment operation
                // on the two expressions.
                op.return_type(dst, src, env)?
            }

            // Get the type of the inner constant expression.
            Self::ConstExpr(c) => c.get_type_checked(env, i)?,
            // Get the type of the result of the block of expressions.
            Self::Many(exprs) => {
                for expr in exprs {
                    if expr.get_type_checked(env, i)? == Type::Never {
                        return Ok(Type::Never);
                    }
                }

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

            // Get the type of a resulting expression after a constant definition.
            Self::LetConst(name, expr, ret) => {
                // Create a new environment with the constant
                let mut new_env = env.clone();
                new_env.define_const(name, expr.clone());
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)? //.simplify(&new_env)?
            }

            // Get the type of a resulting expression after several constant definitions.
            Self::LetConsts(constants, ret) => {
                // Create a new environment with the constants
                let mut new_env = env.clone();
                for (name, c) in constants {
                    // Define the constant in the new environment.
                    new_env.define_const(name, c.clone());
                }
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)? //.simplify(&new_env)?
            }

            // Get the type of a resulting expression after a procedure definition.
            Self::LetProc(name, proc, ret) => {
                // Create a new environment with the procedure
                let mut new_env = env.clone();
                new_env.define_proc(name, proc.clone());
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)? //.simplify(&new_env)?
            }

            // Get the type of a resulting expression after several procedure definitions.
            Self::LetProcs(procs, ret) => {
                // Create a new environment with the procedures
                let mut new_env = env.clone();
                for (name, proc) in procs {
                    // Define the procedure in the new environment.
                    new_env.define_proc(name, proc.clone());
                }
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)? //.simplify(&new_env)?
            }

            // Get the type of a resulting expression after a type definition.
            Self::LetType(name, t, ret) => {
                // Create a new environment with the type
                let mut new_env = env.clone();
                new_env.define_type(name, t.clone());
                new_env.define_type(name, t.clone().simplify(&new_env)?);
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)?.simplify_until_matches(
                    env,
                    Type::Any,
                    |t, env| t.type_check(env).map(|()| true),
                )?
            }

            // Get the type of a resulting expression after several type definitions.
            Self::LetTypes(types, ret) => {
                // Create a new environment with the types
                let mut new_env = env.clone();
                for (name, ty) in types {
                    // Define the type in the new environment.
                    new_env.define_type(name, ty.clone());
                    new_env.define_type(name, ty.clone().simplify(&new_env)?);
                }
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)?.simplify_until_matches(
                    env,
                    Type::Any,
                    |t, env| t.type_check(env).map(|()| true),
                )?
            }

            // Get the type of a resulting expression after a variable definition.
            Self::LetVar(var, t, val, ret) => {
                // Create a new environment with the variable
                let mut new_env = env.clone();
                new_env.define_var(var, t.clone().unwrap_or(val.get_type_checked(env, i)?))?;
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)?
            }

            // Get the type of a resulting expression after several variable definitions.
            Self::LetVars(vars, ret) => {
                // Create a new environment with the variables
                let mut new_env = env.clone();
                for (var, t, val) in vars {
                    // Define the variable in the new environment.
                    new_env
                        .define_var(var, t.clone().unwrap_or(val.get_type_checked(&new_env, i)?))?;
                }
                // Get the type of the return expression in the new environment.
                ret.get_type_checked(&new_env, i)?
            }

            // A while loop returns the None value.
            Self::While(cond, _) => {
                let mut cond = *cond.clone();
                while let Expr::AnnotatedWithSource { expr, .. } = cond {
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
            },

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
            },
            // When statements return either the type of the expression
            // that is evaluated if the condition is true or the else branch.
            Self::When(c, t, e) => {
                // Check the condition, and return the type of the branch
                // that is evaluated.
                if c.clone().as_bool(env)? { t } else { e }.get_type_checked(env, i)?
            }

            // Return the type of a reference to the expression.
            Self::Refer(expr) => Type::Pointer(Box::new(expr.get_type_checked(env, i)?)),
            // Return the type of the expression being dereferenced.
            Self::Deref(expr) => {
                // Get the type of the expression.
                let t = expr.get_type_checked(env, i)?;
                // If the type is a pointer, return the inner type of the pointer.
                if let Type::Pointer(inner) = t {
                    // Return the inner type of the pointer.
                    *inner
                } else if let Type::Pointer(inner) = t.simplify(env)? {
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
                let mut ty = func.get_type_checked(env, i)?;
                ty = ty.simplify_until_matches(
                    env,
                    Type::Proc(vec![], Box::new(Type::Any)),
                    |t, env| Ok(t.is_simple()),
                )?;
                match ty {
                    Type::Proc(_, ret) => *ret,
                    Type::Let(name, t, result) => {
                        let mut new_env = env.clone();
                        new_env.define_type(name, *t.clone());
                        if let Type::Proc(_args, ret) = *result {
                            *ret
                        } else {
                            return Err(Error::ApplyNonProc(self.clone()));
                        }
                    }
                    // // Get the type of an polymorphic type.
                    // Type::Apply(poly, ty_args) => {
                    //     if let Type::Poly(_, ret) = poly {
                    //         // Get the type of the return type after applying the type arguments.
                    //         ret.apply_type_args(ty_args, env)?
                    //     } else {
                    //         // If the type is not a polymorphic type, we cannot apply it.
                    //         return Err(Error::ApplyNonProc(self.clone()));
                    //     }
                    // }
                    _ => return Err(Error::ApplyNonProc(self.clone())),
                }
                // if let Type::Proc(_, ret) = func.get_type_checked(env, i)?.simplify(env)? {
                //     // Get the return type of the procedure.
                //     *ret
                // } else {
                //     // If the value is not a procedure, we cannot apply it.
                //     return Err(Error::ApplyNonProc(self.clone()));
                // }
            }

            // Get the type of a tuple literal.
            Self::Tuple(items) => Type::Tuple(
                // Get the type of each item in the tuple.
                items
                    .clone()
                    // Make an iterator over the items.
                    .into_iter()
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
                    .into_iter()
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
                match val.get_type_checked(env, i)?.simplify_until_matches(
                    env,
                    Type::Struct(BTreeMap::new()),
                    |t, env| {
                        Ok(matches!(
                            t,
                            Type::Tuple(_) | Type::Struct(_) | Type::Union(_)
                        ))
                    },
                ) {
                    // If we're accessing a member of a tuple,
                    // we use the `as_int` interpretation of the field.
                    // This is because tuples are accesed by integer index.
                    Ok(Type::Tuple(items)) => {
                        // Get the index of the field.
                        let n = as_int? as usize;
                        // If the index is in range, return the type of the field.
                        if n < items.len() {
                            // Return the type of the field.
                            items[n].clone()
                        } else {
                            // Otherwise, the field is out of range.
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    // If we're accessing a member of a struct,
                    // we use the `as_symbol` interpretation of the field.
                    // This is because struct members are accessed by name.
                    Ok(Type::Struct(fields)) => {
                        // Get the type of the field.
                        if let Some(t) = fields.get(&as_symbol?) {
                            // Return the type of the field.
                            t.clone()
                        } else {
                            // If the field is not in the struct, return an error.
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    // If we're accessing a member of a union,
                    // we use the `as_symbol` interpretation of the field.
                    // This is because union members are accessed by name.
                    Ok(Type::Union(types)) => {
                        // Get the type of the field.
                        if let Some(t) = types.get(&as_symbol?) {
                            // Return the type of the field.
                            t.clone()
                        } else {
                            // If the field is not in the union, return an error.
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    // // If we're accessing a member whose type is a more complex
                    // // let-binding, we need to look up the type of the member using
                    // // another environment.
                    // Type::Let(name, t, ret) => {
                    //     // Create a new environment with the type of the let-binding
                    //     let mut new_env = env.clone();
                    //     new_env.define_type(name, *t);
                    //     // Get the type of the member in the new environment.
                    //     ret.get_member_offset(field, val, &new_env)?.0
                    // }

                    // If we're accessing a member of a type that is not a tuple,
                    // struct, or union, we cannot access a member.
                    _ => return Err(Error::MemberNotFound(*val.clone(), field.clone())),
                }
            }

            // Get the type of an index access.
            Self::Index(val, _) => match val.get_type_checked(env, i)?.simplify(env)? {
                // Only arrays and pointers can be indexed.
                Type::Array(item, _) => *item,
                Type::Pointer(item) => *item,

                // If we're accessing an index of a type that is not an array or pointer,
                // we cannot access an index.
                _ => return Err(Error::InvalidIndex(self.clone())),
            },
        })
    }

    /// Substitute a type in a given expression.
    fn substitute(&mut self, name: &str, ty: &Type) {
        match self {
            Self::AnnotatedWithSource { expr, .. } => {
                expr.substitute(name, ty);
            }

            Self::ConstExpr(cexpr) => cexpr.substitute(name, ty),
            Self::LetConst(name, cexpr, expr) => {
                cexpr.substitute(name, ty);
                expr.substitute(name, ty)
            }
            Self::LetConsts(cexprs, expr) => {
                for cexpr in cexprs.values_mut() {
                    cexpr.substitute(name, ty);
                }
                expr.substitute(name, ty)
            }
            Self::LetProc(name, proc, expr) => {
                proc.substitute(name, ty);
                expr.substitute(name, ty)
            }
            Self::LetProcs(procs, expr) => {
                for (_, proc) in procs.iter_mut() {
                    proc.substitute(name, ty);
                }
                expr.substitute(name, ty)
            }
            Self::LetVar(var, t, val, ret) => {
                if let Some(t) = t {
                    *t = t.substitute(name, ty);
                }
                val.substitute(name, ty);
                ret.substitute(name, ty)
            }
            Self::LetVars(vars, ret) => {
                for (_, t, val) in vars.iter_mut() {
                    if let Some(t) = t {
                        *t = t.substitute(name, ty);
                    }
                    val.substitute(name, ty);
                }
                ret.substitute(name, ty)
            }

            Self::If(cond, then, els) => {
                cond.substitute(name, ty);
                then.substitute(name, ty);
                els.substitute(name, ty)
            }
            Self::While(cond, body) => {
                cond.substitute(name, ty);
                body.substitute(name, ty)
            }
            Self::LetType(type_name, t, expr) => {
                if type_name != name {
                    *t = t.substitute(name, ty);
                    expr.substitute(name, ty)
                }
            }
            Self::LetTypes(types, expr) => {
                if !types.iter().map(|(a, _)| a == name).any(|i| i) {
                    for (_, t) in types.iter_mut() {
                        *t = t.substitute(name, ty);
                    }
                    expr.substitute(name, ty)
                }
            }
            Self::Many(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.substitute(name, ty);
                }
            }
            Self::When(cond, then_body, else_body) => {
                cond.substitute(name, ty);
                then_body.substitute(name, ty);
                else_body.substitute(name, ty)
            }

            Self::Match(expr, arms) => {
                expr.substitute(name, ty);
                for (_, arm) in arms.iter_mut() {
                    arm.substitute(name, ty);
                }
            }

            Self::IfLet(pat, expr, then_body, else_body) => {
                expr.substitute(name, ty);
                then_body.substitute(name, ty);
                else_body.substitute(name, ty)
            }

            Self::UnaryOp(op, expr) => expr.substitute(name, ty),

            Self::BinaryOp(op, lhs, rhs) => {
                lhs.substitute(name, ty);
                rhs.substitute(name, ty)
            }

            Self::TernaryOp(op, cond, then, els) => {
                cond.substitute(name, ty);
                then.substitute(name, ty);
                els.substitute(name, ty)
            }

            Self::AssignOp(op, lhs, rhs) => {
                lhs.substitute(name, ty);
                rhs.substitute(name, ty)
            }

            Self::Refer(expr) => expr.substitute(name, ty),

            Self::Deref(expr) => expr.substitute(name, ty),

            Self::DerefMut(expr, val) => {
                expr.substitute(name, ty);
                val.substitute(name, ty)
            }

            Self::Apply(expr, args) => {
                expr.substitute(name, ty);
                for arg in args.iter_mut() {
                    arg.substitute(name, ty);
                }
            }

            Self::Return(expr) => expr.substitute(name, ty),

            Self::Array(exprs) | Self::Tuple(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.substitute(name, ty);
                }
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
                for (_, expr) in fields.iter_mut() {
                    expr.substitute(name, ty);
                }
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
