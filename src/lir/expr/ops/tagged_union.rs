use std::collections::BTreeMap;

use super::*;
use crate::asm::{CoreOp, SP};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// Get the Enum value of the tag associated with a tagged union (EnumUnion).
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Tag;

impl UnaryOp for Tag {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        let ty = ty.clone().simplify(env)?;
        Ok(ty
            .simplify_until_matches(env, Type::EnumUnion(BTreeMap::new()), |t, env| {
                Ok(matches!(t, Type::Enum(_) | Type::EnumUnion(_)))
            })
            .is_ok())
    }

    /// Get the type of the result of applying this unary operation to the given type.
    fn return_type(&self, expr: &Expr, env: &Env) -> Result<Type, Error> {
        let mut ty = expr.get_type(env)?;
        ty = ty.simplify_until_matches(env, Type::EnumUnion(BTreeMap::new()), |t, env| {
            Ok(matches!(t, Type::Enum(_) | Type::EnumUnion(_)))
        })?;
        // for _ in 0..Type::SIMPLIFY_RECURSION_LIMIT {
        //     match ty {
        //         Type::Let(_, _, _) | Type::Symbol(_) | Type::Apply(_, _) => {
        //             ty = ty.simplify(env)?
        //         }
        //         Type::EnumUnion(variants) => return Ok(Type::Enum(variants.into_keys().collect())),
        //         found => {
        //             return Err(Error::MismatchedTypes {
        //                 expected: Type::EnumUnion(BTreeMap::new()),
        //                 found,
        //                 expr: expr.clone(),
        //             })
        //         }
        //     }
        // }
        match ty {
            Type::EnumUnion(variants) => Ok(Type::Enum(variants.into_keys().collect())),
            found => Err(Error::MismatchedTypes {
                expected: Type::EnumUnion(BTreeMap::new()),
                found,
                expr: expr.clone(),
            }),
        }
    }

    /// Evaluate this unary operation on the given constant values.
    fn eval(&self, expr: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        let expr = expr.clone().eval(env)?;
        match expr.clone() {
            ConstExpr::EnumUnion(t, variant, _) => {
                if let Type::EnumUnion(variants) = t.clone().simplify(env)? {
                    Ok(ConstExpr::Of(
                        Type::Enum(variants.into_keys().collect()),
                        variant,
                    ))
                } else {
                    Err(Error::MismatchedTypes {
                        expected: Type::EnumUnion(BTreeMap::new()),
                        found: t.clone(),
                        expr: Expr::ConstExpr(expr.clone()),
                    })
                }
            }
            found => Err(Error::MismatchedTypes {
                expected: Type::EnumUnion(BTreeMap::new()),
                found: found.get_type(env)?,
                expr: Expr::ConstExpr(expr.clone()),
            }),
        }
    }

    /// Compile the unary operation.
    fn compile_types(
        &self,
        ty: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        // Get the size of the type.
        let size = ty.get_size(env)?;

        // Copy the tag to a temp register
        output.op(CoreOp::Move {
            src: SP.deref(),
            dst: SP.deref().offset(1 - size as isize),
        });
        output.op(CoreOp::Pop(None, size - 1));

        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for Tag {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "tag")
    }
}

impl Display for Tag {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "tag")
    }
}

/// Get the Union data associated with a tagged union (EnumUnion).
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Data;

impl UnaryOp for Data {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        let ty = ty.clone().simplify(env)?;
        Ok(ty
            .simplify_until_matches(env, Type::EnumUnion(BTreeMap::new()), |t, env| {
                Ok(matches!(t, Type::EnumUnion(_) | Type::Enum(_)))
            })
            .is_ok())
    }

    /// Get the type of the result of applying this unary operation to the given type.
    fn return_type(&self, expr: &Expr, env: &Env) -> Result<Type, Error> {
        let mut ty = expr.get_type(env)?;
        ty = ty.simplify_until_matches(env, Type::EnumUnion(BTreeMap::new()), |t, env| {
            Ok(matches!(t, Type::EnumUnion(_) | Type::Enum(_)))
        })?;
        // for _ in 0..Type::SIMPLIFY_RECURSION_LIMIT {
        //     match ty {
        //         Type::Let(_, _, _) | Type::Symbol(_) | Type::Apply(_, _) => {
        //             ty = ty.simplify(env)?
        //         }
        //         Type::EnumUnion(variants) => return Ok(Type::Union(variants)),
        //         found => {
        //             return Err(Error::MismatchedTypes {
        //                 expected: Type::EnumUnion(BTreeMap::new()),
        //                 found,
        //                 expr: expr.clone(),
        //             })
        //         }
        //     }
        // }
        match ty {
            Type::EnumUnion(variants) => Ok(Type::Union(variants)),
            found => Err(Error::MismatchedTypes {
                expected: Type::EnumUnion(BTreeMap::new()),
                found,
                expr: expr.clone(),
            }),
        }
    }

    /// Evaluate this unary operation on the given constant values.
    fn eval(&self, expr: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        let expr = expr.clone().eval(env)?;
        match expr.clone() {
            ConstExpr::EnumUnion(t, variant, val) => {
                if let Type::EnumUnion(variants) = t {
                    ConstExpr::Union(Type::Union(variants), variant, val).eval(env)
                } else {
                    Err(Error::MismatchedTypes {
                        expected: Type::EnumUnion(BTreeMap::new()),
                        found: t.clone(),
                        expr: Expr::ConstExpr(expr.clone()),
                    })
                }
            }
            found => Err(Error::MismatchedTypes {
                expected: Type::EnumUnion(BTreeMap::new()),
                found: found.get_type(env)?,
                expr: Expr::ConstExpr(expr.clone()),
            }),
        }
    }

    /// Compile the unary operation.
    fn compile_types(
        &self,
        _ty: &Type,
        _env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        // Remove the tag.
        output.op(CoreOp::Pop(None, 1));

        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for Data {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "data")
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "data")
    }
}
