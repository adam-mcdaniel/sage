//! # Bitwise Operations
use crate::{
    asm::{AssemblyProgram, CoreOp, StandardOp, A, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct BitwiseNot;

impl UnaryOp for BitwiseNot {
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        ty.equals(&Type::Int, env).or(ty.equals(&Type::Cell, env))
    }

    fn return_type(&self, x: &Expr, env: &Env) -> Result<Type, Error> {
        let ty = x.get_type(env)?;
        if ty.equals(&Type::Int, env).unwrap_or(false) {
            Ok(Type::Int)
        } else if ty.equals(&Type::Cell, env).unwrap_or(false) {
            Ok(Type::Cell)
        } else {
            Err(Error::MismatchedTypes {
                expected: Type::Cell,
                found: ty,
                expr: x.clone(),
            })
        }
    }

    fn eval(&self, x: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        let result = x.clone().eval(env)?;
        let ty = result.get_type(env)?;
        Ok(match result {
            ConstExpr::Int(i) => ConstExpr::Int(!i),
            ConstExpr::Cell(f) => ConstExpr::Cell(!f),
            _ => {
                return Err(Error::MismatchedTypes {
                    expected: Type::Cell,
                    found: ty,
                    expr: Expr::ConstExpr(x.clone()),
                })
            }
        })
    }

    // fn compile_types(&self, ty: &Type, env: &mut Env) -> Result<AssemblyProgram, Error> {}
    fn compile_types(
        &self,
        ty: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if ty.equals(&Type::Int, env)? || ty.equals(&Type::Cell, env)? {
            output.op(CoreOp::BitwiseNot(SP.deref()));
        } else {
            return Err(Error::InvalidUnaryOpTypes(self.clone_box(), ty.clone()));
        }

        Ok(())
    }

    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Display for BitwiseNot {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "~")
    }
}

impl Debug for BitwiseNot {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "~")
    }
}
