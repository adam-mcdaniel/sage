use crate::{
    asm::{AssemblyProgram, CoreOp, StandardOp, A, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Negate;

impl UnaryOp for Negate {
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        ty.can_decay_to(&Type::Int, env).or(ty.can_decay_to(&Type::Float, env))
    }

    fn return_type(&self, x: &Expr, env: &Env) -> Result<Type, Error> {
        let ty = x.get_type(env)?;
        if ty.can_decay_to(&Type::Int, env).unwrap_or(false) {
            Ok(Type::Int)
        } else if ty.can_decay_to(&Type::Float, env).unwrap_or(false) {
            Ok(Type::Float)
        } else {
            Err(Error::MismatchedTypes {
                expected: Type::Int,
                found: ty,
                expr: x.clone(),
            })
        }
    }

    fn eval(&self, x: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        let result = x.clone().eval(env)?;
        let ty = result.get_type(env)?;
        Ok(match result {
            ConstExpr::Int(i) => ConstExpr::Int(-i),
            ConstExpr::Float(f) => ConstExpr::Float(-f),
            _ => {
                return Err(Error::MismatchedTypes {
                    expected: Type::Int,
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
        if ty.can_decay_to(&Type::Int, env)? {
            output.op(CoreOp::Set(A, 0));
            output.op(CoreOp::Sub {
                src: SP.deref(),
                dst: A,
            });
            output.op(CoreOp::Move {
                src: A,
                dst: SP.deref(),
            });
        } else if ty.can_decay_to(&Type::Float, env)? {
            output.std_op(StandardOp::Set(A, 0.0))?;
            output.std_op(StandardOp::Sub {
                src: SP.deref(),
                dst: A,
            })?;
            output.op(CoreOp::Move {
                src: A,
                dst: SP.deref(),
            });
        } else {
            return Err(Error::InvalidUnaryOpTypes(self.clone_box(), ty.clone()));
        }

        Ok(())
    }

    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Display for Negate {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "-")
    }
}

impl Debug for Negate {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "-")
    }
}
