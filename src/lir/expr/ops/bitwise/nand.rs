//! # Bitwise Operations
use crate::{
    asm::{AssemblyProgram, CoreOp, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// A boolean "BitwiseNand" operation between two values.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct BitwiseNand;

impl BinaryOp for BitwiseNand {
    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        Ok(
            (lhs.equals(&Type::Cell, env)? || lhs.equals(&Type::Int, env)?)
                && (rhs.equals(&Type::Cell, env)? || rhs.equals(&Type::Int, env)?),
        )
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<Type, Error> {
        if lhs.get_type(env)?.equals(&Type::Cell, env)?
            || rhs.get_type(env)?.equals(&Type::Cell, env)?
        {
            Ok(Type::Cell)
        } else {
            Ok(Type::Int)
        }
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        match (lhs.clone().eval(env)?, rhs.clone().eval(env)?) {
            (ConstExpr::Int(a), ConstExpr::Int(b)) => Ok(ConstExpr::Int(!(a & b))),
            (ConstExpr::Cell(a) | ConstExpr::Int(a), ConstExpr::Cell(b) | ConstExpr::Int(b)) => {
                Ok(ConstExpr::Cell(!(a & b)))
            }
            _ => Err(Error::InvalidBinaryOp(
                self.clone_box(),
                Expr::ConstExpr(lhs.clone()),
                Expr::ConstExpr(rhs.clone()),
            )),
        }
    }

    /// Compile the binary operation.
    fn compile_types(
        &self,
        _lhs: &Type,
        _rhs: &Type,
        _env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        output.op(CoreOp::BitwiseNand {
            src: SP.deref(),
            dst: SP.deref().offset(-1),
        });
        output.op(CoreOp::Pop(None, 1));
        Ok(())
    }

    /// Clone this binary operation into a box.
    fn clone_box(&self) -> Box<dyn BinaryOp> {
        Box::new(*self)
    }
}

impl Debug for BitwiseNand {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "~&")
    }
}

impl Display for BitwiseNand {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "~&")
    }
}
