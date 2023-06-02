//! # Logic Operations
//!
//! This module implements the `And`, `Or`, and `Not` structs,
//! which implement boolean logic operations.

use crate::{
    asm::{AssemblyProgram, CoreOp, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// A boolean "And" operation between two values.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct And;

impl BinaryOp for And {
    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        Ok(lhs.equals(&Type::Bool, env)? && rhs.equals(&Type::Bool, env)?)
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, _lhs: &Expr, _rhs: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::Bool)
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        match (lhs.clone().eval(env)?, rhs.clone().eval(env)?) {
            (ConstExpr::Bool(a), ConstExpr::Bool(b)) => Ok(ConstExpr::Bool(a && b)),
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
        output.op(CoreOp::And {
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

/// A boolean "Or" operation between two values.
#[derive(Clone, Copy)]
pub struct Or;

impl BinaryOp for Or {
    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        Ok(lhs.equals(&Type::Bool, env)? && rhs.equals(&Type::Bool, env)?)
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, _lhs: &Expr, _rhs: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::Bool)
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        match (lhs.clone().eval(env)?, rhs.clone().eval(env)?) {
            (ConstExpr::Bool(a), ConstExpr::Bool(b)) => Ok(ConstExpr::Bool(a || b)),
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
        output.op(CoreOp::Or {
            src: SP.deref(),
            dst: SP.deref().offset(-1),
        });
        output.op(CoreOp::Pop(None, 1));
        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn BinaryOp> {
        Box::new(*self)
    }
}

/// A boolean "Not" operation on a value.
#[derive(Clone, Copy)]
pub struct Not;

impl UnaryOp for Not {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, expr: &Type, env: &Env) -> Result<bool, Error> {
        expr.equals(&Type::Bool, env)
    }

    /// Get the type of the result of applying this unary operation to the given type.
    fn return_type(&self, _expr: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::Bool)
    }

    /// Evaluate this unary operation on the given constant values.
    fn eval(&self, expr: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        if let ConstExpr::Bool(x) = expr.clone().eval(env)? {
            Ok(ConstExpr::Bool(!x))
        } else {
            Err(Error::InvalidUnaryOp(
                self.clone_box(),
                Expr::ConstExpr(expr.clone()),
            ))
        }
    }

    /// Compile the unary operation.
    fn compile_types(
        &self,
        _expr: &Type,
        _env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        output.op(CoreOp::Not(SP.deref()));
        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for And {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "and")
    }
}

impl Display for And {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "and")
    }
}

impl Debug for Or {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "or")
    }
}

impl Display for Or {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "or")
    }
}

impl Debug for Not {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "not")
    }
}

impl Display for Not {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "not")
    }
}
