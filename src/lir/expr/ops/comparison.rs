//! # Comparison Operations
//!
//! This module implements comparison operators between two expressions.

use crate::{
    asm::{AssemblyProgram, CoreOp, StandardOp, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// A comparison operation between two values.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Comparison {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl BinaryOp for Comparison {
    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        match (lhs, self, rhs) {
            (Type::Int, Self::LessThan, Type::Int)
            | (Type::Int, Self::LessThanOrEqual, Type::Int)
            | (Type::Int, Self::GreaterThan, Type::Int)
            | (Type::Int, Self::GreaterThanOrEqual, Type::Int) => Ok(true),

            (Type::Float, Self::LessThan, Type::Float)
            | (Type::Float, Self::LessThan, Type::Int)
            | (Type::Int, Self::LessThan, Type::Float)
            | (Type::Int, Self::GreaterThan, Type::Float)
            | (Type::Float, Self::GreaterThan, Type::Int)
            | (Type::Float, Self::GreaterThan, Type::Float) => Ok(true),

            (Type::Unit(name1, a_type), _, Type::Unit(name2, b_type)) => {
                // Make sure that the two units are the same.
                if name1 != name2 {
                    return Ok(false);
                }

                // Make sure that inner types are compatible.
                if !a_type.equals(b_type, env)? {
                    return Ok(false);
                }

                self.can_apply(a_type, b_type, env)
            }
            (a, Self::Equal, b) | (a, Self::NotEqual, b) => {
                Ok(a.can_decay_to(b, env)? && a.get_size(env)? == 1)
            }
            _ => Ok(false),
        }
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, _lhs: &Expr, _rhs: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::Bool)
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        match (lhs.clone().eval(env)?, self, rhs.clone().eval(env)?) {
            (a, Self::Equal, b) => Ok(ConstExpr::Bool(a == b)),
            (a, Self::NotEqual, b) => Ok(ConstExpr::Bool(a != b)),
            (ConstExpr::Int(a), Self::LessThan, ConstExpr::Int(b)) => Ok(ConstExpr::Bool(a < b)),
            (ConstExpr::Int(a), Self::LessThanOrEqual, ConstExpr::Int(b)) => {
                Ok(ConstExpr::Bool(a <= b))
            }
            (ConstExpr::Int(a), Self::GreaterThan, ConstExpr::Int(b)) => Ok(ConstExpr::Bool(a > b)),
            (ConstExpr::Int(a), Self::GreaterThanOrEqual, ConstExpr::Int(b)) => {
                Ok(ConstExpr::Bool(a >= b))
            }
            (ConstExpr::Float(a), Self::LessThan, ConstExpr::Float(b)) => {
                Ok(ConstExpr::Bool(a < b))
            }
            (ConstExpr::Float(a), Self::GreaterThan, ConstExpr::Float(b)) => {
                Ok(ConstExpr::Bool(a > b))
            }
            (ConstExpr::Float(a), Self::LessThan, ConstExpr::Int(b)) => {
                Ok(ConstExpr::Bool(a < b as f64))
            }
            (ConstExpr::Int(a), Self::LessThan, ConstExpr::Float(b)) => {
                Ok(ConstExpr::Bool((a as f64) < b))
            }
            (ConstExpr::Float(a), Self::GreaterThan, ConstExpr::Int(b)) => {
                Ok(ConstExpr::Bool(a > b as f64))
            }
            (ConstExpr::Int(a), Self::GreaterThan, ConstExpr::Float(b)) => {
                Ok(ConstExpr::Bool((a as f64) > b))
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
        lhs: &Type,
        rhs: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        let src = SP.deref();
        let dst = SP.deref().offset(-1);
        let tmp = SP.deref().offset(1);
        // Get the respective core operation for the current expression.
        let core_op = match self {
            Self::Equal => CoreOp::IsEqual {
                a: tmp,
                b: src,
                dst,
            },
            Self::NotEqual => CoreOp::IsNotEqual {
                a: tmp,
                b: src,
                dst,
            },
            Self::LessThan => CoreOp::IsLess {
                a: tmp,
                b: src,
                dst,
            },
            Self::LessThanOrEqual => CoreOp::IsLessEqual {
                a: tmp,
                b: src,
                dst,
            },
            Self::GreaterThan => CoreOp::IsGreater {
                a: tmp,
                b: src,
                dst,
            },
            Self::GreaterThanOrEqual => CoreOp::IsGreaterEqual {
                a: tmp,
                b: src,
                dst,
            },
        };
        let src = SP.deref();
        let dst = SP.deref().offset(-1);
        let tmp = SP.deref().offset(1);
        // Get the respective standard operation for the current expression.
        let std_op = match self {
            Self::Equal => StandardOp::CoreOp(CoreOp::IsEqual {
                a: tmp,
                b: src,
                dst,
            }),
            Self::NotEqual => StandardOp::CoreOp(CoreOp::IsNotEqual {
                a: tmp,
                b: src,
                dst,
            }),
            Self::LessThan => StandardOp::IsLess {
                a: tmp,
                b: src,
                dst,
            },
            Self::LessThanOrEqual => StandardOp::IsLess {
                a: tmp,
                b: src,
                dst,
            },
            Self::GreaterThan => StandardOp::IsGreater {
                a: tmp,
                b: src,
                dst,
            },
            Self::GreaterThanOrEqual => StandardOp::IsGreater {
                a: tmp,
                b: src,
                dst,
            },
        };
        let dst = SP.deref().offset(-1);
        let tmp = SP.deref().offset(1);
        // Now, perform the correct assembly expressions based on the types of the two expressions.
        match (lhs, self, rhs) {
            // If a `Float` and a `Cell` are used, we just interpret the `Cell` as a `Float`.
            (Type::Cell, _, Type::Float) | (Type::Float, _, Type::Cell) => {
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.std_op(std_op)?;
            }
            // Two floats are used as floats.
            (Type::Float, _, Type::Float) => {
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.std_op(std_op)?;
            }
            // An integer used with a float is promoted, and returns a float.
            (Type::Int, _, Type::Float) => {
                output.std_op(StandardOp::ToFloat(SP.deref().offset(-1)))?;
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.std_op(std_op)?;
            }
            (Type::Float, _, Type::Int) => {
                output.std_op(StandardOp::ToFloat(SP.deref()))?;
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.std_op(std_op)?;
            }

            // If cells and/or ints are used, we just use them as integers.
            (Type::Int, _, Type::Int)
            | (Type::Cell, _, Type::Cell)
            | (Type::Cell, _, Type::Int)
            | (Type::Int, _, Type::Cell) => {
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.op(core_op);
            }

            (Type::Unit(_name1, a_type), _, Type::Unit(_name2, b_type)) => {
                return self.compile_types(a_type, b_type, env, output);
            }

            (a, Self::Equal, b) if a.can_decay_to(b, env)? => {
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.op(core_op);
            }
            (a, Self::NotEqual, b) if a.can_decay_to(b, env)? => {
                output.op(CoreOp::Move { src: dst, dst: tmp });
                output.op(core_op);
            }

            // Cannot do arithmetic on other pairs of types.
            _ => {
                return Err(Error::InvalidBinaryOpTypes(
                    Box::new(*self),
                    lhs.clone(),
                    rhs.clone(),
                ))
            }
        }
        // Pop `b` off of the stack: we only needed it to evaluate
        // the arithmetic and store the result to `a` on the stack.
        output.op(CoreOp::Pop(None, 1));
        Ok(())
    }

    fn clone_box(&self) -> Box<dyn BinaryOp> {
        Box::new(*self)
    }
}

impl Debug for Comparison {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Comparison::Equal => write!(f, "=="),
            Comparison::NotEqual => write!(f, "!="),
            Comparison::LessThan => write!(f, "<"),
            Comparison::LessThanOrEqual => write!(f, "<="),
            Comparison::GreaterThan => write!(f, ">"),
            Comparison::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Comparison::Equal => write!(f, "=="),
            Comparison::NotEqual => write!(f, "!="),
            Comparison::LessThan => write!(f, "<"),
            Comparison::LessThanOrEqual => write!(f, "<="),
            Comparison::GreaterThan => write!(f, ">"),
            Comparison::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}
