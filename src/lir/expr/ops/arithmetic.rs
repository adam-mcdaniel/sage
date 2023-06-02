//! # Arithmetic Operations
//!
//! This module implements several arithmetic operations:
//! - `Add`
//! - `Subtract`
//! - `Multiply`
//! - `Divide`
//! - `Remainder`
//! - `Power`

use crate::{
    asm::{AssemblyProgram, CoreOp, StandardOp, SP, A},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};


#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Negate;

impl UnaryOp for Negate {
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        ty.equals(&Type::Int, env).or(ty.equals(&Type::Float, env))
    }

    fn return_type(&self, x: &Expr, env: &Env) -> Result<Type, Error> {
        let ty = x.get_type(env)?;
        if ty.equals(&Type::Int, env).unwrap_or(false) {
            Ok(Type::Int)
        } else if ty.equals(&Type::Float, env).unwrap_or(false) {
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
            _ => return Err(Error::MismatchedTypes {
                expected: Type::Int,
                found: ty,
                expr: Expr::ConstExpr(x.clone()),
            })
        })
    }

    // fn compile_types(&self, ty: &Type, env: &mut Env) -> Result<AssemblyProgram, Error> {}
    fn compile_types(
        &self,
        ty: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if ty.equals(&Type::Int, env)? {
            output.op(CoreOp::Set(A, 0));
            output.op(CoreOp::Sub { src: SP.deref(), dst: A });
            output.op(CoreOp::Move { src: A, dst: SP.deref() });
        } else if ty.equals(&Type::Float, env)? {
            output.std_op(StandardOp::Set(A, 0.0))?;
            output.std_op(StandardOp::Sub { src: SP.deref(), dst: A })?;
            output.op(CoreOp::Move { src: A, dst: SP.deref() });
        } else {
            return Err(Error::InvalidUnaryOpTypes(self.clone_box(), ty.clone()))
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

/// An arithmetic operation.
#[derive(Clone, Copy)]
pub enum Arithmetic {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Power,
}

impl BinaryOp for Arithmetic {
    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(true),
            (Type::Int, Type::Float) | (Type::Float, Type::Int) | (Type::Float, Type::Float) => {
                Ok(true)
            }
            (Type::Int | Type::Float, Type::Cell) | (Type::Cell, Type::Int | Type::Float) => {
                Ok(true)
            }
            (Type::Unit(name1, a_type), Type::Unit(name2, b_type)) => {
                // Make sure that the two units are the same.
                if name1 != name2 {
                    return Ok(false);
                }

                // Make sure that inner types are compatible.
                if !a_type.equals(&b_type, env)? {
                    return Ok(false);
                }

                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<Type, Error> {
        Ok(match (lhs.get_type(env)?, rhs.get_type(env)?) {
            (Type::Int, Type::Int) => Type::Int,
            (Type::Int, Type::Float) | (Type::Float, Type::Int) | (Type::Float, Type::Float) => {
                Type::Float
            }
            (Type::Int | Type::Float, Type::Cell) | (Type::Cell, Type::Int | Type::Float) => {
                Type::Cell
            }

            (Type::Unit(name1, a_type), Type::Unit(name2, b_type)) => {
                // Make sure that the two units are the same.
                if name1 != name2 {
                    return Err(Error::InvalidBinaryOp(
                        Box::new(*self),
                        lhs.clone(),
                        rhs.clone(),
                    ));
                }

                // Make sure that inner types are compatible.
                if !a_type.equals(&b_type, env)? {
                    return Err(Error::InvalidBinaryOp(
                        Box::new(*self),
                        lhs.clone(),
                        rhs.clone(),
                    ));
                }

                Type::Unit(name1.clone(), a_type.clone())
            }
            _ => {
                return Err(Error::InvalidBinaryOp(
                    Box::new(*self),
                    lhs.clone(),
                    rhs.clone(),
                ))
            }
        })
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        match (lhs.clone().eval(env)?, self, rhs.clone().eval(env)?) {
            (ConstExpr::Int(lhs), Arithmetic::Add, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Int(lhs + rhs))
            }
            (ConstExpr::Int(lhs), Arithmetic::Subtract, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Int(lhs - rhs))
            }
            (ConstExpr::Int(lhs), Arithmetic::Multiply, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Int(lhs * rhs))
            }
            (ConstExpr::Int(lhs), Arithmetic::Divide, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Int(lhs / rhs))
            }
            (ConstExpr::Int(lhs), Arithmetic::Remainder, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Int(lhs % rhs))
            }
            (ConstExpr::Int(lhs), Arithmetic::Power, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Int(lhs.pow(rhs as u32)))
            }

            (ConstExpr::Float(lhs), Arithmetic::Add, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs + rhs))
            }
            (ConstExpr::Float(a), Arithmetic::Add, ConstExpr::Int(b))
            | (ConstExpr::Int(b), Arithmetic::Add, ConstExpr::Float(a)) => {
                Ok(ConstExpr::Float(a + b as f64))
            }

            (ConstExpr::Float(lhs), Arithmetic::Multiply, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs * rhs))
            }
            (ConstExpr::Float(a), Arithmetic::Multiply, ConstExpr::Int(b))
            | (ConstExpr::Int(b), Arithmetic::Multiply, ConstExpr::Float(a)) => {
                Ok(ConstExpr::Float(a * b as f64))
            }

            (ConstExpr::Float(lhs), Arithmetic::Subtract, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs - rhs))
            }
            (ConstExpr::Float(lhs), Arithmetic::Subtract, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Float(lhs - rhs as f64))
            }
            (ConstExpr::Int(lhs), Arithmetic::Subtract, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs as f64 - rhs))
            }

            (ConstExpr::Float(lhs), Arithmetic::Divide, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs / rhs))
            }
            (ConstExpr::Float(lhs), Arithmetic::Divide, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Float(lhs / rhs as f64))
            }
            (ConstExpr::Int(lhs), Arithmetic::Divide, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs as f64 / rhs))
            }

            (ConstExpr::Float(lhs), Arithmetic::Remainder, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs % rhs))
            }
            (ConstExpr::Float(lhs), Arithmetic::Remainder, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Float(lhs % rhs as f64))
            }
            (ConstExpr::Int(lhs), Arithmetic::Remainder, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs as f64 % rhs))
            }

            (ConstExpr::Float(lhs), Arithmetic::Power, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float(lhs.powf(rhs)))
            }
            (ConstExpr::Int(lhs), Arithmetic::Power, ConstExpr::Float(rhs)) => {
                Ok(ConstExpr::Float((lhs as f64).powf(rhs)))
            }
            (ConstExpr::Float(lhs), Arithmetic::Power, ConstExpr::Int(rhs)) => {
                Ok(ConstExpr::Float(lhs.powf(rhs as f64)))
            }
            _ => Err(Error::InvalidBinaryOp(
                Box::new(*self),
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
            Self::Add => CoreOp::Add { src, dst },
            Self::Subtract => CoreOp::Sub { src, dst },
            Self::Multiply => CoreOp::Mul { src, dst },
            Self::Divide => CoreOp::Div { src, dst },
            Self::Remainder => CoreOp::Rem { src, dst },
            Self::Power => CoreOp::Many(vec![
                // We can't use the `Pow` operation, because it's not supported by
                // the core variant. So we have to do it with a loop.
                CoreOp::Move {
                    src: dst.clone(),
                    dst: tmp.clone(),
                },
                CoreOp::Set(dst.clone(), 1),
                CoreOp::While(src.clone()),
                CoreOp::Mul {
                    src: tmp.clone(),
                    dst,
                },
                CoreOp::Dec(src),
                CoreOp::End,
            ]),
        };
        let src = SP.deref();
        let dst = SP.deref().offset(-1);
        // Get the respective standard operation for the current expression.
        let std_op = match self {
            Self::Add => StandardOp::Add { src, dst },
            Self::Subtract => StandardOp::Sub { src, dst },
            Self::Multiply => StandardOp::Mul { src, dst },
            Self::Divide => StandardOp::Div { src, dst },
            Self::Remainder => StandardOp::Rem { src, dst },
            Self::Power => StandardOp::Pow { src, dst },
        };
        // Now, perform the correct assembly expressions based on the types of the two expressions.
        match (lhs, rhs) {
            // If a `Float` and a `Cell` are used, we just interpret the `Cell` as a `Float`.
            (Type::Cell, Type::Float) | (Type::Float, Type::Cell) => {
                output.std_op(std_op)?;
            }
            // Two floats are used as floats.
            (Type::Float, Type::Float) => {
                output.std_op(std_op)?;
            }
            // An integer used with a float is promoted, and returns a float.
            (Type::Int, Type::Float) => {
                output.std_op(StandardOp::ToFloat(SP.deref().offset(-1)))?;
                output.std_op(std_op)?;
            }
            (Type::Float, Type::Int) => {
                output.std_op(StandardOp::ToFloat(SP.deref()))?;
                output.std_op(std_op)?;
            }

            // If cells and/or ints are used, we just use them as integers.
            (Type::Int, Type::Int)
            | (Type::Cell, Type::Cell)
            | (Type::Cell, Type::Int)
            | (Type::Int, Type::Cell) => {
                output.op(core_op);
            }

            (Type::Unit(_name1, a_type), Type::Unit(_name2, b_type)) => {
                return self.compile_types(&a_type, &b_type, env, output);
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

impl Debug for Arithmetic {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Arithmetic::Add => write!(f, "+"),
            Arithmetic::Subtract => write!(f, "-"),
            Arithmetic::Multiply => write!(f, "*"),
            Arithmetic::Divide => write!(f, "/"),
            Arithmetic::Remainder => write!(f, "%"),
            Arithmetic::Power => write!(f, "**"),
        }
    }
}

impl Display for Arithmetic {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Arithmetic::Add => write!(f, "+"),
            Arithmetic::Subtract => write!(f, "-"),
            Arithmetic::Multiply => write!(f, "*"),
            Arithmetic::Divide => write!(f, "/"),
            Arithmetic::Remainder => write!(f, "%"),
            Arithmetic::Power => write!(f, "**"),
        }
    }
}
