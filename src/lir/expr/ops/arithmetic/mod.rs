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
    asm::{AssemblyProgram, CoreOp, StandardOp, A, B, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};
use log::*;
mod addition;
mod negate;

pub use addition::*;
pub use negate::*;

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
    /// Compiles the operation on the given expressions.
    fn compile(
        &self,
        lhs: &Expr,
        rhs: &Expr,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if let Expr::Annotated(lhs, metadata) = lhs {
            return self
                .compile(lhs, rhs, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(rhs, metadata) = rhs {
            return self
                .compile(lhs, rhs, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        // trace!("Compiling binary op: {lhs} {self} {rhs} ({self:?})");
        lhs.clone().compile_expr(env, output)?;
        rhs.clone().compile_expr(env, output)?;
        // self.compile_types(&, &rhs.get_type(env)?, env, output)?;
        let lhs_expr = lhs;
        let rhs_expr = rhs;
        let lhs = &lhs_expr.get_type(env)?;
        let rhs = &rhs_expr.get_type(env)?;

        if let (Type::Array(_, _), Arithmetic::Multiply, Type::Int) = (lhs, self, rhs) {
            if !rhs.equals(&Type::Int, env)? {
                return Err(Error::MismatchedTypes { expected: Type::Int, found: rhs.clone(), expr: rhs_expr.clone() })
            }

            // Copy the loop RHS times.
            // This will just evaluate the array, evaluate the int, and then repeatedly
            // copy the array back onto the stack `rhs` times.
            let arr_size = lhs.get_size(env)?;
            // Copy the integer into a register.
            output.op(CoreOp::Many(vec![
                // Pop into B
                CoreOp::Pop(Some(B), 1),
                // Store the address of the array in A.
                CoreOp::GetAddress {
                    addr: SP.deref().offset(1 - arr_size as isize),
                    dst: A,
                },
                // While B != 0
                CoreOp::Dec(B),
                CoreOp::While(B),
                // Push the array back onto the stack, starting at A
                CoreOp::Push(A.deref(), arr_size),
                // Decrement B
                CoreOp::Dec(B),
                CoreOp::End,
            ]));

            return Ok(());
        }

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
                CoreOp::Mul { src: tmp, dst },
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
                return self.compile_types(a_type, b_type, env, output);
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

    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        match (lhs, rhs) {
            (Type::Int, Type::Int) => Ok(true),
            (Type::Int, Type::Float) | (Type::Float, Type::Int) | (Type::Float, Type::Float) => {
                Ok(true)
            }
            (Type::Array(_, _), Type::Int) => Ok(matches!(self, Self::Multiply)),
            (Type::Array(_, _), Type::Type(..)) => Ok(matches!(self, Self::Multiply)),

            (Type::Int | Type::Float | Type::Cell, Type::Cell)
            | (Type::Cell, Type::Int | Type::Float) => Ok(true),
            (Type::Unit(name1, a_type), Type::Unit(name2, b_type)) => {
                // Make sure that the two units are the same.
                if name1 != name2 {
                    return Ok(false);
                }

                // Make sure that inner types are compatible.
                if !a_type.equals(b_type, env)? {
                    return Ok(false);
                }

                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<Type, Error> {
        error!("Can I {self} to {lhs} and {rhs} in {env}?");
        if let Expr::Annotated(lhs, metadata) = lhs {
            if let Expr::Annotated(rhs, _) = rhs {
                return self.return_type(lhs, rhs, env);
            }
            return self
                .return_type(lhs, rhs, env)
                .map_err(|e| e.annotate(metadata.clone()));
        }
        if let Expr::Annotated(rhs, metadata) = rhs {
            return self
                .return_type(lhs, rhs, env)
                .map_err(|e| e.annotate(metadata.clone()));
        }
        let lhs_ty = lhs.get_type(env)?;
        let rhs_ty = rhs.get_type(env)?;

        error!("Can I {self} to {lhs_ty} and {rhs_ty} in {env}?");
        let result = Ok(match (lhs_ty.discard_type_wrapper(), rhs_ty.discard_type_wrapper()) {
            (Type::Int, Type::Int) => Type::Int,
            (Type::Int, Type::Float) | (Type::Float, Type::Int) | (Type::Float, Type::Float) => {
                Type::Float
            }
            (Type::Int | Type::Float | Type::Cell, Type::Cell)
            | (Type::Cell, Type::Int | Type::Float) => Type::Cell,

            (Type::Array(elem, size), other) => {
                if let (Self::Multiply, Expr::ConstExpr(const_rhs)) = (self, rhs) {
                    let size = size.as_int(env)?;
                    if let Ok(n) = const_rhs.clone().as_int(env) {
                        if n <= 0 {
                            error!("Cannot multiply array {lhs} by {n} (not positive)");
                            return Err(Error::InvalidBinaryOp(
                                Box::new(*self),
                                lhs.clone(),
                                rhs.clone(),
                            ));
                        }
                        Type::Array(elem, Box::new(ConstExpr::Int(size * n)))
                    } else {
                        Type::Array(elem, Box::new(ConstExpr::Any))
                    }
                } else {
                    error!("Cannot multiply array {lhs} by {rhs} (not constant = {rhs:?})");
                    return Err(Error::InvalidBinaryOp(
                        Box::new(*self),
                        lhs.clone(),
                        rhs.clone(),
                    ));
                }
            }
            // (Type::Int, Type::Array(_, size)) => {
            //     if let Self::Multiply = self {
            //         Type::Array(Box::new(Type::Int), size.clone() * lhs.as_int(env)?)
            //     } else {
            //         return Err(Error::InvalidBinaryOp(
            //             Box::new(*self),
            //             lhs.clone(),
            //             rhs.clone(),
            //         ));
            //     }
            // }
            (Type::Unit(name1, a_type), Type::Unit(name2, b_type)) => {
                // Make sure that the two units are the same.
                if name1 != name2 {
                    error!("{name1} is not {name2}");
                    return Err(Error::InvalidBinaryOp(
                        Box::new(*self),
                        lhs.clone(),
                        rhs.clone(),
                    ));
                }

                // Make sure that inner types are compatible.
                if !a_type.equals(&b_type, env)? {
                    error!("{a_type} is not {b_type}");
                    return Err(Error::InvalidBinaryOp(
                        Box::new(*self),
                        lhs.clone(),
                        rhs.clone(),
                    ));
                }

                Type::Unit(name1, a_type)
            }
            (a, b) => {
                error!("Unhandled case {a} {self} {b}");
                return Err(Error::InvalidBinaryOp(
                    Box::new(*self),
                    lhs.clone(),
                    rhs.clone(),
                ))
            }
        });
        error!("Well, can I?? {result:?}");
        result
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        match (lhs.clone().eval(env)?, self, rhs.clone().eval(env)?) {
            (ConstExpr::Array(arr), Arithmetic::Multiply, ConstExpr::Int(n)) => {
                // Repeat the array `rhs` times.
                let mut new_arr = Vec::new();
                for _ in 0..n {
                    new_arr.extend(arr.clone());
                }
                Ok(ConstExpr::Array(new_arr))
            }
            // | (ConstExpr::Int(n), Arithmetic::Multiply, ConstExpr::Array(arr)) => {
            //     // Repeat the array `rhs` times.
            //     let mut new_arr = Vec::new();
            //     for _ in 0..n {
            //         new_arr.extend(arr.clone());
            //     }
            //     Ok(ConstExpr::Array(new_arr))
            // }
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
