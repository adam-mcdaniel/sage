//! # Operations
//!
//! This module contains implementations of various
//! tertiary, binary, and unary operations on LIR expressions.
mod arithmetic;
mod assign;
mod bitwise;
mod comparison;
mod io;
mod logic;
mod memory;
mod tagged_union;

pub use arithmetic::*;
pub use assign::*;
pub use bitwise::*;
pub use comparison::*;
pub use io::*;
pub use logic::*;
pub use memory::*;
pub use tagged_union::*;

use crate::{asm::AssemblyProgram, lir::*};
use log::error;
use std::cmp::Ordering;

/// A trait used to implemented an assignment operation.
///
/// This trait is used to implement assignment operations like `+=` and `-=`.
pub trait AssignOp: std::fmt::Debug + std::fmt::Display {
    /// Typechecks the operation on the given expressions.
    fn type_check(&self, dst: &Expr, src: &Expr, env: &Env) -> Result<(), Error> {
        // trace!("Type checking assign op: {dst} {self} {src} ({self:?})");
        if self.can_apply(&dst.get_type(env)?, &src.get_type(env)?, env)? {
            dst.type_check(env).and(src.type_check(env))
        } else {
            error!("Invalid assign op: {dst} {self} {src} ({self:?}) in environment {env}");
            Err(Error::InvalidAssignOp(
                self.clone_box(),
                dst.clone(),
                src.clone(),
            ))
        }
    }
    /// Gets the type of the operation on the given expressions.
    fn return_type(&self, dst: &Expr, src: &Expr, env: &Env) -> Result<Type, Error> {
        if let Expr::Annotated(dst, metadata) = dst {
            return self
                .return_type(dst, src, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if let Expr::Annotated(src, metadata) = src {
            return self
                .return_type(dst, src, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if self.can_apply_exprs(dst, src, env)? {
            dst.get_type(env)
        } else {
            error!("Invalid assign op: {dst} {self} {src} ({self:?}) in environment {env}");
            Err(Error::InvalidAssignOp(
                self.clone_box(),
                dst.clone(),
                src.clone(),
            ))
        }
    }
    /// Clones the operation into a boxed trait object.
    fn clone_box(&self) -> Box<dyn AssignOp>;
    /// Formats the operation for display.
    fn display(&self, dst: &Expr, src: &Expr) -> String {
        format!("{} {} {}", dst, self, src)
    }

    /// Checks if the operation can be applied to the given types.
    fn can_apply(&self, dst: &Type, src: &Type, env: &Env) -> Result<bool, Error>;
    /// Checks if the operation can be applied to the given expressions.
    fn can_apply_exprs(&self, dst: &Expr, src: &Expr, env: &Env) -> Result<bool, Error> {
        if let Expr::Annotated(dst, metadata) = dst {
            return self
                .can_apply_exprs(dst, src, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if let Expr::Annotated(src, metadata) = src {
            return self
                .can_apply_exprs(dst, src, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        self.can_apply(&dst.get_type(env)?, &src.get_type(env)?, env)
    }
    /// Evaluates the operation on the given constant expressions.
    fn eval(&self, dst: &ConstExpr, src: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error>;
    /// Compiles the operation on the given expressions.
    fn compile(
        &self,
        dst: &Expr,
        src: &Expr,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if let Expr::Annotated(dst, metadata) = dst {
            return self
                .compile(dst, src, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if let Expr::Annotated(src, metadata) = src {
            return self
                .compile(dst, src, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        // trace!("Compiling assign op: {dst} {self} {src} ({self:?})");
        dst.clone().compile_expr(env, output)?;
        src.clone().compile_expr(env, output)?;
        self.compile_types(&dst.get_type(env)?, &src.get_type(env)?, env, output)
    }
    /// Compiles the operation on the given types. (Generates the code for the operation.)
    fn compile_types(
        &self,
        dst: &Type,
        src: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error>;
}

/// A trait used to implement a unary operation.
///
/// This trait is used to implement unary operations like `not` and `~`.
pub trait UnaryOp: std::fmt::Debug + std::fmt::Display {
    /// Typechecks the operation on the given expression.
    fn type_check(&self, expr: &Expr, env: &Env) -> Result<(), Error> {
        if let Expr::Annotated(expr, metadata) = expr {
            return self
                .type_check(expr, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        // trace!("Type checking unary op: {self} {expr} ({self:?})");
        if self.can_apply(&expr.get_type(env)?, env)? {
            expr.type_check(env)
        } else {
            error!("Invalid unary op: {self} {expr} ({self:?}) in environment {env}");
            Err(Error::InvalidUnaryOp(self.clone_box(), expr.clone()))
        }
    }
    /// Gets the type of the operation on the given expression.
    fn return_type(&self, expr: &Expr, env: &Env) -> Result<Type, Error> {
        if let Expr::Annotated(expr, metadata) = expr {
            return self
                .return_type(expr, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if self.can_apply_exprs(expr, env)? {
            Ok(expr.get_type(env)?)
        } else {
            error!("Invalid unary op: {self} {expr} ({self:?}) in environment {env}");
            Err(Error::InvalidUnaryOp(self.clone_box(), expr.clone()))
        }
    }
    /// Clones the operation into a boxed trait object.
    fn clone_box(&self) -> Box<dyn UnaryOp>;
    /// Formats the operation for display.
    fn display(&self, expr: &Expr) -> String {
        format!("{} {}", self, expr)
    }
    /// Checks if the operation can be applied to the given type.
    fn can_apply(&self, expr: &Type, env: &Env) -> Result<bool, Error>;
    /// Checks if the operation can be applied to the given expression.
    fn can_apply_exprs(&self, expr: &Expr, env: &Env) -> Result<bool, Error> {
        self.can_apply(&expr.get_type(env)?, env)
    }
    /// Evaluates the operation on the given constant expression.
    fn eval(&self, expr: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error>;
    /// Compiles the operation on the given expression.
    fn compile(
        &self,
        expr: &Expr,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if let Expr::Annotated(expr, metadata) = expr {
            return self
                .compile(expr, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        // trace!("Compiling unary op: {self} {expr} ({self:?})");
        let current_instruction = output.current_instruction();
        expr.clone().compile_expr(env, output)?;
        self.compile_types(&expr.get_type(env)?, env, output)?;
        let message = format!("Compiled unary op: {self} '{expr}' (with operator {self:?})");
        output.log_instructions_after(&self.display(expr), &message, current_instruction);
        Ok(())
    }
    /// Compiles the operation on the given type. (Generates the code for the operation.)
    fn compile_types(
        &self,
        expr: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error>;
}

/// A trait used to implement a binary operation.
///
/// This trait is used to implement binary operations like `+` and `==`.
pub trait BinaryOp: std::fmt::Debug + std::fmt::Display {
    /// Typechecks the operation on the given expressions.
    fn type_check(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<(), Error> {
        if let Expr::Annotated(lhs, metadata) = lhs {
            return self
                .type_check(lhs, rhs, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(rhs, metadata) = rhs {
            return self
                .type_check(lhs, rhs, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        // trace!("Type checking binary op: {lhs} {self} {rhs} ({self:?})");
        if self.can_apply(&lhs.get_type(env)?, &rhs.get_type(env)?, env)? {
            lhs.type_check(env).and(rhs.type_check(env))
        } else {
            error!("Invalid binary op: {lhs} {self} {rhs} ({self:?}) in environment {env}");
            Err(Error::InvalidBinaryOp(
                self.clone_box(),
                lhs.clone(),
                rhs.clone(),
            ))
        }
    }
    /// Gets the type of the operation on the given expressions.
    fn return_type(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<Type, Error> {
        if let Expr::Annotated(lhs, metadata) = lhs {
            return self
                .return_type(lhs, rhs, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(rhs, metadata) = rhs {
            return self
                .return_type(lhs, rhs, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if self.can_apply_exprs(lhs, rhs, env)? {
            lhs.get_type(env)
        } else {
            error!("Invalid binary op: {lhs} {self} {rhs} ({self:?}) in environment {env}");
            Err(Error::InvalidBinaryOp(
                self.clone_box(),
                lhs.clone(),
                rhs.clone(),
            ))
        }
    }
    /// Clones the operation into a boxed trait object.
    fn clone_box(&self) -> Box<dyn BinaryOp>;
    /// Formats the operation for display.
    fn display(&self, lhs: &Expr, rhs: &Expr) -> String {
        format!("{} {} {}", lhs, self, rhs)
    }

    /// Checks if the operation can be applied to the given types.
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error>;
    /// Checks if the operation can be applied to the given expressions.
    fn can_apply_exprs(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<bool, Error> {
        self.can_apply(&lhs.get_type(env)?, &rhs.get_type(env)?, env)
    }
    /// Evaluates the operation on the given constant expressions.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error>;
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
        self.compile_types(&lhs.get_type(env)?, &rhs.get_type(env)?, env, output)?;
        Ok(())
    }
    /// Compiles the operation on the given types. (Generates the code for the operation.)
    fn compile_types(
        &self,
        lhs: &Type,
        rhs: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error>;
}

/// A trait used to implement a ternary operation.
pub trait TernaryOp: std::fmt::Debug + std::fmt::Display {
    /// Typechecks the operation on the given expressions.
    fn type_check(&self, a: &Expr, b: &Expr, c: &Expr, env: &Env) -> Result<(), Error> {
        if let Expr::Annotated(a, metadata) = a {
            return self
                .type_check(a, b, c, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(b, metadata) = b {
            return self
                .type_check(a, b, c, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(c, metadata) = c {
            return self
                .type_check(a, b, c, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if self.can_apply(
            &a.clone().get_type(env)?,
            &b.clone().get_type(env)?,
            &c.get_type(env)?,
            env,
        )? {
            a.type_check(env)
                .and(b.type_check(env))
                .and(c.type_check(env))
        } else {
            Err(Error::InvalidTernaryOp(
                self.clone_box(),
                a.clone(),
                b.clone(),
                c.clone(),
            ))
        }
    }
    /// Gets the type of the operation on the given expressions.
    fn return_type(&self, a: &Expr, b: &Expr, c: &Expr, env: &Env) -> Result<Type, Error> {
        if let Expr::Annotated(a, metadata) = a {
            return self
                .return_type(a, b, c, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(b, metadata) = b {
            return self
                .return_type(a, b, c, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(c, metadata) = c {
            return self
                .return_type(a, b, c, env)
                .map_err(|err| err.annotate(metadata.clone()));
        }

        if self.can_apply_exprs(a, b, c, env)? {
            c.get_type(env)
        } else {
            Err(Error::InvalidTernaryOp(
                self.clone_box(),
                a.clone(),
                b.clone(),
                c.clone(),
            ))
        }
    }
    /// Clones the operation into a boxed trait object.
    fn clone_box(&self) -> Box<dyn TernaryOp>;
    /// Formats the operation for display.
    fn display(&self, a: &Expr, b: &Expr, c: &Expr) -> String {
        format!("{} {} {} {}", a, self, b, c)
    }

    /// Checks if the operation can be applied to the given types.
    fn can_apply(&self, a: &Type, b: &Type, c: &Type, env: &Env) -> Result<bool, Error>;
    /// Checks if the operation can be applied to the given expressions.
    fn can_apply_exprs(&self, a: &Expr, b: &Expr, c: &Expr, env: &Env) -> Result<bool, Error> {
        self.can_apply(&a.get_type(env)?, &b.get_type(env)?, &c.get_type(env)?, env)
    }
    /// Evaluates the operation on the given constant expressions.
    fn eval(
        &self,
        a: &ConstExpr,
        b: &ConstExpr,
        c: &ConstExpr,
        env: &mut Env,
    ) -> Result<ConstExpr, Error>;
    /// Compiles the operation on the given expressions.
    fn compile(
        &self,
        a: &Expr,
        b: &Expr,
        c: &Expr,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if let Expr::Annotated(a, metadata) = a {
            return self
                .compile(a, b, c, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(b, metadata) = b {
            return self
                .compile(a, b, c, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        if let Expr::Annotated(c, metadata) = c {
            return self
                .compile(a, b, c, env, output)
                .map_err(|err| err.annotate(metadata.clone()));
        }
        // trace!("Compiling ternary op: {a} {self} {b} {c} ({self:?})");
        // Evaluate the three expression on the stack.
        a.clone().compile_expr(env, output)?;
        b.clone().compile_expr(env, output)?;
        c.clone().compile_expr(env, output)?;
        // Compile the operation.
        self.compile_types(
            &a.get_type(env)?,
            &b.get_type(env)?,
            &c.get_type(env)?,
            env,
            output,
        )?;
        Ok(())
    }
    /// Compiles the operation on the given types. (Generates the code for the operation.)
    fn compile_types(
        &self,
        a: &Type,
        b: &Type,
        c: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error>;
}

impl PartialEq for dyn AssignOp {
    fn eq(&self, other: &Self) -> bool {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .eq(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl PartialEq for dyn UnaryOp {
    fn eq(&self, other: &Self) -> bool {
        self.display(&Expr::ConstExpr(ConstExpr::Int(0)))
            .eq(&other.display(&Expr::ConstExpr(ConstExpr::Int(0))))
    }
}

impl PartialEq for dyn BinaryOp {
    fn eq(&self, other: &Self) -> bool {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .eq(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl PartialEq for dyn TernaryOp {
    fn eq(&self, other: &Self) -> bool {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .eq(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl PartialOrd for dyn AssignOp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .partial_cmp(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl PartialOrd for dyn UnaryOp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.display(&Expr::ConstExpr(ConstExpr::Int(0)))
            .partial_cmp(&other.display(&Expr::ConstExpr(ConstExpr::Int(0))))
    }
}

impl PartialOrd for dyn BinaryOp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .partial_cmp(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl PartialOrd for dyn TernaryOp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .partial_cmp(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl Eq for dyn AssignOp {}
impl Eq for dyn UnaryOp {}
impl Eq for dyn BinaryOp {}
impl Eq for dyn TernaryOp {}

impl Ord for dyn AssignOp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .cmp(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl Ord for dyn UnaryOp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.display(&Expr::ConstExpr(ConstExpr::Int(0)))
            .cmp(&other.display(&Expr::ConstExpr(ConstExpr::Int(0))))
    }
}

impl Ord for dyn BinaryOp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .cmp(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl Ord for dyn TernaryOp {
    fn cmp(&self, other: &Self) -> Ordering {
        self.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        )
        .cmp(&other.display(
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
            &Expr::ConstExpr(ConstExpr::Int(0)),
        ))
    }
}

impl Clone for Box<dyn AssignOp> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl Clone for Box<dyn UnaryOp> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl Clone for Box<dyn BinaryOp> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

impl Clone for Box<dyn TernaryOp> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}
