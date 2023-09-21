//! # Assignment Operations
//!
//! This module implements the `Assign` struct, which wraps a `BinaryOp` and
//! implements the `AssignOp` trait. This turns any binary operation into an
//! assignment operation (such as transforming `+` into `+=`).

use crate::{asm::AssemblyProgram, lir::*};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// An assignment operation. This is used to implement assignment operators like `+=`.
#[derive(Clone)]
pub struct Assign(pub Box<dyn BinaryOp>);

impl Assign {
    /// Create a new assignment operation.
    pub fn new(op: impl BinaryOp + 'static) -> Self {
        Self(Box::new(op))
    }
}

impl AssignOp for Assign {
    /// Can this binary operation be applied to the given types?
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        if let Type::Pointer(mutability, t) = lhs.clone().simplify(env)? {
            if mutability.is_mutable() {
                return self.0.can_apply(&t, rhs, env);
            }
        }
        Err(Error::InvalidAssignOpTypes(
            self.clone_box(),
            lhs.clone(),
            rhs.clone(),
        ))
    }

    /// Get the type of the result of applying this binary operation to the given types.
    fn return_type(&self, _lhs: &Expr, _rhs: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::None)
    }

    /// Evaluate this binary operation on the given constant values.
    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        self.0.eval(lhs, rhs, env)
    }

    /// Compile the assignment operation.
    fn compile(
        &self,
        lhs: &Expr,
        rhs: &Expr,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        // TODO: This is a bit of a hack.

        // Create temporary variables for the lhs and rhs.
        let expr = Expr::let_var(
            // Create the lhs variable.
            lhs.to_string(),
            Mutability::Any,
            None,
            lhs.clone(),
            Expr::let_var(
                // Create the rhs variable.
                rhs.to_string(),
                Mutability::Any,
                None,
                rhs.clone(),
                Expr::DerefMut(
                    // Assign the operation to the lhs.
                    Box::new(Expr::var(lhs.to_string())),
                    // Perform the operation.
                    Box::new(Expr::BinaryOp(
                        self.0.clone_box(),
                        Box::new(Expr::var(lhs.to_string()).deref()),
                        Box::new(rhs.clone()),
                    )),
                ),
            ), // Compile the operation.
        );

        expr.compile_expr(env, output)
    }

    /// Compile the assignment operation.
    fn compile_types(
        &self,
        _ptr_type: &Type,
        _val_type: &Type,
        _env: &mut Env,
        _output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        unimplemented!("Don't use this function, use compile instead")
    }

    /// Clone this operation into a trait object.
    fn clone_box(&self) -> Box<dyn AssignOp> {
        Box::new(self.clone())
    }
}

impl Debug for Assign {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}=", self.0)
    }
}

impl Display for Assign {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}=", self.0)
    }
}
