use super::*;
use crate::asm::{CoreOp, StandardOp, SP, A};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct New;

impl UnaryOp for New {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        Ok(ty.get_size(env)? > 0)
    }

    /// Get the type of the result of applying this unary operation to the given type.
    fn return_type(&self, expr: &Expr, env: &Env) -> Result<Type, Error> {
        let ty = expr.get_type(env)?;
        Ok(Type::Pointer(Box::new(ty)))
    }

    /// Evaluate this unary operation on the given constant values.
    fn eval(&self, expr: &ConstExpr, _env: &mut Env) -> Result<ConstExpr, Error> {
        Err(Error::InvalidConstExpr(expr.clone()))
    }

    /// Compile the unary operation.
    fn compile_types(
        &self,
        ty: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        let size = ty.get_size(env)?;

        // output.op(CoreOp::Next(SP, None));
        output.op(CoreOp::Set(A, size as isize));
        output.std_op(StandardOp::Alloc(A)).map_err(|_| Error::UnsupportedOperation(Expr::UnaryOp(self.clone_box(), Box::new(Expr::ConstExpr(ConstExpr::None)))))?;
        output.op(CoreOp::Copy {
            dst: A.deref(),
            src: SP.deref().offset(1 - size as isize),
            size,
        });
        output.op(CoreOp::Pop(None, size));
        output.op(CoreOp::Push(A, 1));
        // output.op(CoreOp::Pop(Some(A.deref()), size));
        // output.op(CoreOp::Push(A, 1));
        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for New {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "new")
    }
}

impl Display for New {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "new")
    }
}



#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Delete;

impl UnaryOp for Delete {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        ty.equals(&Type::Pointer(Box::new(Type::Any)), env)
    }

    /// Get the type of the result of applying this unary operation to the given type.
    fn return_type(&self, _expr: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::None)
    }

    /// Evaluate this unary operation on the given constant values.
    fn eval(&self, expr: &ConstExpr, _env: &mut Env) -> Result<ConstExpr, Error> {
        Err(Error::InvalidConstExpr(expr.clone()))
    }

    /// Compile the unary operation.
    fn compile_types(
        &self,
        _ty: &Type,
        _env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        output.std_op(StandardOp::Free(SP.deref())).map_err(|_| Error::UnsupportedOperation(Expr::UnaryOp(self.clone_box(), Box::new(Expr::ConstExpr(ConstExpr::None)))))?;
        output.op(CoreOp::Pop(None, 1));
        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for Delete {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "del")
    }
}

impl Display for Delete {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "del")
    }
}
