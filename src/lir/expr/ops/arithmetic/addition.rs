use crate::{
    asm::{AssemblyProgram, CoreOp, StandardOp, A, SP},
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::collections::BTreeMap;

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Add;

impl Add {
    fn return_type_from_types(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<Type, Error> {
        match (lhs.clone(), rhs.clone()) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Float, Type::Float) | (Type::Float, Type::Int) | (Type::Int, Type::Float) => {
                Ok(Type::Float)
            }
            (Type::Array(t1, size1), Type::Array(t2, size2)) => {
                if t1.equals(&t2, env)? {
                    Ok(Type::Array(
                        t1,
                        Box::new(self.eval(&size1, &size2, &mut env.clone())?),
                    ))
                } else {
                    Err(Error::InvalidBinaryOpTypes(
                        self.clone_box(),
                        lhs.clone(),
                        rhs.clone(),
                    ))
                }
            }
            (Type::Struct(elems1), Type::Struct(elems2)) => {
                for key in elems1.keys() {
                    if elems2.contains_key(key) {
                        // Cannot already have the same member
                        return Err(Error::InvalidBinaryOpTypes(
                            self.clone_box(),
                            lhs.clone(),
                            rhs.clone(),
                        ));
                    }
                }
                Ok(Type::Struct(
                    elems1.into_iter().chain(elems2.into_iter()).collect(),
                ))
            }
            (Type::Tuple(elems1), Type::Tuple(elems2)) => Ok(Type::Tuple(
                elems1.into_iter().chain(elems2.into_iter()).collect(),
            )),
            _ => Err(Error::InvalidBinaryOpTypes(
                self.clone_box(),
                lhs.clone(),
                rhs.clone(),
            )),
        }
    }
}

impl BinaryOp for Add {
    fn can_apply(&self, lhs: &Type, rhs: &Type, env: &Env) -> Result<bool, Error> {
        self.return_type_from_types(lhs, rhs, env).map(|_| true)
    }

    fn return_type(&self, lhs: &Expr, rhs: &Expr, env: &Env) -> Result<Type, Error> {
        let ty = lhs.get_type(env)?;
        self.return_type_from_types(&ty, &rhs.get_type(env)?, env)
    }

    fn eval(&self, lhs: &ConstExpr, rhs: &ConstExpr, env: &mut Env) -> Result<ConstExpr, Error> {
        // let result = x.clone().eval(env)?;
        // let ty = result.get_type(env)?;

        Ok(match (lhs.clone().eval(env)?, rhs.clone().eval(env)?) {
            (ConstExpr::Int(a), ConstExpr::Int(b)) => ConstExpr::Int(a + b),
            (ConstExpr::Float(a), ConstExpr::Int(b)) | (ConstExpr::Int(b), ConstExpr::Float(a)) => {
                ConstExpr::Float(a + b as f64)
            }
            (ConstExpr::Float(a), ConstExpr::Float(b)) => ConstExpr::Float(a + b),
            (ConstExpr::Array(mut a), ConstExpr::Array(mut b)) => {
                a.append(&mut b);
                ConstExpr::Array(a)
            }
            (ConstExpr::Tuple(mut a), ConstExpr::Tuple(mut b)) => {
                a.append(&mut b);
                ConstExpr::Tuple(a)
            }
            (ConstExpr::Struct(mut a), ConstExpr::Struct(mut b)) => {
                a.append(&mut b);
                ConstExpr::Struct(a)
            }
            _ => {
                return Err(Error::InvalidBinaryOp(
                    self.clone_box(),
                    Expr::ConstExpr(lhs.clone()),
                    Expr::ConstExpr(rhs.clone()),
                ))
            }
        })
    }

    // fn compile_types(&self, ty: &Type, env: &mut Env) -> Result<AssemblyProgram, Error> {}
    fn compile_types(
        &self,
        lhs: &Type,
        rhs: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        match (lhs.clone().simplify(env)?, rhs.clone().simplify(env)?) {
            (Type::Int, Type::Int) => {
                output.op(CoreOp::Add {
                    src: SP.deref(),
                    dst: SP.deref().offset(-1),
                });
                output.op(CoreOp::Pop(None, 1))
            }
            (Type::Float, Type::Float) => {
                output.std_op(StandardOp::Add {
                    src: SP.deref(),
                    dst: SP.deref().offset(-1),
                })?;
                output.op(CoreOp::Pop(None, 1))
            }
            (Type::Int, Type::Float) => {
                output.std_op(StandardOp::ToFloat(SP.deref().offset(-1)))?;
                output.std_op(StandardOp::Add {
                    src: SP.deref(),
                    dst: SP.deref().offset(-1),
                })?;
                output.op(CoreOp::Pop(None, 1))
            }
            (Type::Float, Type::Int) => {
                output.std_op(StandardOp::ToFloat(SP.deref()))?;
                output.std_op(StandardOp::Add {
                    src: SP.deref(),
                    dst: SP.deref().offset(-1),
                })?;
                output.op(CoreOp::Pop(None, 1))
            }
            (Type::Array(_, _), Type::Array(_, _)) | (Type::Tuple(_), Type::Tuple(_)) => {
                // Nothing to do!
            }
            (Type::Struct(lhs_fields), Type::Struct(rhs_fields)) => {
                // Get the new struct type so we can actually set the elements in the right layout
                let result = self.return_type_from_types(lhs, rhs, env)?;

                // Get the offsets of each element
                let mut lhs_offsets = BTreeMap::new();
                let mut rhs_offsets = BTreeMap::new();

                let mut offset = 0;
                for (key, ty) in lhs_fields {
                    lhs_offsets.insert(key, offset);
                    offset += ty.get_size(env)?;
                }
                offset = 0;
                for (key, ty) in rhs_fields {
                    rhs_offsets.insert(key, offset);
                    offset += ty.get_size(env)?;
                }

                let lhs_size = lhs.get_size(env)?;
                let rhs_size = rhs.get_size(env)?;
                output.op(CoreOp::Move { src: SP, dst: A });
                let rhs_bp = A.deref().offset(1 - rhs_size as isize);
                let lhs_bp = rhs_bp.offset(-(lhs_size as isize));

                if let Type::Struct(result_fields) = &result {
                    for (name, ty) in result_fields {
                        if let Some(offset) = lhs_offsets.get(name) {
                            output.op(CoreOp::Push(
                                lhs_bp.offset(*offset as isize),
                                ty.clone().get_size(env)?,
                            ));
                        } else if let Some(offset) = rhs_offsets.get(name) {
                            output.op(CoreOp::Push(
                                rhs_bp.offset(*offset as isize),
                                ty.clone().get_size(env)?,
                            ));
                        } else {
                            return Err(Error::InvalidBinaryOpTypes(
                                self.clone_box(),
                                lhs.clone(),
                                rhs.clone(),
                            ));
                        }
                    }

                    output.op(CoreOp::Copy {
                        src: SP.deref().offset(1 - result.get_size(env)? as isize),
                        dst: lhs_bp,
                        size: result.get_size(env)?,
                    });
                    output.op(CoreOp::Pop(
                        None,
                        lhs_size + rhs_size - result.get_size(env)?,
                    ));
                    return Ok(());
                }

                return Err(Error::InvalidBinaryOpTypes(
                    self.clone_box(),
                    lhs.clone(),
                    rhs.clone(),
                ));
            }
            _ => {
                return Err(Error::InvalidBinaryOpTypes(
                    self.clone_box(),
                    lhs.clone(),
                    rhs.clone(),
                ))
            }
        }

        Ok(())
    }

    /// Compiles the operation on the given expressions.
    fn compile(
        &self,
        lhs: &Expr,
        rhs: &Expr,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        let lhs_type = lhs.get_type(env)?;
        let rhs_type = rhs.get_type(env)?;

        match (lhs.clone(), rhs.clone()) {
            (Expr::ConstExpr(lhs), Expr::ConstExpr(rhs)) => return self.eval(&lhs, &rhs, env)?.compile_expr(env, output),
            (Expr::ConstExpr(lhs), rhs) => match (lhs.eval(env)?, &rhs_type) {
                (ConstExpr::Int(lhs), Type::Int | Type::Cell) => {
                    rhs.compile_expr(env, output)?;
                    output.op(CoreOp::Set(A, lhs));
                    output.op(CoreOp::Add {
                        src: A,
                        dst: SP.deref(),
                    });
                    return Ok(());
                }
                (ConstExpr::Float(lhs), Type::Float) => {
                    rhs.compile_expr(env, output)?;
                    output.std_op(StandardOp::Set(A, lhs))?;
                    output.std_op(StandardOp::Add {
                        src: A,
                        dst: SP.deref(),
                    })?;
                    return Ok(());
                }
                (ConstExpr::Int(lhs), Type::Float) => {
                    rhs.compile_expr(env, output)?;
                    output.std_op(StandardOp::Set(A, lhs as f64))?;
                    output.std_op(StandardOp::Add {
                        src: A,
                        dst: SP.deref(),
                    })?;
                    return Ok(());
                }
                (ConstExpr::Float(lhs), Type::Int | Type::Cell) => {
                    rhs.compile_expr(env, output)?;
                    output.std_op(StandardOp::ToFloat(SP.deref()))?;
                    output.std_op(StandardOp::Set(A, lhs))?;
                    output.std_op(StandardOp::Add {
                        src: A,
                        dst: SP.deref(),
                    })?;
                    return Ok(());
                }
                _ => {}
            },
            (lhs, Expr::ConstExpr(rhs)) => match (&lhs_type, rhs.eval(env)?) {
                (Type::Int | Type::Cell, ConstExpr::Int(rhs)) => {
                    lhs.compile_expr(env, output)?;
                    output.op(CoreOp::Set(A, rhs));
                    output.op(CoreOp::Add {
                        src: A,
                        dst: SP.deref(),
                    });
                    return Ok(());
                }
                (Type::Float, ConstExpr::Float(rhs)) => {
                    lhs.compile_expr(env, output)?;
                    output.std_op(StandardOp::Set(A, rhs))?;
                    output.std_op(StandardOp::Add {
                        src: A,
                        dst: SP.deref(),
                    })?;
                    return Ok(());
                }
                (Type::Float, ConstExpr::Int(rhs)) => {
                    lhs.compile_expr(env, output)?;
                    output.std_op(StandardOp::Set(A, rhs as f64))?;
                    output.std_op(StandardOp::Add {
                        src: A,
                        dst: SP.deref(),
                    })?;
                    return Ok(());
                }
                (Type::Int | Type::Cell, ConstExpr::Float(rhs)) => {
                    lhs.compile_expr(env, output)?;
                    output.std_op(StandardOp::ToFloat(SP.deref()))?;
                    output.std_op(StandardOp::Set(A, rhs))?;
                    output.std_op(StandardOp::Add {
                        src: A,
                        dst: SP.deref(),
                    })?;
                    return Ok(());
                }
                _ => {}
            },
            _ => {}
        }

        lhs.clone().compile_expr(env, output)?;
        rhs.clone().compile_expr(env, output)?;
        self.compile_types(&lhs_type, &rhs_type, env, output)
    }

    fn clone_box(&self) -> Box<dyn BinaryOp> {
        Box::new(*self)
    }
}

impl Display for Add {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "+")
    }
}

impl Debug for Add {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "+")
    }
}
