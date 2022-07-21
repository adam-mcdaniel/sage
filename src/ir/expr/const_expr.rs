use super::{
    super::{Simplify, Type},
    Compile, CoreBuiltin, GetType, GetSize, Env, Error, Expr, GetType, Procedure, StandardBuiltin,
};
use crate::asm::{AssemblyProgram, CoreOp, StandardOp, A, B, C, FP, SP};
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub enum ConstExpr {
    None,
    Symbol(String),
    Int(i32),
    Float(f64),
    Char(char),
    Bool(bool),
    Of(Type, String),

    Tuple(Vec<Self>),
    Array(Vec<Self>),
    Struct(BTreeMap<String, Self>),
    Union(BTreeMap<String, Type>, String, Box<Self>),

    CoreBuiltin(CoreBuiltin),
    StandardBuiltin(StandardBuiltin),
    Proc(Procedure),
}

impl ConstExpr {
    pub fn eval(self, env: &Env) -> Result<Self, Error> {
        self.eval_checked(env, 0)
    }

    fn eval_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        let i = i + 1;
        if i > 1000 {
            Err(Error::RecursionDepthConst(self))
        } else {
            match self {
                Self::None
                | Self::Int(_)
                | Self::Float(_)
                | Self::Char(_)
                | Self::Bool(_)
                | Self::Of(_, _)
                | Self::CoreBuiltin(_)
                | Self::StandardBuiltin(_)
                | Self::Proc(_) => Ok(self),

                Self::Symbol(name) => {
                    if let Some(c) = env.consts.get(&name) {
                        Ok(c.clone())
                    } else {
                        Err(Error::ConstNotDefined(name))
                    }
                }

                Self::Tuple(items) => Ok(Self::Tuple(
                    items
                        .clone()
                        .into_iter()
                        .map(|c| c.eval_checked(env, i))
                        .collect::<Result<Vec<Self>, Error>>()?,
                )),
                Self::Array(items) => Ok(Self::Array(
                    items
                        .clone()
                        .into_iter()
                        .map(|c| c.eval_checked(env, i))
                        .collect::<Result<Vec<Self>, Error>>()?,
                )),
                Self::Struct(fields) => Ok(Self::Struct(
                    fields
                        .into_iter()
                        .map(|(k, c)| Ok((k, c.eval_checked(env, i)?)))
                        .collect::<Result<BTreeMap<String, Self>, Error>>()?,
                )),
                Self::Union(types, variant, val) => Ok(Self::Union(
                    types,
                    variant,
                    Box::new(val.eval_checked(env, i)?),
                )),
            }
        }
    }

    pub fn as_int(self, env: &Env) -> Result<i32, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Int(n)) => Ok(n),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    pub fn as_bool(self, env: &Env) -> Result<bool, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Bool(b)) => Ok(b),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    pub fn as_symbol(self, env: &Env) -> Result<String, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Symbol(name)) => Ok(name),
            Ok(other) => Err(Error::NonSymbol(other)),
            Err(err) => Err(err),
        }
    }
}


impl Compile for ConstExpr {
    fn compile(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        match self.eval(env)? {
            Self::None => {},
            Self::Char(ch) => {
                output.op(CoreOp::Set(A, ch as usize as isize));
                output.op(CoreOp::Push(A, 1));
            }
            Self::Bool(x) => {
                output.op(CoreOp::Set(A, x as isize));
                output.op(CoreOp::Push(A, 1));
            }
            Self::Int(n) => {
                output.op(CoreOp::Set(A, n as isize));
                output.op(CoreOp::Push(A, 1));
            }
            Self::Float(f) => {
                output.std_op(StandardOp::Set(A, f));
                output.op(CoreOp::Push(A, 1));
            }
            Self::Of(enum_type, variant) => {
                if let Type::Enum(variants) = enum_type.simplify(env)? {

                }
                // output.op(StandardOp::Set(A, variant));
                // output.op(CoreOp::Push(A, 1));
            }

            Self::Symbol(name) => if let Some((t, offset)) = env.get_var(&name) {
                output.op(CoreOp::Many(vec![
                    CoreOp::Move { src: FP, dst: A },
                    CoreOp::Set(B, *offset),
                    CoreOp::Index { src: A, offset: B, dst: C },
                    CoreOp::Push(C.deref(), t.get_size(env)?)
                ]))
            } else {
                return Err(Error::SymbolNotDefined(name))
            }
        }
        Ok(())
    }
}


impl Simplify for ConstExpr {
    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        self.eval_checked(env, i)
    }
}

impl GetType for ConstExpr {
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error> {
        Ok(match self.clone().eval(env)? {
            Self::None => Type::None,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Char(_) => Type::Char,
            Self::Bool(_) => Type::Bool,
            Self::Of(enum_type, _) => enum_type,
            Self::Tuple(items) => Type::Tuple(
                items
                    .clone()
                    .into_iter()
                    .map(|c| c.get_type_checked(env, i))
                    .collect::<Result<Vec<Type>, Error>>()?,
            ),
            Self::Array(items) => Type::Array(
                Box::new(if !items.is_empty() {
                    items[0].get_type_checked(env, i)?
                } else {
                    Type::Any
                }),
                Box::new(Self::Int(items.len() as i32)),
            ),
            Self::Struct(fields) => Type::Struct(
                fields
                    .into_iter()
                    .map(|(k, c)| Ok((k, c.get_type_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            Self::Union(types, variant, val) => {
                if let Some(t) = types.get(&variant) {
                    t.clone()
                } else {
                    return Err(Error::MemberNotFound(
                        Expr::ConstExpr(self.clone()),
                        ConstExpr::Symbol(variant),
                    ));
                }
            }

            Self::Proc(proc) => proc.get_type_checked(env, i)?,
            Self::CoreBuiltin(builtin) => builtin.get_type_checked(env, i)?,
            Self::StandardBuiltin(builtin) => builtin.get_type_checked(env, i)?,

            Self::Symbol(name) => return Err(Error::ConstNotDefined(name)),
        })
    }
}
