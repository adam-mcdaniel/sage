use crate::asm::{AssemblyProgram, CoreOp, StandardOp, A, FP, SP};
use crate::ir::{
    Compile, CoreBuiltin, Env, Error, Expr, GetSize, GetType, Procedure, Simplify, StandardBuiltin,
    Type, TypeCheck,
};
use std::collections::BTreeMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ConstExpr {
    None,
    Null,
    Symbol(String),
    Int(i32),
    Float(f64),
    Char(char),
    Bool(bool),
    Of(Type, String),

    Tuple(Vec<Self>),
    Array(Vec<Self>),
    Struct(BTreeMap<String, Self>),
    Union(Type, String, Box<Self>),

    CoreBuiltin(CoreBuiltin),
    StandardBuiltin(StandardBuiltin),
    Proc(Procedure),
}

impl ConstExpr {
    pub fn proc(args: Vec<(String, Type)>, ret: Type, body: impl Into<Expr>) -> Self {
        Self::Proc(Procedure::new(args, ret, body))
    }

    pub fn app(self, args: Vec<Expr>) -> Expr {
        Expr::from(self).app(args)
    }

    pub fn eval(self, env: &Env) -> Result<Self, Error> {
        self.eval_checked(env, 0)
    }

    fn eval_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        let i = i + 1;
        if i > 500 {
            Err(Error::RecursionDepthConst(self))
        } else {
            match self {
                Self::None
                | Self::Null
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
                        Ok(Self::Symbol(name))
                    }
                }

                Self::Tuple(items) => Ok(Self::Tuple(
                    items
                        .into_iter()
                        .map(|c| c.eval_checked(env, i))
                        .collect::<Result<Vec<Self>, Error>>()?,
                )),
                Self::Array(items) => Ok(Self::Array(
                    items
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

impl TypeCheck for ConstExpr {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        match self {
            Self::None
            | Self::Null
            | Self::Int(_)
            | Self::Float(_)
            | Self::Char(_)
            | Self::Bool(_) => Ok(()),

            Self::CoreBuiltin(builtin) => builtin.type_check(env),
            Self::StandardBuiltin(builtin) => builtin.type_check(env),
            Self::Proc(proc) => proc.type_check(env),

            Self::Symbol(name) => {
                if env.consts.get(name).is_some() || env.get_var(name).is_some() {
                    Ok(())
                } else {
                    Err(Error::SymbolNotDefined(name.clone()))
                }
            }

            Self::Of(t, variant) => {
                if let Type::Enum(variants) = t {
                    if variants.contains(variant) {
                        Ok(())
                    } else {
                        Err(Error::VariantNotFound(t.clone(), variant.clone()))
                    }
                } else {
                    Err(Error::VariantNotFound(t.clone(), variant.clone()))
                }
            }

            Self::Tuple(items) => {
                for item in items {
                    item.type_check(env)?;
                }
                Ok(())
            }

            Self::Array(items) => {
                for item in items {
                    item.type_check(env)?;
                }
                Ok(())
            }

            Self::Struct(fields) => {
                for item in fields.values() {
                    item.type_check(env)?;
                }
                Ok(())
            }

            Self::Union(t, variant, val) => {
                if let Type::Union(fields) = t {
                    if fields.get(variant).is_some() {
                        val.type_check(env)?;
                        Ok(())
                    } else {
                        Err(Error::VariantNotFound(t.clone(), variant.clone()))
                    }
                } else {
                    Err(Error::VariantNotFound(t.clone(), variant.clone()))
                }
            }
        }
    }
}

impl Compile for ConstExpr {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        match self.eval(env)? {
            Self::None => {}
            Self::Null => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), 0));
            }
            Self::Char(ch) => {
                output.op(CoreOp::Comment(format!("push char {ch:?}")));
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), ch as usize as isize));
            }
            Self::Bool(x) => {
                output.op(CoreOp::Comment(format!("push bool {x}")));
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), x as isize));
            }
            Self::Int(n) => {
                output.op(CoreOp::Comment(format!("push int {n}")));
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), n as isize));
            }
            Self::Float(f) => {
                output.op(CoreOp::Next(SP, None));
                output.std_op(StandardOp::Set(SP.deref(), f))?;
            }
            Self::Tuple(items) => {
                for item in items {
                    item.compile_expr(env, output)?;
                }
            }
            Self::Array(items) => {
                for item in items {
                    item.compile_expr(env, output)?;
                }
            }
            Self::Struct(items) => {
                for (_, expr) in items {
                    expr.compile_expr(env, output)?;
                }
            }
            Self::Union(types, variant, val) => {
                let result_type = Self::Union(types, variant, val.clone()).get_type(env)?;
                let result_size = result_type.get_size(env)?;
                let val_size = val.get_size(env)?;

                val.compile_expr(env, output)?;
                output.op(CoreOp::Next(
                    SP,
                    Some(result_size as isize - val_size as isize),
                ));
            }
            Self::CoreBuiltin(builtin) => {
                builtin.compile_expr(env, output)?;
            }
            Self::StandardBuiltin(builtin) => {
                builtin.compile_expr(env, output)?;
            }
            Self::Proc(proc) => {
                proc.compile_expr(env, output)?;
            }
            Self::Of(enum_type, variant) => {
                if let Type::Enum(mut variants) = enum_type.clone().simplify(env)? {
                    variants.sort();
                    if let Ok(index) = variants.binary_search(&variant) {
                        output.op(CoreOp::Set(A, index as isize));
                        output.op(CoreOp::Push(A, 1));
                    } else {
                        return Err(Error::VariantNotFound(enum_type, variant));
                    }
                } else {
                    return Err(Error::VariantNotFound(enum_type, variant));
                }
            }

            Self::Symbol(name) => {
                if let Some((t, offset)) = env.get_var(&name) {
                    output.op(CoreOp::Comment(format!("load var '{}'", name)));
                    output.op(CoreOp::Push(FP.deref().offset(*offset), t.get_size(env)?))
                } else {
                    return Err(Error::SymbolNotDefined(name));
                }
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
            Self::Null => Type::Pointer(Box::new(Type::Any)),
            Self::None => Type::None,
            Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Char(_) => Type::Char,
            Self::Bool(_) => Type::Bool,
            Self::Of(enum_type, _) => enum_type,
            Self::Tuple(items) => Type::Tuple(
                items
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

            Self::Union(t, _, _) => t,

            Self::Proc(proc) => proc.get_type_checked(env, i)?,
            Self::CoreBuiltin(builtin) => builtin.get_type_checked(env, i)?,
            Self::StandardBuiltin(builtin) => builtin.get_type_checked(env, i)?,

            Self::Symbol(name) => {
                if let Some((t, _)) = env.get_var(&name) {
                    t.clone()
                } else {
                    return Err(Error::SymbolNotDefined(name));
                }
            }
        })
    }
}
