use std::collections::BTreeMap;

use super::{ConstExpr, Env, Error, Expr, GetSize, Simplify};

pub trait TypeCheck {
    fn type_check(&self, env: &Env) -> Result<(), Error>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Symbol(String),
    None,
    Int,
    Float,
    Cell,
    Char,
    Bool,
    Enum(Vec<String>),
    Tuple(Vec<Self>),
    Array(Box<Self>, Box<ConstExpr>),
    Struct(BTreeMap<String, Self>),
    Union(BTreeMap<String, Self>),
    Proc(Vec<Type>, Box<Type>),
    Pointer(Box<Self>),
    Any,
    Never,
}

impl Type {
    pub fn can_cast_to(&self, other: &Self, env: &Env) -> Result<bool, Error> {
        self.can_cast_to_checked(other, env, 0)
    }

    fn can_cast_to_checked(&self, other: &Self, env: &Env, i: usize) -> Result<bool, Error> {
        if self == other {
            return Ok(true);
        }

        let i = i + 1;
        if i > 500 {
            return Err(Error::RecursionDepthTypeEquality(
                self.clone(),
                other.clone(),
            ));
        }

        match (self, other) {
            (Self::Symbol(a), Self::Symbol(b)) if a == b => Ok(true),

            (Self::Int, Self::Float) | (Self::Float, Self::Int) => Ok(true),
            (Self::Int, Self::Char) | (Self::Char, Self::Int) => Ok(true),
            (Self::Int, Self::Bool) | (Self::Bool, Self::Int) => Ok(true),
            (Self::Int, Self::Enum(_)) | (Self::Enum(_), Self::Int) => Ok(true),

            (Self::Cell, Self::Int) | (Self::Int, Self::Cell) => Ok(true),
            (Self::Cell, Self::Float) | (Self::Float, Self::Cell) => Ok(true),
            (Self::Cell, Self::Char) | (Self::Char, Self::Cell) => Ok(true),
            (Self::Cell, Self::Bool) | (Self::Bool, Self::Cell) => Ok(true),

            (Self::Any, _) | (_, Self::Any) => Ok(true),

            (Self::Proc(args1, ret1), Self::Proc(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return Ok(false);
                }
                for (a, b) in args1.iter().zip(args2.iter()) {
                    if !a.can_cast_to_checked(b, env, i)? {
                        return Ok(false);
                    }
                }
                ret1.can_cast_to_checked(ret2, env, i)
            }

            (a, b) => {
                if a.equals(b, env)? {
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    pub fn equals(&self, other: &Self, env: &Env) -> Result<bool, Error> {
        self.equals_checked(other, env, 0)
    }

    /// Are two types structurally equal?
    /// This function will always terminate.
    fn equals_checked(&self, other: &Self, env: &Env, i: usize) -> Result<bool, Error> {
        if self == other {
            return Ok(true);
        }

        let i = i + 1;
        if i > 500 {
            return Err(Error::RecursionDepthTypeEquality(
                self.clone(),
                other.clone(),
            ));
        }

        Ok(match (self, other) {
            (Self::Symbol(a), Self::Symbol(b)) => {
                a == b || self.clone().simplify(env)?.equals_checked(other, env, i)?
            }
            (Self::Symbol(x), y) | (y, Self::Symbol(x)) => Self::Symbol(x.clone())
                .simplify(env)?
                .equals_checked(y, env, i)?,
            (Self::Any, _)
            | (_, Self::Any)
            | (Self::Never, _)
            | (_, Self::Never)
            | (Self::None, Self::None)
            | (Self::Bool, Self::Bool)
            | (Self::Char, Self::Char)
            | (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::Cell, Self::Cell) => true,

            (Self::Enum(a), Self::Enum(b)) => {
                let mut a = a.clone();
                let mut b = b.clone();
                a.sort();
                b.sort();
                a == b
            }
            (Self::Tuple(a), Self::Tuple(b)) => {
                for (item1, item2) in a.iter().zip(b.iter()) {
                    if !item1.equals_checked(item2, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }
            (Self::Array(t1, size1), Self::Array(t2, size2)) => {
                t1.equals_checked(t2, env, i)?
                    && size1.clone().as_int(env)? == size2.clone().as_int(env)?
            }
            (Self::Struct(a), Self::Struct(b)) => {
                for ((name1, item1), (name2, item2)) in a.iter().zip(b.iter()) {
                    if name1 != name2 || !item1.equals_checked(item2, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }

            (Self::Union(a), Self::Union(b)) => {
                for ((name1, item1), (name2, item2)) in a.iter().zip(b.iter()) {
                    if name1 != name2 || !item1.equals_checked(item2, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }

            (Self::Proc(args1, ret1), Self::Proc(args2, ret2)) => {
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if !arg1.equals_checked(arg2, env, i)? {
                        return Ok(false);
                    }
                }
                ret1.equals_checked(ret2, env, i)?
            }

            (Self::Pointer(t1), Self::Pointer(t2)) => t1.equals_checked(t2, env, i)?,

            _ => false,
        })
    }

    pub(super) fn get_member_offset(
        &self,
        member: &ConstExpr,
        expr: &Expr,
        env: &Env,
    ) -> Result<usize, Error> {
        match self {
            Type::Struct(members) => {
                let mut offset = 0;
                for (k, t) in members.clone() {
                    if &ConstExpr::Symbol(k) == member {
                        return Ok(offset);
                    }

                    let size = t.get_size(env)?;
                    offset += size;
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Tuple(items) => {
                let mut offset = 0;
                for (i, t) in items.iter().enumerate() {
                    if &ConstExpr::Int(i as i32) == member {
                        return Ok(offset);
                    }

                    let size = t.get_size(env)?;
                    offset += size;
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Union(types) if types.contains_key(&member.clone().as_symbol(env)?) => Ok(0),
            _ => Err(Error::MemberNotFound(expr.clone(), member.clone())),
        }
    }
}

impl GetSize for Type {
    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error> {
        let i = i + 1;
        Ok(match self {
            Self::None | Self::Never => 0,
            Self::Any => return Err(Error::UnsizedType(self.clone())),

            Self::Symbol(name) => {
                if let Some(t) = env.types.get(name) {
                    t.get_size_checked(env, i)?
                } else {
                    return Err(Error::TypeNotDefined(name.clone()));
                }
            }

            Self::Int
            | Self::Float
            | Self::Char
            | Self::Bool
            | Self::Cell
            | Self::Enum(_)
            | Self::Pointer(_)
            | Self::Proc(_, _) => 1,

            Self::Tuple(items) => items.iter().flat_map(|t| t.get_size_checked(env, i)).sum(),
            Self::Array(elem, size) => {
                elem.get_size_checked(env, i)? * size.clone().as_int(env)? as usize
            }
            Self::Struct(fields) => fields
                .iter()
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                .sum(),
            Self::Union(types) => types
                .iter()
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                .max()
                .unwrap_or(0),
        })
    }
}

impl Simplify for Type {
    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        let i = i + 1;
        Ok(match self {
            Self::Symbol(name) => {
                if let Some(t) = env.types.get(&name) {
                    t.clone().simplify_checked(env, i)?
                } else {
                    return Err(Error::TypeNotDefined(name.clone()));
                }
            }

            Self::None
            | Self::Never
            | Self::Any
            | Self::Int
            | Self::Float
            | Self::Char
            | Self::Bool
            | Self::Cell
            | Self::Enum(_) => self.clone(),
            Self::Pointer(inner) => Self::Pointer(Box::new(inner.simplify_checked(env, i)?)),

            Self::Proc(args, ret) => Self::Proc(
                args.into_iter()
                    .flat_map(|t| t.simplify_checked(env, i))
                    .collect(),
                Box::new(ret.simplify_checked(env, i)?),
            ),

            Self::Tuple(items) => Self::Tuple(
                items
                    .into_iter()
                    .flat_map(|t| t.simplify_checked(env, i))
                    .collect(),
            ),
            Self::Array(inner, size) => Self::Array(
                Box::new(inner.simplify_checked(env, i)?),
                Box::new(size.eval(env)?),
            ),
            Self::Struct(fields) => Self::Struct(
                fields
                    .into_iter()
                    .map(|(k, t)| Ok((k, t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),
            Self::Union(types) => Self::Union(
                types
                    .into_iter()
                    .map(|(k, t)| Ok((k, t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),
        })
    }
}
