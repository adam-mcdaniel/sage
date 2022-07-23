use std::collections::BTreeMap;

use super::{ConstExpr, Env, Error, Expr, GetSize, Simplify};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Symbol(String),
    None,
    Int,
    Float,
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
            Type::Union(_) => Ok(0),
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
                    .map(|(k, t)| Ok((k.clone(), t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),
            Self::Union(types) => Self::Union(
                types
                    .into_iter()
                    .map(|(k, t)| Ok((k.clone(), t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),
        })
    }
}
