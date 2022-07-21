use super::{super::Type, ConstExpr, GetType, Env, Error};
use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub enum Expr {
    ConstExpr(ConstExpr),
    Block(Vec<Self>),

    Let(String, Option<Type>, Box<Self>, Box<Self>),
    While(Box<Self>, Box<Self>),

    If(Box<Self>, Box<Self>, Box<Self>),
    When(ConstExpr, Box<Self>, Box<Self>),

    Refer(Box<Self>),
    Deref(Box<Self>),
    DerefMut(Box<Self>, Box<Self>),

    Apply(Box<Self>, Vec<Self>),
    Return(Box<Self>),

    Array(Vec<Self>),
    Tuple(Vec<Self>),
    Union(BTreeMap<String, Type>, String, Box<Self>),
    Struct(BTreeMap<String, Self>),

    Member(Box<Self>, ConstExpr),
    Index(Box<Self>, Box<Self>),
}

impl GetType for Expr {
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error> {
        let i = i + 1;

        Ok(match self {
            Self::ConstExpr(c) => c.get_type_checked(env, i)?,
            Self::Block(items) => {
                if let Some(expr) = items.last() {
                    expr.get_type_checked(env, i)?
                } else {
                    Type::None
                }
            }

            Self::Let(var, t, val, ret) => {
                let mut new_env = env.clone();
                new_env.def_var(var.clone(), match t {
                    Some(t) => t.clone(),
                    None => val.get_type_checked(env, i)?
                })?;

                ret.get_type_checked(&mut new_env, i)?
            }

            Self::While(_, _) => Type::Never,

            Self::If(_, t, _) => t.get_type_checked(env, i)?,
            Self::When(c, t, e) => if c.clone().as_bool(env)? {
                t
            } else {
                e
            }.get_type_checked(env, i)?,

            Self::Refer(expr) => {
                Type::Pointer(Box::new(expr.get_type_checked(env, i)?))
            }
            Self::Deref(expr) => {
                if let Type::Pointer(inner) = expr.get_type_checked(env, i)? {
                    *inner
                } else {
                    return Err(Error::DerefNonPointer(self.clone()))
                }
            },
            Self::DerefMut(_, _) => Type::None,

            Self::Return(_) => Type::Never,
            Self::Apply(func, _) => {
                if let Type::Proc(_, ret) = func.get_type_checked(env, i)? {
                    *ret
                } else {
                    return Err(Error::ApplyNonProc(self.clone()))
                }
            }

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
                Box::new(ConstExpr::Int(items.len() as i32)),
            ),
            Self::Struct(fields) => Type::Struct(
                fields.clone()
                    .into_iter()
                    .map(|(k, c)| Ok((k, c.get_type_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            Self::Union(types, variant, _) => {
                if let Some(t) = types.get(variant) {
                    t.clone()
                } else {
                    return Err(Error::MemberNotFound(
                        self.clone(),
                        ConstExpr::Symbol(variant.clone()),
                    ));
                }
            }

            Self::Member(val, field) => {
                let as_symbol = field.clone().as_symbol(env);
                let as_int = field.clone().as_int(env);
                match val.get_type_checked(env, i)? {
                    Type::Tuple(items) => {
                        let n = as_int? as usize;
                        if n < items.len() {
                            items[n].clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()))
                        }
                    }
                    Type::Struct(fields) => if let Some(t) = fields.get(&as_symbol?) {
                        t.clone()
                    } else {
                        return Err(Error::MemberNotFound(*val.clone(), field.clone()))
                    }
                    Type::Union(types) => if let Some(t) = types.get(&as_symbol?) {
                        t.clone()
                    } else {
                        return Err(Error::MemberNotFound(*val.clone(), field.clone()))
                    }

                    _ => return Err(Error::MemberNotFound(*val.clone(), field.clone()))
                }
            }

            Self::Index(val, _) => {
                match val.get_type_checked(env, i)? {
                    Type::Array(item, _) => *item,
                    Type::Pointer(item) => *item,

                    _ => return Err(Error::InvalidIndex(self.clone()))
                }
            }
        })
    }
}