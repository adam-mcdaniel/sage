use super::{super::Type, Compile, ConstExpr, Env, Error, GetSize, GetType};
use crate::asm::{AssemblyProgram, CoreOp, A, B, C, FP, SP};
use std::collections::BTreeMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    ConstExpr(ConstExpr),
    Many(Vec<Self>),

    Const(String, ConstExpr, Box<Self>),
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
    Union(Type, String, Box<Self>),
    Struct(BTreeMap<String, Self>),

    Member(Box<Self>, ConstExpr),
    Index(Box<Self>, Box<Self>),
}

impl From<ConstExpr> for Expr {
    fn from(c: ConstExpr) -> Self {
        Expr::ConstExpr(c)
    }
}

impl Expr {
    pub fn field(self, field: ConstExpr) -> Self {
        Expr::Member(Box::new(self), field)
    }

    pub fn idx(self, idx: impl Into<Self>) -> Self {
        Expr::Index(Box::new(self), Box::new(idx.into()))
    }

    pub fn let_var(
        var: impl ToString,
        t: Option<Type>,
        e: impl Into<Self>,
        ret: impl Into<Self>,
    ) -> Self {
        Expr::Let(var.to_string(), t, Box::new(e.into()), Box::new(ret.into()))
    }

    pub fn let_vars(vars: BTreeMap<&str, (Option<Type>, Self)>, ret: impl Into<Self>) -> Self {
        let mut result = ret.into();
        for (var, (t, val)) in vars {
            result = Expr::Let(var.to_string(), t, Box::new(val), Box::new(result));
        }
        result
    }

    pub fn structure(vars: BTreeMap<&str, Self>) -> Self {
        let mut result = BTreeMap::new();
        for (var, val) in vars {
            result.insert(var.to_string(), val);
        }
        Self::Struct(result)
    }

    pub fn var(var: impl ToString) -> Self {
        Expr::ConstExpr(ConstExpr::Symbol(var.to_string()))
    }

    pub fn app(self, args: Vec<Self>) -> Self {
        Expr::Apply(Box::new(self), args)
    }

    pub fn if_then(self, t: impl Into<Self>, e: impl Into<Self>) -> Self {
        Expr::If(Box::new(self), Box::new(t.into()), Box::new(e.into()))
    }

    pub fn while_loop(self, body: impl Into<Self>) -> Self {
        Expr::While(Box::new(self), Box::new(body.into()))
    }

    pub fn refer(self) -> Self {
        Expr::Refer(Box::new(self))
    }

    pub fn deref(self) -> Self {
        Expr::Deref(Box::new(self))
    }

    pub fn deref_mut(self, e: impl Into<Self>) -> Self {
        Expr::DerefMut(Box::new(self), Box::new(e.into()))
    }
}

impl Compile for Expr {
    fn compile(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        match self {
            Self::ConstExpr(expr) => expr.compile(env, output)?,
            Self::Many(exprs) => {
                for expr in exprs {
                    expr.compile(env, output)?;
                }
            }

            Self::Const(name, expr, body) => {
                let mut new_env = env.clone();
                new_env.consts.insert(name, expr);
                body.compile(&mut new_env, output)?;
            }
            Self::Apply(f, args) => match *f {
                Expr::ConstExpr(ConstExpr::CoreBuiltin(builtin)) => {
                    for arg in args {
                        arg.compile(env, output)?;
                    }
                    builtin.compile(env, output)?;
                }
                Expr::ConstExpr(ConstExpr::StandardBuiltin(builtin)) => {
                    for arg in args {
                        arg.compile(env, output)?;
                    }
                    builtin.compile(env, output)?;
                }
                proc => {
                    for arg in args {
                        arg.compile(env, output)?;
                    }
                    proc.compile(env, output)?;
                    output.op(CoreOp::Pop(Some(A), 1));
                    output.op(CoreOp::Call(A));
                }
            },
            Self::Return(e) => {
                let args_size = env.get_args_size();
                let ret_size = e.get_size(env)?;
                // Execute the body to leave the return value
                e.compile(env, output)?;

                // Overwrite the arguments with the return value
                output.op(CoreOp::Copy {
                    dst: FP.deref().offset(1 - args_size as isize),
                    src: SP.deref().offset(1 - ret_size as isize),
                    size: ret_size,
                });
                // Decrement the stack pointer by the difference between the size of the
                // arguments and return value, to leave the return value on the stack.
                output.op(CoreOp::Prev(
                    SP,
                    Some(args_size as isize - ret_size as isize),
                ));
                output.op(CoreOp::Return);
            }
            Self::Let(name, specifier, e, body) => {
                // Get the type of the variable using its specifier,
                // or by deducing the type ourselves.
                let t = if let Some(t) = specifier {
                    t
                } else {
                    e.get_type(env)?
                };
                output.op(CoreOp::Comment(format!("begin let '{name}' val")));
                // Compile the expression to leave the value on the stack.
                e.compile(env, output)?;
                output.op(CoreOp::Comment(format!("begin let '{name}' body")));

                // Create a new scope
                let mut new_env = env.clone();
                // Get the size of the variable we are writing to.
                let var_size = t.get_size(env)?;
                new_env.def_var(name.clone(), t)?;

                let result_type = body.get_type(&new_env)?;
                let result_size = result_type.get_size(&new_env)?;
                // Compile the body under the new scope
                body.compile(&mut new_env, output)?;

                output.op(CoreOp::Comment(format!("destruct '{name}'")));
                output.op(CoreOp::Copy {
                    src: SP.deref().offset(1 - result_size as isize),
                    dst: SP
                        .deref()
                        .offset(1 - var_size as isize - result_size as isize),
                    size: result_size,
                });
                output.op(CoreOp::Pop(None, var_size));
                output.op(CoreOp::Comment(format!("end let '{name}'")));
            }
            Self::While(cond, body) => {
                // Eval the condition
                cond.clone().compile(env, output)?;
                output.op(CoreOp::Pop(Some(A), 1));
                // While the condition
                output.op(CoreOp::While(A));
                // Compile the body
                body.compile(env, output)?;
                // Eval the condition again
                cond.compile(env, output)?;
                output.op(CoreOp::Pop(Some(A), 1));
                // Label the end of the loop
                output.op(CoreOp::End);
            }
            Self::If(c, t, e) => {
                // Compile the condition
                c.clone().compile(env, output)?;
                output.op(CoreOp::Pop(Some(A), 1));
                // If the condition is true
                output.op(CoreOp::If(A));
                // Compile the true branch
                t.compile(env, output)?;
                // If the condition is false
                output.op(CoreOp::Else);
                // Compile the false branch
                e.compile(env, output)?;
                // Label the end of the if statement
                output.op(CoreOp::End);
            }
            Self::When(c, t, e) => if c.as_bool(env)? { t } else { e }.compile(env, output)?,

            Self::Deref(ptr) => {
                // Compile the pointer
                let ptr_type = ptr.get_type(env)?;
                ptr.clone().compile(env, output)?;
                if let Type::Pointer(inner) = ptr_type {
                    output.op(CoreOp::Pop(Some(A), 1));
                    output.op(CoreOp::Push(A.deref(), inner.get_size(env)?));
                } else {
                    return Err(Error::DerefNonPointer(*ptr));
                }
            }
            Self::DerefMut(ptr, val) => {
                // Push the value to the stack
                let val_type = val.get_type(env)?;
                val.compile(env, output)?;

                // Compile the pointer
                ptr.compile(env, output)?;

                // Get the size of the value to store
                let size = val_type.get_size(env)?;
                // Pop the pointer into A
                output.op(CoreOp::Pop(Some(A), 1));
                // Copy the result of the compiled value into the pointer
                output.op(CoreOp::Copy {
                    src: SP.deref().offset(1 - size as isize),
                    dst: A.deref(),
                    size,
                });
                // Pop the value off the stack
                output.op(CoreOp::Pop(None, size));
            }

            Self::Array(elems) => {
                // Compile the elements
                for elem in elems {
                    elem.compile(env, output)?;
                }
            }

            Self::Tuple(items) => {
                // Compile the items
                for item in items {
                    item.compile(env, output)?;
                }
            }

            Self::Struct(items) => {
                // Compile the items
                for (_, val) in items {
                    val.compile(env, output)?;
                }
            }

            Self::Union(types, variant, val) => {
                let result_type =
                    Self::Union(types.clone(), variant.clone(), val.clone()).get_type(env)?;
                let result_size = result_type.get_size(env)?;
                let val_size = val.get_size(env)?;

                val.compile(env, output)?;
                output.op(CoreOp::Next(
                    SP,
                    Some(result_size as isize - val_size as isize),
                ));
            }
            Self::Index(val, idx) => {
                let t = Self::Index(val.clone(), idx.clone()).get_type(env)?;
                let size = t.get_size(env)?;
                let val_type = val.get_type(env)?;
                let val_size = val_type.get_size(env)?;
                match val_type {
                    Type::Array(ref elem, _) => {
                        let elem_size = elem.get_size(env)?;
                        val.compile(env, output)?;
                        idx.compile(env, output)?;

                        output.op(CoreOp::Pop(Some(B), 1));
                        output.op(CoreOp::Set(A, elem_size as isize));
                        output.op(CoreOp::Mul { dst: B, src: A });

                        output.op(CoreOp::GetAddress {
                            addr: SP.deref().offset(1 - val_size as isize),
                            dst: A,
                        });
                        output.op(CoreOp::Index {
                            src: A,
                            offset: B,
                            dst: C,
                        });

                        output.op(CoreOp::Copy {
                            src: C.deref(),
                            dst: SP.deref().offset(1 - val_size as isize),
                            size,
                        });
                        output.op(CoreOp::Pop(None, val_size - size));
                    }
                    Type::Pointer(elem) => {
                        idx.compile(env, output)?;
                        val.compile(env, output)?;

                        let elem_size = elem.get_size(env)?;
                        output.op(CoreOp::Pop(Some(A), 1));
                        output.op(CoreOp::Pop(Some(B), 1));
                        output.op(CoreOp::Set(C, elem_size as isize));
                        output.op(CoreOp::Mul { dst: B, src: C });
                        output.op(CoreOp::Index {
                            src: A.deref(),
                            offset: B,
                            dst: C,
                        });
                        output.op(CoreOp::Push(C.deref(), elem_size));
                    }
                    _ => unreachable!(),
                }
            }

            Self::Member(ref val, ref member) => {
                let size = self.get_size(env)?;
                let val_type = val.get_type(env)?;
                let val_size = val_type.get_size(env)?;
                let offset = val_type.get_member_offset(member, &self, env)?;
                val.clone().compile(env, output)?;
                output.op(CoreOp::Copy {
                    src: SP.deref().offset(1 - val_size as isize + offset as isize),
                    dst: SP.deref().offset(1 - val_size as isize),
                    size,
                });
                output.op(CoreOp::Pop(None, val_size - size));
            }

            Self::Refer(val) => match *val {
                Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                    if let Some((_, offset)) = env.get_var(&name) {
                        output.op(CoreOp::Many(vec![
                            CoreOp::Move { src: FP, dst: A },
                            CoreOp::Set(B, *offset),
                            CoreOp::Index {
                                src: A,
                                offset: B,
                                dst: C,
                            },
                            CoreOp::Push(C, 1),
                        ]))
                    } else {
                        return Err(Error::SymbolNotDefined(name.clone()));
                    }
                }
                Expr::Deref(ptr) => {
                    ptr.compile(env, output)?;
                }
                Expr::Member(val, name) => {
                    let val_type = val.get_type(env)?;
                    Self::Refer(val.clone()).compile(env, output)?;
                    let offset = val_type.get_member_offset(&name, &*val, env)?;
                    output.op(CoreOp::Pop(Some(A), 1));
                    output.op(CoreOp::Set(B, offset as isize));
                    output.op(CoreOp::Index {
                        src: A,
                        offset: B,
                        dst: C,
                    });
                    output.op(CoreOp::Push(C, 1));
                }

                Expr::Index(val, idx) => {
                    let val_type = val.get_type(env)?;
                    match val_type {
                        Type::Array(ref elem, _) => {
                            Self::Refer(val.clone()).compile(env, output)?;
                            idx.compile(env, output)?;

                            let elem_size = elem.get_size(env)?;
                            output.op(CoreOp::Pop(Some(B), 1));
                            output.op(CoreOp::Pop(Some(A), 1));
                            output.op(CoreOp::Set(C, elem_size as isize));
                            output.op(CoreOp::Mul { dst: B, src: C });

                            output.op(CoreOp::Index {
                                src: A,
                                offset: B,
                                dst: C,
                            });
                            output.op(CoreOp::Push(C, 1));
                        }
                        Type::Pointer(elem) => {
                            idx.compile(env, output)?;
                            val.compile(env, output)?;

                            let elem_size = elem.get_size(env)?;
                            output.op(CoreOp::Pop(Some(A), 1));
                            output.op(CoreOp::Pop(Some(B), 1));
                            output.op(CoreOp::Set(C, elem_size as isize));
                            output.op(CoreOp::Mul { dst: B, src: C });
                            output.op(CoreOp::Index {
                                src: A.deref(),
                                offset: B,
                                dst: C,
                            });
                            output.op(CoreOp::Push(C, 1));
                        }
                        _ => return Err(Error::InvalidIndex(Expr::Index(val, idx))),
                    }
                }

                other => return Err(Error::InvalidRefer(other)),
            },
        }
        Ok(())
    }
}

impl GetType for Expr {
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error> {
        let i = i + 1;

        Ok(match self {
            Self::ConstExpr(c) => c.get_type_checked(env, i)?,
            Self::Many(exprs) => {
                if let Some(expr) = exprs.last() {
                    expr.get_type_checked(env, i)?
                } else {
                    Type::None
                }
            }

            Self::Const(var, const_val, ret) => {
                let mut new_env = env.clone();
                new_env
                    .consts
                    .insert(var.clone(), const_val.clone().eval(env)?);

                ret.get_type_checked(&mut new_env, i)?
            }

            Self::Let(var, t, val, ret) => {
                let mut new_env = env.clone();
                new_env.def_var(
                    var.clone(),
                    match t {
                        Some(t) => t.clone(),
                        None => val.get_type_checked(env, i)?,
                    },
                )?;

                ret.get_type_checked(&mut new_env, i)?
            }

            Self::While(_, _) => Type::Never,

            Self::If(_, t, _) => t.get_type_checked(env, i)?,
            Self::When(c, t, e) => {
                if c.clone().as_bool(env)? { t } else { e }.get_type_checked(env, i)?
            }

            Self::Refer(expr) => Type::Pointer(Box::new(expr.get_type_checked(env, i)?)),
            Self::Deref(expr) => {
                if let Type::Pointer(inner) = expr.get_type_checked(env, i)? {
                    *inner
                } else {
                    return Err(Error::DerefNonPointer(self.clone()));
                }
            }
            Self::DerefMut(_, _) => Type::None,

            Self::Return(_) => Type::Never,
            Self::Apply(func, _) => {
                if let Type::Proc(_, ret) = func.get_type_checked(env, i)? {
                    *ret
                } else {
                    return Err(Error::ApplyNonProc(self.clone()));
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
                fields
                    .clone()
                    .into_iter()
                    .map(|(k, c)| Ok((k, c.get_type_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            Self::Union(t, _, _) => t.clone(),

            Self::Member(val, field) => {
                let as_symbol = field.clone().as_symbol(env);
                let as_int = field.clone().as_int(env);
                match val.get_type_checked(env, i)? {
                    Type::Tuple(items) => {
                        let n = as_int? as usize;
                        if n < items.len() {
                            items[n].clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    Type::Struct(fields) => {
                        if let Some(t) = fields.get(&as_symbol?) {
                            t.clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    Type::Union(types) => {
                        if let Some(t) = types.get(&as_symbol?) {
                            t.clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }

                    _ => return Err(Error::MemberNotFound(*val.clone(), field.clone())),
                }
            }

            Self::Index(val, _) => match val.get_type_checked(env, i)? {
                Type::Array(item, _) => *item,
                Type::Pointer(item) => *item,

                _ => return Err(Error::InvalidIndex(self.clone())),
            },
        })
    }
}
