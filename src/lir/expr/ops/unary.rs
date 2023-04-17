use super::*;

use crate::{io::*, lir::*, asm::{Location, CoreOp, SP, A, B, C, AssemblyProgram}};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

/// Print a value to a given output.
#[derive(Clone, Copy)]
pub enum Put {
    Debug,
    Display
}

impl Put {
    pub fn debug(addr: Location, t: &Type, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        match t.clone().simplify(env)? {
            Type::Pointer(x) => {
                for ch in format!("&{x}").chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
            Type::Bool => {
                output.op(CoreOp::If(addr.clone()));
                for c in "true".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
                output.op(CoreOp::Else);
                for c in "false".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
                output.op(CoreOp::End);
            }
            Type::None => {
                for c in "None".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
            Type::Any => {
                for c in "Any".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
            Type::Cell => {
                output.op(CoreOp::Put(addr.clone(), Output::stdout_int()));
                for ch in " (Cell)".to_string().chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
            Type::Int => {
                output.op(CoreOp::Put(addr.clone(), Output::stdout_int()));
            }
            Type::Float => {
                output.op(CoreOp::Put(addr.clone(), Output::stdout_float()));
            }
            Type::Char => {
                output.op(CoreOp::Set(A, '\'' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
                output.op(CoreOp::Put(addr.clone(), Output::stdout_char()));
                output.op(CoreOp::Set(A, '\'' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
            }
            Type::Never => {
                for c in format!("Never").chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
    
            Type::Enum(variants) => {
                for variant in variants.iter() {
                    let variant_id = Type::variant_index(&variants, variant).unwrap();
    
                    output.op(CoreOp::Move { src: addr.clone(), dst: A });
                    output.op(CoreOp::Set(B, variant_id as isize));
                    // Check if the value is the same as the variant ID
                    output.op(CoreOp::IsEqual { a: A, b: B, dst: C });
                    output.op(CoreOp::If(C));
                    for c in variant.chars() {
                        output.op(CoreOp::Set(A, c as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                    
                    for c in format!(" of {t}").chars() {
                        output.op(CoreOp::Set(A, c as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }

                    output.op(CoreOp::End);
                }
            }
    
            Type::Array(ty, array_len_expr) => {
                let array_len = array_len_expr.as_int(env)?;
    
                let ty_size = ty.get_size(env)? as isize;
    
                output.op(CoreOp::Set(A, '[' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
                for i in 0..array_len as isize {
                    Self::debug(addr.offset(i * ty_size), &ty, env, output)?;
                    if i < array_len as isize - 1 {
                        output.op(CoreOp::Set(A, ',' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        output.op(CoreOp::Set(A, ' ' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                }
                output.op(CoreOp::Set(A, ']' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
            }
    
            Type::Struct(fields) => {
                for c in "struct {".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
                let mut offset = 0;
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    for c in field_name.chars() {
                        output.op(CoreOp::Set(A, c as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                    output.op(CoreOp::Set(A, '=' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    Self::debug(addr.offset(offset), field_type, env, output)?;
                    if i < fields.len() - 1 {
                        output.op(CoreOp::Set(A, ',' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        output.op(CoreOp::Set(A, ' ' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        offset += field_type.get_size(env)? as isize;
                    }
                }
                output.op(CoreOp::Set(A, '}' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
            }
    
            Type::Tuple(types) => {
                output.op(CoreOp::Set(A, '(' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
                let mut offset = 0;
                for (i, ty) in types.iter().enumerate() {
                    Self::debug(addr.offset(offset), ty, env, output)?;
                    if i < types.len() - 1 {
                        output.op(CoreOp::Set(A, ',' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        output.op(CoreOp::Set(A, ' ' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        offset += ty.get_size(env)? as isize;
                    }
                }
                output.op(CoreOp::Set(A, ')' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
            }
    
            Type::Proc(args, ret) => {
                for c in "proc(".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
                for (i, ty) in args.iter().enumerate() {
                    for ch in ty.to_string().chars() {
                        output.op(CoreOp::Set(A, ch as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                    if i < args.len() - 1 {
                        output.op(CoreOp::Set(A, ',' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        output.op(CoreOp::Set(A, ' ' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                }
                for ch in ") -> ".to_string().chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
    
                for ch in ret.to_string().chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
    
            Type::Unit(name, ty) => {
                Self::debug(addr, &ty, env, output)?;
                for ch in format!(" ({})", name).chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
    
            Type::Symbol(name) => {
                t.type_check(env)?;
                for ch in name.chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
    
            Type::Union(fields) => {
                for c in "union {".chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    for c in field_name.chars() {
                        output.op(CoreOp::Set(A, c as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                    output.op(CoreOp::Set(A, ':' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    output.op(CoreOp::Set(A, ' ' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    for ch in field_type.to_string().chars() {
                        output.op(CoreOp::Set(A, ch as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                    output.op(CoreOp::Set(A, ' ' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    output.op(CoreOp::Set(A, '=' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    output.op(CoreOp::Set(A, ' ' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    Self::debug(addr.clone(), field_type, env, output)?;
                    if i < fields.len() - 1 {
                        output.op(CoreOp::Set(A, ',' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                        output.op(CoreOp::Set(A, ' ' as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                }
                output.op(CoreOp::Set(A, '}' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
            }
    
            Type::Let(_, _, _) => {
                return Err(Error::InvalidUnaryOpTypes(Box::new(Self::Debug), t.clone()))
            }
        }
        Ok(())
    }

    pub fn display(addr: Location, t: &Type, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        match t.clone().simplify(env)? {
            Type::Cell => {
                output.op(CoreOp::Put(addr, Output::stdout_int()));
            }
            Type::Char => {
                output.op(CoreOp::Put(addr, Output::stdout_char()));
            }

            Type::Enum(variants) => {
                for variant in variants.iter() {
                    let variant_id = Type::variant_index(&variants, variant).unwrap();
    
                    output.op(CoreOp::Move { src: addr.clone(), dst: A });
                    output.op(CoreOp::Set(B, variant_id as isize));
                    // Check if the value is the same as the variant ID
                    output.op(CoreOp::IsEqual { a: A, b: B, dst: C });
                    output.op(CoreOp::If(C));
                    for c in variant.chars() {
                        output.op(CoreOp::Set(A, c as u8 as isize));
                        output.op(CoreOp::Put(A, Output::stdout_char()));
                    }
                    output.op(CoreOp::End);
                }
            }
    
    
            Type::Array(ty, array_len_expr) => {
                let array_len = array_len_expr.as_int(env)?;
    
                let ty_size = ty.get_size(env)? as isize;
                if ty.equals(&Type::Char, env)? {
                    for i in 0..array_len as isize {
                        output.op(CoreOp::Put(addr.offset(i), Output::stdout_char()));
                    }
                } else {
                    output.op(CoreOp::Set(A, '[' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                    for i in 0..array_len as isize {
                        Self::debug(addr.offset(i * ty_size), &ty, env, output)?;
                        if i < array_len as isize - 1 {
                            output.op(CoreOp::Set(A, ',' as u8 as isize));
                            output.op(CoreOp::Put(A, Output::stdout_char()));
                            output.op(CoreOp::Set(A, ' ' as u8 as isize));
                            output.op(CoreOp::Put(A, Output::stdout_char()));
                        }
                    }
                    output.op(CoreOp::Set(A, ']' as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
            }
            
            _ => {
                Self::debug(addr, t, env, output)?;
            }
        }
        Ok(())
    }
}


impl UnaryOp for Put {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, _expr: &Type, _env: &Env) -> Result<bool, Error> {
        Ok(true)
    }

    /// Get the type of the result of applying this unary operation to the given type.
    fn return_type(&self, _expr: &Expr, _env: &Env) -> Result<Type, Error> {
        Ok(Type::None)
    }

    /// Evaluate this unary operation on the given constant values.
    fn eval(&self, _expr: &ConstExpr, _env: &mut Env) -> Result<ConstExpr, Error> {
        Ok(ConstExpr::None)
    }

    /// Compile the unary operation.
    fn compile_types(&self, ty: &Type, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        // Get the size of the type.
        let size = ty.get_size(env)? as isize;

        // Calculate the address of the expression on the stack.
        let addr = SP.deref().offset(-size + 1);
        match self {
            Self::Debug => Self::debug(addr, ty, env, output)?,
            Self::Display => Self::display(addr, ty, env, output)?,
        }

        output.op(CoreOp::Pop(None, size as usize));
        Ok(())
    }
    
    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for Put {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", match self {
            Self::Debug => "debug",
            Self::Display => "put",
        })
    }
}

impl Display for Put {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", match self {
            Self::Debug => "debug",
            Self::Display => "put",
        })
    }
}