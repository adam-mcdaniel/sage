use super::*;

use crate::{
    asm::{AssemblyProgram, CoreOp, Location, A, B, C, SP},
    io::*,
    lir::*,
};
use ::core::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Get;

impl UnaryOp for Get {
    /// Can this unary operation be applied to the given type?
    fn can_apply(&self, ty: &Type, env: &Env) -> Result<bool, Error> {
        ty.equals(&Type::Pointer(Box::new(Type::Char)), env)
            .or(ty.equals(&Type::Pointer(Box::new(Type::Int)), env))
            .or(ty.equals(&Type::Pointer(Box::new(Type::Float)), env))
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
        ty: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        if ty.equals(&Type::Pointer(Box::new(Type::Char)), env)? {
            output.op(CoreOp::Get(SP.deref().deref(), Input::stdin_char()));
        } else if ty.equals(&Type::Pointer(Box::new(Type::Int)), env)? {
            output.op(CoreOp::Get(SP.deref().deref(), Input::stdin_int()));
        } else if ty.equals(&Type::Pointer(Box::new(Type::Float)), env)? {
            output.op(CoreOp::Get(SP.deref().deref(), Input::stdin_float()));
        } else {
            return Err(Error::UnsupportedOperation(Expr::UnaryOp(
                self.clone_box(),
                Box::new(Expr::ConstExpr(ConstExpr::None)),
            )));
        }

        output.op(CoreOp::Pop(None, 1));
        Ok(())
    }

    /// Clone this operation into a box.
    fn clone_box(&self) -> Box<dyn UnaryOp> {
        Box::new(*self)
    }
}

impl Debug for Get {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "get")
    }
}

impl Display for Get {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "get")
    }
}

/// Print a value to a given output.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Put {
    Debug,
    Display,
}

impl Put {
    pub fn debug(
        addr: Location,
        t: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        match t.clone().simplify(env)? {
            Type::Pointer(_) => {
                for ch in format!("&(").chars() {
                    output.op(CoreOp::Set(A, ch as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
                }
                output.op(CoreOp::Put(addr, Output::stdout_int()));
                output.op(CoreOp::Set(A, ')' as u8 as isize));
                output.op(CoreOp::Put(A, Output::stdout_char()));
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

                    output.op(CoreOp::Move {
                        src: addr.clone(),
                        dst: A,
                    });
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

            Type::EnumUnion(fields) => {
                // Calculate the address of the tag and the data
                let tag_address = SP.deref();
                let data_address = SP.deref().offset(1 - (t.get_size(env)? as isize));
                // The list of possible variants
                let variants: Vec<String> = fields.clone().into_keys().collect();
                // Iterate through all of the possible tags and check if the value is one of them
                for (name, variant_t) in fields.iter() {
                    if let Some(tag_value) = Type::variant_index(&variants, name) {
                        // Check if the value's tag is equal to tag for the name
                        output.op(CoreOp::Set(A, tag_value as isize));
                        output.op(CoreOp::IsEqual {
                            a: tag_address.clone(),
                            b: A,
                            dst: B,
                        });
                        output.op(CoreOp::If(B));
                        Self::debug(data_address.clone(), variant_t, env, output)?;
                        for c in format!(" of {name}").chars() {
                            output.op(CoreOp::Set(A, c as u8 as isize));
                            output.op(CoreOp::Put(A, Output::stdout_char()));
                        }
                        output.op(CoreOp::End);
                    } else {
                        return Err(Error::VariantNotFound(t.clone(), name.clone()));
                    }
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

            _ => return Err(Error::InvalidUnaryOpTypes(Box::new(Self::Debug), t.clone())),
        }
        Ok(())
    }

    pub fn display(
        addr: Location,
        t: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
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

                    output.op(CoreOp::Move {
                        src: addr.clone(),
                        dst: A,
                    });
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

                for c in format!(" of {t}").chars() {
                    output.op(CoreOp::Set(A, c as u8 as isize));
                    output.op(CoreOp::Put(A, Output::stdout_char()));
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
    fn compile_types(
        &self,
        ty: &Type,
        env: &mut Env,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
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
        write!(
            f,
            "{}",
            match self {
                Self::Debug => "debug",
                Self::Display => "put",
            }
        )
    }
}

impl Display for Put {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(
            f,
            "{}",
            match self {
                Self::Debug => "debug",
                Self::Display => "put",
            }
        )
    }
}
