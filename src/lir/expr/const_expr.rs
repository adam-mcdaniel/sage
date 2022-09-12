use crate::asm::{AssemblyProgram, CoreOp, StandardOp, A, FP, SP};
use crate::lir::{
    Compile, CoreBuiltin, Env, Error, Expr, GetSize, GetType, Procedure, Simplify, StandardBuiltin,
    Type, TypeCheck,
};
use crate::NULL;
use core::fmt;
use std::collections::BTreeMap;

/// A compiletime expression.
#[derive(Clone, PartialEq)]
pub enum ConstExpr {
    /// The unit, or "void" instance.
    None,
    /// The null pointer constant.
    Null,
    /// A named constant.
    Symbol(String),
    /// A constant integer value.
    Int(i32),
    /// A constant floating point value.
    ///
    /// These can be used at compile time even when compiling to core,
    /// but are not allowed at runtime in the core variant.
    Float(f64),
    /// A constant chararacter.
    Char(char),
    /// A constant boolean value.
    Bool(bool),
    /// A constant enum variant.
    Of(Type, String),

    /// Get the size of a type (in cells) as a constant int.
    SizeOfType(Type),
    /// Get the size of an expression's type (in cells) as a constant int.
    /// This will not evaluate the inner expression.
    SizeOfExpr(Box<Expr>),

    /// A tuple of constant values.
    Tuple(Vec<Self>),
    /// An array of constant values.
    Array(Vec<Self>),
    /// A structure of constant values.
    Struct(BTreeMap<String, Self>),
    /// A union of constant values.
    Union(Type, String, Box<Self>),

    /// A builtin implemented in handwritten core assembly.
    CoreBuiltin(CoreBuiltin),
    /// A builtin implemented in handwritten standard assembly.
    StandardBuiltin(StandardBuiltin),
    /// A procedure.
    Proc(Procedure),
}

impl ConstExpr {
    /// Construct a procedure.
    pub fn proc(args: Vec<(String, Type)>, ret: Type, body: impl Into<Expr>) -> Self {
        Self::Proc(Procedure::new(args, ret, body))
    }

    /// Apply this procedure or builtin to a list of expressions *at runtime*.
    pub fn app(self, args: Vec<Expr>) -> Expr {
        Expr::from(self).app(args)
    }

    /// Evaluate this constant expression at compile time,
    /// and get the result.
    pub fn eval(self, env: &Env) -> Result<Self, Error> {
        self.eval_checked(env, 0)
    }

    /// Evaluate this constant with stack overflow prevention.
    ///
    /// The `i` is a counter for the number of recursions caused by an `eval` call.
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

                Self::SizeOfType(t) => Ok(Self::Int(t.get_size(env)? as i32)),
                Self::SizeOfExpr(e) => Ok(Self::Int(e.get_size(env)? as i32)),

                Self::Symbol(name) => {
                    if let Some(c) = env.get_const(&name) {
                        c.clone().eval_checked(env, i)
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

    /// Try to get this constant expression as an integer.
    pub fn as_int(self, env: &Env) -> Result<i32, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Int(n)) => Ok(n),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    /// Try to get this constant expression as a boolean value.
    pub fn as_bool(self, env: &Env) -> Result<bool, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Bool(b)) => Ok(b),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    /// Try to get this constant expression as a symbol (like in LISP).
    pub fn as_symbol(self, env: &Env) -> Result<String, Error> {
        match self {
            // Check to see if the constexpr is already a symbol.
            Self::Symbol(name) => Ok(name),
            // If not, evaluate it and see if it's a symbol.
            other => match other.eval_checked(env, 0)? {
                Self::Symbol(name) => Ok(name),
                other => Err(Error::NonSymbol(other)),
            },
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
            | Self::Bool(_)
            | Self::SizeOfType(_) => Ok(()),

            Self::SizeOfExpr(e) => e.type_check(env),

            Self::CoreBuiltin(builtin) => builtin.type_check(env),
            Self::StandardBuiltin(builtin) => builtin.type_check(env),
            Self::Proc(proc) => proc.type_check(env),

            Self::Symbol(name) => {
                if env.get_const(name).is_some()
                    || env.get_proc(name).is_some()
                    || env.get_var(name).is_some()
                {
                    Ok(())
                } else {
                    Err(Error::SymbolNotDefined(name.clone()))
                }
            }

            Self::Of(t, variant) => {
                if let Type::Enum(variants) = t.clone().simplify(env)? {
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
                // Confirm the type supplied is a union.
                if let Type::Union(fields) = t.clone().simplify(env)? {
                    // Confirm that the variant is contained within the union.
                    if fields.get(variant).is_some() {
                        // Typecheck the value being assigned to the variant.
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
        let mut comment = format!("{self:?}");
        comment.truncate(70);
        output.comment(format!("compiling constant `{comment}`"));
        
        match self {
            Self::None => {}
            Self::Null => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), NULL));
            }
            Self::Char(ch) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), ch as usize as isize));
            }
            Self::Bool(x) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), x as isize));
            }
            Self::Int(n) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), n as isize));
            }
            Self::Float(f) => {
                output.op(CoreOp::Next(SP, None));
                output.std_op(StandardOp::Set(SP.deref(), f))?;
            }
            Self::SizeOfType(t) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), t.get_size(env)? as isize));
            }
            Self::SizeOfExpr(e) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), e.get_size(env)? as isize));
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
                // Get the mangled name of the procedure.
                let name = proc.get_name().to_string();

                if !env.has_proc(&name) {
                    // If the procedure is not yet defined, define it.
                    env.define_proc(&name, proc);
                }

                // Push the procedure onto the stack.
                env.push_proc(&name, output)?;
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
                // Compile a symbol.
                if let Some((t, offset)) = env.get_var(&name) {
                    // If the symbol is a variable, push it onto the stack.
                    output.op(CoreOp::Push(FP.deref().offset(*offset), t.get_size(env)?))
                } else {
                    // If the symbol is not a variable, evaluate it like a constant.
                    match Self::Symbol(name).eval(env)? {
                        // If the symbol isn't a constant, try to get the procedure
                        // with the same name.
                        Self::Symbol(name) => env.push_proc(&name, output)?,
                        // If the symbol is a constant, push it onto the stack.
                        x => x.compile_expr(env, output)?,
                    }
                }
            }
        }
        output.comment("done".to_string());
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
        Ok(match self.clone() {
            Self::Null => Type::Pointer(Box::new(Type::Any)),
            Self::None => Type::None,
            Self::SizeOfType(_) | Self::SizeOfExpr(_) | Self::Int(_) => Type::Int,
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
                    // If the symbol is a variable, get the variables type.
                    t.clone()
                } else {
                    // Otherwise, evaluate the symbol as a constant.
                    match Self::Symbol(name).eval(env)? {
                        Self::Symbol(name) => {
                            // If the symbol isn't a constant, try to get the procedure
                            // with the same name.
                            if let Some(proc) = env.get_proc(&name) {
                                // Then, return the type of the procedure.
                                proc.get_type_checked(env, i)?
                            } else {
                                // If the procedure isn't defined, then this symbol isn't defined.
                                return Err(Error::SymbolNotDefined(name));
                            }
                        }
                        // Get the type of the constant.
                        x => x.get_type_checked(env, i)?,
                    }
                }
            }
        })
    }
}


impl fmt::Debug for ConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CoreBuiltin(builtin) => {
                write!(f, "{builtin:?}")
            },
            Self::StandardBuiltin(builtin) => {
                write!(f, "{builtin:?}")
            },
            Self::Proc(proc) => {
                write!(f, "{proc:?}")
            },
            Self::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    write!(f, "{item:?}")?;
                    if i < items.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            },
            Self::Struct(fields) => {
                write!(f, "struct {{")?;
                for (i, (field, val)) in fields.iter().enumerate() {
                    write!(f, "{field} = {val:?}")?;
                    if i < fields.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            },
            Self::Union(ty, variant, val) => {
                write!(f, "union {{ {variant} = {val:?}, {ty:?}.. }}")
            },
            Self::Array(items) => write!(f, "{items:?}"),
            Self::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Self::Char(ch) => write!(f, "{ch:?}"),
            Self::Int(n) => write!(f, "{n:?}"),
            Self::Float(n) => write!(f, "{n:?}"),
            Self::None => write!(f, "None"),
            Self::Null => write!(f, "Null"),
            
            Self::Symbol(name) => write!(f, "{name}"),
            Self::Of(t, name) => write!(f, "{name} of {t:?}"),
            Self::SizeOfExpr(expr) => write!(f, "sizeofexpr({expr:?}"),
            Self::SizeOfType(ty) => write!(f, "sizeof({ty:?}"),
        }
    }
}