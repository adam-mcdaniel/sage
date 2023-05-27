//! # Constant Expressions
//!
//! Constant expressions are expressions that can be evaluated at compile time.
//!
//! They are used in a few places:
//! - Array lengths
//! - Getting the size of types and expressions
//! - Procedures
//! - Builtin functions
//! - Enum variants

use crate::lir::{
    CoreBuiltin, Env, Error, Expr, GetSize, GetType, Procedure, Simplify, StandardBuiltin, Type,
};

use core::fmt;
use std::collections::BTreeMap;

/// A compiletime expression.
#[derive(Clone, Debug, PartialEq)]
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

    /// Get the type of an expression. (as an array of chars)
    TypeOf(Box<Expr>),

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
    /// A tagged union of constant values.
    EnumUnion(Type, String, Box<Self>),

    /// A builtin implemented in handwritten core assembly.
    CoreBuiltin(CoreBuiltin),
    /// A builtin implemented in handwritten standard assembly.
    StandardBuiltin(StandardBuiltin),
    /// A procedure.
    Proc(Procedure),

    /// Cast a constant expression to another type.
    As(Box<Self>, Type),
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

    /// Cast an expression as another type.
    pub fn as_type(self, t: Type) -> Self {
        Self::As(Box::new(self), t)
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

                Self::TypeOf(expr) => Ok(Self::Array(
                    expr.get_type_checked(env, i)?
                        .to_string()
                        .chars()
                        .map(|c| Self::Char(c))
                        .collect(),
                )),

                Self::As(expr, cast_ty) => {
                    let found = expr.get_type_checked(env, i)?;
                    if !found.can_cast_to(&cast_ty, env)? {
                        return Err(Error::InvalidAs(
                            Expr::ConstExpr(*expr.clone()),
                            found,
                            cast_ty.clone(),
                        ));
                    }

                    expr.eval_checked(env, i)
                }

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
                Self::EnumUnion(types, variant, val) => Ok(Self::Union(
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

impl Simplify for ConstExpr {
    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        self.eval_checked(env, i)
    }
}

impl GetType for ConstExpr {
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error> {
        Ok(match self.clone() {
            Self::As(expr, cast_ty) => {
                let found = expr.get_type_checked(env, i)?;
                if !found.can_cast_to(&cast_ty, env)? {
                    return Err(Error::InvalidAs(
                        Expr::ConstExpr(self.clone()),
                        found,
                        cast_ty.clone(),
                    ));
                }

                cast_ty
            }
            Self::TypeOf(expr) => {
                let size = expr.get_type_checked(env, i)?.to_string().len();
                Type::Array(Box::new(Type::Char), Box::new(Self::Int(size as i32)))
            }
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
            Self::EnumUnion(t, _, _) => t,

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

impl fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CoreBuiltin(builtin) => {
                write!(f, "{builtin}")
            }
            Self::StandardBuiltin(builtin) => {
                write!(f, "{builtin}")
            }
            Self::TypeOf(expr) => {
                write!(f, "typeof({expr})")
            }
            Self::Proc(proc) => {
                write!(f, "{proc}")
            }
            Self::As(expr, ty) => {
                write!(f, "{expr} as {ty}")
            }
            Self::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    write!(f, "{item}")?;
                    if i < items.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
            Self::Struct(fields) => {
                write!(f, "struct {{")?;
                for (i, (field, val)) in fields.iter().enumerate() {
                    write!(f, "{field} = {val}")?;
                    if i < fields.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Union(ty, variant, val) => {
                write!(f, "union {{ {variant} = {val}, {ty}.. }}")
            }
            Self::EnumUnion(ty, variant, val) => {
                write!(f, "union {{ {variant} = {val}, {ty}.. }}")
            }
            Self::Array(items) => {
                write!(f, "[")?;
                for (i, val) in items.iter().enumerate() {
                    write!(f, "{val}")?;
                    if i < items.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "]")
            }
            Self::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Self::Char(ch) => write!(f, "{ch}"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::None => write!(f, "None"),
            Self::Null => write!(f, "Null"),

            Self::Symbol(name) => write!(f, "{name}"),
            Self::Of(t, name) => write!(f, "{name} of {t}"),
            Self::SizeOfExpr(expr) => write!(f, "sizeofexpr({expr}"),
            Self::SizeOfType(ty) => write!(f, "sizeof({ty}"),
        }
    }
}
