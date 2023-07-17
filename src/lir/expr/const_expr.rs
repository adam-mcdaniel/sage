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
    CoreBuiltin, Env, Error, Expr, GetSize, GetType, PolyProcedure, Procedure, Simplify,
    StandardBuiltin, Type, TypeCheck,
};
use crate::parse::SourceCodeLocation;

use core::fmt;
use std::collections::BTreeMap;

/// A compiletime expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ConstExpr {
    // A constant expression annotated with its source code location.
    AnnotatedWithSource {
        expr: Box<Self>,
        loc: SourceCodeLocation,
    },

    /// Bind a list of types in a constant expression.
    LetTypes(Vec<(String, Type)>, Box<Self>),

    /// The unit, or "void" instance.
    None,
    /// The null pointer constant.
    Null,
    /// A named constant.
    Symbol(String),
    /// A constant integer value.
    Int(i64),
    /// A constant integer value representing a cell on the tape.
    Cell(i64),
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
    /// A polymorphic procedure.
    PolyProc(PolyProcedure),
    /// Monomorphize a constant expression with some type arguments.
    Monomorphize(Box<Self>, Vec<Type>),

    /// Cast a constant expression to another type.
    As(Box<Self>, Type),
}

impl ConstExpr {
    /// Construct a procedure.
    pub fn proc(
        common_name: Option<String>,
        args: Vec<(String, Type)>,
        ret: Type,
        body: impl Into<Expr>,
    ) -> Self {
        Self::Proc(Procedure::new(common_name, args, ret, body))
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
                Self::AnnotatedWithSource { expr, loc } => expr.eval_checked(env, i).map_err(|e| e.with_loc(&loc)),

                Self::None
                | Self::Null
                | Self::Cell(_)
                | Self::Int(_)
                | Self::Float(_)
                | Self::Char(_)
                | Self::Bool(_)
                | Self::Of(_, _)
                | Self::CoreBuiltin(_)
                | Self::StandardBuiltin(_)
                | Self::Proc(_)
                | Self::PolyProc(_) => Ok(self),

                Self::LetTypes(bindings, expr) => {
                    let mut new_env = env.clone();
                    for (name, ty) in bindings {
                        new_env.define_type(name, ty);
                    }
                    expr.eval_checked(&new_env, i)
                }

                Self::Monomorphize(expr, ty_args) => Ok(Self::Monomorphize(
                    Box::new(expr.eval_checked(env, i)?),
                    ty_args,
                )),

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

                Self::SizeOfType(t) => Ok(Self::Int(t.get_size(env)? as i64)),
                Self::SizeOfExpr(e) => Ok(Self::Int(e.get_size(env)? as i64)),

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
                Self::EnumUnion(types, variant, val) => Ok(Self::EnumUnion(
                    types,
                    variant,
                    Box::new(val.eval_checked(env, i)?),
                )),
            }
        }
    }

    /// Try to get this constant expression as an integer.
    pub fn as_int(self, env: &Env) -> Result<i64, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Int(n)) => Ok(n),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    /// Try to get this constant expression as a float.
    pub fn as_float(self, env: &Env) -> Result<f64, Error> {
        match self.eval_checked(env, 0) {
            Ok(Self::Float(n)) => Ok(n),
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
            Self::AnnotatedWithSource { expr, loc } => {
                expr.get_type_checked(env, i).map_err(|e| e.with_loc(&loc))?
            }

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
            Self::LetTypes(bindings, expr) => {
                let mut new_env = env.clone();
                for (name, ty) in bindings {
                    new_env.define_type(&name, ty);
                }
                expr.get_type_checked(&new_env, i)?.simplify_until_matches(
                    env,
                    Type::Any,
                    |t, env| t.type_check(env).map(|_| true),
                )?
            }
            Self::Monomorphize(expr, ty_args) => {
                // Type::Apply(Box::new(expr.get_type_checked(env, i)?.simplify(env)?), ty_args.into_iter().map(|t| t.simplify(env)).collect::<Result<Vec<Type>, Error>>()?).perform_template_applications(env, &mut HashMap::new(), 0)?
                let result = Type::Apply(Box::new(expr.get_type_checked(env, i)?), ty_args.clone());
                result
            }
            Self::TypeOf(expr) => {
                let size = expr.get_type_checked(env, i)?.to_string().len();
                Type::Array(Box::new(Type::Char), Box::new(Self::Int(size as i64)))
            }
            Self::Null => Type::Pointer(Box::new(Type::Any)),
            Self::None => Type::None,
            Self::SizeOfType(_) | Self::SizeOfExpr(_) | Self::Int(_) => Type::Int,
            Self::Float(_) => Type::Float,
            Self::Char(_) => Type::Char,
            Self::Cell(_) => Type::Cell,
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
                Box::new(Self::Int(items.len() as i64)),
            ),
            Self::Struct(fields) => Type::Struct(
                fields
                    .into_iter()
                    .map(|(k, c)| Ok((k, c.get_type_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            Self::Union(t, _, _) => t,
            Self::EnumUnion(t, _, _) => t,

            Self::PolyProc(proc) => proc.get_type_checked(env, i)?,
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

    fn substitute(&mut self, name: &str, ty: &Type) {
        match self {
            Self::AnnotatedWithSource { expr, .. } => {
                expr.substitute(name, ty);
            }
            Self::As(expr, cast_ty) => {
                expr.substitute(name, ty);
                *cast_ty = cast_ty.substitute(name, ty);
            }
            Self::LetTypes(bindings, expr) => {
                // if bindings.iter().map(|(n, _)| n).any(|n| n == name) {
                //     return;
                // }
                for (_, ty_bind) in bindings {
                    *ty_bind = ty_bind.substitute(name, ty);
                }
                expr.substitute(name, ty);
            }
            Self::Monomorphize(expr, ty_args) => {
                expr.substitute(name, ty);
                for ty_arg in ty_args {
                    *ty_arg = ty_arg.substitute(name, ty);
                }
            }
            Self::TypeOf(expr) => {
                expr.substitute(name, ty);
            }
            Self::Null => {}
            Self::None => {}
            Self::SizeOfType(inner_ty) => {
                *inner_ty = inner_ty.substitute(name, ty);
            }
            Self::SizeOfExpr(expr) => {
                expr.substitute(name, ty);
            }
            Self::Cell(_) => {}
            Self::Int(_) => {}
            Self::Float(_) => {}
            Self::Char(_) => {}
            Self::Bool(_) => {}
            Self::Of(enum_type, _) => {
                *enum_type = enum_type.substitute(name, ty);
            }
            Self::Tuple(items) => {
                for item in items {
                    item.substitute(name, ty);
                }
            }
            Self::Array(items) => {
                for item in items {
                    item.substitute(name, ty);
                }
            }
            Self::Struct(fields) => {
                for (_, item) in fields {
                    item.substitute(name, ty);
                }
            }
            Self::Union(inner, _, expr) => {
                *inner = inner.substitute(name, ty);
                expr.substitute(name, ty);
            }
            Self::EnumUnion(inner, _, expr) => {
                *inner = inner.substitute(name, ty);
                expr.substitute(name, ty);
            }
            Self::PolyProc(proc) => {
                proc.substitute(name, ty);
            }
            Self::Proc(proc) => {
                proc.substitute(name, ty);
            }
            Self::CoreBuiltin(builtin) => {
                builtin.substitute(name, ty);
            }
            Self::StandardBuiltin(builtin) => {
                builtin.substitute(name, ty);
            }
            Self::Symbol(_) => {}
        }
    }
}

impl fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::AnnotatedWithSource { expr, .. } => {
                write!(f, "{expr}")
            }
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
            Self::PolyProc(proc) => {
                write!(f, "{proc}")
            }
            Self::LetTypes(bindings, expr) => {
                write!(f, "type ")?;
                for (i, (name, ty)) in bindings.iter().enumerate() {
                    write!(f, "{name} = {ty}")?;
                    if i < bindings.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, " in {expr}")
            }
            Self::Monomorphize(expr, ty_args) => {
                write!(f, "{expr}<")?;
                for (i, ty) in ty_args.iter().enumerate() {
                    write!(f, "{ty}")?;
                    if i < ty_args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ">")
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
            Self::Char(ch) => write!(f, "{ch:?}"),
            Self::Cell(n) => write!(f, "{n:x}"),
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::None => write!(f, "None"),
            Self::Null => write!(f, "Null"),

            Self::Symbol(name) => write!(f, "{name}"),
            Self::Of(t, name) => write!(f, "{t} of {name}"),
            Self::SizeOfExpr(expr) => write!(f, "sizeofexpr({expr}"),
            Self::SizeOfType(ty) => write!(f, "sizeof({ty}"),
        }
    }
}
