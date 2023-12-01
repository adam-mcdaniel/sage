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
    Annotation, CoreBuiltin, Declaration, Env, Error, Expr, FFIProcedure, GetSize, GetType,
    Mutability, PolyProcedure, Procedure, Simplify, StandardBuiltin, Type,
};
use log::*;

use core::fmt;
use std::collections::BTreeMap;

/// A compiletime expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ConstExpr {
    // A constant expression annotated with its source code location.
    Annotated(Box<Self>, Annotation),

    /// Bind a list of types in a constant expression.
    Declare(Box<Declaration>, Box<Self>),

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

    /// A type as a constant expression.
    Type(Type),
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
    /// A foreign function interface binding.
    FFIProcedure(FFIProcedure),
    /// A procedure.
    Proc(Procedure),
    /// A polymorphic procedure.
    PolyProc(PolyProcedure),
    /// Monomorphize a constant expression with some type arguments.
    Monomorphize(Box<Self>, Vec<Type>),

    Template(Vec<String>, Box<Self>),

    /// Get an attribute of a constant expression.
    Member(Box<Self>, Box<Self>),

    /// Cast a constant expression to another type.
    As(Box<Self>, Type),
}

impl ConstExpr {
    pub fn monomorphize(self, ty_args: Vec<Type>) -> Self {
        Self::Monomorphize(Box::new(self), ty_args)
    }

    pub fn template(&self, params: Vec<String>) -> Self {
        if let Self::Proc(proc) = self {
            Self::PolyProc(PolyProcedure::from_mono(proc.clone(), params))
        } else {
            Self::Template(params, Box::new(self.clone()))
        }
        // Self::Template(params, Box::new(self.clone()))
    }

    /// Construct a procedure.
    pub fn proc(
        common_name: Option<String>,
        args: Vec<(String, Mutability, Type)>,
        ret: Type,
        body: impl Into<Expr>,
    ) -> Self {
        Self::Proc(Procedure::new(common_name, args, ret, body))
    }

    /// Return this expression, but with a given declaration in scope.
    pub fn with(&self, older_decls: impl Into<Declaration>) -> Self {
        match self {
            // If the expression is an annotated expression, we need to unwrap it.
            Self::Annotated(expr, annotation) => {
                // Just unwrap the expression and recurse.
                expr.with(older_decls).annotate(annotation.clone())
            }

            // If the expression is a declaration, we need to merge the declarations.
            Self::Declare(younger_decls, expr) => {
                // Start with the older declarations.
                let mut result = older_decls.into();
                // Add the younder declarations to the older declarations.
                result.append(*younger_decls.clone());
                // Return the merged declaration.
                Self::Declare(Box::new(result), expr.clone())
            }

            // Return the expression with the declaration in scope.
            _ => Self::Declare(Box::new(older_decls.into()), Box::new(self.clone())),
        }
    }

    /// Annotate this constant expression with a source code location.
    pub fn annotate(self, annotation: Annotation) -> Self {
        // Check to see if the expression is already annotated.
        match self {
            Self::Annotated(expr, mut annotations) => {
                // If it is, add the new annotation to the list of annotations.
                annotations |= annotation;
                Self::Annotated(expr, annotations)
            }
            // Otherwise, create a new annotation.
            expr => Self::Annotated(Box::new(expr), annotation),
        }
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

    /// Get a field from a structure, union, or tuple.
    ///
    /// For tuples, use an `Int` constant expression to access the nth field (zero indexed).
    /// For unions or structures, use a `Symbol` constant expression to access the field.
    pub fn field(self, field: ConstExpr) -> Self {
        Self::Member(self.into(), field.into())
    }

    /// Evaluate this constant with stack overflow prevention.
    ///
    /// The `i` is a counter for the number of recursions caused by an `eval` call.
    fn eval_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        let i = i + 1;
        if i > 10 {
            error!("Recursion depth exceeded while evaluating: {self}");
            Err(Error::RecursionDepthConst(self))
        } else {
            trace!("Evaluating constexpr: {self}");
            match self {
                Self::Template(params, expr) => {
                    // If the inner expr is a procedure, return a `polyproc`.
                    match expr.clone().eval_checked(env, i) {
                        Ok(Self::Proc(proc)) => {
                            info!("Creating polyproc from mono proc: {proc}");
                            return Ok(Self::PolyProc(PolyProcedure::from_mono(proc, params)));
                        }
                        _ => {
                            info!("Creating template from expr: {expr}");
                            return Ok(Self::Template(params, expr));
                        }
                    }
                }

                Self::Annotated(expr, metadata) => expr
                    .eval_checked(env, i)
                    .map_err(|e| e.annotate(metadata.clone())),

                Self::Member(container, member) => {
                    let container_ty = container.get_type_checked(env, i)?;
                    // container_ty.add_monomorphized_associated_consts(env)?;
                    Ok(
                        match (container.clone().eval(env)?, member.clone().eval(env)?) {
                            (Self::Tuple(tuple), Self::Int(n)) => {
                                // If the index is out of bounds, return an error.
                                if n >= tuple.len() as i64 || n < 0 {
                                    return Err(Error::MemberNotFound(
                                        (*container).into(),
                                        (*member).into(),
                                    ));
                                }
                                tuple[n as usize].clone().eval_checked(env, i)?
                            }
                            (Self::Struct(fields), Self::Symbol(name)) => {
                                // If the field is not in the struct, return an error.
                                if !fields.contains_key(&name) {
                                    if let Some(constant) =
                                        env.get_associated_const(&container_ty, &name)
                                    {
                                        return constant.clone().eval_checked(env, i);
                                        // return Ok(constant.clone());
                                    }
                                    return Err(Error::MemberNotFound(
                                        (*container).into(),
                                        (*member).into(),
                                    ));
                                }
                                fields[&name].clone().eval_checked(env, i)?
                            }
                            (Self::Type(ty), Self::Symbol(name)) => {
                                if let Some(constant) = env.get_associated_const(&ty, &name) {
                                    constant.clone().eval_checked(env, i)?
                                } else {
                                    if let Ok(Some(constant)) =
                                        member.clone().as_symbol(env).and_then(|name| {
                                            Ok(env.get_associated_const(&container_ty, &name))
                                        })
                                    {
                                        return constant.clone().eval_checked(env, i);
                                        // return Ok(constant.clone());
                                    }
                                    error!(
                                        "Member access not implemented for: {container_ty} . {member}"
                                    );
                                    return Err(Error::SymbolNotDefined(name));
                                }
                            }
                            _ => {
                                if let Ok(Some(constant)) =
                                    member.clone().as_symbol(env).and_then(|name| {
                                        Ok(env.get_associated_const(&container_ty, &name))
                                    })
                                {
                                    return constant.clone().eval_checked(env, i);
                                    // return Ok(constant.clone());
                                }
                                error!(
                                    "Member access not implemented for: {container_ty} . {member}"
                                );
                                return Err(Error::MemberNotFound(
                                    (*container).into(),
                                    (*member).into(),
                                ));
                            }
                        },
                    )
                }
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
                | Self::FFIProcedure(_)
                | Self::Proc(_)
                | Self::PolyProc(_)
                | Self::Type(_) => Ok(self),

                Self::Declare(bindings, expr) => {
                    debug!("Declaring compile time bindings: {bindings}");
                    let mut new_env = env.clone();
                    new_env.add_compile_time_declaration(&bindings)?;
                    expr.eval_checked(&new_env, i)
                }

                Self::Monomorphize(expr, ty_args) => {
                    // env.add_monomorphized_associated_consts(expr.get_type(env)?, ty_args.clone())?;
                    // let template_ty = expr.get_type_checked(env, i)?;

                    // let result = if let Self::Template(params, ret) = *expr.clone() {
                    //     if params.len() != ty_args.len() {
                    //         return Err(Error::InvalidMonomorphize(*expr));
                    //     }
                    //     let mut ret = ret.clone();

                    //     for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                    //         ret.substitute(param, ty_arg);
                    //     }
                    //     ret.eval_checked(env, i)?
                    // } else {
                    //     Self::Monomorphize(Box::new(expr.eval_checked(env, i)?), ty_args.clone())
                    // };

                    let result = match expr.clone().eval(env)? {
                        Self::Template(params, ret) => {
                            if params.len() != ty_args.len() {
                                return Err(Error::InvalidMonomorphize(*expr));
                            }
                            let mut ret = ret.clone();

                            for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                                ret.substitute(param, ty_arg);
                            }
                            *ret
                        }
                        Self::PolyProc(proc) => {
                            Self::Proc(proc.monomorphize(ty_args.clone(), env)?)
                        }
                        _ => Self::Monomorphize(
                            Box::new(expr.eval_checked(env, i)?),
                            ty_args.clone(),
                        ),
                    };

                    // if env.has_any_associated_const(&template_ty) {
                    // }
                    // let mono_ty = result.get_type_checked(env, i)?;
                    // env.add_monomorphized_associated_consts(template_ty, mono_ty, ty_args.clone())?;

                    Ok(result)
                }

                Self::TypeOf(expr) => Ok(Self::Array(
                    expr.get_type_checked(env, i)?
                        .to_string()
                        .chars()
                        .map(Self::Char)
                        .collect(),
                )),

                Self::As(expr, cast_ty) => {
                    let found = expr.get_type_checked(env, i)?;
                    if !found.can_cast_to(&cast_ty, env)? {
                        return Err(Error::InvalidAs(
                            Expr::ConstExpr(*expr.clone()),
                            found,
                            cast_ty,
                        ));
                    }

                    expr.eval_checked(env, i)
                }

                Self::SizeOfType(t) => Ok(Self::Int(t.get_size(env)? as i64)),
                Self::SizeOfExpr(e) => Ok(Self::Int(e.get_size(env)? as i64)),

                Self::Symbol(name) => {
                    if let Some(c) = env.get_const(&name) {
                        c.clone().eval_checked(env, i)
                    } else if let Some(t) = env.get_type(&name) {
                        Ok(Self::Type(t.clone()))
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
        // trace!("Getting int from constexpr: {self}");
        match self.eval_checked(env, 0) {
            Ok(Self::Int(n)) => Ok(n),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    /// Try to get this constant expression as a float.
    pub fn as_float(self, env: &Env) -> Result<f64, Error> {
        // trace!("Getting float from constexpr: {self}");
        match self.eval_checked(env, 0) {
            Ok(Self::Float(n)) => Ok(n),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    /// Try to get this constant expression as a boolean value.
    pub fn as_bool(self, env: &Env) -> Result<bool, Error> {
        // trace!("Getting bool from constexpr: {self}");
        match self.eval_checked(env, 0) {
            Ok(Self::Bool(b)) => Ok(b),
            Ok(other) => Err(Error::NonIntegralConst(other)),
            Err(err) => Err(err),
        }
    }

    /// Try to get this constant expression as a symbol (like in LISP).
    pub fn as_symbol(self, env: &Env) -> Result<String, Error> {
        // trace!("Getting symbol from constexpr: {self}");
        match self {
            // Check to see if the constexpr is already a symbol.
            Self::Symbol(name) => Ok(name),
            // If not, evaluate it and see if it's a symbol.
            other => match other.eval(env)? {
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
        trace!("Getting type from constexpr: {self}");
        Ok(match self.clone() {
            Self::Template(params, expr) => {
                let mut new_env = env.clone();
                for param in &params {
                    new_env.define_type(param, Type::Symbol(param.clone()));
                }
                Type::Poly(params, expr.get_type_checked(&new_env, i)?.into())
                // return Ok(Type::Poly(
                //     params,
                //     expr.get_type_checked(env, i)?.simplify(env)?.into()
                // ));
            }

            Self::Type(t) => Type::Type(t.into()),

            Self::Member(val, field) => {
                // Get the field to access (as a symbol)
                let as_symbol = field.clone().as_symbol(env);
                // Get the field to access (as an integer)
                let as_int = field.clone().as_int(env);

                let val_type = val.get_type_checked(env, i)?;
                val_type.add_monomorphized_associated_consts(env)?;
                // Get the type of the value to get the member of.
                match &val_type.simplify_until_concrete(env)? {
                    Type::Unit(_unit_name, inner_ty) => {
                        // Get the type of the field.
                        env.get_type_of_associated_const(&inner_ty, &as_symbol?)
                            .ok_or(Error::MemberNotFound(
                                (*val.clone()).into(),
                                (*field.clone()).into(),
                            ))?
                    }

                    Type::Type(ty) => {
                        // Get the associated constant expression's type.
                        env.get_type_of_associated_const(&ty, &as_symbol?).ok_or(
                            Error::MemberNotFound((*val.clone()).into(), (*field.clone()).into()),
                        )?
                    }
                    // If we're accessing a member of a tuple,
                    // we use the `as_int` interpretation of the field.
                    // This is because tuples are accesed by integer index.
                    Type::Tuple(items) => {
                        // Get the index of the field.
                        let n = as_int? as usize;
                        // If the index is in range, return the type of the field.
                        if n < items.len() {
                            // Return the type of the field.
                            items[n].clone()
                        } else {
                            return ConstExpr::Member(
                                ConstExpr::Type(val_type).into(),
                                field.clone().into(),
                            )
                            .get_type(env)
                            .map_err(|e| {
                                Error::MemberNotFound(
                                    (*val.clone()).into(),
                                    (*field.clone()).into(),
                                )
                            });
                        }
                    }
                    // If we're accessing a member of a struct,
                    // we use the `as_symbol` interpretation of the field.
                    // This is because struct members are accessed by name.
                    Type::Struct(fields) => {
                        // Get the type of the field.
                        if let Some(t) = fields.get(&as_symbol?) {
                            // Return the type of the field.
                            t.clone()
                        } else {
                            // If the field is not in the struct, return an error.
                            return ConstExpr::Member(
                                ConstExpr::Type(val_type).into(),
                                field.clone().into(),
                            )
                            .get_type(env)
                            .map_err(|e| {
                                Error::MemberNotFound(
                                    (*val.clone()).into(),
                                    (*field.clone()).into(),
                                )
                            });
                        }
                    }
                    // If we're accessing a member of a union,
                    // we use the `as_symbol` interpretation of the field.
                    // This is because union members are accessed by name.
                    Type::Union(types) => {
                        // Get the type of the field.
                        if let Some(t) = types.get(&as_symbol?) {
                            // Return the type of the field.
                            t.clone()
                        } else {
                            // If the field is not in the union, return an error.
                            // return Err(Error::MemberNotFound((*val.clone()).into(), (*field.clone()).into()));

                            return ConstExpr::Member(
                                ConstExpr::Type(val_type).into(),
                                field.clone().into(),
                            )
                            .get_type(env)
                            .map_err(|e| {
                                Error::MemberNotFound(
                                    (*val.clone()).into(),
                                    (*field.clone()).into(),
                                )
                            });
                        }
                    }

                    // If we're accessing a member of a type that is not a tuple,
                    // struct, union, or pointer, we cannot access a member.
                    _ => {
                        return ConstExpr::Member(
                            ConstExpr::Type(val_type).into(),
                            field.clone().into(),
                        )
                        .get_type(env)
                        .map_err(|e| {
                            Error::MemberNotFound((*val.clone()).into(), (*field.clone()).into())
                        });
                    }
                }
            }

            Self::Annotated(expr, metadata) => expr
                .get_type_checked(env, i)
                .map_err(|e| e.annotate(metadata))?,

            Self::As(expr, cast_ty) => {
                let found = expr.get_type_checked(env, i)?;
                if !found.can_cast_to(&cast_ty, env)? {
                    return Err(Error::InvalidAs(
                        Expr::ConstExpr(self.clone()),
                        found,
                        cast_ty,
                    ));
                }

                cast_ty
            }
            Self::Declare(bindings, expr) => {
                let mut new_env = env.clone();
                new_env.add_compile_time_declaration(&bindings)?;
                expr.get_type_checked(&new_env, i)?
                    .simplify_until_type_checks(&new_env)?
            }
            Self::Monomorphize(expr, ty_args) => {
                let template_ty = expr.get_type_checked(env, i)?;
                // if env.has_any_associated_const(&expr_ty) {
                //     env.add_monomorphized_associated_consts(expr_ty.clone(), ty_args.clone())?;
                // }
                let mono_ty = if let Self::Template(params, ret) = *expr.clone() {
                    if params.len() != ty_args.len() {
                        return Err(Error::InvalidMonomorphize(*expr));
                    }

                    let mut ret = ret.clone();

                    for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                        ret.substitute(param, ty_arg);
                    }
                    ret.get_type_checked(env, i)?
                        .simplify_until_type_checks(env)?
                    // let mut new_env = env.clone();
                    // for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                    //     new_env.define_type(param, ty_arg.clone());
                    // }
                    // ret.get_type_checked(&new_env, i)?
                    //     .simplify_until_type_checks(&new_env)?
                    // } else if let Self::PolyProc(proc) = *expr.clone() {
                    //     proc.get_type_checked(env, i)?
                    //         .apply(ty_args.clone())
                    //         .simplify_until_type_checks(env)?
                } else {
                    Type::Apply(Box::new(template_ty.clone()), ty_args.clone())
                };

                env.add_monomorphized_associated_consts(
                    template_ty,
                    mono_ty.clone(),
                    ty_args.clone(),
                )?;

                mono_ty
            }
            Self::TypeOf(expr) => {
                let size = expr.get_type_checked(env, i)?.to_string().len();
                Type::Array(Box::new(Type::Char), Box::new(Self::Int(size as i64)))
            }
            Self::Null => Type::Pointer(Mutability::Any, Box::new(Type::Any)),
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
            Self::FFIProcedure(ffi_proc) => ffi_proc.get_type_checked(env, i)?,

            Self::Symbol(name) => {
                if let Some((_, ty, _)) = env.get_var(&name) {
                    // If the symbol is a variable, get the variables type.
                    ty.clone()
                } else if let Some((_, t, _)) = env.get_static_var(&name) {
                    // If the symbol is a static variable, push it onto the stack.
                    t.clone()
                } else if let Some(t) = env.get_type(&name) {
                    // If this is the name of a type, then the type of the symbol is the type itself.
                    Type::Type(t.clone().into())
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

    fn substitute(&mut self, name: &str, subsitution: &Type) {
        match self {
            Self::Type(t) => {
                *t = t.substitute(name, subsitution);
            }

            Self::Template(params, ret) => {
                if params.contains(&name.to_string()) {
                    return;
                }
                ret.substitute(name, subsitution);
            }
            Self::Member(container, member) => {
                container.substitute(name, subsitution);
                member.substitute(name, subsitution);
            }
            Self::Annotated(expr, _) => {
                expr.substitute(name, subsitution);
            }
            Self::As(expr, cast_ty) => {
                expr.substitute(name, subsitution);
                *cast_ty = cast_ty.substitute(name, subsitution);
            }
            Self::Declare(bindings, expr) => {
                bindings.substitute(name, subsitution);
                expr.substitute(name, subsitution);
            }
            Self::Monomorphize(expr, ty_args) => {
                expr.substitute(name, subsitution);
                for ty_arg in ty_args {
                    *ty_arg = ty_arg.substitute(name, subsitution);
                }
            }
            Self::TypeOf(expr) => {
                expr.substitute(name, subsitution);
            }
            Self::Null => {}
            Self::None => {}
            Self::SizeOfType(inner_ty) => {
                *inner_ty = inner_ty.substitute(name, subsitution);
            }
            Self::SizeOfExpr(expr) => {
                expr.substitute(name, subsitution);
            }
            Self::Cell(_) => {}
            Self::Int(_) => {}
            Self::Float(_) => {}
            Self::Char(_) => {}
            Self::Bool(_) => {}
            Self::Of(enum_type, _) => {
                *enum_type = enum_type.substitute(name, subsitution);
            }
            Self::Tuple(items) => {
                for item in items {
                    item.substitute(name, subsitution);
                }
            }
            Self::Array(items) => {
                for item in items {
                    item.substitute(name, subsitution);
                }
            }
            Self::Struct(fields) => {
                for item in fields.values_mut() {
                    item.substitute(name, subsitution);
                }
            }
            Self::Union(inner, _, expr) => {
                *inner = inner.substitute(name, subsitution);
                expr.substitute(name, subsitution);
            }
            Self::EnumUnion(inner, _, expr) => {
                *inner = inner.substitute(name, subsitution);
                expr.substitute(name, subsitution);
            }
            Self::PolyProc(proc) => {
                proc.substitute(name, subsitution);
            }
            Self::Proc(proc) => {
                proc.substitute(name, subsitution);
            }
            Self::CoreBuiltin(builtin) => {
                builtin.substitute(name, subsitution);
            }
            Self::StandardBuiltin(builtin) => {
                builtin.substitute(name, subsitution);
            }
            Self::FFIProcedure(ffi_proc) => {
                ffi_proc.substitute(name, subsitution);
            }
            Self::Symbol(_) => {
                // A constant symbol cannot be substituted for a type variable.
            }
        }
    }
}

impl fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Template(params, expr) => {
                write!(f, "<")?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{param}")?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "> {expr}")
            }

            Self::Type(t) => {
                write!(f, "{t}")
            }
            Self::Member(container, member) => {
                write!(f, "{container}.{member}")
            }
            Self::Annotated(expr, _) => {
                write!(f, "{expr}")
            }
            Self::FFIProcedure(ffi_proc) => {
                write!(f, "{ffi_proc}")
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
            Self::Declare(bindings, expr) => {
                write!(f, "let {bindings} in {expr}")
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
            Self::SizeOfExpr(expr) => write!(f, "sizeof({expr}"),
            Self::SizeOfType(ty) => write!(f, "sizeof<{ty}>()"),
        }
    }
}
