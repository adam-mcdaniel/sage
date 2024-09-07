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
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};

/// A compiletime expression.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ConstExpr {
    // A constant expression annotated with its source code location.
    Annotated(Box<Self>, Annotation),

    /// Bind a list of types in a constant expression.
    Declare(Box<Declaration>, Box<Self>),

    /// The expression equal to any other expression.
    Any,
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

    Template(Vec<(String, Option<Type>)>, Box<Self>),

    /// Get an attribute of a constant expression.
    Member(Box<Self>, Box<Self>),

    /// Cast a constant expression to another type.
    As(Box<Self>, Type),
}

unsafe impl Send for ConstExpr {}
unsafe impl Sync for ConstExpr {}

impl ConstExpr {
    pub fn monomorphize(self, ty_args: Vec<Type>) -> Self {
        Self::Monomorphize(Box::new(self), ty_args)
    }

    pub fn equals(&self, other: &Self, env: &Env) -> bool {
        debug!("{self} == {other} in const?");
        match (self, other) {
            (Self::Any, _) | (_, Self::Any) => true,
            (Self::Type(Type::Any), _) | (_, Self::Type(Type::Any)) => true,
            (Self::Symbol(name), other) | (other, Self::Symbol(name)) => {
                if let Some(c) = env.get_const(name) {
                    c.equals(other, env)
                } else {
                    &Self::Symbol(name.clone()) == other
                }
            }
            (a, b) => a == b,
        }
    }

    pub fn template(&self, params: Vec<(String, Option<Type>)>) -> Self {
        match self {
            Self::Proc(proc) => Self::PolyProc(PolyProcedure::from_mono(proc.clone(), params)),
            Self::Declare(decls, inner) => inner.template(params).with(decls.clone()),
            Self::Annotated(inner, metadata) => inner.template(params).annotate(metadata.clone()),
            _ => Self::Template(params, Box::new(self.clone())),
        }
    }

    /// Evaluate a variable in the current scope.
    pub fn var(var: impl ToString) -> Self {
        Self::Symbol(var.to_string())
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

            Self::Proc(proc) => Self::Proc(proc.with(older_decls)),
            Self::PolyProc(proc) => Self::PolyProc(proc.with(older_decls)),

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
        let output = format!("{}", self);
        let result = self.eval_checked(env, 0)?;
        trace!("Evaluated constexpr: {output} -> {result}");
        Ok(result)
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
        let i: usize = i + 1;
        if i > 100 {
            error!("Recursion depth exceeded while evaluating: {self}");
            Err(Error::RecursionDepthConst(self))
        } else {
            trace!("Evaluating constexpr: {self}");
            match self {
                Self::Template(params, expr) => {
                    // If the inner expr is a procedure, return a `polyproc`.
                    match expr.clone().eval_checked(env, i) {
                        Ok(Self::Proc(proc)) => {
                            debug!("Creating polyproc from mono proc: {proc}");
                            Ok(Self::PolyProc(PolyProcedure::from_mono(proc, params)))
                        }
                        Ok(Self::Declare(decls, inner)) => Ok(Self::Template(params, inner)
                            .with(decls)
                            .eval_checked(env, i)?),
                        _ => {
                            debug!("Creating template from expr: {expr}");
                            Ok(Self::Template(params, expr))
                        }
                    }
                }

                Self::Annotated(expr, metadata) => expr
                    .eval_checked(env, i)
                    .map_err(|e| e.annotate(metadata.clone())),

                Self::Member(container, member) => {
                    let container_ty = container.get_type_checked(env, i)?;
                    debug!("Member access on type: {container_ty:?}: {container} . {member}");
                    Ok(match (*container.clone(), *member.clone()) {
                        (Self::Annotated(inner, metadata), member) => {
                            Self::Member(inner, member.into())
                                .eval_checked(env, i)
                                .map_err(|e| e.annotate(metadata.clone()))?
                        }

                        (Self::Declare(decls, item), field) => {
                            let access = item.field(field);
                            if let Ok(expr) = access.clone().eval_checked(env, i) {
                                if !matches!(expr, Self::Member(_, _)) {
                                    return expr.eval_checked(env, i);
                                }
                            }
                            let mut new_env = env.clone();
                            new_env.add_compile_time_declaration(&decls)?;
                            access.eval_checked(&new_env, i)?.with(decls)
                        }

                        (Self::Symbol(name), member) => {
                            if env.get_const(&name).is_some() {
                                container
                                    .eval_checked(env, i)?
                                    .field(member)
                                    .eval_checked(env, i)?
                            } else {
                                if let Ok(Some((constant, _))) = member
                                    .clone()
                                    .as_symbol(env)
                                    .map(|name| env.get_associated_const(&container_ty, &name))
                                {
                                    debug!("Getting associated const: {container_ty} . {member}");
                                    return constant.eval_checked(env, i);
                                }
                                debug!(
                                    "Member access not implemented for: {container_ty} . {member}"
                                );
                                return Err(Error::MemberNotFound((*container).into(), member));
                            }
                        }

                        (Self::Tuple(tuple), Self::Int(n)) => {
                            // If the index is out of bounds, return an error.
                            if n >= tuple.len() as i64 || n < 0 {
                                error!("Tuple index out of bounds: {container_ty} . {member}");
                                return Err(Error::MemberNotFound((*container).into(), *member));
                            }
                            trace!("Found tuple field: {container_ty} . {member}");
                            tuple[n as usize].clone().eval_checked(env, i)?
                        }
                        (Self::Struct(fields), Self::Symbol(name)) => {
                            // If the field is not in the struct, return an error.
                            if !fields.contains_key(&name) {
                                if let Some((constant, _)) =
                                    env.get_associated_const(&container_ty, &name)
                                {
                                    return constant.eval_checked(env, i);
                                }
                                debug!(
                                    "Struct member access of {member} not implemented for: {container_ty}"
                                );
                                return Err(Error::MemberNotFound((*container).into(), *member));
                            }
                            trace!("Found struct field: {container_ty} . {member}");
                            fields[&name].clone().eval_checked(env, i)?
                        }
                        (Self::Type(ty), Self::Int(n)) => {
                            warn!("Getting member {n} from {ty}");
                            if ty.is_const_param() {
                                let cexpr = ty.simplify_until_const_param(env, false)?;
                                return cexpr.field(Self::Int(n)).eval_checked(env, i)
                            } else {
                                // return Err(Error::MemberNotFound((*container).into(), *member));
                                return Ok(Self::Type(ty).field(Self::Int(n)));
                            }
                        }
                        (Self::Type(ty), Self::Symbol(name)) => {
                            debug!("Getting member {name} from {ty}");
                            if ty.is_const_param() {
                                let cexpr = ty.simplify_until_const_param(env, false)?;
                                return cexpr.eval_checked(env, i)?.field(Self::Symbol(name)).eval_checked(env, i)
                            }

                            if let Some((constant, _)) = env.get_associated_const(&ty, &name) {
                                constant.eval_checked(env, i)?
                            } else {
                                if let Ok(Some((constant, _))) = member
                                    .clone()
                                    .as_symbol(env)
                                    .map(|name| env.get_associated_const(&container_ty, &name))
                                {
                                    return constant.clone().eval_checked(env, i);
                                }
                                error!(
                                    "Type member access not implemented for: {container_ty} . {member}, symbol {name} not defined"
                                );
                                return Ok(Self::Type(ty).field(Self::Symbol(name)));
                            }
                        }

                        (Self::Member(..), member) => {
                            if let Ok(Some((constant, _))) = member
                                .clone()
                                .as_symbol(env)
                                .map(|name| env.get_associated_const(&container_ty, &name))
                            {
                                debug!("Getting associated const: {container_ty} . {member}");
                                return constant.eval_checked(env, i);
                            }
                            debug!("Member access not implemented for: {container_ty} . {member}");
                            return Err(Error::MemberNotFound((*container).into(), member));
                            // return container.eval_checked(env, i)?.field(member).eval(env)
                        }
                        _ => {
                            if let Ok(Some((constant, _))) = member
                                .clone()
                                .as_symbol(env)
                                .map(|name| env.get_associated_const(&container_ty, &name))
                            {
                                debug!("Getting associated const: {container_ty} . {member}");
                                return constant.eval_checked(env, i);
                            }
                            debug!("Member access not implemented for: {container_ty} . {member}");
                            return Err(Error::MemberNotFound((*container).into(), *member));
                        }
                    })
                }
                
                Self::Any
                | Self::None
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
                | Self::PolyProc(_) => Ok(self),
                Self::Type(ty) => {
                    if ty.is_const_param() {
                        let cexpr = ty.simplify_until_const_param(env, false)?;
                        cexpr.eval_checked(env, i)
                    } else {
                        Ok(Self::Type(ty.clone()))
                    }
                    // match ty.clone().simplify(env)? {
                    //     Type::ConstParam(cexpr) => ,
                    //     ty => Ok(Self::Type(ty.clone()))
                    // }
                }

                Self::Declare(bindings, expr) => {
                    debug!("Declaring compile time bindings: {bindings}");
                    let mut new_env = env.clone();
                    new_env.add_compile_time_declaration(&bindings)?;
                    Ok(expr.eval_checked(&new_env, i)?.with(bindings))
                }

                Self::Monomorphize(expr, ty_args) => Ok(match expr.clone().eval(env)? {
                    Self::Template(params, ret) => {
                        if params.len() != ty_args.len() {
                            return Err(Error::InvalidMonomorphize(*expr));
                        }
                        let mut ret = ret.clone();

                        for ((param, _), ty_arg) in params.iter().zip(ty_args.iter()) {
                            ret.substitute(param, ty_arg);
                        }
                        *ret
                    }
                    Self::PolyProc(proc) => {
                        Self::Proc(proc.monomorphize(ty_args.clone(), env)?)
                    },
                    Self::Declare(bindings, expr) => {
                        let mut new_env = env.clone();
                        new_env.add_compile_time_declaration(&bindings)?;
                        expr.monomorphize(ty_args.clone())
                            .eval_checked(&new_env, i)?
                            .with(bindings)
                    }
                    Self::Annotated(_inner, metadata) => expr
                        .monomorphize(ty_args.clone())
                        .eval_checked(env, i)
                        .map_err(|x| x.annotate(metadata))?,

                    _other => {
                        Self::Monomorphize(Box::new(expr.eval_checked(env, i)?), ty_args.clone())
                    }
                }),

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
        trace!("Getting int from constexpr: {self}");
        match self {
            Self::Int(n) => Ok(n),
            other => match other.eval(env)? {
                Self::Int(n) => Ok(n),
                other => Err(Error::NonIntegralConst(other)),
            },
        }
    }

    /// Try to get this constant expression as a character.
    pub fn as_char(self, env: &Env) -> Result<char, Error> {
        trace!("Getting char from constexpr: {self}");
        match self {
            Self::Char(ch) => Ok(ch),
            other => match other.eval(env)? {
                Self::Char(ch) => Ok(ch),
                other => Err(Error::NonIntegralConst(other)),
            },
        }
    }

    /// Try to get this constant expression as a float.
    pub fn as_float(self, env: &Env) -> Result<f64, Error> {
        trace!("Getting float from constexpr: {self}");
        match self {
            Self::Float(n) => Ok(n),
            other => match other.eval(env)? {
                Self::Float(n) => Ok(n),
                other => Err(Error::NonIntegralConst(other)),
            },
        }
    }

    /// Try to get this constant expression as a boolean value.
    pub fn as_bool(self, env: &Env) -> Result<bool, Error> {
        trace!("Getting bool from constexpr: {self}");
        match self {
            Self::Bool(n) => Ok(n),
            other => match other.eval(env)? {
                Self::Bool(n) => Ok(n),
                other => Err(Error::NonIntegralConst(other)),
            },
        }
    }

    /// Try to get this constant expression as a symbol (like in LISP).
    pub fn as_symbol(self, env: &Env) -> Result<String, Error> {
        trace!("Getting symbol from constexpr: {self} ---- {self}");
        match self {
            // Check to see if the constexpr is already a symbol.
            Self::Symbol(name) => Ok(name),
            // If not, evaluate it and see if it's a symbol.
            other => match other.eval(env)? {
                Self::Symbol(name) => Ok(name),
                other => {
                    // error!("Could not convert {other} to symbol");
                    Err(Error::NonSymbol(other))
                },
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
            Self::Any => Type::Any,
            Self::Template(params, expr) => {
                let mut new_env = env.clone();
                for (param, ty) in &params {
                    if let Some(ty) = ty {
                        new_env.define_type(param, ty.clone());
                    } else {
                        new_env.define_type(param, Type::Symbol(param.clone()));
                    }
                }
                Type::Poly(params, expr.get_type_checked(&new_env, i)?.into())
            }

            Self::Type(t) => {
                debug!("Getting type of type {t}");
                let result = if t.is_const_param() {
                    let cexpr = t.simplify_until_const_param(env, false)?;
                    cexpr.get_type(env)?
                } else {
                    Type::Type(t.into())
                };
                // let result = match t.clone().simplify(env)? {
                //     Type::ConstParam(cexpr) => cexpr.get_type(env)?,
                //     t => Type::Type(t.into())
                // };
                // let result = if let Type::ConstParam(cexpr) = t.clone().simplify(env)? {
                //     // Type::ConstParam(cexpr.eval(env)?.into())
                //     cexpr.get_type(env)?
                // } else {
                //     Type::Type(t.clone().into())
                // };
                // debug!("Got type of type {t} = {result}");
                result
            }

            Self::Member(val, field) => {
                // Get the field to access (as a symbol)
                trace!("Getting type of container access");
                let as_symbol = field.clone().as_symbol(env);
                // Get the field to access (as an integer)
                let as_int = field.clone().as_int(env);

                let val_type = val.get_type_checked(env, i)?;
                debug!("Got type of container access {val} . {field}\nContainer: {val_type:?}, is_const_param: {}", val_type.is_const_param());
                // val_type.add_monomorphized_associated_consts(env)?;
                // Get the type of the value to get the member of.
                let val_type = val_type.simplify_until_concrete(env, false)?;
                match &val_type {
                    Type::Unit(_unit_name, inner_ty) => {
                        // Get the type of the field.
                        env.get_type_of_associated_const(inner_ty, &as_symbol?)
                            .ok_or(Error::MemberNotFound((*val.clone()).into(), *field.clone()))?
                    }

                    Type::Pointer(_found_mutability, t) => {
                        let val = &Expr::ConstExpr(*val);
                        let t = t.clone().simplify_until_concrete(env, false)?;
                        match t.get_member_offset(&field, val, env) {
                            Ok((t, _)) => t,
                            Err(_) => {
                                return ConstExpr::Member(ConstExpr::Type(val_type.clone()).into(), field.clone())
                                    .get_type(env)
                                    .or_else(|e| {
                                        debug!("Could not type check member access of constant, falling back on runtime access: {val} . {field} -- {e}");
                                        Expr::Member(val.clone().into(), *field.clone()).get_type(env)
                                    });
                            }
                        }
                    }

                    Type::Type(ty) => {
                        debug!("Got type {ty}");
                        if ty.is_const_param() {
                            ty.simplify_until_const_param(env, false)?.eval(env)?.field(*field).get_type_checked(env, i)?
                        } else {
                            // Get the associated constant expression's type.
                            if let Ok((ty, _)) = ty.get_member_offset(&field, &Expr::from(self.clone()), env) {
                                return Ok(ty);
                            }
                            env.get_type_of_associated_const(ty, &as_symbol?)
                                .ok_or(Error::MemberNotFound((*val.clone()).into(), *field.clone()))?
                        }
                    }
                    // If we're accessing a member of a tuple,
                    // we use the `as_int` interpretation of the field.
                    // This is because tuples are accesed by integer index.
                    Type::Tuple(items) => {
                        trace!("Getting type of associated const: {val_type} . {as_int:?} {as_symbol:?}");
                        if as_symbol.is_ok() {
                            return env
                                .get_type_of_associated_const(&val_type, &as_symbol?)
                                .ok_or(Error::MemberNotFound(
                                    (*val.clone()).into(),
                                    *field.clone(),
                                ));
                        }

                        // Get the index of the field.
                        let n = as_int? as usize;
                        // If the index is in range, return the type of the field.
                        if n < items.len() {
                            // Return the type of the field.
                            items[n].clone()
                        } else {
                            return ConstExpr::Member(
                                ConstExpr::Type(val_type).into(),
                                field.clone(),
                            )
                            .get_type(env)
                            .map_err(|_e| {
                                Error::MemberNotFound((*val.clone()).into(), *field.clone())
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
                                field.clone(),
                            )
                            .get_type(env)
                            .map_err(|_e| {
                                Error::MemberNotFound((*val.clone()).into(), *field.clone())
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
                                field.clone(),
                            )
                            .get_type(env)
                            .map_err(|_e| {
                                Error::MemberNotFound((*val.clone()).into(), *field.clone())
                            });
                        }
                    }

                    // If we're accessing a member of a type that is not a tuple,
                    // struct, union, or pointer, we cannot access a member.
                    _ => {
                        debug!("Member access not implemented for type: {val_type} . {field}");
                        return ConstExpr::Member(ConstExpr::Type(val_type.clone()).into(), field.clone())
                            .get_type(env)
                            .or_else(|e| {
                                debug!("Could not type check member access of constant, falling back on runtime access: {val} . {field} -- {e}");
                                Expr::Member(Expr::ConstExpr(*val.clone()).into(), *field.clone()).get_type(env)
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

            Self::Declare(bindings, expr) => expr.get_type_checked(env, i).or_else(|_| {
                let mut new_env = env.clone();
                new_env.add_compile_time_declaration(&bindings)?;
                expr.get_type_checked(&new_env, i)
            })?,

            Self::Monomorphize(expr, ty_args) => {
                let template_ty = expr.get_type_checked(env, i)?;
                let expr = expr.clone().eval(env)?;
                let mono_ty = match expr.clone() {
                    Self::Annotated(expr, metadata) => {
                        return expr
                            .get_type_checked(env, i)
                            .map_err(|e| e.annotate(metadata));
                    }
                    Self::Declare(bindings, expr) => {
                        let mut new_env = env.clone();
                        new_env.add_compile_time_declaration(&bindings)?;
                        return expr
                            .monomorphize(ty_args)
                            .get_type_checked(&new_env, i)?
                            .simplify_until_type_checks(env);
                    }
                    Self::Template(params, ret) => {
                        debug!("Getting type of monomorphized template {self}");
                        if params.len() != ty_args.len() {
                            return Err(Error::InvalidMonomorphize(expr));
                        }

                        let mut ret = ret.clone();
                        let mut new_env = env.clone();
                        for ((param, ty), ty_arg) in params.iter().zip(ty_args.iter()) {
                            if let Type::ConstParam(cexpr) = ty_arg {
                                if let Some(expected_ty) = ty {
                                    let expected = expected_ty.clone();
                                    let found = cexpr.get_type_checked(env, i)?;
                                    if !found.equals(expected_ty, env)? {
                                        error!("Mismatch in expected type for constant parameter");
                                        return Err(Error::MismatchedTypes { expected, found, expr: (*cexpr.clone()).into() })
                                    }
                                    ret.substitute(param, ty_arg)
                                }
                                ret.substitute(param, ty_arg);
                                new_env.define_const(param, *cexpr.clone());
                            } else {
                                ret.substitute(param, ty_arg);
                                new_env.define_type(param, ty_arg.clone());
                            }
                        }
                        debug!("Result: {ret}");
                        let ret = ret.get_type_checked(&new_env, i)?
                            .simplify_until_poly(&new_env, false)?;
                        ret
                    }
                    _ => {
                        // debug!("Monomorphizing non-template: {expr}");
                        Type::Apply(Box::new(template_ty.clone()), ty_args.clone())
                    }
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
                debug!("Getting type of {name} in {env}");
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
                    debug!("{name} is not a var, static var, or type in {env}");
                    match Self::Symbol(name).eval(env)? {
                        Self::Symbol(name) => {
                            // If the symbol isn't a constant, try to get the procedure
                            // with the same name.
                            if let Some(proc) = env.get_proc(&name) {
                                // Then, return the type of the procedure.
                                proc.get_type_checked(env, i)?
                            } else {
                                debug!("Could not find symbol {name} in environment {env}");
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

    fn substitute(&mut self, name: &str, substitution: &Type) {
        match self {
            Self::Type(t) => {
                *t = t.substitute(name, substitution);
            }

            Self::Template(params, ret) => {
                if params.iter().map(|x| x.0.clone()).collect::<Vec<_>>().contains(&name.to_string()) {
                    return;
                }
                ret.substitute(name, substitution);
            }
            Self::Member(container, member) => {
                container.substitute(name, substitution);
                member.substitute(name, substitution);
            }
            Self::Annotated(expr, _) => {
                expr.substitute(name, substitution);
            }
            Self::As(expr, cast_ty) => {
                expr.substitute(name, substitution);
                *cast_ty = cast_ty.substitute(name, substitution);
            }
            Self::Declare(bindings, expr) => {
                bindings.substitute(name, substitution);
                expr.substitute(name, substitution);
            }
            Self::Monomorphize(expr, ty_args) => {
                expr.substitute(name, substitution);
                for ty_arg in ty_args {
                    *ty_arg = ty_arg.substitute(name, substitution);
                }
            }
            Self::TypeOf(expr) => {
                expr.substitute(name, substitution);
            }
            Self::Null => {}
            Self::None => {}
            Self::SizeOfType(inner_ty) => {
                *inner_ty = inner_ty.substitute(name, substitution);
            }
            Self::SizeOfExpr(expr) => {
                expr.substitute(name, substitution);
            }
            Self::Cell(_) => {}
            Self::Int(_) => {}
            Self::Float(_) => {}
            Self::Char(_) => {}
            Self::Bool(_) => {}
            Self::Of(enum_type, _) => {
                *enum_type = enum_type.substitute(name, substitution);
            }
            Self::Tuple(items) => {
                for item in items {
                    item.substitute(name, substitution);
                }
            }
            Self::Array(items) => {
                for item in items {
                    item.substitute(name, substitution);
                }
            }
            Self::Struct(fields) => {
                for item in fields.values_mut() {
                    item.substitute(name, substitution);
                }
            }
            Self::Union(inner, _, expr) => {
                *inner = inner.substitute(name, substitution);
                expr.substitute(name, substitution);
            }
            Self::EnumUnion(inner, _, expr) => {
                *inner = inner.substitute(name, substitution);
                expr.substitute(name, substitution);
            }
            Self::PolyProc(proc) => {
                proc.substitute(name, substitution);
            }
            Self::Proc(proc) => {
                proc.substitute(name, substitution);
            }
            Self::CoreBuiltin(builtin) => {
                builtin.substitute(name, substitution);
            }
            Self::StandardBuiltin(builtin) => {
                builtin.substitute(name, substitution);
            }
            Self::FFIProcedure(ffi_proc) => {
                ffi_proc.substitute(name, substitution);
            }
            Self::Symbol(symbol_name) if symbol_name == name => {
                // A constant symbol cannot be substituted for a type variable.
                debug!("Subbing {self} for {substitution}");
                *self = ConstExpr::Type(substitution.clone());
                debug!("Subbed into {self}");
            }
            _ => {}
        }
    }
}

impl fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => write!(f, "any"),
            Self::Template(params, expr) => {
                write!(f, "<")?;
                for (i, (param, ty)) in params.iter().enumerate() {
                    write!(f, "{param}")?;
                    if let Some(ty) = ty {
                        write!(f, ": {ty}")?;
                    }
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
                write!(f, "{expr} with {bindings}")
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

// Implement Hash for ConstExpr.
impl Hash for ConstExpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Template(params, expr) => {
                state.write_u8(0);
                params.hash(state);
                expr.hash(state);
            }
            Self::Type(t) => {
                state.write_u8(1);
                t.hash(state);
            }
            Self::Member(container, member) => {
                state.write_u8(2);
                container.hash(state);
                member.hash(state);
            }
            Self::Annotated(expr, _) => {
                state.write_u8(3);
                expr.hash(state);
            }
            Self::FFIProcedure(ffi_proc) => {
                state.write_u8(4);
                ffi_proc.hash(state);
            }
            Self::CoreBuiltin(builtin) => {
                state.write_u8(5);
                builtin.hash(state);
            }
            Self::StandardBuiltin(builtin) => {
                state.write_u8(6);
                builtin.hash(state);
            }
            Self::TypeOf(expr) => {
                state.write_u8(7);
                expr.hash(state);
            }
            Self::Proc(proc) => {
                state.write_u8(8);
                proc.hash(state);
            }
            Self::PolyProc(proc) => {
                state.write_u8(9);
                proc.hash(state);
            }
            Self::Declare(bindings, expr) => {
                state.write_u8(10);
                bindings.hash(state);
                expr.hash(state);
            }
            Self::Monomorphize(expr, ty_args) => {
                state.write_u8(11);
                expr.hash(state);
                ty_args.hash(state);
            }
            Self::As(expr, ty) => {
                state.write_u8(12);
                expr.hash(state);
                ty.hash(state);
            }
            Self::Tuple(items) => {
                state.write_u8(13);
                items.hash(state);
            }
            Self::Struct(fields) => {
                state.write_u8(14);
                fields.hash(state);
            }
            Self::Union(ty, variant, val) => {
                state.write_u8(15);
                ty.hash(state);
                variant.hash(state);
                val.hash(state);
            }
            Self::EnumUnion(ty, variant, val) => {
                state.write_u8(16);
                ty.hash(state);
                variant.hash(state);
                val.hash(state);
            }
            Self::Array(items) => {
                state.write_u8(17);
                items.hash(state);
            }
            Self::Bool(x) => {
                state.write_u8(18);
                x.hash(state);
            }
            Self::Char(ch) => {
                state.write_u8(19);
                ch.hash(state);
            }
            Self::Cell(n) => {
                state.write_u8(20);
                n.hash(state);
            }
            Self::Int(n) => {
                state.write_u8(21);
                n.hash(state);
            }
            Self::Float(n) => {
                state.write_u8(22);
                n.to_bits().hash(state);
            }
            Self::None => {
                state.write_u8(23);
            }
            Self::Null => {
                state.write_u8(24);
            }
            Self::Symbol(name) => {
                state.write_u8(25);
                name.hash(state);
            }
            Self::Of(t, name) => {
                state.write_u8(26);
                t.hash(state);
                name.hash(state);
            }
            Self::SizeOfExpr(expr) => {
                state.write_u8(27);
                expr.hash(state);
            }
            Self::SizeOfType(ty) => {
                state.write_u8(28);
                ty.hash(state);
            }
            Self::Any => state.write_u8(29),
        }
    }
}

impl Eq for ConstExpr {}

impl From<PolyProcedure> for ConstExpr {
    fn from(value: PolyProcedure) -> Self {
        Self::PolyProc(value)
    }
}

impl From<Procedure> for ConstExpr {
    fn from(value: Procedure) -> Self {
        Self::Proc(value)
    }
}

impl From<Type> for ConstExpr {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}