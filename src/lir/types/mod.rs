//! # Types Module
//!
//! This module contains a collection of types and traits
//! used to implement and confirm the soundness of the LIR
//! typesystem.
use super::{ConstExpr, Env, Error, Expr, Simplify};
use core::fmt;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Mutex;

mod check;
mod inference;
mod size;
pub use check::*;
pub use inference::*;
pub use size::*;

use log::*;

/// Mutability of a pointer.
/// This is used to provide type safety for pointers.
/// A mutable pointer can be used to mutate the value it points to.
/// An immutable pointer can only be used to read the value it points to.
/// An `Any` pointer can be used to read or write the value it points to; this is
/// used to override pointer access protections for compiler-builtins.
#[derive(Copy, Clone, Default, Debug, Eq, PartialOrd, Ord, Hash)]
pub enum Mutability {
    /// Mutable access to a value.
    Mutable,
    /// Immutable access to a value.
    /// This is the default way to access a value.
    #[default]
    Immutable,
    /// Unchecked access to a value. This is used to override access protections.
    Any,
}

impl Mutability {
    /// Can a pointer of this mutability decay to a pointer of another mutability?
    /// Mutable pointers can decay to immutable pointers, but immutable pointers
    /// cannot decay to mutable pointers. `Any`  pointers are unchecked..
    pub fn can_decay_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Mutable, _) => true,
            (Self::Immutable, Self::Immutable) => true,
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            _ => false,
        }
    }

    /// Can this data be accessed mutably?
    pub fn is_mutable(&self) -> bool {
        matches!(self, Self::Mutable) || matches!(self, Self::Any)
    }

    /// Is this data protected against mutation?
    pub fn is_constant(&self) -> bool {
        matches!(self, Self::Immutable) || matches!(self, Self::Any)
    }
}

/// Convert a boolean into a mutability.
/// True is mutable, false is immutable.
impl From<bool> for Mutability {
    fn from(b: bool) -> Self {
        if b {
            Self::Mutable
        } else {
            Self::Immutable
        }
    }
}

/// Convert a mutability into a boolean.
/// It will return true if the data is mutable (or any), and false if it is immutable.
impl From<Mutability> for bool {
    fn from(m: Mutability) -> Self {
        m.is_mutable()
    }
}

/// Check if two access types are equal.
impl PartialEq for Mutability {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Mutable is equal to mutable.
            (Self::Mutable, Self::Mutable) => true,
            // Immutable is equal to immutable.
            (Self::Immutable, Self::Immutable) => true,
            // Any is equal to any other mutability.
            (Self::Any, _) | (_, Self::Any) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mutable => write!(f, "mut"),
            Self::Immutable => write!(f, "const"),
            Self::Any => write!(f, "any"),
        }
    }
}

/// The representation of a type in the LIR type system.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Bind a type to a name in a temporary scope.
    Let(String, Box<Self>, Box<Self>),

    /// This type is identified by its name. Most types are checked according
    /// to structural equality, but this type is checked according to name.
    /// Structural equality is also verified in addition to name equality.
    ///
    /// The inner type acts exactly the same, but it can only be type-equal
    /// to another Unit type with the same name and inner type.
    Unit(String, Box<Self>),

    /// A named type.
    Symbol(String),
    /// The type of void expressions.
    None,
    /// The integer type.
    Int,
    /// The floating-point number type.
    Float,
    /// The type of the most basic unit of memory.
    Cell,
    /// The type of a character.
    Char,
    /// The type of a boolean value.
    Bool,
    /// An enumeration of a list of possible named values.
    /// A boolean could be considered an enumeration of `true` and `false`.
    Enum(Vec<String>),
    /// A heterogenous collection of types. This is a product type.
    Tuple(Vec<Self>),
    /// An array of a given type, with a constant size.
    Array(Box<Self>, Box<ConstExpr>),
    /// A tuple with named members. This is a product type.
    Struct(BTreeMap<String, Self>),
    /// An enumeration of a list of possible types. This is a sum type.
    /// The enum union is essentially a regular union type, but with a tag
    /// to guarantee typesafety of accessing members.
    ///
    /// The tag is stored at the very beginning of the value.
    EnumUnion(BTreeMap<String, Self>),

    /// A trait object. This is internally represented as an `EnumUnion` over the possible
    /// types that can be stored in the trait object.
    // TraitObject(TraitObject),

    /// A type. This converts a type into a value.
    Type(Box<Type>),

    /// A union of a list of possible types mapped to named members.
    /// A union's value is reinterpreted as a single type, depending on the member accessed.
    /// Unions' values are stored starting at the beginning of the union's address in memory,
    /// and are padded at the end with zeroes.
    ///
    /// This is a sum type.
    Union(BTreeMap<String, Self>),
    /// A procedure with a list of parameters and a return type.
    Proc(Vec<Type>, Box<Type>),
    /// A pointer to another type.
    Pointer(Mutability, Box<Self>),
    /// A type reserved by the compiler.
    /// This type is equal to any other type.
    /// The NULL pointer, for example, is of type `Pointer(Any)`.
    Any,
    /// The type of an expression that will never return, or doesn't resolve to a value.
    Never,

    /// A polymorphic, parametric type.
    /// This type is used with the `Apply` type to create a concrete type.
    /// The type takes a list of symbols that are substituted with concrete types.
    /// The type is then simplified to a concrete type when combined with `Apply`.
    /// This type is used to implement generics.
    Poly(Vec<String>, Box<Self>),

    /// A type that constructs a concrete type from a polymorphic type.
    /// This type is used to implement generics.
    Apply(Box<Self>, Vec<Self>),
}

lazy_static::lazy_static! {
    // Monomorphized types are recursive.
    static ref RECURSIVE_TYPES: Mutex<HashSet<Type>> = Mutex::new(HashSet::new());
    static ref NON_RECURSIVE_TYPES: Mutex<HashSet<Type>> = Mutex::new(HashSet::new());
}

unsafe impl Send for Type {}
unsafe impl Sync for Type {}

impl Type {
    /// This is the maximum number of times a type will be simplified recursively.
    pub const SIMPLIFY_RECURSION_LIMIT: usize = 30;

    pub fn is_recursive(&self, env: &Env) -> Result<bool, Error> {
        let mut symbols = HashSet::new();
        self.is_recursive_helper(&mut symbols, env)
    }

    pub fn is_recursive_helper(
        &self,
        symbols: &mut HashSet<String>,
        env: &Env,
    ) -> Result<bool, Error> {
        if RECURSIVE_TYPES.lock().unwrap().contains(self) {
            return Ok(true);
        } else if NON_RECURSIVE_TYPES.lock().unwrap().contains(self) {
            return Ok(false);
        }

        let result = match self {
            Self::Symbol(name) => {
                if symbols.contains(name) {
                    Ok(true)
                } else {
                    symbols.insert(name.clone());
                    if let Some(t) = env.get_type(name) {
                        t.is_recursive_helper(symbols, env)
                    } else {
                        warn!("Couldn't find type {}", name);
                        Ok(false)
                    }
                }
            }

            Self::Let(name, t, ret) => {
                symbols.insert(name.clone());
                if t.is_recursive_helper(symbols, env)? {
                    Ok(true)
                } else {
                    ret.is_recursive_helper(symbols, env)
                }
            }

            Self::Apply(template, args) => {
                if template.is_recursive_helper(symbols, env)? {
                    Ok(true)
                } else {
                    for arg in args {
                        if arg.is_recursive_helper(symbols, env)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
            }

            Self::Poly(params, t) => {
                for param in params {
                    symbols.remove(param);
                }
                t.is_recursive_helper(symbols, env)
            }

            Self::Proc(args, ret) => {
                for arg in args {
                    if arg.is_recursive_helper(symbols, env)? {
                        return Ok(true);
                    }
                }
                ret.is_recursive_helper(symbols, env)
            }

            Self::Pointer(_, t) => t.is_recursive_helper(symbols, env),
            Self::Unit(_, t) => t.is_recursive_helper(symbols, env),
            Self::Type(t) => t.is_recursive_helper(symbols, env),
            Self::Array(t, _) => t.is_recursive_helper(symbols, env),
            Self::Tuple(items) => {
                for item in items {
                    if item.is_recursive_helper(symbols, env)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Self::Struct(fields) => {
                for (_, t) in fields {
                    if t.is_recursive_helper(symbols, env)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Self::Union(fields) => {
                for (_, t) in fields {
                    if t.is_recursive_helper(symbols, env)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Self::EnumUnion(fields) => {
                for (_, t) in fields {
                    if t.is_recursive_helper(symbols, env)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Self::Enum(_) => Ok(false),
            Self::None
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Any
            | Self::Never => Ok(false),
        };
        // Save the result for later.
        if matches!(result, Ok(true)) {
            let mut recursive_types = RECURSIVE_TYPES.lock().unwrap();
            // RECURSIVE_TYPES.lock().unwrap().insert(self.clone());
            recursive_types.insert(self.clone());
        } else if matches!(result, Ok(false)) {
            // NON_RECURSIVE_TYPES.lock().unwrap().insert(self.clone());
            let mut non_recursive_types = NON_RECURSIVE_TYPES.lock().unwrap();
            non_recursive_types.insert(self.clone());
        }

        result
    }

    pub fn strip_template(&self, env: &Env) -> Self {
        debug!("strip_template: {}", self);
        match self {
            Self::Apply(template, _) => template.strip_template(env),
            Self::Poly(_, template) => *template.clone(),
            Self::Symbol(name) => {
                if let Some(t) = env.get_type(name) {
                    t.strip_template(env)
                } else {
                    warn!("Couldn't find type {}", name);
                    self.clone()
                }
            }
            _ => self.clone(),
        }
    }

    pub fn is_poly(&self) -> bool {
        matches!(self, Self::Poly(_, _))
    }

    pub fn get_monomorph_template_args(
        &self,
        template: &Self,
        matched_symbols: &mut HashMap<String, Self>,
        param_symbols: &HashSet<String>,
        env: &Env,
    ) -> Result<(), Error> {
        debug!(
            "get_monomorph_template_args: {} template: {}",
            self, template
        );
        match (self, template) {
            (Self::Struct(fields1), Self::Struct(fields2)) => {
                for (field_name, field_ty) in fields1 {
                    if let Some(field_ty2) = fields2.get(field_name) {
                        field_ty.get_monomorph_template_args(
                            field_ty2,
                            matched_symbols,
                            param_symbols,
                            env,
                        )?;
                    }
                }
            }
            (Self::Union(fields1), Self::Union(fields2)) => {
                for (field_name, field_ty) in fields1 {
                    if let Some(field_ty2) = fields2.get(field_name) {
                        field_ty.get_monomorph_template_args(
                            field_ty2,
                            matched_symbols,
                            param_symbols,
                            env,
                        )?;
                    }
                }
            }

            (Self::EnumUnion(fields1), Self::EnumUnion(fields2)) => {
                for (field_name, field_ty) in fields1 {
                    if let Some(field_ty2) = fields2.get(field_name) {
                        field_ty.get_monomorph_template_args(
                            field_ty2,
                            matched_symbols,
                            param_symbols,
                            env,
                        )?;
                    }
                }
            }

            (Self::Tuple(fields1), Self::Tuple(fields2)) => {
                for (field_ty1, field_ty2) in fields1.iter().zip(fields2.iter()) {
                    field_ty1.get_monomorph_template_args(
                        field_ty2,
                        matched_symbols,
                        param_symbols,
                        env,
                    )?;
                }
            }

            (Self::Array(inner1, _), Self::Array(inner2, _)) => {
                inner1.get_monomorph_template_args(inner2, matched_symbols, param_symbols, env)?;
            }

            (Self::Pointer(_, inner1), Self::Pointer(_, inner2)) => {
                inner1.get_monomorph_template_args(inner2, matched_symbols, param_symbols, env)?;
            }

            (Self::Proc(args1, ret1), Self::Proc(args2, ret2)) => {
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    arg1.get_monomorph_template_args(arg2, matched_symbols, param_symbols, env)?;
                }
                ret1.get_monomorph_template_args(ret2, matched_symbols, param_symbols, env)?;
            }

            (Self::Unit(_, inner1), Self::Unit(_, inner2)) => {
                inner1.get_monomorph_template_args(inner2, matched_symbols, param_symbols, env)?;
            }

            (Self::Apply(template1, args1), Self::Apply(template2, args2)) => {
                template1.get_monomorph_template_args(
                    template2,
                    matched_symbols,
                    param_symbols,
                    env,
                )?;
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    arg1.get_monomorph_template_args(arg2, matched_symbols, param_symbols, env)?;
                }
            }

            (Self::Apply(template, args), Self::Poly(params, ret)) => {
                for (param, arg) in params.iter().zip(args.iter()) {
                    // ret.get_monomorph_template_args(arg, symbols, env)?;
                    matched_symbols.insert(param.clone(), arg.clone());
                    debug!("Found match {}: {}", param, arg);
                }
                template.get_monomorph_template_args(ret, matched_symbols, param_symbols, env)?;
            }

            (Self::Apply(template, args), other) => {
                if let Ok(Self::Poly(params, ret)) = template.simplify_until_poly(env) {
                    let mut ret = *ret.clone();
                    for (param, arg) in params.iter().zip(args.iter()) {
                        ret = ret.substitute(param, arg);
                    }

                    ret.get_monomorph_template_args(other, matched_symbols, param_symbols, env)?;
                }
            }

            (Self::Symbol(name), template) => {
                if let Some(t) = env.get_type(name) {
                    t.get_monomorph_template_args(template, matched_symbols, param_symbols, env)?;
                }
            }

            (other, Self::Symbol(name)) => {
                if param_symbols.contains(name) {
                    info!("Found match {}: {}", name, other);
                    matched_symbols.insert(name.clone(), other.clone());
                } else if let Some(t) = env.get_type(name) {
                    info!("Symbol {name} is {t}");
                    other.get_monomorph_template_args(t, matched_symbols, param_symbols, env)?;
                }
            }

            (a, b) => {
                if a != b {
                    error!(
                        "get_monomorph_template_args: Couldn't match {} to {}",
                        self, template
                    );
                }
            }
        }

        Ok(())
    }

    pub fn is_monomorph_of(&self, template: &Self, env: &Env) -> Result<bool, Error> {
        match (self, template) {
            (Self::Apply(template1, _), template2) => template1.equals(template2, env),
            (concrete, Self::Poly(params, result)) => {
                let mut result = *result.clone();
                for param in params {
                    result = result.substitute(param, &Type::Any);
                }
                concrete.equals(&result, env)
            }
            (Self::Symbol(name), _) => {
                if let Some(t) = env.get_type(name) {
                    return t.is_monomorph_of(template, env);
                }
                Ok(false)
            }

            (_, Self::Symbol(name)) => {
                if let Some(t) = env.get_type(name) {
                    return self.is_monomorph_of(t, env);
                }
                Ok(false)
            }

            _ => Ok(false),
        }
    }

    pub fn add_monomorphized_associated_consts(&self, env: &Env) -> Result<(), Error> {
        // warn!("add_monomorphized_associated_consts: Adding monomorphized associated consts for {}", self);
        match self.clone() {
            Self::Apply(template, args) => {
                let simplified_args = args
                    .iter()
                    .map(|t| t.simplify_until_concrete(env))
                    .collect::<Result<Vec<_>, _>>()?;

                let mut mono_ty = Self::Apply(template.clone(), simplified_args.clone());
                mono_ty = mono_ty.simplify_until_concrete(env)?;
                // warn!("add_monomorphized_associated_consts: Adding monomorphized associated consts for {self} to {}", mono_ty);

                env.add_monomorphized_associated_consts(*template, mono_ty, simplified_args)?;
            }

            Self::Struct(fields) => {
                for (_, field_ty) in fields {
                    field_ty.add_monomorphized_associated_consts(env)?;
                }
            }

            Self::Tuple(fields) => {
                for field_ty in fields {
                    field_ty.add_monomorphized_associated_consts(env)?;
                }
            }

            Self::Union(fields) => {
                for (_, field_ty) in fields {
                    field_ty.add_monomorphized_associated_consts(env)?;
                }
            }

            Self::EnumUnion(fields) => {
                for (_, field_ty) in fields {
                    field_ty.add_monomorphized_associated_consts(env)?;
                }
            }

            Self::Array(inner, _)
            | Self::Pointer(_, inner)
            | Self::Type(inner)
            | Self::Unit(_, inner) => {
                inner.add_monomorphized_associated_consts(env)?;
            }

            Self::Proc(args, ret) => {
                for arg in args {
                    arg.add_monomorphized_associated_consts(env)?;
                }
                ret.add_monomorphized_associated_consts(env)?;
            }

            Self::Symbol(name) => {
                if let Some(t) = env.get_type(&name) {
                    t.add_monomorphized_associated_consts(env)?;
                }
            }

            Self::Poly(_, _t) => {
                // t.add_monomorphized_associated_consts(env)?;
            }
            Self::Let(_, _, _)
            | Self::Any
            | Self::None
            | Self::Never
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Enum(_) => {}
        }
        Ok(())
    }

    pub fn get_template_params(&self, env: &Env) -> Vec<String> {
        debug!("get_template_params: {}", self);
        match self.simplify_until_poly(env) {
            Ok(Self::Poly(params, _)) => {
                debug!(
                    "get_template_params: {} params: {}",
                    self,
                    params.join(", ")
                );
                params.clone()
            }
            Ok(Self::Symbol(name)) => {
                if let Some(t) = env.get_type(&name) {
                    let result = t.get_template_params(env);
                    debug!(
                        "get_template_params: {} params: {}",
                        name,
                        result.join(", ")
                    );
                    result
                } else {
                    warn!("get_template_params: Couldn't find type {}", name);
                    vec![]
                }
            }
            result => {
                match result {
                    Ok(result) => warn!("get_template_params: Couldn't find template params for {result}"),
                    Err(e) => error!("get_template_params: Couldn't simplify {self} to a polymorphic type due to {e}")
                }
                vec![]
            }
        }
    }

    pub fn apply(&self, args: Vec<Self>) -> Self {
        Self::Apply(Box::new(self.clone()), args)
    }

    /// Is this type in a simple form?
    /// A simple form is a form that does not require any immediate simplification.
    /// For example, a `Poly` type is not simple, because it requires a `Apply` type
    /// to deduce anything about the type. A let binding is also not simple, because
    /// it requires the inner type to be substituted for the name. A symbol is not simple,
    /// because it requires a lookup in the environment to determine the type. Other
    /// types, like a tagged union, which may be pattern matched over the inner types,
    /// does not require an immediate lookup to use some of its type information.
    pub fn is_simple(&self) -> bool {
        match self {
            Self::None
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Any
            | Self::Never
            | Self::Enum(_)
            | Self::Type(_) => true,
            Self::Unit(_, t) => t.is_simple(),
            Self::Tuple(inner) => inner.iter().all(|t| t.is_simple()),
            Self::Array(inner, expr) => inner.is_simple() && matches!(**expr, ConstExpr::Int(_)),
            Self::Proc(args, ret) => args.iter().all(|t| t.is_simple()) && ret.is_simple(),
            Self::Pointer(_, inner) => inner.is_simple(),
            Self::Struct(inner) | Self::Union(inner) | Self::EnumUnion(inner) => {
                inner.iter().all(|(_, t)| t.is_simple())
            }
            Self::Symbol(_) => false,
            Self::Poly(_params, _ret) => true,

            Self::Apply(template, args) => {
                if let Self::Poly(ref params, ref ret) = **template {
                    if params.len() != args.len() {
                        return false;
                    }
                    let mut ret = *ret.clone();
                    for (param, arg) in params.iter().zip(args.iter()) {
                        if !arg.is_simple() {
                            return false;
                        }
                        ret = ret.substitute(param, arg);
                    }

                    ret.is_concrete()
                    // true
                } else {
                    template.is_simple() && args.iter().all(|t| t.is_simple())
                }
            }
            Self::Let(_, _, ret) => ret.is_simple(),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Poly(_, _) | Self::Symbol(_) | Self::Apply(_, _) | Self::Let(_, _, _) => false,
            Self::None
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Any
            | Self::Never
            | Self::Enum(_)
            | Self::EnumUnion(_)
            | Self::Struct(_)
            | Self::Union(_)
            | Self::Proc(_, _)
            | Self::Tuple(_)
            | Self::Unit(_, _)
            | Self::Type(_)
            | Self::Array(_, _)
            | Self::Pointer(_, _) => true,
        }
    }

    /// Is this type an irreducible, atomic type?
    pub fn is_atomic(&self) -> bool {
        match self {
            Self::None
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Any
            | Self::Never
            | Self::Enum(_)
            | Self::Type(_) => true,
            Self::Unit(_, t) => t.is_atomic(),
            Self::Tuple(inner) => inner.iter().all(|t| t.is_atomic()),
            Self::Array(inner, expr) => inner.is_atomic() && matches!(**expr, ConstExpr::Int(_)),
            Self::Proc(args, ret) => args.iter().all(|t| t.is_atomic()) && ret.is_atomic(),
            Self::Pointer(_, inner) => inner.is_atomic(),
            Self::Struct(inner) => inner.iter().all(|(_, t)| t.is_atomic()),
            Self::EnumUnion(inner) => inner.iter().all(|(_, t)| t.is_atomic()),
            // Self::Poly(_, _) | Self::Symbol(_) | Self::Apply(_, _) => false,
            // Self::Let(_, _, ret) => ret.is_atomic(),
            _ => false,
        }
    }

    /// Is first argument of function a reference?
    pub fn is_self_param_reference(&self, env: &Env) -> Result<bool, Error> {
        Ok(match self.simplify_until_concrete(env)? {
            Self::Proc(args, _) => {
                if let Some(Self::Pointer(_, _)) = args.first() {
                    true
                } else {
                    false
                }
            }
            _ => false,
        })
    }

    /// Get the first argument's mutability (if it is a pointer)
    pub fn get_self_param_mutability(&self, env: &Env) -> Option<Mutability> {
        match self.simplify_until_concrete(env) {
            Ok(Self::Proc(args, _)) => {
                if let Some(Self::Pointer(mutability, _)) = args.first() {
                    Some(*mutability)
                } else {
                    None
                }
            }
            Ok(Self::Poly(_, inner)) => inner.get_self_param_mutability(env),

            Ok(Self::Apply(template, args)) => {
                if let Self::Poly(ref params, ref inner) = *template {
                    let mut inner = *inner.clone();
                    for (param, arg) in params.iter().zip(args.iter()) {
                        inner = inner.substitute(param, arg);
                    }
                    inner.get_self_param_mutability(env)
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    /// Simplify until the type passes the type checker.
    pub fn simplify_until_type_checks(&self, env: &Env) -> Result<Self, Error> {
        self.clone()
            .simplify_until_matches(env, Type::Any, |t, env| t.type_check(env).map(|_| true))
    }

    fn possibly_has_members(&self) -> bool {
        if matches!(
            self,
            Self::Tuple(_) | Self::Struct(_) | Self::Union(_) | Self::Pointer(_, _) | Self::Type(_)
        ) {
            return true;
        }

        if let Self::Unit(_, inner) = self {
            return inner.possibly_has_members();
        }

        false
    }

    /// Simplify a type until you can get its members.
    pub fn simplify_until_has_members(&self, env: &Env) -> Result<Self, Error> {
        let result = self
            .clone()
            .simplify_until_matches(env, Type::Any, |t, _| Ok(t.possibly_has_members()));

        if result.is_err() {
            warn!("Couldn't simplify {} to a type with members", self);
        }

        result
    }

    fn is_union(&self) -> bool {
        if matches!(self, Self::Union(_) | Self::EnumUnion(_)) {
            return true;
        }

        if let Self::Unit(_, inner) = self {
            return inner.is_union();
        }

        false
    }

    /// Simplify a type until it's a union.
    pub fn simplify_until_union(&self, env: &Env) -> Result<Self, Error> {
        let result = self
            .clone()
            .simplify_until_matches(env, Type::Any, |t, _| Ok(t.is_union()));
        if result.is_err() {
            warn!("Couldn't simplify {} to a union", self);
        }
        result
    }

    fn has_variants(&self) -> bool {
        if matches!(self, Self::Enum(_) | Self::EnumUnion(_)) {
            return true;
        }

        if let Self::Unit(_, inner) = self {
            return inner.has_variants();
        }

        false
    }

    /// Simplify a type until you can get its variants.
    pub fn simplify_until_has_variants(&self, env: &Env) -> Result<Self, Error> {
        let result = self
            .clone()
            .simplify_until_matches(env, Type::Enum(vec![]), |t, _| Ok(t.has_variants()));
        if result.is_err() {
            warn!("Couldn't simplify {} to a type with variants", self);
        }
        result
    }

    fn is_polymorphic(&self) -> bool {
        if matches!(self, Self::Poly(_, _)) {
            return true;
        }

        if let Self::Unit(_, inner) = self {
            return inner.is_polymorphic();
        }

        false
    }

    pub fn simplify_until_atomic(&self, env: &Env) -> Result<Self, Error> {
        let result = self
            .clone()
            .simplify_until_matches(env, Type::Any, |t, _| Ok(t.is_atomic()));
        if result.is_err() {
            warn!("Couldn't simplify {} to an atomic type", self);
        }
        result
    }
    pub fn simplify_until_simple(&self, env: &Env) -> Result<Self, Error> {
        let result = self
            .clone()
            .simplify_until_matches(env, Type::Any, |t, _| Ok(t.is_simple()));
        if result.is_err() {
            warn!("Couldn't simplify {} to a simple type", self);
        }
        result
    }

    /// Simplify until the type is a polymorphic type.
    pub fn simplify_until_poly(&self, env: &Env) -> Result<Self, Error> {
        let result = self.clone().simplify_until_matches(
            env,
            Type::Poly(vec![], Box::new(Type::Any)),
            |t, _| Ok(t.is_polymorphic()),
        );
        if result.is_err() {
            warn!("Couldn't simplify {} to a polymorphic type", self);
        }
        result
    }

    /// Simplify until the type is concrete.
    pub fn simplify_until_concrete(&self, env: &Env) -> Result<Self, Error> {
        let result = self
            .clone()
            .simplify_until_matches(env, Type::Any, |t, _env| Ok(t.is_concrete()));
        if result.is_err() {
            warn!("Couldn't simplify {} to a concrete type", self);
        }
        result
    }

    /// Simplify an expression until it matches a given function which "approves" of a type.
    /// This will perform template applications to simplify the type if possible as well.
    ///
    /// This is usually too verbose to be used on its own, better to make a wrapper function
    /// that calls this with a more specific use case/purpose.
    pub fn simplify_until_matches(
        self,
        env: &Env,
        expected: Self,
        f: impl Fn(&Self, &Env) -> Result<bool, Error>,
    ) -> Result<Self, Error> {
        let mut simplified = self;
        // for _ in 0..Self::SIMPLIFY_RECURSION_LIMIT {
        for _ in 0..3 {
            if f(&simplified, env)? || simplified.is_atomic() {
                return Ok(simplified);
            }
            simplified = simplified.perform_template_applications(env, &mut HashMap::new())?
        }
        Err(Error::CouldntSimplify(simplified, expected))
    }

    /// Create a let-bound type.
    pub fn let_bind(name: &str, t: Self, ret: Self) -> Self {
        Self::Let(name.to_string(), Box::new(t), Box::new(ret))
    }

    /// Calculate the integral value of a variant in an enum.
    pub fn variant_index(variants: &[String], variant: &String) -> Option<usize> {
        let mut variants = variants.to_vec();
        variants.sort();
        if let Ok(index) = variants.binary_search(variant) {
            Some(index)
        } else {
            None
        }
    }

    /// Does this type contain a symbol with the given name?
    /// This will not count overshadowded versions of the symbol (overwritten by let-bindings).
    pub fn contains_symbol(&self, name: &str) -> bool {
        match self {
            Self::Unit(_unit_name, t) => {
                // Does the inner symbol use this type variable?
                t.contains_symbol(name)
            }
            Self::Type(t) => t.contains_symbol(name),
            Self::Poly(ty_params, template) => {
                if ty_params.contains(&name.to_string()) {
                    // This type variable is shadowed by a template variable.
                    false
                } else {
                    // Does the inner symbol use this type variable?
                    template.contains_symbol(name)
                }
            }
            Self::Apply(poly, ty_args) => {
                // Does the polymorphic type use this type variable?
                poly.contains_symbol(name)
                    // Do any of the type arguments use this type variable?
                    || ty_args.iter().any(|t| t.contains_symbol(name))
            }
            Self::Let(typename, t, ret) => {
                // We always check the type being bound to a variable.
                // We only check the body of the let if the variable isn't overshadowed, however.
                t.contains_symbol(name) || (typename != name && ret.contains_symbol(name))
            }
            Self::Symbol(typename) => typename == name,
            Self::None
            | Self::Never
            | Self::Any
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Enum(_) => false,

            Self::Tuple(items) => items.iter().any(|t| t.contains_symbol(name)),
            Self::Array(t, _) => t.contains_symbol(name),
            Self::Struct(fields) => fields.values().any(|t| t.contains_symbol(name)),
            Self::Union(fields) => fields.values().any(|t| t.contains_symbol(name)),
            Self::EnumUnion(fields) => fields.values().any(|t| t.contains_symbol(name)),

            Self::Proc(params, ret) => {
                params.iter().any(|t| t.contains_symbol(name)) || ret.contains_symbol(name)
            }
            Self::Pointer(_, t) => t.contains_symbol(name),
        }
    }

    /// Substitute all occurences of a symbol with another type.
    /// This will not traverse into let-bindings when the symbol is overshadowed.
    pub fn substitute(&self, name: &str, substitution: &Self) -> Self {
        match self {
            Self::Type(t) => Self::Type(Box::new(t.substitute(name, substitution))),
            Self::Let(typename, binding, ret) => Self::Let(
                typename.clone(),
                if typename == name {
                    // If the variable is overshadowed, then don't substitute.
                    binding.clone()
                } else {
                    // If the variable is not overshadowed, then we're free to substitute.
                    Box::new(binding.substitute(name, substitution))
                },
                if typename == name {
                    // If the variable is overshadowed, then don't substitute in the body of the let.
                    ret.clone()
                } else {
                    // If the variable is not overshadowed, then we're free to substitute in the body of the let.
                    ret.substitute(name, substitution).into()
                },
            ),
            Self::Poly(ty_params, template) => {
                if ty_params.contains(&name.to_string()) {
                    // This type variable is shadowed by a template variable.
                    self.clone()
                } else {
                    // Does the inner symbol use this type variable?
                    Self::Poly(
                        ty_params.clone(),
                        template.substitute(name, substitution).into(),
                    )
                }
            }
            Self::Apply(poly, ty_args) => Self::Apply(
                Box::new(poly.substitute(name, substitution)),
                ty_args
                    .iter()
                    .map(|t| t.substitute(name, substitution))
                    .collect(),
            ),
            Self::Symbol(typename) => {
                if typename == name {
                    return substitution.clone();
                }
                Self::Symbol(typename.clone())
            }
            Self::Unit(unit_name, inner) => Self::Unit(
                unit_name.clone(),
                Box::new(inner.substitute(name, substitution)),
            ),
            Self::None
            | Self::Never
            | Self::Any
            | Self::Int
            | Self::Float
            | Self::Cell
            | Self::Char
            | Self::Bool
            | Self::Enum(_) => self.clone(),
            Self::Tuple(items) => Self::Tuple(
                items
                    .iter()
                    .map(|field_t| field_t.substitute(name, substitution))
                    .collect(),
            ),
            Self::Array(item_t, size) => Self::Array(
                Box::new(item_t.substitute(name, substitution)),
                size.clone(),
            ),
            Self::Struct(fields) => Self::Struct(
                fields
                    .iter()
                    .map(|(field_name, field_t)| {
                        (field_name.clone(), field_t.substitute(name, substitution))
                    })
                    .collect(),
            ),
            Self::Union(fields) => Self::Union(
                fields
                    .iter()
                    .map(|(field_name, field_t)| {
                        (field_name.clone(), field_t.substitute(name, substitution))
                    })
                    .collect(),
            ),
            Self::EnumUnion(fields) => Self::EnumUnion(
                fields
                    .iter()
                    .map(|(field_name, field_t)| {
                        (field_name.clone(), field_t.substitute(name, substitution))
                    })
                    .collect(),
            ),
            Self::Proc(args, ret) => Self::Proc(
                args.iter()
                    .map(|arg| arg.substitute(name, substitution))
                    .collect(),
                Box::new(ret.substitute(name, substitution)),
            ),
            Self::Pointer(mutability, ptr) => {
                Self::Pointer(*mutability, Box::new(ptr.substitute(name, substitution)))
            }
        }
    }

    /// Does this type have an element type matching the supplied type?
    /// If this type is an array of Integers, for example, then this function
    /// will return true if the supplied type is an Integer.
    pub fn has_element_type(&self, element: &Self, env: &Env) -> Result<bool, Error> {
        match self {
            Self::Array(inner, _) => inner.equals(element, env),
            // Self::Pointer(_, inner) => inner.equals(element, env),
            _ => Ok(false),
        }
    }

    /// Can this type decay into another type?
    ///
    /// A type can decay into another type if the compiler can automatically convert the type
    /// into the other type. *This is distinct from the compiler seeing the types as equal.*
    /// The two types are *not* equal; the compiler will just automatically perform the conversion.
    ///
    /// For all cases right now, a decay does nothing; the representations of the types
    /// in the compiler are the same for all types of decay.
    pub fn can_decay_to(&self, desired: &Self, env: &Env) -> Result<bool, Error> {
        trace!("Checking if {} can decay to {}", self, desired);
        if self.equals(desired, env)? {
            return Ok(true);
        }

        // if !self.is_simple() || !desired.is_simple() {
        //     return Ok(false);
        // }

        match (self, desired) {
            // (Self::Unit(_, inner), _) => inner.can_decay_to(desired, env),
            (Self::Unit(name1, t1), Self::Unit(name2, t2)) => {
                if name1 == name2 {
                    t1.can_decay_to(t2, env)
                } else {
                    Ok(false)
                }
            }
            // (Self::Unit(_, inner), other) | (other, Self::Unit(_, inner)) => other.equals(inner, env),
            (expanded, Self::Symbol(name)) => {
                if let Some(t) = env.get_type(name) {
                    if expanded.equals(t, env)? {
                        trace!("{} can decay to {}", expanded, t);
                        return Ok(true);
                    }

                    return expanded.can_decay_to(t, env);
                }

                Ok(false)
            }

            // Can we decay a pointer to a pointer?
            (
                Self::Pointer(found_mutability, found_elem_ty),
                Self::Pointer(desired_mutabilty, desired_elem_ty),
            ) => {
                // Check if the mutabilities and element types can decay.
                if found_mutability.can_decay_to(desired_mutabilty)
                    && found_elem_ty.equals(desired_elem_ty, env)?
                {
                    // If they can, then we can decay.
                    trace!("{} can decay to {}", self, desired);
                    return Ok(true);
                }

                // Check if the mutabilities are compatible, and check if this type points to an array of elements of the desired element type.
                if found_mutability.can_decay_to(desired_mutabilty)
                    && found_elem_ty.has_element_type(desired_elem_ty, env)?
                {
                    // If so, then we can decay.
                    return Ok(true);
                }

                Ok(false)
            }

            // Can a tuple decay to another tuple?
            (Self::Tuple(found_fields), Self::Tuple(desired_fields)) => {
                // If the tuples have different numbers of fields, then we can't decay.
                if found_fields.len() != desired_fields.len() {
                    return Ok(false);
                }
                // Check if each field can decay to the corresponding field in the other tuple.
                for (found_field, desired_field) in found_fields.iter().zip(desired_fields.iter()) {
                    if !found_field.can_decay_to(desired_field, env)? {
                        // If any field can't decay, then we can't decay.
                        return Ok(false);
                    }
                }
                // If all fields can decay, then we can decay.
                Ok(true)
            }

            // Can an array decay to another array?
            (Self::Array(found_elem_ty, _), Self::Array(desired_elem_ty, _)) => {
                // Check if the element types can decay, and if the sizes are equal.
                if found_elem_ty.can_decay_to(desired_elem_ty, env)?
                    && self.get_size(env)? == desired.get_size(env)?
                {
                    trace!("{} can decay to {}", self, desired);
                    return Ok(true);
                }

                // Check if the element types are compatible, and if the sizes are equal.
                if found_elem_ty.has_element_type(desired_elem_ty, env)?
                    && self.get_size(env)? == desired.get_size(env)?
                {
                    trace!("{} can decay to {}", self, desired);
                    return Ok(true);
                }

                Ok(false)
            }

            // Can a struct decay to another struct?
            (Self::EnumUnion(found_fields), Self::EnumUnion(desired_fields))
            | (Self::Union(found_fields), Self::Union(desired_fields))
            | (Self::Struct(found_fields), Self::Struct(desired_fields)) => {
                // If the structs have a different number of fields, then they can't be equal.
                if found_fields.len() != desired_fields.len() {
                    return Ok(false);
                }
                // For each name in the first struct, check that the second struct has the same name.
                for (found_field_name, found_field_ty) in found_fields.iter() {
                    if let Some(desired_field_ty) = desired_fields.get(found_field_name) {
                        if !found_field_ty.can_decay_to(desired_field_ty, env)? {
                            // If any field can't decay, then we can't decay.
                            return Ok(false);
                        }
                    } else {
                        // If there's a difference in the names of the fields, then we can't decay.
                        return Ok(false);
                    }
                }
                // If we've made it this far, we know all the fields match and can decay.
                trace!("{} can decay to {}", self, desired);
                Ok(true)
            }

            // (Type::Apply(f, args), other) => {
            //     match *f.clone() {
            //         Type::Poly(params, template) => {
            //             let mut template = *template.clone();
            //             for (param, arg) in params.iter().zip(args.iter()) {
            //                 template = template.substitute(param, arg);
            //             }
            //             template.can_decay_to(other, env)
            //         }

            //     }
            //     // let mut f = f.clone();
            //     // for arg in args {
            //     //     f = f.substitute(, substitution)
            //     // }
            // }
            (Type::Cell, Type::Int) | (Type::Int, Type::Cell) => Ok(true),

            (a, b) => a.equals(b, env),
        }
    }

    /// Can this type be cast to another type?
    pub fn can_cast_to(&self, other: &Self, env: &Env) -> Result<bool, Error> {
        self.can_cast_to_checked(other, env, 0)
    }

    /// Can this type be cast to another type?
    /// This function should always halt (type casting *MUST* be decidable).
    fn can_cast_to_checked(&self, other: &Self, env: &Env, i: usize) -> Result<bool, Error> {
        trace!("Checking if {} can be cast to {}", self, other);

        if self == other {
            return Ok(true);
        }

        let i = i + 1;
        if i > Self::SIMPLIFY_RECURSION_LIMIT {
            error!(
                "Recursion depth limit reached while checking if {} can be cast to {}",
                self, other
            );
            return Err(Error::RecursionDepthTypeEquality(
                self.clone(),
                other.clone(),
            ));
        }

        let result = match (self, other) {
            (Self::Let(name, t, ret), other) | (other, Self::Let(name, t, ret)) => {
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                ret.can_cast_to_checked(other, &new_env, i)
            }

            (Self::Symbol(a), Self::Symbol(b)) if a == b => Ok(true),
            // If we're casting a value to a named type, then we need to check that the named type can be cast.
            (a, Self::Symbol(b)) | (Self::Symbol(b), a) => {
                // Get the named type...
                if let Some(t) = env.get_type(b) {
                    // ...and check if the value can be cast to it.
                    a.can_cast_to_checked(t, env, i)
                } else {
                    // If the named type doesn't exist, then we can't cast.
                    Ok(false)
                }
            }

            // Two Units can only be cast between one another if they have the same name, and the types inside them can be cast.
            (Self::Unit(unit_name1, t1), Self::Unit(unit_name2, t2))
                if unit_name1 == unit_name2 =>
            {
                t1.can_cast_to_checked(t2, env, i)
            }
            // If we're casting to or from a Unit, we can only cast if the type inside the Unit can be cast.
            (Self::Unit(_, t), other) | (other, Self::Unit(_, t)) => {
                t.can_cast_to_checked(other, env, i)
            }

            (Self::Int, Self::Float) | (Self::Float, Self::Int) => Ok(true),
            (Self::Int, Self::Char) | (Self::Char, Self::Int) => Ok(true),
            (Self::Int, Self::Bool) | (Self::Bool, Self::Int) => Ok(true),
            (Self::Int, Self::Enum(_)) | (Self::Enum(_), Self::Int) => Ok(true),

            (Self::Cell, Self::Int) | (Self::Int, Self::Cell) => Ok(true),
            (Self::Cell, Self::Float) | (Self::Float, Self::Cell) => Ok(true),
            (Self::Cell, Self::Char) | (Self::Char, Self::Cell) => Ok(true),
            (Self::Cell, Self::Bool) | (Self::Bool, Self::Cell) => Ok(true),

            (Self::Pointer(_, _), Self::Cell) => Ok(true),
            (Self::Pointer(found, _), Self::Pointer(desired, _)) => Ok(found.can_decay_to(desired)),

            (Self::Any, _) | (_, Self::Any) => Ok(true),

            (Self::Struct(fields1), Self::Struct(fields2)) => {
                // If the structs have a different number of fields, then they can't be equal.
                if fields1.len() != fields2.len() {
                    return Ok(false);
                }
                // For each name in the first struct, check that the second struct has the same name.
                for (name1, t1) in fields1.iter() {
                    if let Some(t2) = fields2.get(name1) {
                        // Then, check that the types under the name are equal.
                        if !t1.can_cast_to_checked(t2, env, i)? {
                            return Ok(false);
                        }
                    } else {
                        return Ok(false);
                    }
                }
                // If we've made it this far, then we can reasonably assume the user is trying to
                // cast between two very similar structs. If all their fields have the same names,
                // and the types under those names are equal, then we can assume they are the same
                // kind of struct.
                Ok(true)
            }

            (Self::Union(fields1), Self::Union(fields2)) => {
                // If the unions have a different number of fields, then they can't be equal.
                if fields1.len() != fields2.len() {
                    return Ok(false);
                }
                // For each name in the first union, check that the second union has the same name.
                for (name1, t1) in fields1.iter() {
                    if let Some(t2) = fields2.get(name1) {
                        // Then, check that the types under the name are equal.
                        if !t1.can_cast_to_checked(t2, env, i)? {
                            return Ok(false);
                        }
                    } else {
                        return Ok(false);
                    }
                }
                // If we've made it this far, then we can reasonably assume the user is trying to
                // cast between two very similar unions. If all their fields have the same names,
                // and the types under those names are equal, then we can assume they are the same
                // kind of union.
                Ok(true)
            }

            (Self::Proc(args1, ret1), Self::Proc(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return Ok(false);
                }
                for (a, b) in args1.iter().zip(args2.iter()) {
                    if !a.can_cast_to_checked(b, env, i)? {
                        return Ok(false);
                    }
                }
                ret1.can_cast_to_checked(ret2, env, i)
            }

            (a, b) => a.can_decay_to(b, env),
        }?;

        trace!("Can cast? {} -> {}: {}", self, other, result);

        Ok(result)
    }

    /// Are two types structurally equal?
    pub fn equals(&self, other: &Self, env: &Env) -> Result<bool, Error> {
        self.equals_checked(other, &mut HashSet::new(), env, 0)
    }

    /// Perform type applications if possible.
    pub fn perform_template_applications(
        &self,
        env: &Env,
        previous_applications: &mut HashMap<(Type, Vec<Type>), Type>,
    ) -> Result<Self, Error> {
        let _before = self.to_string();
        trace!("Performing template applications on {}", self);
        // let is_recursive = matches!(self, Self::Apply(_, _)) || self.is_recursive(env)?;
        // If the type is an Apply on a Poly, then we can perform the application.
        // First, perform the applications on the type arguments.
        // We can use memoization with the previous_applications HashMap to avoid infinite recursion.
        Ok(match self.clone().simplify(env)? {
            Self::Apply(poly, ty_args) => {
                let pair = (
                    poly.perform_template_applications(env, previous_applications)?,
                    ty_args
                        .into_iter()
                        .map(|t| t.perform_template_applications(env, previous_applications))
                        .collect::<Result<Vec<_>, _>>()?,
                );
                if let Some(t) = previous_applications.get(&pair) {
                    t.clone()
                } else {
                    let (poly, ty_args) = pair;

                    match poly {
                        Self::Poly(params, mono_ty) => {
                            let _poly = Self::Poly(params.clone(), mono_ty.clone());
                            let mut mono_ty = *mono_ty;
                            for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                                mono_ty = mono_ty.substitute(param, ty_arg);
                            }
                            mono_ty
                        }
                        Self::Symbol(s) => match env.get_type(s.as_str()).cloned() {
                            Some(Self::Poly(params, mono_ty)) => {
                                let mut mono_ty = *mono_ty;
                                for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                                    mono_ty = mono_ty.substitute(param, ty_arg);
                                }
                                mono_ty
                            }
                            Some(other) => Self::Apply(Box::new(other), ty_args),
                            None => self.clone(),
                        },
                        _ => self.clone(),
                    }
                }
            }
            Self::Pointer(mutability, inner) => Self::Pointer(
                mutability,
                Box::new(inner.perform_template_applications(env, previous_applications)?),
            ),
            Self::Proc(args, ret) => Self::Proc(
                args.into_iter()
                    .map(|t| t.perform_template_applications(env, previous_applications))
                    .collect::<Result<Vec<_>, _>>()?,
                Box::new(ret.perform_template_applications(env, previous_applications)?),
            ),
            Self::Struct(fields) if !self.is_recursive(env)? => Self::Struct(
                fields
                    .into_iter()
                    .map(|(name, t)| {
                        Ok((
                            name,
                            t.perform_template_applications(env, previous_applications)?,
                        ))
                    })
                    .collect::<Result<BTreeMap<_, _>, Error>>()?,
            ),
            Self::Union(fields) if !self.is_recursive(env)? => Self::Union(
                fields
                    .into_iter()
                    .map(|(name, t)| {
                        Ok((
                            name,
                            t.perform_template_applications(env, previous_applications)?,
                        ))
                    })
                    .collect::<Result<BTreeMap<_, _>, Error>>()?,
            ),

            Self::EnumUnion(fields) if !self.is_recursive(env)? => Self::EnumUnion(
                fields
                    .into_iter()
                    .map(|(name, t)| {
                        Ok((
                            name,
                            t.perform_template_applications(env, previous_applications)?,
                        ))
                    })
                    .collect::<Result<BTreeMap<_, _>, Error>>()?,
            ),

            Self::Tuple(items) if !self.is_recursive(env)? => Self::Tuple(
                items
                    .into_iter()
                    .map(|t| t.perform_template_applications(env, previous_applications))
                    .collect::<Result<Vec<_>, _>>()?,
            ),

            Self::Array(item_t, size) if !self.is_recursive(env)? => Self::Array(
                Box::new(item_t.perform_template_applications(env, previous_applications)?),
                size,
            ),

            other => other,
        })
    }

    /// Are two types structurally equal?
    /// This function should always halt (type equality *MUST* be decidable).
    fn equals_checked(
        &self,
        other: &Self,
        compared_symbols: &mut HashSet<(String, String)>,
        env: &Env,
        i: usize,
    ) -> Result<bool, Error> {
        if self == other {
            return Ok(true);
        }

        if i >= Self::SIMPLIFY_RECURSION_LIMIT {
            warn!(
                "Recursion depth limit reached while checking if {} equals {}",
                self, other
            );
            return Ok(false);
        }
        trace!("Checking if {} equals {}", self, other);

        let i = i + 1;

        Ok(match (self, other) {
            (Self::Any, _)
            | (_, Self::Any)
            | (Self::Never, _)
            | (_, Self::Never)
            | (Self::None, Self::None)
            | (Self::Bool, Self::Bool)
            | (Self::Char, Self::Char)
            | (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::Cell, Self::Cell) => true,

            (Self::Symbol(a), Self::Symbol(b)) => {
                if a == b {
                    // If the two types have the same name, they must equal the same type
                    // under the same environment.
                    true
                } else if i > Self::SIMPLIFY_RECURSION_LIMIT / 2
                    && compared_symbols.contains(&(a.clone(), b.clone()))
                {
                    // If we've seen this comparison of these two symbols before, and we've done
                    // lots of symbol replacements (indicated by `i`), then we're probably trying
                    // to compare two distinct types which are asymptotically unequal.
                    false
                } else {
                    // To get as much specific information as possible about type errors,
                    // we just say the types are unequal if we cannot simplify symbols further.
                    compared_symbols.insert((a.clone(), b.clone()));
                    match (env.get_type(a), env.get_type(b)) {
                        (Some(a), Some(b)) => a.equals_checked(b, compared_symbols, env, i)?,
                        _ => false,
                    }
                }
            }
            (Self::Symbol(x), y) | (y, Self::Symbol(x)) => {
                // To get as much specific information as possible about type errors,
                // we just say the types are unequal if we cannot simplify symbols further.
                match env.get_type(x) {
                    Some(t) => t.equals_checked(y, compared_symbols, env, i)?,
                    None => false,
                }
            }

            (Self::Let(name1, t1, ret1), Self::Let(name2, t2, ret2)) => {
                if !t1.contains_symbol(name1) {
                    // If the type we're binding doesn't contain itself, then we can
                    // just substitute it in the body and compare it to the other type.
                    ret1.substitute(name1, t1)
                        .equals_checked(other, compared_symbols, env, i)?
                } else if !t2.contains_symbol(name2) {
                    // If the type we're binding doesn't contain itself, then we can
                    // just substitute it in the body and compare it to the other type.
                    self.equals_checked(&ret2.substitute(name2, t2), compared_symbols, env, i)?
                } else {
                    // Otherwise, we are comparing recursive types.

                    // Check if the bound types are equal when substituting in the name of the other let's bound variable.
                    // Then, confirm that the results of the let bodies are equal under the substitution.
                    t2.equals_checked(
                        &t1.substitute(name1, &Self::Symbol(name2.clone())),
                        compared_symbols,
                        env,
                        i,
                    )? && ret2.equals_checked(
                        &ret1.substitute(name1, &Self::Symbol(name2.clone())),
                        compared_symbols,
                        env,
                        i,
                    )?
                }
            }
            (Self::Let(name, t, ret), x) | (x, Self::Let(name, t, ret)) => {
                if !t.contains_symbol(name) {
                    // If the type we're binding doesn't contain itself, then we can
                    // just substitute it in the body and compare it to the other type.
                    ret.substitute(name, t)
                        .equals_checked(x, compared_symbols, env, i)?
                } else {
                    // If the type does contain itself, we'll have to do some more legwork.
                    // Create a new environment with the type binding.
                    let mut new_env = env.clone();
                    new_env.define_type(
                        name,
                        if **ret == Self::Symbol(name.clone()) {
                            // If the name we're binding to is the result of the let-binding, then we can
                            // substitute the name under the bound type with the original entire let-binding.
                            t.substitute(name, &Self::Let(name.clone(), t.clone(), ret.clone()))
                        } else {
                            // Otherwise, we can't reason much about the type and so we just bind
                            // it to the name unmodified.
                            *t.clone()
                        },
                    );

                    // Check if the two types are equal under the new environment.
                    ret.equals_checked(x, compared_symbols, &new_env, i)?
                }
            }

            // If we're comparing two units, then we can just compare their names and confirm
            // their structures are equal.
            (Self::Unit(unit_name1, t1), Self::Unit(unit_name2, t2)) => {
                unit_name1 == unit_name2 && t1.equals_checked(t2, compared_symbols, env, i)?
            }

            (Self::Enum(a), Self::Enum(b)) => {
                let mut a = a.clone();
                let mut b = b.clone();
                a.sort();
                b.sort();
                a == b
            }

            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for (item1, item2) in a.iter().zip(b.iter()) {
                    if !item1.equals_checked(item2, compared_symbols, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }
            (Self::Array(t1, size1), Self::Array(t2, size2)) => {
                t1.equals_checked(t2, compared_symbols, env, i)?
                    && size1.clone().as_int(env)? == size2.clone().as_int(env)?
            }
            (Self::Struct(a), Self::Struct(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for ((name1, item1), (name2, item2)) in a.iter().zip(b.iter()) {
                    if name1 != name2 || !item1.equals_checked(item2, compared_symbols, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }

            (Self::Union(a), Self::Union(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for ((name1, item1), (name2, item2)) in a.iter().zip(b.iter()) {
                    if name1 != name2 || !item1.equals_checked(item2, compared_symbols, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }

            (Self::EnumUnion(a), Self::EnumUnion(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }
                for ((name1, item1), (name2, item2)) in a.iter().zip(b.iter()) {
                    if name1 != name2 || !item1.equals_checked(item2, compared_symbols, env, i)? {
                        return Ok(false);
                    }
                }
                true
            }

            (Self::Proc(args1, ret1), Self::Proc(args2, ret2)) => {
                if args1.len() != args2.len() {
                    return Ok(false);
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    if !arg1.equals_checked(arg2, compared_symbols, env, i)? {
                        return Ok(false);
                    }
                }
                ret1.equals_checked(ret2, compared_symbols, env, i)?
            }

            (Self::Pointer(m1, t1), Self::Pointer(m2, t2)) => {
                m1 == m2 && t1.equals_checked(t2, compared_symbols, env, i)?
            }

            (Self::Poly(ty_params1, template1), Self::Poly(ty_params2, template2)) => {
                if ty_params1.len() != ty_params2.len() {
                    return Ok(false);
                }

                if ty_params1 == ty_params2 {
                    // If the two templates have the same type parameters, then we can just compare
                    // the two templates.
                    return template1.equals_checked(template2, compared_symbols, env, i);
                }

                // Create a new environment.
                let mut new_env = env.clone();
                for (name1, name2) in ty_params1.iter().zip(ty_params2.iter()) {
                    // In the new environment, bind the two type parameters to the same type.
                    let combined_name = format!("{name1}+{name2}");
                    let combined_ty = Self::Unit(combined_name, Box::new(Type::Any));
                    new_env.define_type(name1, combined_ty.clone());
                    new_env.define_type(name2, combined_ty);
                }

                // Now, we can compare the two templates under the new environment.
                template1.equals_checked(template2, compared_symbols, &new_env, i)?
            }

            (Self::Apply(poly1, ty_args1), Self::Apply(poly2, ty_args2)) => {
                if poly1.equals_checked(poly2, compared_symbols, env, i)? {
                    // If the two polymorphic types are equal, then we can just compare the two
                    // types' parameters.
                    if ty_args1.len() != ty_args2.len() {
                        return Ok(false);
                    }

                    // Iterate over the two type arguments and compare them.
                    for (arg1, arg2) in ty_args1.iter().zip(ty_args2.iter()) {
                        // If the two arguments are not equal, then the two types are not equal.
                        if !arg1.equals_checked(arg2, compared_symbols, env, i)? {
                            return Ok(false);
                        }
                    }

                    true
                } else if let Self::Poly(ty_params, template) = poly1.clone().simplify(env)? {
                    let mut template = *template.clone();
                    for (param, arg) in ty_params.iter().zip(ty_args1.iter()) {
                        template = template.substitute(param, arg);
                    }
                    template.equals_checked(other, compared_symbols, env, i)?
                } else if let Self::Poly(ty_params, template) = poly2.clone().simplify(env)? {
                    let mut template = *template.clone();
                    for (param, arg) in ty_params.iter().zip(ty_args2.iter()) {
                        template = template.substitute(param, arg);
                    }
                    template.equals_checked(other, compared_symbols, env, i)?
                } else {
                    // If the two polymorphic types are not equal, then we can't just compare the two
                    // types' parameters. We need to simplify the types first.
                    // other.clone().simplify_until_concrete(env)?.equals_checked(
                    //     self,
                    //     compared_symbols,
                    //     env,
                    //     i,
                    // )?
                    false
                }
            }

            (Self::Apply(poly, ty_args), b) | (b, Self::Apply(poly, ty_args)) => {
                Self::Apply(poly.clone(), ty_args.clone())
                    .simplify_until_concrete(env)?
                    .equals_checked(b, compared_symbols, env, i)?
            }

            _ => {
                // trace!("{} is not equal to {}", a, b);
                false
            }
        })
    }

    /// Get the type and offset of a member of a type.
    /// This will confirm the member exists, and then get its type and offset in memory from
    /// the base address of the value.
    pub(super) fn get_member_offset(
        &self,
        member: &ConstExpr,
        expr: &Expr,
        env: &Env,
    ) -> Result<(Type, usize), Error> {
        trace!("Getting offset of member {member} in expression {self} in the environment {env}");
        match self {
            Type::Pointer(_, t) => t.get_member_offset(member, expr, env),
            Type::Struct(members) => {
                let mut offset = 0;
                for (k, t) in members.clone() {
                    // If this element is the requested member
                    if &ConstExpr::Symbol(k) == member {
                        // Return the member's type and the offset in memory
                        // from the value's address
                        return Ok((t.simplify(env)?, offset));
                    }

                    let size = t.get_size(env)?;
                    offset += size;
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Tuple(items) => {
                let mut offset = 0;
                for (i, t) in items.iter().enumerate() {
                    // If this element is the requested member
                    if &ConstExpr::Int(i as i64) == member {
                        // Simplify the type under the environment
                        let result = t.clone().simplify(env)?;
                        // Return the member's type and its offset in memory
                        // from the value's address.
                        return Ok((result, offset));
                    }

                    let size = t.get_size(env)?;
                    offset += size;
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Union(types) => match types.get(&member.clone().as_symbol(env)?) {
                Some(t) => Ok((t.clone().simplify(env)?, 0)),
                None => Err(Error::MemberNotFound(expr.clone(), member.clone())),
            },
            Type::Let(name, t, ret) => {
                // Create a new scope and define the new type within it
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                // Find the member offset of the returned type under the new scope
                //
                // NOTE:
                // We simplify the type before AND after getting the member offset because
                // we want to make sure that recursive types don't leave undefined symbols
                // in the in the resulting type.
                let (t, offset) = ret
                    .clone()
                    .simplify(env)?
                    .get_member_offset(member, expr, &new_env)?;
                Ok((t.simplify(&new_env)?, offset))
            }

            Type::Unit(_unit_name, t) => t.get_member_offset(member, expr, env),

            Type::Apply(_, _) | Type::Poly(_, _) => {
                let t = self.simplify_until_concrete(env)?;
                t.get_member_offset(member, expr, env)
            }

            Type::Symbol(name) => {
                if let Some(t) = env.get_type(name) {
                    t.get_member_offset(member, expr, env)
                } else {
                    error!("Type {self} not defined in environment {env}");
                    Err(Error::TypeNotDefined(name.clone()))
                }
            }

            _ => Err(Error::MemberNotFound(expr.clone(), member.clone())),
        }
    }

    pub(super) fn type_check_member(
        &self,
        member: &ConstExpr,
        expr: &Expr,
        env: &Env,
    ) -> Result<(), Error> {
        match self {
            Type::Type(ty) => {
                let name = member.clone().as_symbol(env)?;

                if env.has_associated_const(ty, &name) {
                    Ok(())
                } else {
                    Err(Error::MemberNotFound(expr.clone(), member.clone()))
                }
            }

            Type::Pointer(_, t) => t.type_check_member(member, expr, env),

            Type::Struct(members) => {
                for (k, _) in members.clone() {
                    // If this element is the requested member
                    if &ConstExpr::Symbol(k) == member {
                        // Return the member's type and the offset in memory
                        // from the value's address
                        return Ok(());
                    }
                }
                // error!("{} does not have member {}", self, member);
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Tuple(items) => {
                for (i, _) in items.iter().enumerate() {
                    // If this element is the requested member
                    if &ConstExpr::Int(i as i64) == member {
                        // Return the member's type and its offset in memory
                        // from the value's address.
                        return Ok(());
                    }
                }

                let name = member.clone().as_symbol(env)?;
                if env.has_associated_const(self, &name) {
                    Ok(())
                } else {
                    Err(Error::MemberNotFound(expr.clone(), member.clone()))
                }
            }
            Type::Union(types) => {
                let name = member.clone().as_symbol(env)?;
                match types.get(&name) {
                    Some(_) => Ok(()),
                    None => {
                        if env.has_associated_const(self, &name) {
                            Ok(())
                        } else {
                            Err(Error::MemberNotFound(expr.clone(), member.clone()))
                        }
                    }
                }
            }
            Type::Let(name, t, ret) => {
                // Create a new scope and define the new type within it
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                trace!("Type checking member of let-bound type {self}");
                // Find the member offset of the returned type under the new scope
                //
                // NOTE:
                // We simplfy the type before AND after getting the member offset because
                // we want to make sure that recursive types don't leave undefined symbols
                // in the in the resulting type.
                ret.clone()
                    .simplify(env)?
                    .type_check_member(member, expr, &new_env)
            }

            Type::Unit(_unit_name, t) => t.type_check_member(member, expr, env),

            Type::Symbol(name) => {
                if let Some(t) = env.get_type(name) {
                    t.type_check_member(member, expr, env)
                } else {
                    // error!("Type {self} not defined in environment {env}");
                    Err(Error::TypeNotDefined(name.clone()))
                }
            }

            Type::Apply(_, _) | Type::Poly(_, _) => {
                let t = self.simplify_until_concrete(env)?;
                trace!("Simplified {self} to {t}");
                t.type_check_member(member, expr, env)
            }

            Type::Any => {
                // Any type can have any member
                warn!("Type checking member `{member}` of Any type {self}");
                Ok(())
            }

            other => {
                // Check if type has an associated constant with the given name
                let name = member.clone().as_symbol(env)?;
                if env.has_associated_const(other, &name) {
                    Ok(())
                } else {
                    // error!("Type {self} does not have member {member}");
                    Err(Error::MemberNotFound(expr.clone(), member.clone()))
                }
            }
        }
    }
}

impl Simplify for Type {
    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        trace!("Simplifying type {self} in environment {env}");
        if self.is_atomic() {
            return Ok(self);
        }

        let i = i + 1;
        if i > Self::SIMPLIFY_RECURSION_LIMIT {
            error!("Recursion depth limit reached while simplifying type {self}");
            return Err(Error::UnsizedType(self.clone()));
        }

        let _s = self.to_string();
        let result = match self {
            Self::Type(t) => Self::Type(t),

            Self::None
            | Self::Never
            | Self::Any
            | Self::Int
            | Self::Float
            | Self::Char
            | Self::Bool
            | Self::Cell
            | Self::Enum(_)
            | Self::Poly(_, _) => self.clone(),
            Self::Pointer(mutability, inner) => {
                Self::Pointer(mutability, Box::new(inner.simplify_checked(env, i)?))
            }

            Self::Let(name, t, ret) => {
                // Is the bound type recursive?
                if t.contains_symbol(&name) {
                    // If so, create a new environment with the new type bound.
                    let mut new_env = env.clone();
                    new_env.define_type(&name, *t.clone());
                    // Simplify the result of the let body under the new environment.
                    let result = ret.clone().simplify_checked(&new_env, i)?;
                    if *ret == Self::Symbol(name.clone()) {
                        trace!("Let-bound type returns binding, replacing {name} with bound type {t} in {result}");
                        // If the let body is the bound type, then we can subsitute the bound type
                        // for a copy of the whole let-binding.
                        result.substitute(&name, &Self::Let(name.clone(), t, ret))
                    } else {
                        // Otherwise, we can't reason much about the result, so return what
                        // we have.
                        result
                    }
                } else {
                    trace!("Let-bound type {t} does not contain bound variable {name}, can substitute into {ret}");
                    // If the type isn't recursive, we can just substitute the variable for the type binding!
                    ret.substitute(&name, &t).simplify_checked(env, i)?
                }
            }

            Self::Unit(unit_name, t) => {
                Self::Unit(unit_name, Box::new(t.simplify_checked(env, i)?))
                // self
            }

            Self::Symbol(ref name) => {
                if let Some(t) = env.get_type(name) {
                    t.clone()
                } else {
                    self.clone()
                }
            }

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
                    .map(|(k, t)| Ok((k, t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),
            Self::Union(types) => Self::Union(
                types
                    .into_iter()
                    .map(|(k, t)| Ok((k, t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),
            Self::EnumUnion(types) => Self::EnumUnion(
                types
                    .into_iter()
                    .map(|(k, t)| Ok((k, t.simplify_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            Self::Apply(poly, ty_args) => {
                // let poly_clone = poly.clone();
                // match *poly {
                //     Self::Poly(params, mut result) => {
                //         for (param, arg) in params.iter().zip(ty_args.iter()) {
                //             *result = result.substitute(param, arg);
                //         }
                //         // *result = result.simplify_checked(env, i)?;

                //         // Add mono type to environment
                //         env.add_monomorphized_associated_consts(*poly_clone, *result.clone(), ty_args)?;
                //         *result
                //     }
                //     _ => Self::Apply(Box::new(poly.simplify_checked(env, i)?), ty_args)
                // }

                Self::Apply(Box::new(poly.simplify_checked(env, i)?), ty_args)
            }
        };
        Ok(result)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Type(t) => write!(f, "{t}"),
            Self::Any => write!(f, "Any"),
            Self::Never => write!(f, "Never"),
            Self::Pointer(mutability, ty) => {
                write!(f, "&")?;
                if mutability.is_mutable() {
                    write!(f, "mut ")?;
                }
                write!(f, "{ty}")
            }
            Self::Bool => write!(f, "Bool"),
            Self::Char => write!(f, "Char"),
            Self::Cell => write!(f, "Cell"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::None => write!(f, "None"),
            Self::Array(ty, len) => write!(f, "[{ty} * {len}]"),
            Self::Poly(ty_params, template) => {
                write!(f, "(")?;
                for (i, param) in ty_params.iter().enumerate() {
                    write!(f, "{param}")?;
                    if i < ty_params.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ") => {template}")
            }
            Self::Apply(poly, ty_args) => {
                write!(f, "({poly})<")?;
                for (i, arg) in ty_args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if i < ty_args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ">")
            }
            Self::Enum(variants) => {
                write!(f, "enum {{")?;
                for (i, variant) in variants.iter().enumerate() {
                    write!(f, "{variant}")?;
                    if i < variants.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
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
                write!(f, "{{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    write!(f, "{name}: {ty}")?;
                    if i < fields.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Union(fields) => {
                write!(f, "union {{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    write!(f, "{name}: {ty}")?;
                    if i < fields.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::EnumUnion(fields) => {
                write!(f, "enum {{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    write!(f, "{name} {ty}")?;
                    if i < fields.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Proc(args, ret) => {
                if args.is_empty() {
                    write!(f, "() -> {ret}")?;
                    return Ok(());
                }
                if args.len() == 1 {
                    write!(f, "def({arg}) -> {ret}", arg = args[0])?;
                    return Ok(());
                }
                write!(f, "(")?;
                for (i, ty) in args.iter().enumerate() {
                    write!(f, "{ty}")?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ") -> {ret}")
            }

            Self::Symbol(name) => write!(f, "{name}"),
            Self::Unit(unit_name, _ty) => write!(f, "unit {unit_name}"),
            Self::Let(name, ty, ret) => write!(f, "let {name} = {ty} in {ret}"),
        }
    }
}

impl Eq for Type {}
impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Any => {
                state.write_u8(0);
            }
            Self::Never => {
                state.write_u8(1);
            }
            Self::Pointer(m, ty) => {
                state.write_u8(2);
                m.hash(state);
                ty.hash(state);
            }
            Self::Bool => {
                state.write_u8(3);
            }
            Self::Char => {
                state.write_u8(4);
            }
            Self::Cell => {
                state.write_u8(5);
            }
            Self::Int => {
                state.write_u8(6);
            }
            Self::Float => {
                state.write_u8(7);
            }
            Self::None => {
                state.write_u8(8);
            }
            Self::Array(ty, len) => {
                state.write_u8(9);
                ty.hash(state);
                len.to_string().hash(state);
            }
            Self::Poly(ty_params, template) => {
                state.write_u8(10);
                ty_params.hash(state);
                template.hash(state);
            }
            Self::Apply(poly, ty_args) => {
                state.write_u8(11);
                poly.hash(state);
                ty_args.hash(state);
            }
            Self::Enum(variants) => {
                state.write_u8(12);
                variants.hash(state);
            }
            Self::Tuple(items) => {
                state.write_u8(13);
                items.hash(state);
            }
            Self::Struct(fields) => {
                state.write_u8(14);
                fields.hash(state);
            }
            Self::Union(fields) => {
                state.write_u8(15);
                fields.hash(state);
            }
            Self::EnumUnion(fields) => {
                state.write_u8(16);
                fields.hash(state);
            }
            Self::Proc(args, ret) => {
                state.write_u8(17);
                args.hash(state);
                ret.hash(state);
            }
            Self::Symbol(name) => {
                state.write_u8(18);
                name.hash(state);
            }
            Self::Unit(unit_name, ty) => {
                state.write_u8(19);
                unit_name.hash(state);
                ty.hash(state);
            }
            Self::Let(name, ty, ret) => {
                state.write_u8(20);
                name.hash(state);
                ty.hash(state);
                ret.hash(state);
            }
            Self::Type(t) => {
                state.write_u8(21);
                t.hash(state);
            }
        }
    }
}
