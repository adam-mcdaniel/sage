//! # Types Module
//!
//! This module contains a collection of types and traits
//! used to implement and confirm the soundness of the LIR
//! typesystem.
use super::{ConstExpr, Env, Error, Expr, Simplify};
use core::fmt;
use std::collections::{BTreeMap, HashMap, HashSet};
mod check;
mod inference;
mod size;
pub use check::*;
pub use inference::*;
pub use size::*;

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
    Pointer(Box<Self>),
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

impl Type {
    /// This is the maximum number of times a type will be simplified recursively.
    pub const SIMPLIFY_RECURSION_LIMIT: usize = 20;

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
            Self::Poly(_, _)
            | Self::Symbol(_)
            | Self::Apply(_, _)
            | Self::Let(_, _, _) => false,
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
            | Self::Array(_, _)
            | Self::Tuple(_)
            | Self::Unit(_, _)
            | Self::Pointer(_) => true,
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
            | Self::Enum(_) => true,
            _ => false
        }
    }

    // pub fn contains(&self,)

    /// Simplify an expression until it matches a given function.
    pub fn simplify_until_matches(
        self,
        env: &Env,
        expected: Self,
        f: impl Fn(&Self, &Env) -> Result<bool, Error>,
    ) -> Result<Self, Error> {
        let mut simplified = self;
        for _ in 0..Self::SIMPLIFY_RECURSION_LIMIT {
            // eprintln!("STAGE {i}: {simplified}", i = i, simplified = simplified);
            // if f(&simplified, env)? || matches!(simplified, Type::Never | Type::Int | Type::Any | Type::Cell | Type::Bool | Type::Float | Type::Char | Type::None | Type::Enum(_)) {
            if f(&simplified, env)? || simplified.is_atomic() {
                return Ok(simplified);
            }
            simplified = simplified.simplify(env)?.perform_template_applications(env, &mut HashMap::new(), 0)?
        }
        Err(Error::CouldntSimplify(simplified, expected))
    }

    /// Create a let-bound type.
    pub fn let_bind(name: &str, t: Self, ret: Self) -> Self {
        Self::Let(name.to_string(), Box::new(t), Box::new(ret))
    }

    /// Calculate the integral value of a variant in an enum.
    pub fn variant_index(variants: &Vec<String>, variant: &String) -> Option<usize> {
        let mut variants = variants.clone();
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
            Self::Pointer(t) => t.contains_symbol(name),
        }
    }

    /// Substitute all occurences of a symbol with another type.
    /// This will not traverse into let-bindings when the symbol is overshadowed.
    pub fn substitute(&self, name: &str, substitution: &Self) -> Self {
        match self {
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
                    template.substitute(name, substitution).into()
                }
            }
            Self::Apply(poly, ty_args) => {
                Self::Apply(
                    Box::new(poly.substitute(name, substitution)),
                    ty_args
                        .iter()
                        .map(|t| t.substitute(name, substitution))
                        .collect(),
                )
            },
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
            Self::Pointer(ptr) => Self::Pointer(Box::new(ptr.substitute(name, substitution))),
        }
    }

    /// Can this type be cast to another type?
    pub fn can_cast_to(&self, other: &Self, env: &Env) -> Result<bool, Error> {
        self.can_cast_to_checked(other, env, 0)
    }

    /// Can this type be cast to another type?
    /// This function should always halt (type casting *MUST* be decidable).
    fn can_cast_to_checked(&self, other: &Self, env: &Env, i: usize) -> Result<bool, Error> {
        if self == other {
            return Ok(true);
        }

        let i = i + 1;
        if i > 500 {
            return Err(Error::RecursionDepthTypeEquality(
                self.clone(),
                other.clone(),
            ));
        }

        match (self, other) {
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
            (Self::Cell, Self::Pointer(_)) | (Self::Pointer(_), Self::Cell) => Ok(true),
            (Self::Pointer(_), Self::Pointer(_)) => Ok(true),

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

            (a, b) => {
                if a.equals(b, env)? {
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    /// Are two types structurally equal?
    pub fn equals(&self, other: &Self, env: &Env) -> Result<bool, Error> {
        self.equals_checked(other, &mut HashSet::new(), env, 0)
    }

    /// Perform type applications if possible.
    pub fn perform_template_applications(&self, env: &Env, previous_applications: &mut HashMap<(Type, Vec<Type>), Type>, i: usize) -> Result<Self, Error> {
        // If the type is an Apply on a Poly, then we can perform the application.
        // First, perform the applications on the type arguments.
        // We can use memoization with the previous_applications HashMap to avoid infinite recursion.
        match self.clone().simplify(env)? {
            Self::Apply(poly, ty_args) => {
                let pair = (*poly, ty_args.into_iter().map(|t| t.clone().simplify(env)).collect::<Result<Vec<_>, _>>()?);
                if let Some(t) = previous_applications.get(&pair) {
                    return Ok(t.clone());
                }
                let (poly, ty_args) = pair;

                match poly {
                    Self::Poly(params, template) => {
                        let save = template.clone();
                        let mut template = *template;
                        for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                            template = template.substitute(param, ty_arg);
                        }
                        // template = template.simplify(env)?;
                        // previous_applications.insert((Self::Poly(params, save), ty_args), template.clone());
                        Ok(template)
                    }
                    Self::Symbol(s) => {
                        match env.get_type(s.as_str()).cloned() {
                            Some(Self::Poly(params, template)) => {
                                let save = template.clone();
                                let mut template = *template;
                                for (param, ty_arg) in params.iter().zip(ty_args.iter()) {
                                    template = template.substitute(param, ty_arg);
                                }
                                // template = template.simplify(env)?;
                                // previous_applications.insert((Self::Poly(params, save), ty_args), template.clone());
                                Ok(template)
                            }
                            Some(other) => {
                                Ok(Self::Apply(Box::new(other), ty_args))
                            }
                            None => {
                                Ok(self.clone())
                            }
                        }
                    }
                    _ => {
                        Ok(self.clone())
                    }
                }
            }
            other => {
                return Ok(other);
            }
        }
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

        if i > 50 {
            return Ok(false);
        }

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
                } else if i > 10 && compared_symbols.contains(&(a.clone(), b.clone())) {
                    // If we've seen this comparison of these two symbols before, and we've done
                    // lots of symbol replacements (indicated by `i`), then we're probably trying
                    // to compare two distinct types which are asymptotically unequal.
                    false
                } else {
                    // To get as much specific information as possible about type errors,
                    // we just say the types are unequal if we cannot simplify symbols further.
                    compared_symbols.insert((a.clone(), b.clone()));
                    match (env.get_type(a), env.get_type(b)) {
                        (Some(a), Some(b)) => a.equals_checked(b, compared_symbols, env, i + 1)?,
                        _ => false,
                    }
                }
            }
            (Self::Symbol(x), y) | (y, Self::Symbol(x)) => {
                // To get as much specific information as possible about type errors,
                // we just say the types are unequal if we cannot simplify symbols further.
                match env.get_type(x) {
                    Some(t) => t.equals_checked(y, compared_symbols, env, i + 1)?,
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
                unit_name1 == unit_name2 && t1.equals_checked(t2, compared_symbols, env, i + 1)?
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

            (Self::Pointer(t1), Self::Pointer(t2)) => {
                t1.equals_checked(t2, compared_symbols, env, i)?
            }

            (Self::Poly(ty_params1, template1), Self::Poly(ty_params2, template2)) => {
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
                } else {
                    // If the two polymorphic types are not equal, then we can't just compare the two
                    // types' parameters. We need to simplify the types first.
                    self.clone().simplify_checked(env, i)?.equals_checked(
                        &other.clone().simplify_checked(env, i)?,
                        compared_symbols,
                        env,
                        i,
                    )?
                }
            }

            (Self::Apply(poly, ty_args), b) | (b, Self::Apply(poly, ty_args)) => {
                let a = Self::Apply(poly.clone(), ty_args.clone());
                // If the type is a polymorphic type, then we need to simplify it first.
                let f = poly.clone().simplify_until_matches(env, Type::Poly(vec![], Box::new(Type::Any)), |t, env| {
                    Ok(matches!(t, Type::Poly(_, _)))
                })?;
                // let a = a.simplify_until_matches(env, Type::Any, |t, env| Ok(t.is_simple()))?;
                
                match f {
                    Self::Poly(ty_params, mut template) => {
                        // let mut new_env = env.clone();
                        for (name, ty) in ty_params.iter().zip(ty_args.iter()) {
                            // new_env.define_type(name, ty.clone());
                            *template = template.substitute(name, ty);
                        }
                        template.equals_checked(b, compared_symbols, env, i)?
                    }
                //     // Self::Symbol(_) => {
                //     //     // If the type is not a polymorphic type, then we can compare it to the
                //     //     // other type.
                //     //     // return a.equals_checked(b, compared_symbols, env, i);
                //     //     false
                //     // }
                    _ => {
                        // If the type is not a polymorphic type, then we can compare it to the
                        // other type.
                        // return a.equals_checked(b, compared_symbols, env, i);
                        false
                    }
                }
            }

            _ => false,
        })
    }

    pub(super) fn get_member_offset(
        &self,
        member: &ConstExpr,
        expr: &Expr,
        env: &Env,
    ) -> Result<(Type, usize), Error> {
        match self {
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
                    if &ConstExpr::Int(i as i32) == member {
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
                // We simplfy the type before AND after getting the member offset because
                // we want to make sure that recursive types don't leave undefined symbols
                // in the in the resulting type.
                let (t, offset) = ret
                    .clone()
                    .simplify(env)?
                    .get_member_offset(member, expr, &new_env)?;
                Ok((t.simplify(&new_env)?, offset))
            }

            Type::Unit(_unit_name, t) => t.get_member_offset(member, expr, env),

            Type::Symbol(name) => {
                if let Some(t) = env.get_type(name) {
                    t.get_member_offset(member, expr, env)
                } else {
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
            Type::Struct(members) => {
                for (k, _) in members.clone() {
                    // If this element is the requested member
                    if &ConstExpr::Symbol(k) == member {
                        // Return the member's type and the offset in memory
                        // from the value's address
                        return Ok(());
                    }
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Tuple(items) => {
                for (i, _) in items.iter().enumerate() {
                    // If this element is the requested member
                    if &ConstExpr::Int(i as i32) == member {
                        // Return the member's type and its offset in memory
                        // from the value's address.
                        return Ok(());
                    }
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Union(types) => match types.get(&member.clone().as_symbol(env)?) {
                Some(_) => Ok(()),
                None => Err(Error::MemberNotFound(expr.clone(), member.clone())),
            },
            Type::Let(name, t, ret) => {
                // Create a new scope and define the new type within it
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
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
                    Err(Error::TypeNotDefined(name.clone()))
                }
            }

            Type::Any => {
                // Any type can have any member
                Ok(())
            }

            _ => Err(Error::MemberNotFound(expr.clone(), member.clone())),
        }
    }
}

impl Simplify for Type {
    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        let i = i + 1;
        // let s = self.to_string();
        let result = match self {
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
            Self::Pointer(inner) => Self::Pointer(Box::new(inner.simplify_checked(env, i)?)),

            Self::Let(name, t, ret) => {
                // Is the bound type recursive?
                if t.contains_symbol(&name) {
                    // If so, create a new environment with the new type bound.
                    let mut new_env = env.clone();
                    new_env.define_type(&name, *t.clone());
                    // Simplify the result of the let body under the new environment.
                    let result = ret.clone().simplify_checked(&new_env, i)?;
                    if *ret == Self::Symbol(name.clone()) {
                        // If the let body is the bound type, then we can subsitute the bound type
                        // for a copy of the whole let-binding.
                        result.substitute(&name, &Self::Let(name.clone(), t, ret))
                    } else {
                        // Otherwise, we can't reason much about the result, so return what
                        // we have.
                        result
                    }
                } else {
                    // If the type isn't recursive, we can just substitute the variable for the type binding!
                    ret.substitute(&name, &t).simplify_checked(env, i)?
                }
            }

            Self::Unit(unit_name, t) => {
                Self::Unit(unit_name, Box::new(t.simplify_checked(env, i)?))
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
                Self::Apply(Box::new(poly.simplify_checked(env, i)?), ty_args)
                // Simplify the type arguments supplied to the polymorphic type
                // let ty_args = ty_args
                //     .clone()
                //     .into_iter()
                //     .map(|t| t.simplify_checked(env, i))
                //     .collect::<Result<Vec<Type>, Error>>()?;

                //match *poly.clone() {
                    // Self::Symbol(name) => {
                    //     if let Some(Self::Poly(ty_params, template)) = env.get_type(&name) {
                    //         if ty_params.len() != ty_args.len() {
                    //             return Err(Error::InvalidTemplateArgs(Self::Apply(poly, ty_args)));
                    //         }
                    //         let mut result = *template.clone();
                    //         for (name, ty) in ty_params.into_iter().zip(ty_args.iter()) {
                    //             // result = Self::let_bind(&name, ty.clone(), result);
                    //             if ty != &Self::Symbol(name.clone()) {
                    //                 result = Self::let_bind(&name, ty.clone(), result);
                    //             }
                    //         }
                    //         result
                    //     } else {
                    //         Self::Apply(poly, ty_args)
                    //     }
                    // }
                    // Self::Let(name, t, ret) => {
                    //     Self::Apply(Box::new(Self::Let(name, t, ret).simplify_checked(env, i)?), ty_args)
                    // }
                // match *poly.clone() {
                //     Self::Symbol(name) => {
                //         if let Some(Self::Poly(ty_params, template)) = env.get_type(&name).map(|t| t.clone()) {
                //             // if ty_params.len() != ty_args.len() {
                //             //     return Err(Error::InvalidTemplateArgs(Self::Apply(poly, ty_args)));
                //             // }
                //             // let mut result = *template.clone();
                //             // for (name, ty) in ty_params.into_iter().zip(ty_args.iter()) {
                //             //     // result = Self::let_bind(&name, ty.clone(), result);
                //             //     result = result.substitute(&name, ty);
                //             //     // result = Self::let_bind(&name, if ty == &Self::Symbol(name.clone()) {
                //             //     //     continue;
                //             //     // } else {
                //             //     //     ty.clone()
                //             //     // }, result);
                //             // }
                //             // result//.simplify_checked(env, i)?
                //             Self::Apply(Box::new(Self::Poly(ty_params, template)), ty_args)
                //         } else {
                //             Self::Apply(poly, ty_args)
                //         }
                //     }
                //     // Self::Poly(ty_params, template) => {
                //     //     // Confirm that the number of type arguments matches the number of type parameters.
                //     //     // If not, return an error.
                //     //     if ty_params.len() != ty_args.len() {
                //     //         return Err(Error::InvalidTemplateArgs(Self::Apply(poly, ty_args)));
                //     //     }

                //     //     // Substitute the type arguments for the type parameters in the template.
                //     //     let mut result = *template.clone();
                //     //     for (name, ty) in ty_params.into_iter().zip(ty_args.iter()) {
                //     //         result = result.substitute(&name, ty);
                //     //             // result = result.substitute(&name, &ty.clone().simplify_checked(env, i)?);
                //     //         // result = Self::let_bind(&name, if ty == &Self::Symbol(name.clone()) {
                //     //         //     continue;
                //     //         // } else {
                //     //         //     ty.clone()
                //     //         // }, result);
                //     //         // // Otherwise, add the binding.
                //     //         // result = Self::let_bind(&name, ty.clone(), result);
                //     //     }

                //     //     // result.simplify_checked(env, i)?
                //     //     result
                //     // }
                //     other => Self::Apply(Box::new(other), ty_args),
                // }
            }
        };
        // eprintln!("{s} {i}");
        // eprintln!("simplified: {result}");
        Ok(result)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => write!(f, "*"),
            Self::Never => write!(f, "Never"),
            Self::Pointer(ty) => write!(f, "&{ty}"),
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
                write!(f, "({poly})[")?;
                for (i, arg) in ty_args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if i < ty_args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "]")
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
                    write!(f, "{name} = {ty}")?;
                    if i < fields.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Proc(args, ret) => {
                write!(f, "proc(")?;
                for (i, ty) in args.iter().enumerate() {
                    write!(f, "{ty}")?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ") -> {ret}")
            }

            Self::Symbol(name) => write!(f, "{name}"),
            Self::Unit(unit_name, ty) => write!(f, "unit {unit_name} = {ty}"),
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
            Self::Pointer(ty) => {
                state.write_u8(2);
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
        }
    }
}