use std::collections::{BTreeMap, HashSet};
use core::fmt;
use super::{ConstExpr, Env, Error, Expr, GetSize, Simplify};

/// A value that can be typechecked.
pub trait TypeCheck {
    /// Type check the expression.
    fn type_check(&self, env: &Env) -> Result<(), Error>;
}

impl TypeCheck for Type {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        // TODO: Also add checks for infinitely sized types.
        match self {
            Self::Any
            | Self::Never
            | Self::None
            | Self::Cell
            | Self::Int
            | Self::Float
            | Self::Bool
            | Self::Char
            | Self::Enum(_) => Ok(()),

            Self::Symbol(name) => {
                if env.get_type(name).is_some() {
                    Ok(())
                } else {
                    Err(Error::TypeNotDefined(name.clone()))
                }
            }
            Self::Let(name, t, ret) => {
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                t.type_check(&new_env)?;
                ret.type_check(&new_env)
            }
            Self::Array(t, e) => {
                t.type_check(env)?;
                e.type_check(env)
            }
            Self::Tuple(ts) => {
                for t in ts {
                    t.type_check(env)?;
                }
                Ok(())
            }
            Self::Struct(fields) | Self::Union(fields) => {
                for t in fields.values() {
                    t.type_check(&env)?;
                }
                Ok(())
            }

            Self::Proc(args, ret) => {
                for t in args {
                    t.type_check(env)?;
                }
                ret.type_check(env)
            }

            Self::Pointer(t) => t.type_check(env),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Type {
    /// Bind a type to a name in a temporary scope.
    Let(String, Box<Self>, Box<Self>),
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
    /// A boolean could be considered an enumeration of True and False.
    Enum(Vec<String>),
    /// A collection of types.
    Tuple(Vec<Self>),
    /// An array of a given type, with a constant size.
    Array(Box<Self>, Box<ConstExpr>),
    /// A tuple with named members.
    Struct(BTreeMap<String, Self>),
    /// A union of a list of possible types mapped to named members.
    /// A union's value is reinterpreted as a single type, depending on the member accessed.
    /// Unions' values are stored starting at the beginning of the union's address in memory,
    /// and are padded at the end with zeroes.
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
}

impl Type {
    /// Does this type contain a symbol with the given name?
    /// This will not count overshadowded versions of the symbol (overwritten by let-bindings).
    pub fn contains_symbol(&self, name: &str) -> bool {
        match self {
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
            Self::Proc(params, ret) => {
                params.iter().any(|t| t.contains_symbol(name)) || ret.contains_symbol(name)
            }
            Self::Pointer(t) => t.contains_symbol(name),
        }
    }

    /// Substitute all occurences of a symbol with another type.
    /// This will not traverse into let-bindings where the symbol is overshadowed.
    pub fn substitute(&self, name: &str, t: &Self) -> Self {
        match self {
            Self::Let(typename, binding, ret) => Self::Let(
                typename.clone(),
                Box::new(binding.substitute(name, t)),
                if typename == name {
                    // If the variable is overshadowed, then don't substitute in the body of the let.
                    ret.clone()
                } else {
                    // If the variable is not overshadowed, then we're free to substitute in the body of the let.
                    ret.substitute(name, t).into()
                },
            ),
            Self::Symbol(typename) => {
                if typename == name {
                    return t.clone();
                }
                Self::Symbol(typename.clone())
            }
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
                    .map(|field_t| field_t.substitute(name, t))
                    .collect(),
            ),
            Self::Array(item_t, size) => {
                Self::Array(Box::new(item_t.substitute(name, t)), size.clone())
            }
            Self::Struct(fields) => Self::Struct(
                fields
                    .iter()
                    .map(|(field_name, field_t)| (field_name.clone(), field_t.substitute(name, t)))
                    .collect(),
            ),
            Self::Union(fields) => Self::Union(
                fields
                    .iter()
                    .map(|(field_name, field_t)| (field_name.clone(), field_t.substitute(name, t)))
                    .collect(),
            ),
            Self::Proc(args, ret) => Self::Proc(
                args.iter().map(|arg| arg.substitute(name, t)).collect(),
                Box::new(ret.substitute(name, t)),
            ),
            Self::Pointer(ptr) => Self::Pointer(Box::new(ptr.substitute(name, t))),
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
                    if &ConstExpr::Symbol(k) == member {
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
                    if &ConstExpr::Int(i as i32) == member {
                        return Ok((t.clone().simplify(env)?, offset));
                    }

                    let size = t.get_size(env)?;
                    offset += size;
                }
                Err(Error::MemberNotFound(expr.clone(), member.clone()))
            }
            Type::Union(types) => match types.get(&member.clone().as_symbol(env)?) {
                Some(t) => Ok((t.clone().simplify(env)?, 0)),
                None => Err(Error::MemberNotFound(expr.clone(), member.clone()))
            },

            Type::Let(name, t, ret) => {
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                ret.get_member_offset(member, expr, &new_env)
            }

            Type::Symbol(name) => if let Some(t) = env.get_type(name) {
                t.get_member_offset(member, expr, env)
            } else {
                Err(Error::TypeNotDefined(name.clone()))
            },

            _ => Err(Error::MemberNotFound(expr.clone(), member.clone())),
        }
    }
}

impl GetSize for Type {
    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error> {
        let i = i + 1;
        Ok(match self {
            Self::None | Self::Never => 0,
            Self::Any => return Err(Error::UnsizedType(self.clone())),

            Self::Let(name, t, ret) => {
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                ret.get_size_checked(&new_env, i)?
            }

            Self::Symbol(name) => {
                if let Some(t) = env.get_type(name) {
                    t.get_size_checked(env, i)?
                } else {
                    return Err(Error::TypeNotDefined(name.clone()));
                }
            }

            Self::Int
            | Self::Float
            | Self::Char
            | Self::Bool
            | Self::Cell
            | Self::Enum(_)
            | Self::Pointer(_)
            | Self::Proc(_, _) => 1,

            Self::Tuple(items) => items.iter().flat_map(|t| t.get_size_checked(env, i)).sum(),
            Self::Array(elem, size) => {
                elem.get_size_checked(env, i)? * size.clone().as_int(env)? as usize
            }
            Self::Struct(fields) => fields
                .iter()
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                .sum(),
            Self::Union(types) => types
                .iter()
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                .max()
                .unwrap_or(0),
        })
    }
}

impl Simplify for Type {
    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error> {
        let i = i + 1;

        Ok(match self {
            Self::None
            | Self::Never
            | Self::Any
            | Self::Int
            | Self::Float
            | Self::Char
            | Self::Bool
            | Self::Cell
            | Self::Enum(_) => self.clone(),
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
                        result.substitute(&name, &Self::Let(name.clone(), t.clone(), ret.clone()))
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
        })
    }
}


impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => write!(f, "*"),
            Self::Never => write!(f, "Never"),
            Self::Pointer(ty) => write!(f, "&{ty:?}"),
            Self::Bool => write!(f, "Bool"),
            Self::Char => write!(f, "Char"),
            Self::Cell => write!(f, "Cell"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::None => write!(f, "None"),
            Self::Array(ty, len) => write!(f, "[{ty:?} * {len:?}]"),
            Self::Enum(variants) => {
                write!(f, "enum {{")?;
                for (i, variant) in variants.iter().enumerate() {
                    write!(f, "{variant:?}")?;
                    if i < variants.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    write!(f, "{item:?}")?;
                    if i < items.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
            Self::Struct(fields) => {
                write!(f, "struct {{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    write!(f, "{name:?}: {ty:?}")?;
                    if i < fields.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Union(fields) => {
                write!(f, "union {{")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    write!(f, "{name:?}: {ty:?}")?;
                    if i < fields.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Proc(args, ret) => {
                write!(f, "proc(")?;
                for (i, ty) in args.iter().enumerate() {
                    write!(f, "{ty:?}")?;
                    if i < args.len() - 1{
                        write!(f, ", ")?
                    }
                }
                write!(f, ") -> {ret:?}")
            },
            
            Self::Symbol(name) => write!(f, "{name}"),
            Self::Let(name, ty, ret) => write!(f, "let {name} = {ty:?} in {ret:?}"),

        }
    }
}