//! # Type Size
//!
//! This module contains the `GetSize` trait, which is used to calculate the size
//! of a type in units of cells.
//!
//! ## Size of a Type
//!
//! |Type|Size|
//! |---|---|
//! |`None`|0|
//! |`Never`|0|
//! |`Any`|**undefined**|
//! |`unit _ = T`|`T`'s size|
//! |`T`|`T`'s size|
//! |`Int`|1|
//! |`Bool`|1|
//! |`Char`|1|
//! |`String`|1|
//! |`[T * N]`|`T`'s size multiplied by `N`|
//! |`(A, B, ...)`|The sum of the sizes of `A`, `B`, ...|
//! |`struct {a: A, b: B, ...}`|The sum of the sizes of `A`, `B`, ...|
//! |`union {a: A, b: B, ... i: Int = 5}`|The max size of any type of `A`, `B`, ...|
//! |`proc(A, B, ...) -> T`|1|
//! |`enum {A, B, ...}`|1|
use super::*;

/// Get the size of something in memory (number of cells).
pub trait GetSize {
    /// Get the size of something in memory (number of cells).
    fn get_size(&self, env: &Env) -> Result<usize, Error> {
        self.get_size_checked(env, 0)
    }

    /// Get the size of something in memory, but limit the number of recursive
    /// calls to prevent stack overflow. `i` is a counter to prevent infinite
    /// recursion.
    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error>;
}

/// Calculate the size of a type in units of cells.
impl GetSize for Type {
    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error> {
        let i = i + 1;
        Ok(match self {
            // None or Never are not real types, so they have no size.
            // They are not represented with data. They have zero size.
            Self::None | Self::Never => 0,
            // The `Any` type is really a placeholder for a type that is not yet
            // known. It *does* have a size, unlike `None` and `Never`, but we
            // don't know what it is yet. So we return an error.
            //
            // **Its size is undefined.**
            Self::Any => return Err(Error::UnsizedType(self.clone())),

            // Get the size of an inline type.
            Self::Let(name, t, ret) => {
                // We need to create a new environment with the type defined in
                // it, so that we can get the size of the resulting type.
                let mut new_env = env.clone();
                new_env.define_type(name, *t.clone());
                // Get the size of the resulting type.
                ret.get_size_checked(&new_env, i)?
            }

            // Get the size of a named type.
            Self::Symbol(name) => {
                // Get the type from the environment.
                if let Some(t) = env.get_type(name) {
                    // Get the size of the type.
                    t.get_size_checked(env, i)?
                } else {
                    // If the type is not defined, return an error.
                    return Err(Error::TypeNotDefined(name.clone()));
                }
            }

            // Get the size of a unit type. (Its size is the size of its inner type.)
            Self::Unit(_unit_name, t) => t.get_size_checked(env, i)?,

            // These types are all one cell.
            Self::Int
            | Self::Float
            | Self::Char
            | Self::Bool
            | Self::Cell
            | Self::Enum(_)
            | Self::Pointer(_)
            | Self::Proc(_, _) => 1,

            // Tuple types are the sum of the sizes of their elements.
            Self::Tuple(items) => items.iter().flat_map(|t| t.get_size_checked(env, i)).sum(),
            // Array types are the size of their element type times the size of
            // the array.
            Self::Array(elem, size) => {
                elem.get_size_checked(env, i)? * size.clone().as_int(env)? as usize
            }
            // Struct types are the sum of the sizes of their fields.
            Self::Struct(fields) => fields
                // Make an iterator over the fields.
                .iter()
                // Get the size of each field.
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                // Sum the sizes of all the fields.
                .sum(),
            // Union types are the size of the largest field. (All other fields are padded to this size.)
            Self::Union(types) => types
                // Make an iterator over the fields.
                .iter()
                // Get the size of each field.
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                // Get the largest size.
                .max()
                // If there are no fields, just return 0.
                .unwrap_or(0),

            // EnumUnion types are the size of the largest field + 1 (for the tag). (All other fields are padded to this size.)
            Self::EnumUnion(types) => types
                // Make an iterator over the fields.
                .iter()
                // Get the size of each field.
                .flat_map(|(_, t)| t.get_size_checked(env, i))
                // Get the largest size.
                .max()
                // If there are no fields, just return 0.
                .unwrap_or(0)
                // Add 1 for the tag.
                + 1,
        })
    }
}

/// Implement `GetSize` for any type that implements `GetType`.
///
/// We can do this because the size of a variable should be exactly
/// the size of its type.
impl<T> GetSize for T
where
    T: GetType,
{
    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error> {
        self.get_type(env)?.get_size_checked(env, i)
    }
}
