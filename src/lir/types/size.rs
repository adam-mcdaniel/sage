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
use log::{debug, error, trace, warn};

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
        trace!("Getting the size of type {self} in environment {env} with depth {i}");
        // eprintln!("GetSize::get_size_checked: i = {i} {self}");
        // eprintln!("Env: {env:?}");
        if i > Type::SIMPLIFY_RECURSION_LIMIT {
            error!("Recursion limit reached while calculating size of type {self}");
            return Err(Error::UnsizedType(self.clone()));
        }
        // eprintln!("GetSize::get_size_checked: i = {i} {self}");

        if env.has_precalculated_size(self) {
            return env.get_precalculated_size(self).ok_or_else(|| {
                error!("Type {self} has no size in environment {env}");
                Error::UnsizedType(self.clone())
            });
        }

        Ok(match self {
            // None or Never are not real types, so they have no size.
            // They are not represented with data. They have zero size.
            Self::None | Self::Never => 0,
            // The `Any` type is really a placeholder for a type that is not yet
            // known. It *does* have a size, unlike `None` and `Never`, but we
            // don't know what it is yet. So we return an error.
            //
            // **Its size is undefined.**
            Self::Any => {
                return Err(Error::UnsizedType(self.clone()))
            },

            // Get the size of an inline type.
            Self::Let(name, t, ret) => {
                // We need to create a new environment with the type defined in
                // it, so that we can get the size of the resulting type.
                let mut new_env = env.clone();
                // new_env.define_type_size(*t.clone(), t.get_size_checked(env, i)?);
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
                    error!("Type {name} not defined in environment {env}");
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
                .map(|(_, t)| t.get_size_checked(env, i))
                // Catch any errors.
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                // Sum the sizes of all the fields.
                .sum(),
            // Union types are the size of the largest field. (All other fields are padded to this size.)
            Self::Union(types) => types
                // Make an iterator over the fields.
                .iter()
                // Get the size of each field.
                .map(|(_, t)| t.get_size_checked(env, i))
                // Catch any errors.
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                // Get the largest size.
                .max()
                // If there are no fields, just return 0.
                .unwrap_or(0),

            // EnumUnion types are the size of the largest field + 1 (for the tag). (All other fields are padded to this size.)
            Self::EnumUnion(types) => {
                types
                // Make an iterator over the fields.
                .iter()
                // Get the size of each field.
                .map(|(_, t)| t.get_size_checked(env, i))
                // Catch any errors.
                .collect::<Result<Vec<_>, _>>()?.into_iter()
                // Get the largest size.
                .max()
                // If there are no fields, just return 0.
                .unwrap_or(0)
                // Add 1 for the tag.
                + 1
            }

            // Get the size of an `Apply` type.
            Self::Apply(poly, _ty_args) => {
                if let Ok(size) = poly.get_size_checked(env, i) {
                    return Ok(size);
                }

                let result = self.clone().simplify_until_matches(
                    env,
                    Type::Poly(vec![], Box::new(Type::Any)),
                    |t, _env| Ok(t.is_simple()),
                )?;

                result.get_size_checked(env, i)?

                // match *poly.clone() {
                //     Self::Poly(ty_params, template) => {
                //         // If all the arguments are atomic, we can substitute them into the template
                //         // and get the size of the result.
                //         if ty_args.iter().all(|t| t.is_atomic()) {
                //             // Create a new environment with the type parameters defined.
                //             let mut result = *poly.clone();
                //             for (name, ty) in ty_params.iter().zip(ty_args.iter()) {
                //                 result = result.substitute(name, ty);
                //             }
                //             // Get the size of the resulting type.
                //             result.get_size_checked(env, i)?
                //         } else {
                //             // If any of the arguments are not atomic, we can't substitute them into the template.
                //             // So we return an error.
                //             return Err(Error::UnsizedType(self.clone()));
                //         }
                //     }
                //     _ => {
                //         return Err(Error::ApplyNonTemplate(self.clone()))
                //     }
                // }
                // return Err(Error::UnsizedType(self.clone()))

                // self
                //     .clone()
                //     .simplify(env)?
                //     .perform_template_applications(env, &mut HashMap::new(), i)?
                //     .get_size_checked(env, i)?

                // match poly.clone().simplify(env)? {
                //     Self::Poly(ty_params, template) => {
                //         // Make sure the number of type arguments matches the number of type parameters.
                //         if ty_args.len() != ty_params.len() {
                //             return Err(Error::InvalidTemplateArgs(self.clone()));
                //         }
                //         // Create a new environment with the type parameters defined.
                //         let mut result = *template.clone();
                //         for (name, ty) in ty_params.iter().zip(ty_args.iter()) {
                //             // result = result.substitute(name, ty);
                //             result = Type::let_bind(name, if Type::Symbol(name.clone()) == *ty {
                //                 Type::Unit(name.clone(), Box::new(Type::Any))
                //             } else {
                //                 ty.clone()
                //             }, result);
                //         }
                //         // Get the size of the resulting type.
                //         result.get_size_checked(&env, i)?
                //     }
                //     Self::Let(name, t, ret) => {
                //         // We need to create a new environment with the type defined in
                //         // it, so that we can get the size of the resulting type.
                //         let mut new_env = env.clone();
                //         // new_env.define_type_size(*t.clone(), t.get_size_checked(env, i)?);
                //         // new_env.define_type(name.clone(), *t.clone());
                //         new_env.define_type(name, t.simplify_checked(env, i)?);
                //         // Get the size of the resulting type.
                //         ret.get_size_checked(&new_env, i)?
                //     }
                //     _ => {
                //         return Err(Error::ApplyNonTemplate(self.clone()));
                //     }
                // }
            }

            Self::Poly(_ty_params, _template) => {
                // let mut template = *template.clone();
                // for ty_param in ty_params {
                //     // template = template.substitute(ty_param, &Type::Unit(ty_param.clone(), Box::new(Type::Any)));
                //     template = Type::let_bind(ty_param, Type::Unit(ty_param.clone(), Box::new(Type::Any)), template);
                // }
                // let mut new_env = env.clone();
                // for ty_param in ty_params {
                //     new_env.define_type(ty_param, Type::Unit(ty_param.clone(), Box::new(Type::Any)));
                // }
                // if let Ok(size) = template.get_size_checked(&new_env, i) {
                //     return Ok(size);
                // }

                // let result = Self::Apply(
                //     Box::new(self.clone()),
                //     ty_params
                //         .iter()
                //         .map(|ty_param| Type::Unit(ty_param.clone(), Box::new(Type::Any)))
                //         .collect(),
                // ).get_size_checked(env, i);
                return Err(Error::SizeOfTemplate(self.clone()));
            }
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
