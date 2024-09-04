use crate::parse::SourceCodeLocation;
use core::ops::{BitOr, BitOrAssign};
use serde_derive::{Deserialize, Serialize};
use std::collections::BTreeSet;

/// An annotation for metadata about an LIR expression.
/// This is used for error reporting, debugging, optimization,
/// and for representing the LIR in a human-readable format.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Annotation {
    /// The source code location of the expression.
    Location(SourceCodeLocation),
    /// Is this expression a constant?
    Constant(bool),
    /// Is this expression dead code?
    DeadCode(bool),
    /// Is this expression compiler-generated?
    CompilerGenerated(bool),
    /// Is this expression a temporary?
    Temporary(bool),
    /// Many annotations can be attached to an expression.
    /// This is a list of them.
    Many(BTreeSet<Annotation>),
    /// No annotation.
    None,
}

impl Annotation {
    /// An annotation for dead code.
    pub const DEAD_CODE: Annotation = Annotation::DeadCode(true);
    /// An annotation for live code.
    pub const LIVE_CODE: Annotation = Annotation::DeadCode(false);

    /// An annotation for compiler-generated code.
    pub const COMPILER_GENERATED: Annotation = Annotation::CompilerGenerated(true);
    /// An annotation for user-generated code.
    pub const USER_GENERATED: Annotation = Annotation::CompilerGenerated(false);

    /// An annotation for a temporary.
    pub const TEMPORARY: Annotation = Annotation::Temporary(true);

    /// An annotation for a constant.
    pub const CONSTANT: Annotation = Annotation::Constant(true);

    /// Does this annotation have a location?
    pub fn has_location(&self) -> bool {
        match self {
            Annotation::Location(_) => true,
            Annotation::Many(annotations) => annotations.iter().any(|a| a.has_location()),
            _ => false,
        }
    }

    /// Is this annotation none?
    pub fn is_none(&self) -> bool {
        match self {
            Annotation::None => true,
            Annotation::Many(annotations) if annotations.is_empty() => true,
            Annotation::Many(annotations) => annotations.iter().all(|a| a.is_none()),
            _ => false,
        }
    }

    /// Is this annotation a location?
    pub fn is_location(&self) -> bool {
        matches!(self, Annotation::Location(_))
    }

    /// Get the location of this annotation.
    pub fn location(&self) -> Option<&SourceCodeLocation> {
        match self {
            Annotation::Location(location) => Some(location),
            Annotation::Many(annotations) => annotations.iter().find_map(|a| a.location()),
            _ => None,
        }
    }

    /// Is this dead code?
    pub fn is_dead_code(&self) -> bool {
        match self {
            Annotation::DeadCode(dead_code) => *dead_code,
            Annotation::Many(annotations) => {
                annotations.contains(&Self::DEAD_CODE)
                    || annotations.iter().any(|a| a.is_dead_code())
            }
            _ => false,
        }
    }

    /// Is this compiler-generated?
    pub fn is_compiler_generated(&self) -> bool {
        match self {
            Annotation::CompilerGenerated(compiler_generated) => *compiler_generated,
            Annotation::Many(annotations) => {
                annotations.contains(&Self::COMPILER_GENERATED)
                    || annotations.iter().any(|a| a.is_compiler_generated())
            }
            _ => false,
        }
    }

    /// Is this a temporary?
    pub fn is_temporary(&self) -> bool {
        match self {
            Annotation::Temporary(temporary) => *temporary,
            Annotation::Many(annotations) => {
                annotations.contains(&Self::TEMPORARY)
                    || annotations.iter().any(|a| a.is_temporary())
            }
            _ => false,
        }
    }

    /// Remove any existing location from this annotation.
    fn purge_existing_location(&mut self) {
        match self {
            // If this is a location, remove it.
            Annotation::Location(_) => *self = Annotation::None,
            // If this is a list of annotations, remove any existing locations.
            Annotation::Many(annotations) => {
                // The new list of annotations.
                let mut result = BTreeSet::new();
                // Iterate over the annotations.
                for mut annotation in annotations.clone() {
                    // If this is a location, or none, skip it.
                    if annotation.is_location() || annotation.is_none() {
                        continue;
                    }
                    // Otherwise, remove any sub-locations it might have.
                    annotation.purge_existing_location();
                    // Check if it's none again.
                    if annotation.is_none() || annotation.has_location() {
                        continue;
                    }
                    // Otherwise, insert it into the result.
                    result.insert(annotation);
                }
                // Set the result.
                *self = if result.is_empty() {
                    // If the result is empty, set it to none.
                    Annotation::None
                } else if result.len() == 1 {
                    // If the result has one element, set it to that element.
                    result.into_iter().next().unwrap()
                } else {
                    // Otherwise, return the list of annotations.
                    Annotation::Many(result)
                };
            }
            _ => {}
        }
    }
}

impl BitOr for Annotation {
    type Output = Annotation;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut result = self.clone();
        result |= rhs;
        result
    }
}

impl BitOrAssign for Annotation {
    fn bitor_assign(&mut self, rhs: Self) {
        // If both have locations, remove the existing location in favor of the incoming one.
        if self.has_location() && rhs.has_location() {
            self.purge_existing_location();
        }

        // If the incoming annotation is none, skip it.
        if rhs.is_none() {
            return;
        }

        // If this annotation is none, set it to the incoming one.
        if self.is_none() {
            *self = rhs;
            return;
        }

        // Otherwise, merge the annotations.
        match (self, rhs) {
            (Annotation::Many(a), Annotation::Many(b)) => {
                // If both are many, concatenate them.
                a.append(&mut b.clone());
            }
            (Annotation::Many(a), b) => {
                // If this is many, insert the incoming annotation.
                a.insert(b);
            }
            (a, Annotation::Many(mut b)) => {
                // If the incoming annotation is many, insert this one into it.
                b.insert(a.clone());
                *a = Annotation::Many(b);
            }
            (a, b) => {
                // Otherwise, create a new set and insert both.
                let mut set = BTreeSet::new();
                // Insert both.
                set.insert(a.clone());
                set.insert(b);
                // Set the result.
                *a = Annotation::Many(set);
            }
        }
    }
}

impl From<SourceCodeLocation> for Annotation {
    fn from(value: SourceCodeLocation) -> Self {
        // Create a location annotation.
        Annotation::Location(value)
    }
}
