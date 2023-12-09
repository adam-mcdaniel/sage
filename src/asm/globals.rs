use crate::asm::{CoreOp, Error, Location, GP};

use core::fmt;
use std::collections::HashMap;

use log::*;

/// A lookup for all the global variables in an assembly program.
#[derive(Clone, Debug)]
pub struct Globals {
    /// The locations, offsets, and sizes of global variables.
    globals: HashMap<String, (Location, usize, usize)>,
    /// The next available GP offset.
    next_gp_offset: usize,
    /// A cache of resolved locations.
    memoized_resolutions: HashMap<Location, Location>,
}

impl Default for Globals {
    /// Create a new empty `Globals` lookup.
    fn default() -> Self {
        Self {
            globals: HashMap::new(),
            next_gp_offset: 0,
            memoized_resolutions: HashMap::new(),
        }
    }
}

impl Globals {
    /// Create a new empty `Globals` lookup.
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolve the global variables in a location to produce an addressable location.
    pub fn resolve(&mut self, location: &Location) -> Result<Location, Error> {
        // Check if the location has already been resolved
        if let Some(loc) = self.memoized_resolutions.get(location) {
            return Ok(loc.clone());
        }
        // Resolve the location
        let result = match location {
            // If the location is already an address, return it
            Location::Address(n) => Location::Address(*n),
            // If the location is indirect, resolve the location it points to
            Location::Indirect(loc) => self.resolve(loc)?.deref(),
            // If the location is an offset, resolve the location it's offset from
            Location::Offset(loc, offset) => self.resolve(loc)?.offset(*offset),
            // If the location is a global variable, resolve the location of the global variable
            // with its offset from GP
            Location::Global(name) => {
                if let Some((loc, _, _)) = self.globals.get(name) {
                    // Resolve the location of the global variable
                    loc.clone()
                } else {
                    // If the global variable is not found, return an error
                    error!("Global variable {name} not found in environment {self}");
                    Err(Error::UndefinedGlobal(name.clone()))?
                }
            }
        };
        // If resolved it to a different location, we must've found a global variable!
        // Log the resolution
        if location != &result {
            trace!("Resolved {} to {}", location, result);
        }

        // Insert the resolved location into the cache for quick lookup later
        self.memoized_resolutions
            .insert(location.clone(), result.clone());

        Ok(result)
    }

    /// Add a global variable to the list of globals.
    pub fn add_global(&mut self, name: String, size: usize) -> Location {
        if let Some((loc, _, _)) = self.globals.get(&name) {
            return loc.clone();
        }

        trace!("Current GP offset: {}", self.next_gp_offset);
        let offset = self.next_gp_offset;
        self.next_gp_offset += size;
        let loc = GP.deref().offset(offset as isize);

        trace!("Adding global variable {name} with size {size} at {loc}");
        self.globals.insert(name, (loc.clone(), offset, size));
        loc
    }

    /// Get the size of the global variables.
    /// This is the number of cells that the global variables occupy.
    pub fn get_size(&self) -> usize {
        self.next_gp_offset
    }

    /// Get the location, and size of a global variable.
    pub fn get_global(&self, name: &str) -> Option<&(Location, usize, usize)> {
        self.globals.get(name)
    }

    /// Get the location of a global variable.
    pub fn get_global_location(&mut self, name: &str) -> Option<Location> {
        self.globals
            .get(name)
            .cloned() // Get the global variable
            // Resolve the location of the global variable
            .and_then(|(loc, _, _)| self.resolve(&loc).ok())
    }

    /// Get the size of a global variable.
    /// This is the number of cells that the global variable occupies.
    pub fn get_global_size(&self, name: &str) -> Option<usize> {
        self.globals.get(name).map(|(_, _, size)| *size)
    }
}

impl fmt::Display for Globals {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (name, (loc, offset, size)) in &self.globals {
            writeln!(
                f,
                "{} // Location: {}, Offset: {}, Size: {}",
                CoreOp::Global {
                    name: name.clone(),
                    size: *size
                },
                loc,
                offset,
                size
            )?;
        }
        Ok(())
    }
}
