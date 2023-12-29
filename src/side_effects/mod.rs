//! # Side Effects Module 
//! 
//! This module implements all the types related to the foreign function interface
//! and I/O operations for the Sage VM and all other stages of IR

pub mod ffi;
pub mod io;

pub use ffi::*;
pub use io::*;
