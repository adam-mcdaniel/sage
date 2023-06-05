//! # Procedure
//!
//! This module implements mono and poly procedures.
//! Mono procedures are procedures which are not polymorphic.
//! Poly procedures are procedures which are polymorphic.
//! Polymorphic procedures take a list of type arguments, and produce a monomorphized
//! version of the procedure.
//! 
//! ## Mono
//! 
//! A procedure of LIR code which can be applied to a list of arguments.
//! This can be compiled directly to assembly.
//!
//! ## Poly
//! 
//! Polymorphic procedures take a list of type arguments, and produce a monomorphized
//! version of the procedure. This can then be compiled directly to assembly.
mod mono;
mod poly;

pub use mono::*;
pub use poly::*;