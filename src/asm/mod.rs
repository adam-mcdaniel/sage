//! # Assembly Module
//! 
//! This module contains a small assembly language implemented
//! over the virtual machine.

pub mod core;
pub use self::core::*;

pub mod std;
pub use self::std::*;


#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Error {
    UndefinedLabel(String),
    UnmatchedEnd,
    UnexpectedElse,
}