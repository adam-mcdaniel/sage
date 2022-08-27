pub mod vm;
pub mod asm;
pub mod lir;
pub mod targets;
pub mod parse;

/// The value of the NULL pointer constant.
/// 
/// I've chosen to use the smallest value that can be expressed by an 8-bit signed integer.
/// This is because I want to make sure that this works with 8-bit machines as well.
/// The value of this constant might change in the future though.
pub const NULL: isize = i8::MIN as isize;
