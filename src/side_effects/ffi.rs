use std::fmt::{self, Display, Formatter, Result as FmtResult};

/// A type that can be used in the FFI.
/// 
/// Whenever a call is made to a foreign function, the arguments and return value
/// must be of a type that can be represented in the FFI. This is a subset of the
/// types that can be represented in the language. The target handles taking the
/// data out of the tape and putting it into the FFI, and vice versa.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FFIType {
    Char,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Pointer(Box<Self>),
    Array(Box<Self>, usize),
    Struct(Vec<(String, Self)>),
    Union(Vec<(String, Self)>),
    Function(Vec<Self>, Box<Self>),
    Void,
}

impl FFIType {
    /// Get the size of the FFI type's representation as cells on the turing tape.
    /// Each of the signed/unsigned integers, floats, and pointers are 1 cell.
    /// Arrays are the size of the element type times the number of elements.
    /// Structs and unions are the sum of the sizes of their fields.
    /// Functions are 1 cell.
    /// Void is 0 cells.
    pub fn size_as_cells(&self) -> usize {
        match self {
            Self::Void => 0,

            Self::Char
            | Self::Int8
            | Self::Int16
            | Self::Int32
            | Self::Int64
            | Self::UInt8
            | Self::UInt16
            | Self::UInt32
            | Self::UInt64
            | Self::Float32
            | Self::Float64
            | Self::Pointer(_)
            | Self::Function(_, _) => 1,
            
            Self::Array(ty, len) => ty.size_as_cells() * len,
            Self::Struct(fields) | Self::Union(fields) => {
                fields.iter().map(|(_, ty)| ty.size_as_cells()).sum()
            }
        }
    }
}


/// This is an FFI binding, which is used to call a foreign function in the virtual machine code.
/// 
/// The name is the symbol for the foreign function, and the type is the type of the foreign
/// function. The type lets the target know how to call the foreign function, and the name
/// lets the target know what symbol to use to call the foreign function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FFIBinding {
    pub name: String,
    pub ty: FFIType,
}

impl Display for FFIBinding {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.name)
    }
}