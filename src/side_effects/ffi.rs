use std::fmt::{Display, Formatter, Result as FmtResult};

/// This is an FFI binding, which is used to call a foreign function in the virtual machine code.
///
/// The name is the symbol for the foreign function. The input cells is the number of cells that
/// the foreign function will read from the FFI channel. The output cells is the number of cells
/// that the foreign function will write to the FFI channel.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FFIBinding {
    pub name: String,
    pub input_cells: usize,
    pub output_cells: usize,
}

impl FFIBinding {
    /// Create a new FFI binding.
    pub fn new(name: String, input_cells: usize, output_cells: usize) -> Self {
        Self {
            name,
            input_cells,
            output_cells,
        }
    }
}

impl Display for FFIBinding {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.name)
    }
}
