//! The standard instructions of the virtual machine are defined here.
//!
//! ## Purpose of the Standard Instructions
//!
//! Standard instructions are instructions that *should* be implemented for
//! every target possible. Standard instructions should only not be implemented
//! for targets like physical hardware, where the program is executed on the
//! bare metal (a custom CPU or FPGA).
//!
//! Additionally, an implementation may implement *some* of the standard
//! instructions, but not all of them. This is to allow for the implementation
//! of a target to be as flexible as possible. A target may, for example,
//! not support a `GetFloat` instruction, but it may support other floating
//! point operations.
//!
//! ## Additional I/O Instructions
//!
//! The standard instructions also introduce new I/O instructions:
//! 1. `GetChar` and `PutChar`. These get a character of input
//!    from the *user*.
//! 2. `GetInt` and `PutInt`. These get an integer of input from
//!    the *user*.
//! 3. `GetFloat` and `PutFloat`. These get a floating point value
//!    from the *user*.
//!
//! I emphasize the *user* here, because I want to clarify that these instructions
//! are intended to talk directly with the user. `Get` and `Put`, in the core
//! instructions, function as a universal bus of communication.
//!
//! If you `Get`, you're getting data from the world encoded in an ambiguous manner.
//! If you `GetChar`, though, you're *guaranteed* to get a character of input from
//! the user directly. These instructions guarantee standard programs the ability
//! to request data from the user.
//!
//! This way, a developer can write a program in such a manner that user input
//! cannot be confused with custom encoded instructions sent to and from the I/O device
//! using `Put` and `Get`.
use super::{CoreOp, CoreProgram, Error, VirtualMachineProgram};
use crate::side_effects::*;
use core::fmt;
use std::collections::HashMap;

impl VirtualMachineProgram for StandardProgram {
    fn op(&mut self, op: CoreOp) {
        if let Some(StandardOp::CoreOp(last_core_op)) = self.0.last().cloned() {
            match (last_core_op, op) {
                (CoreOp::Move(n), CoreOp::Move(m)) => {
                    self.0.pop();
                    if m + n != 0 {
                        self.0.push(StandardOp::CoreOp(CoreOp::Move(n + m)))
                    }
                }
                (CoreOp::Set(_), CoreOp::Set(m)) => {
                    self.0.pop();
                    self.op(CoreOp::Set(m))
                }
                (CoreOp::Deref, CoreOp::Refer) => {
                    self.0.pop();
                }
                (CoreOp::Else, CoreOp::End) => {
                    self.0.pop();
                    self.op(CoreOp::End);
                }
                (CoreOp::Move(_), CoreOp::Refer) => {
                    self.0.pop();
                    self.op(CoreOp::Refer)
                }
                (CoreOp::Move(n), CoreOp::Set(m)) => {
                    self.0.pop();
                    self.op(CoreOp::Set(m));
                    self.op(CoreOp::Move(n));
                }
                (CoreOp::Move(n), CoreOp::Offset(off, m)) => {
                    self.0.pop();
                    self.op(CoreOp::Offset(off, m));
                    self.op(CoreOp::Move(n));
                }
                (CoreOp::Store(m), CoreOp::Store(n)) if n == m => {}
                (CoreOp::Load(m), CoreOp::Load(n)) if n == m => {}
                (CoreOp::Store(m), CoreOp::Load(n)) if n == m => {}
                (CoreOp::Load(m), CoreOp::Store(n)) if n == m => {}
                (CoreOp::Set(n), CoreOp::IsNonNegative(1)) if n.len() == 1 => {
                    self.0.pop();
                    self.op(CoreOp::Set(vec![(n[0] >= 0) as i64]))
                }

                (_, op) => {
                    self.0.push(StandardOp::CoreOp(op));
                }
            }
        } else {
            self.0.push(StandardOp::CoreOp(op));
        }
    }

    fn std_op(&mut self, op: StandardOp) -> Result<(), Error> {
        self.0.push(op);
        Ok(())
    }

    fn code(&self) -> Result<CoreProgram, StandardProgram> {
        let mut result = vec![];
        for op in self.0.clone() {
            if let StandardOp::CoreOp(core_op) = op {
                result.push(core_op)
            } else {
                return Err(self.clone());
            }
        }
        Ok(CoreProgram(result))
    }
}

/// A program of core and standard virtual machine instructions.
#[derive(Default, Clone, PartialEq, PartialOrd)]
pub struct StandardProgram(pub Vec<StandardOp>);

impl StandardProgram {
    /// Flatten a core program so that all of its functions
    /// are defined sequentially at the beginning.
    pub fn flatten(self) -> Self {
        Self(flatten(self.0).0)
    }

    /// Get the code outside of any functions.
    pub fn get_main(&self) -> Vec<StandardOp> {
        flatten(self.0.clone()).2
    }

    /// Get the code for each function.
    pub fn get_functions(&self) -> HashMap<i32, Vec<StandardOp>> {
        flatten(self.0.clone()).1
    }

    /// Get the code outside of any functions, and the code for each function.
    pub fn get_main_and_functions(self) -> (Vec<StandardOp>, HashMap<i32, Vec<StandardOp>>) {
        let (_, functions, main) = flatten(self.0);
        (main, functions)
    }
}

/// Take all of the functions defined in a list of StandardOps,
/// and flatten their definitions. This will take nested functions
/// and un-nest them while preserving the order in which functions are defined.
///
/// All the function definitions will be placed at the top of the returned list.
fn flatten(
    code: Vec<StandardOp>,
) -> (
    Vec<StandardOp>,
    HashMap<i32, Vec<StandardOp>>,
    Vec<StandardOp>,
) {
    let mut functions: HashMap<i32, Vec<StandardOp>> = HashMap::new();

    // The current function body we are in.
    let mut fun = -1;
    // Keep track of when we end the current function,
    // instead of just an if-else-conditional or a while loop.
    // This is essentially the number of end statements remaining before
    // we can end the scope.
    let mut matching_end = 0;
    // Keep track of each `matching_end`, and the scope we were previously in, for each nested scope.
    let mut scope_stack = vec![];
    // The main instructions of the program. (Not in a function.)
    let mut main_instructions = vec![];
    for std_op in code {
        match &std_op {
            StandardOp::CoreOp(CoreOp::Function) => {}
            _ => {
                if scope_stack.is_empty() {
                    // If we are not defining a function,
                    // push the instruction to the main instructions.
                    main_instructions.push(std_op.clone());
                }
            }
        }

        if let StandardOp::CoreOp(op) = &std_op {
            match op {
                CoreOp::Function => {
                    // If we are declaring a new function,
                    // push the info about the current scope onto the scope
                    // stack to resume later.
                    scope_stack.push((fun, matching_end));
                    // Reset the matching-end counter for the new scope.
                    matching_end = 0;
                    // Start defining the next function.
                    fun += 1;
                    // If that function is already defined,
                    // just go past the last function defined.
                    if functions.contains_key(&fun) {
                        fun = functions.len() as i32
                    }
                }
                CoreOp::If | CoreOp::While => {
                    // Increment the number of matching `End`
                    // instructions to end the scope.
                    matching_end += 1
                }
                CoreOp::End => {
                    // If the scope has ended
                    if matching_end == 0 {
                        // Get the function body we're defining.
                        functions.entry(fun).or_default().push(std_op);
                        // Resume flattening the previous scope.
                        (fun, matching_end) = scope_stack.pop().unwrap();
                        continue;
                    } else {
                        // Otherwise, the scope is still going.
                        // Decrement the counter and continue.
                        matching_end -= 1;
                    }
                }
                _ => {}
            }
        }

        // Insert the current instruction to the right function's definition.
        functions.entry(fun).or_default().push(std_op);
    }

    // Clone the functions so that we can remove them from the map.
    let mut result_functions = functions.clone();
    result_functions.remove(&-1);

    // The final output code.
    let mut result = vec![];

    // For every function, insert its body into the resulting output code.
    for i in 0..=functions.len() as i32 {
        if let Some(body) = functions.remove(&i) {
            result.extend(body);
        }
    }

    // Insert the remaining code into the output code.
    if let Some(body) = functions.remove(&-1) {
        result.extend(body);
    }

    // Return the output code
    (result, result_functions, main_instructions)
}

impl fmt::Display for StandardProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut comment_count = 0;
        let mut indent = 0;
        for (i, op) in self.0.iter().enumerate() {
            if f.alternate() {
                if let StandardOp::CoreOp(CoreOp::Comment(comment)) = op {
                    if f.alternate() {
                        write!(f, "{:8}  ", "")?;
                    }
                    comment_count += 1;
                    writeln!(f, "{}// {}", "   ".repeat(indent), comment,)?;
                    continue;
                }

                write!(f, "{:08x?}: ", i - comment_count)?;
            } else if let StandardOp::CoreOp(CoreOp::Comment(_)) = op {
                continue;
            }

            writeln!(
                f,
                "{}{}",
                match op {
                    StandardOp::CoreOp(CoreOp::Function | CoreOp::If | CoreOp::While) => {
                        indent += 1;
                        "   ".repeat(indent - 1)
                    }
                    StandardOp::CoreOp(CoreOp::Else) => {
                        "   ".repeat(indent - 1)
                    }
                    StandardOp::CoreOp(CoreOp::End) => {
                        indent -= 1;
                        "   ".repeat(indent)
                    }
                    _ => "   ".repeat(indent),
                },
                op
            )?
        }
        Ok(())
    }
}

/// An individual standard virtual machine instruction.
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum StandardOp {
    /// Execute a core instruction.
    CoreOp(CoreOp),

    /// Set the register equal to a constant floating point value.
    Set(Vec<f64>),

    /// Take the value of the register, and allocate that number of cells in memory.
    /// Set the register to the lowest address of the allocated memory.
    Alloc,

    /// Free the memory pointed to by the register.
    Free,

    /// Convert the register from a float to an integer.
    ToInt(usize),
    /// Convert the register from an integer to a float.
    ToFloat(usize),

    /// Add the value pointed to on the tape to the register (as floats).
    Add(usize),
    /// Subtract the value pointed to on the tape from the register (as floats).
    Sub(usize),
    /// Multiply the register by the value pointed to on the tape (as floats).
    Mul(usize),
    /// Divide the register by the value pointed to on the tape (as floats).
    Div(usize),
    /// Store the remainder of the register and the value pointed to in the
    /// tape (as floats) into the register.
    Rem(usize),
    /// Negate the value of the register (as a float).
    Neg(usize),

    /// Make the register equal to the integer 1 if the register (as a float)
    /// is not negative, otherwise make it equal to 0.
    IsNonNegative(usize),

    /// Store the sine of the register (as a float) into the register.
    /// The argument is the size of the register vector.
    Sin(usize),
    /// Store the cosine of the register (as a float) into the register.
    Cos(usize),
    /// Store the tangent of the register (as a float) into the register.
    Tan(usize),
    /// Store the inverse-sine of the register (as a float) into the register.
    ASin(usize),
    /// Store the inverse-cosine of the register (as a float) into the register.
    ACos(usize),
    /// Store the inverse-tangent of the register (as a float) into the register.
    ATan(usize),
    /// Store the value of the register (as a float) to the power of the value pointed to on the tape (as a float) into the register.
    Pow(usize),

    /// Get a value from the input interface / device and store it in the register.
    /// This is intended to function something like system calls for using any external
    /// functionality that can't be implemented in the virtual machine, such as I/O or OS operations.
    ///
    /// Whenever a value is returned from the foreign function interface, it is stored in the
    /// FFI buffer of cells. Whenever an FFI function is called, it will receive its arguments
    /// from this buffer.
    Peek,
    /// Write the value of the register to the output interface / device.
    /// This is intended to function something like system calls for using any external
    /// functionality that can't be implemented in the virtual machine, such as I/O or OS operations.
    ///
    /// Whenever a value is returned from the foreign function interface, it is stored in the
    /// FFI buffer of cells. Whenever an FFI function is called, it will receive its arguments
    /// from this buffer.
    Poke,

    /// Call a foreign function interface function.
    Call(FFIBinding),
}

impl fmt::Display for StandardOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StandardOp::CoreOp(op) => write!(f, "{}", op),
            StandardOp::Set(val) => write!(f, "set-f {:?}", val),
            StandardOp::Alloc => write!(f, "alloc"),
            StandardOp::Free => write!(f, "free"),
            StandardOp::ToInt(n) => write!(f, "to-int {n}"),
            StandardOp::ToFloat(n) => write!(f, "to-float {n}"),
            StandardOp::Add(n) => write!(f, "add-f {n}"),
            StandardOp::Sub(n) => write!(f, "sub-f {n}"),
            StandardOp::Mul(n) => write!(f, "mul-f {n}"),
            StandardOp::Div(n) => write!(f, "div-f {n}"),
            StandardOp::Rem(n) => write!(f, "rem-f {n}"),
            StandardOp::Neg(n) => write!(f, "neg-f {n}"),
            StandardOp::IsNonNegative(n) => write!(f, "gez-f {n}"),
            StandardOp::Sin(n) => write!(f, "sin {n}"),
            StandardOp::Cos(n) => write!(f, "cos {n}"),
            StandardOp::Tan(n) => write!(f, "tan {n}"),
            StandardOp::ASin(n) => write!(f, "asin {n}"),
            StandardOp::ACos(n) => write!(f, "acos {n}"),
            StandardOp::ATan(n) => write!(f, "atan {n}"),
            StandardOp::Pow(n) => write!(f, "pow {n}"),
            StandardOp::Peek => write!(f, "peek"),
            StandardOp::Poke => write!(f, "poke"),
            StandardOp::Call(binding) => write!(f, "call {}", binding),
        }
    }
}

impl From<CoreProgram> for StandardProgram {
    fn from(core: CoreProgram) -> Self {
        Self(core.0.into_iter().map(StandardOp::CoreOp).collect())
    }
}
