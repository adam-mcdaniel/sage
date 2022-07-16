use crate::vm::{CoreProgram, CoreOp};
use std::io::{stdin, stdout, Read, Write};


/// Create an input / output device for the virtual machine interpreter
/// to operate on. The method `get` retrieves the device's input, and the
/// function `put` writes to the devices output.
pub trait Device {
    fn get(&mut self) -> Result<isize, String>;
    fn put(&mut self, val: isize) -> Result<(), String>;
}

/// A device used for testing the compiler. This simply keeps a buffer
/// of sample input to supply to the virtual machine, and keeps an output
/// buffer to keep track of the output of the virtual machine.
/// 
/// The tests interpret the program and populate the device with output.
/// Then, we check the devices output against the correct output.
pub struct TestingDevice {
    pub input: Vec<isize>,
    pub output: Vec<isize>,
}

impl TestingDevice {
    /// Create a new testing device with some given sample input.
    pub fn new(sample_input: impl ToString) -> Self {
        Self { input: sample_input
            .to_string()
            .chars()
            .map(|ch| ch as isize)
            .collect(), output: vec![] }
    }

    /// Get the output of the testing device as a string (ascii).
    pub fn output_str(&self) -> String {
        let mut result = String::new();
        for ch in &self.output {
            result.push(*ch as u8 as char)
        }
        result
    }
}

/// Make the testing device work with the interpreter.
impl Device for TestingDevice {
    fn get(&mut self) -> Result<isize, String> {
        if !self.input.is_empty() {
            Ok(self.input.remove(0))
        } else {
            Err(String::from("ran out of input"))
        }
    }

    fn put(&mut self, val: isize) -> Result<(), String> {
        self.output.push(val);
        Ok(())
    }
}

impl Default for TestingDevice {
    fn default() -> Self {
        Self { input: vec![], output: vec![] }
    }
}

/// A device used for standard input and output.
/// This simply retrieves a character from standard-in with `get`,
/// and writes a character to standard-out with `put`.
pub struct StandardDevice;

impl Device for StandardDevice {
    fn get(&mut self) -> Result<isize, String> {
        // Buffer with exactly 1 character of space
        let mut ch = [0];
        // Flush stdout to write any prompts for the user
        if stdout().flush().is_err() {
            Err(String::from("could not flush output"))
        } else if stdin().read(&mut ch).is_ok() {
            // If the buffer was successfully read into, return the result.
            Ok(ch[0] as isize)
        } else {
            // Otherwise, the buffer was not read into, and the input failed.
            Err(String::from("could not read input"))
        }
    }

    fn put(&mut self, val: isize) -> Result<(), String> {
        // Print the character without a newline
        print!("{}", val as u8 as char);
        Ok(())
    }
}

impl Default for Interpreter<StandardDevice> {
    fn default() -> Self {
        Self {
            device: StandardDevice,
            pointer: 0,
            register: 0,
            cells: vec![],
            functions: vec![],
            calls: vec![],
            refs: vec![],
            i: 0,
            done: false,
        }
    }
}

/// The interpreter which runs the virtual machine program.
pub struct Interpreter<T> where T: Device {
    /// The interpreter's I/O device.
    device: T,
    /// The current pointer on the turing tape.
    pointer: usize,
    /// The register (which contains a single cell of data).
    register: isize,
    /// The turing tape (composed of integer cells)
    cells: Vec<isize>,
    /// The addresses of defined functions. `functions[N]` is the
    /// instruction pointer for the Nth function's code.
    functions: Vec<usize>,
    /// The call stack of instruction pointers. Whenever a function
    /// is called, the instruction pointer is pushed here. Whenever
    /// a function returns, its instruction pointer is popped from here.
    calls: Vec<usize>,
    /// The stack of dereferences made by the program (to be undone
    /// by a reference instruction).
    refs: Vec<usize>,
    /// The instruction pointer.
    i: usize,
    /// Is the interpreter finished interpreting?s
    done: bool
}

impl<T> Interpreter<T> where T: Device {
    pub fn new(device: T) -> Self {
        Self {
            device,
            pointer: 0,
            register: 0,
            cells: vec![],
            functions: vec![],
            calls: vec![],
            refs: vec![],
            i: 0,
            done: false,
        }
    }

    /// Fetch the current instruction pointed to in the program
    fn fetch<'a>(&self, code: &'a CoreProgram) -> Option<&'a CoreOp> {
        if self.i < code.0.len() {
            Some(&code.0[self.i])
        } else {
            None
        }
    }
    
    /// Dereference the current pointer on the tape.
    fn deref(&mut self) {
        // Add the old pointer to the dereference stack.
        self.refs.push(self.pointer);
        // Set the pointer to the address on the tape.
        self.pointer = *self.get_cell() as usize;
    }

    /// Undo a dereference.
    fn refer(&mut self) -> Result<(), String> {
        // Get the previous value of the pointer before
        // the last dereference instruction.
        if let Some(old) = self.refs.pop() {
            self.pointer = old;
            Ok(())
        } else {
            // There was no previous dereference, throw an error
            Err(String::from("cannot Refer due to empty Deref stack"))
        }
    }

    /// Call the Nth function defined in the program, where N is the value of the register.
    fn call(&mut self) -> Result<(), String> {
        // If the function has been defined
        if self.functions.len() >= self.register as usize {
            // Push the current instruction pointer to the call stack
            self.calls.push(self.i);
            self.i = self.functions[self.register as usize];
            Ok(())
        } else {
            // Throw an error if not defined
            Err(format!("function {} not defined", self.register))
        }
    }

    /// Return from the current function.
    fn ret(&mut self) {
        // If we're returning from a function, jump to the old instruction pointer.
        if let Some(old) = self.calls.pop() {
            self.i = old
        } else {
            // Otherwise, we're finished with the program.
            self.done = true
        }
    }

    /// Jump to the matching "Else" for this "If" instruction.
    fn jmp_to_else(&mut self, code: &CoreProgram) {
        let mut matching = 1;
        while matching > 0 {
            self.i += 1;
            match self.fetch(code) {
                Some(CoreOp::If) | Some(CoreOp::While) | Some(CoreOp::Function) => matching += 1,
                Some(CoreOp::End) => matching -= 1,
                Some(CoreOp::Else) if matching == 1 => {
                    return
                }
                _ => {}
            }
        }
    }

    /// Jump to the matching "End" for this "Else", "While",
    /// or "Function" instruction.
    fn jmp_to_end(&mut self, code: &CoreProgram) {
        let mut matching = 1;
        while matching > 0 {
            self.i += 1;
            match self.fetch(code) {
                Some(CoreOp::If) | Some(CoreOp::While) | Some(CoreOp::Function) => matching += 1,
                Some(CoreOp::End) => matching -= 1,
                _ => {}
            }
        }
    }

    /// Jump back to the matching instruction for a given "End" instruction.
    /// 
    /// ```hs
    /// If (ends up here)
    ///     ...
    /// End (calls jmp_back_to_matching)
    /// ```
    fn jmp_back_to_matching(&mut self, code: &CoreProgram) {
        let mut matching = 1;
        while matching > 0 {
            self.i -= 1;
            match self.fetch(code) {
                Some(CoreOp::If) | Some(CoreOp::While) | Some(CoreOp::Function) => matching -= 1,
                Some(CoreOp::End) => matching += 1,
                _ => {}
            }
        }
    }

    /// Get the matching instruction for a given "End" instruction.
    /// 
    /// ```hs
    /// If (this function would return this instruction)
    ///    ...
    /// End (called on this as the current instruction)
    /// ```
    fn get_matching_for_end<'a>(&mut self, code: &'a CoreProgram) -> Option<&'a CoreOp> {
        let mut matching = 1;
        let mut i = self.i;
        while matching > 0 && i > 0 {
            i -= 1;
            match code.0[i] {
                CoreOp::If | CoreOp::While | CoreOp::Function => matching -= 1,
                CoreOp::End => matching += 1,
                _ => {}
            }
        }

        if matching == 0 {
            Some(&code.0[i])
        } else {
            None
        }
    }

    /// Get the current cell pointed to on the turing tape.
    fn get_cell(&mut self) -> &mut isize {
        while self.pointer >= self.cells.len() {
            self.cells.push(0)
        }

        &mut self.cells[self.pointer]
    }

    /// Run a core program using this interpreter and its device.
    pub fn run(mut self, code: &CoreProgram) -> Result<T, String> {
        while !self.done {
            self.step(code)?
        }
        println!("{:?}", self.cells);
        Ok(self.device)
    }

    /// Run a single step of the interpreter.
    fn step(&mut self, code: &CoreProgram) -> Result<(), String> {
        if let Some(op) = self.fetch(code) {
            match op {
                CoreOp::Comment(_) => {}
                CoreOp::Constant(n) => self.register = *n,
                CoreOp::Function => {
                    if !self.functions.contains(&self.i) {
                        self.functions.push(self.i);
                        self.functions.sort()
                    }
                    self.jmp_to_end(code)
                },
                CoreOp::Call => self.call()?,
                CoreOp::Return => self.ret(),
                CoreOp::While => {
                    if self.register == 0 {
                        self.jmp_to_end(code)
                    }
                },
                CoreOp::If => {
                    if self.register == 0 {
                        self.jmp_to_else(code)
                    }
                }
                CoreOp::Else => {
                    self.jmp_to_end(code)
                },
                CoreOp::End => {
                    if self.register != 0 {
                        if let Some(CoreOp::While) = self.get_matching_for_end(code) {
                            self.jmp_back_to_matching(code)
                        }
                    }
                }
    
                CoreOp::Save => *self.get_cell() = self.register,
                CoreOp::Restore => self.register = *self.get_cell(),
    
                CoreOp::Move(n) => {
                    if *n >= 0 {
                        self.pointer += *n as usize
                    } else {
                        self.pointer -= -*n as usize
                    }
                },
    
                CoreOp::Where => self.register = self.pointer as isize,
                CoreOp::Deref => self.deref(),
                CoreOp::Refer => self.refer()?,
    
                CoreOp::Inc => self.register += 1,
                CoreOp::Dec => self.register -= 1,
                CoreOp::Add => self.register += *self.get_cell(),
                CoreOp::Sub => self.register -= *self.get_cell(),
                CoreOp::Mul => self.register *= *self.get_cell(),
                CoreOp::Div => {
                    let d = *self.get_cell();
                    if d != 0 {
                        self.register /= d
                    }
                },
                CoreOp::Rem => {
                    let d = *self.get_cell();
                    if d != 0 {
                        self.register %= d
                    }
                },
    
                CoreOp::IsNonNegative => self.register = if self.register >= 0 { 1 } else { 0 },
                CoreOp::Get => self.register = self.device.get()?,
                CoreOp::Put => {
                    self.device.put(self.register)?
                },
            }
            self.i += 1
        } else {
            self.done = true
        }
        Ok(())
    }
}