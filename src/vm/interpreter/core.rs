//! # Core Interpreter Module
//! 
//! This module implements an interpreter for the Core virtual machine
//! variant.
use crate::vm::{CoreOp, CoreProgram, Device, StandardDevice};

impl Default for CoreInterpreter<StandardDevice> {
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
pub struct CoreInterpreter<T>
where
    T: Device,
{
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
    done: bool,
}

impl<T> CoreInterpreter<T>
where
    T: Device,
{
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
        let cell = *self.get_cell();
        if cell < 0 {
            panic!("Dereferencing negative cell with value {cell:?}");
        }
        // Set the pointer to the address on the tape.
        self.pointer = cell as usize;
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
    fn call(&mut self, code: &CoreProgram) -> Result<(), String> {
        // If the function has been defined
        if self.functions.len() > self.register as usize {
            // Push the current instruction pointer to the call stack
            self.calls.push(self.i);
            self.i = self.functions[self.register as usize];
            Ok(())
        } else {
            // If the function hasn't been defined yet, we'll have to find it.

            // Push the return address onto the call stack.
            self.calls.push(self.i);
            // Scan all the function definitions from the start of the program until we find it.
            self.i = 0;
            let mut count = -1;
            while count < self.register {
                // Every time we find a function, it will increment `count`.
                match self.fetch(code) {
                    Some(CoreOp::Function) => {
                        count += 1;
                    }
                    Some(_) => {}
                    None => return Err(format!("function {} not defined", self.register)),
                }
                // If `count` hasn't reached the function we want,
                // keep going.
                if count < self.register {
                    self.i += 1;
                }
            }

            // If we've reached the function we want, add it to the definitions.
            if !self.functions.contains(&self.i) {
                self.functions.push(self.i);
                self.functions.sort()
            }
            Ok(())
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
                Some(CoreOp::Else) if matching == 1 => return,
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
            self.cells.extend(vec![0; 1000]);
        }

        &mut self.cells[self.pointer]
    }

    /// Run a core program using this interpreter and its device.
    pub fn run(mut self, code: &CoreProgram) -> Result<T, String> {
        while !self.done {
            self.step(code)?
        }
        Ok(self.device)
    }

    /// Run a single step of the interpreter.
    fn step(&mut self, code: &CoreProgram) -> Result<(), String> {
        if let Some(op) = self.fetch(code) {
            match op {
                CoreOp::Comment(_) => {}
                CoreOp::Set(n) => self.register = *n,
                CoreOp::Function => {
                    if !self.functions.contains(&self.i) {
                        self.functions.push(self.i);
                        self.functions.sort()
                    }
                    self.jmp_to_end(code)
                }
                CoreOp::Call => self.call(code)?,
                CoreOp::Return => self.ret(),
                CoreOp::While => {
                    if self.register == 0 {
                        self.jmp_to_end(code)
                    }
                }
                CoreOp::If => {
                    if self.register == 0 {
                        self.jmp_to_else(code)
                    }
                }
                CoreOp::Else => self.jmp_to_end(code),
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
                }

                CoreOp::Where => self.register = self.pointer as isize,
                CoreOp::Deref => self.deref(),
                CoreOp::Refer => self.refer()?,

                CoreOp::Index => self.register += *self.get_cell(),
                CoreOp::BitwiseNand => {
                    self.register = !(self.register & *self.get_cell());
                }
                CoreOp::Add => self.register += *self.get_cell(),
                CoreOp::Sub => self.register -= *self.get_cell(),
                CoreOp::Mul => self.register *= *self.get_cell(),
                CoreOp::Div => {
                    let d = *self.get_cell();
                    if d != 0 {
                        self.register /= d
                    }
                }
                CoreOp::Rem => {
                    let d = *self.get_cell();
                    if d != 0 {
                        self.register %= d
                    }
                }

                CoreOp::IsNonNegative => self.register = if self.register >= 0 { 1 } else { 0 },
                CoreOp::Get => self.register = self.device.get()?,
                CoreOp::Put => self.device.put(self.register)?,
            }
            self.i += 1
        } else {
            self.done = true
        }
        Ok(())
    }
}
