use crate::vm::{CoreOp, Device, StandardDevice, StandardOp, StandardProgram};
use std::io::{stderr, stdin, Read, Write};

pub fn as_float(n: isize) -> f64 {
    f64::from_bits(n as u64)
}

pub fn as_int(n: f64) -> isize {
    n.to_bits() as isize
}

impl Default for StandardInterpreter<StandardDevice> {
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

/// The interpreter which runs the standard variant of virtual machine programs.
pub struct StandardInterpreter<T>
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

impl<T> StandardInterpreter<T>
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
    fn fetch<'a>(&self, code: &'a StandardProgram) -> Option<&'a StandardOp> {
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
    fn jmp_to_else(&mut self, code: &StandardProgram) {
        let mut matching = 1;
        while matching > 0 {
            self.i += 1;
            match self.fetch(code) {
                Some(StandardOp::CoreOp(CoreOp::If))
                | Some(StandardOp::CoreOp(CoreOp::While))
                | Some(StandardOp::CoreOp(CoreOp::Function)) => matching += 1,
                Some(StandardOp::CoreOp(CoreOp::End)) => matching -= 1,
                Some(StandardOp::CoreOp(CoreOp::Else)) if matching == 1 => return,
                _ => {}
            }
        }
    }

    /// Jump to the matching "End" for this "Else", "While",
    /// or "Function" instruction.
    fn jmp_to_end(&mut self, code: &StandardProgram) {
        let mut matching = 1;
        while matching > 0 {
            self.i += 1;
            match self.fetch(code) {
                Some(StandardOp::CoreOp(CoreOp::If))
                | Some(StandardOp::CoreOp(CoreOp::While))
                | Some(StandardOp::CoreOp(CoreOp::Function)) => matching += 1,
                Some(StandardOp::CoreOp(CoreOp::End)) => matching -= 1,
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
    fn jmp_back_to_matching(&mut self, code: &StandardProgram) {
        let mut matching = 1;
        while matching > 0 {
            self.i -= 1;
            match self.fetch(code) {
                Some(StandardOp::CoreOp(CoreOp::If))
                | Some(StandardOp::CoreOp(CoreOp::While))
                | Some(StandardOp::CoreOp(CoreOp::Function)) => matching -= 1,
                Some(StandardOp::CoreOp(CoreOp::End)) => matching += 1,
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
    fn get_matching_for_end<'a>(&mut self, code: &'a StandardProgram) -> Option<&'a StandardOp> {
        let mut matching = 1;
        let mut i = self.i;
        while matching > 0 && i > 0 {
            i -= 1;
            match code.0[i] {
                StandardOp::CoreOp(CoreOp::If)
                | StandardOp::CoreOp(CoreOp::While)
                | StandardOp::CoreOp(CoreOp::Function) => matching -= 1,
                StandardOp::CoreOp(CoreOp::End) => matching += 1,
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
    pub fn run(mut self, code: &StandardProgram) -> Result<T, String> {
        while !self.done {
            self.step(code)?
        }
        Ok(self.device)
    }

    /// Run a single step of the interpreter.
    fn step(&mut self, code: &StandardProgram) -> Result<(), String> {
        if let Some(op) = self.fetch(code) {
            match op {
                StandardOp::CoreOp(core_op) => match core_op {
                    CoreOp::Comment(_) => {}
                    CoreOp::Set(n) => self.register = *n,
                    CoreOp::Function => {
                        if !self.functions.contains(&self.i) {
                            self.functions.push(self.i);
                            self.functions.sort()
                        }
                        self.jmp_to_end(code)
                    }
                    CoreOp::Call => self.call()?,
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
                            if let Some(StandardOp::CoreOp(CoreOp::While)) =
                                self.get_matching_for_end(code)
                            {
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
                },

                StandardOp::Set(n) => self.register = as_int(*n),
                StandardOp::ToInt => {
                    // self.register = f64::from_bits(self.register as u64) as isize
                    self.register = as_float(self.register) as isize;
                }
                StandardOp::ToFloat => {
                    // self.register = (self.register as f64).to_bits() as isize
                    self.register = as_int(self.register as f64);
                }
                // let cell = f64::from_bits(*self.get_cell() as u64);
                // self.register = (f64::from_bits(self.register as u64) + cell).to_bits() as isize;
                StandardOp::Add => {
                    let a = as_float(self.register);
                    let b = as_float(*self.get_cell());
                    self.register = as_int(a + b)
                }
                StandardOp::Sub => {
                    let a = as_float(self.register);
                    let b = as_float(*self.get_cell());
                    self.register = as_int(a - b)
                }
                StandardOp::Mul => {
                    let a = as_float(self.register);
                    let b = as_float(*self.get_cell());
                    self.register = as_int(a * b)
                }
                StandardOp::Div => {
                    let a = as_float(self.register);
                    let b = as_float(*self.get_cell());
                    self.register = as_int(a / b)
                }
                StandardOp::Rem => {
                    let a = as_float(self.register);
                    let b = as_float(*self.get_cell());
                    self.register = as_int(a % b)
                }
                StandardOp::IsNonNegative => self.register = if self.register >= 0 { 1 } else { 0 },
                StandardOp::Sin => self.register = as_int(as_float(self.register).sin()),
                StandardOp::Cos => self.register = as_int(as_float(self.register).cos()),
                StandardOp::Tan => self.register = as_int(as_float(self.register).tan()),
                StandardOp::ASin => self.register = as_int(as_float(self.register).asin()),
                StandardOp::ACos => self.register = as_int(as_float(self.register).acos()),
                StandardOp::ATan => self.register = as_int(as_float(self.register).atan()),
                StandardOp::Pow => {
                    self.register = as_int(as_float(self.register).powf(as_float(*self.get_cell())))
                }

                StandardOp::GetFloat => {
                    let mut buf = String::new();
                    if stderr().flush().is_err() {
                        return Err("Could not flush output".to_string());
                    }
                    if stdin().read_line(&mut buf).is_err() {
                        return Err("Could not get user input".to_string());
                    }
                    self.register = as_int(buf.trim().parse::<f64>().unwrap_or(0.0))
                }
                StandardOp::GetInt => {
                    let mut buf = String::new();
                    if stderr().flush().is_err() {
                        return Err("Could not flush output".to_string());
                    }
                    if stdin().read_line(&mut buf).is_err() {
                        return Err("Could not get user input".to_string());
                    }
                    self.register = buf.trim().parse::<isize>().unwrap_or(0)
                }
                StandardOp::GetChar => {
                    let mut buf = [0];
                    if stderr().flush().is_err() {
                        return Err("Could not flush output".to_string());
                    }
                    if stdin().read(&mut buf).is_err() {
                        return Err("Could not get user input".to_string());
                    }
                    self.register = buf[0] as isize;
                }
                StandardOp::PutFloat => {
                    eprint!("{:?}", as_float(self.register))
                }
                StandardOp::PutInt => {
                    eprint!("{:?}", self.register)
                }
                StandardOp::PutChar => {
                    eprint!("{}", self.register as u8 as char)
                }

                op => panic!("unimplemented op: {:?}", op),
            }
            self.i += 1
        } else {
            self.done = true
        }
        Ok(())
    }
}
