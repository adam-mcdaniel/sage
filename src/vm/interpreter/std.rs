//! # Standard Interpreter Module
//!
//! This module implements an interpreter for the Standard virtual machine
//! variant.

use crate::vm::{CoreOp, Device, StandardDevice, StandardOp, StandardProgram};

/// A function to reinterpret the bits of an integer as a float.
pub fn as_float(n: i64) -> f64 {
    f64::from_bits(n as u64)
}

/// A function to reinterpret the bits of a float as an integer.
pub fn as_int(n: f64) -> i64 {
    n.to_bits() as i64
}

impl Default for StandardInterpreter<StandardDevice> {
    fn default() -> Self {
        Self::new(StandardDevice::default())
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
    register: Vec<i64>,
    /// The turing tape (composed of integer cells)
    cells: Vec<i64>,
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
            register: vec![0; 1024],
            cells: vec![],
            functions: vec![],
            calls: vec![],
            refs: vec![],
            i: 0,
            done: false,
        }
    }

    fn reg_scalar(&self) -> i64 {
        self.register[0]
    }

    fn reg_mut_scalar(&mut self) -> &mut i64 {
        &mut self.register[0]
    }

    fn reg_vector(&self) -> &Vec<i64> {
        &self.register
    }

    fn reg_mut_vector(&mut self) -> &mut Vec<i64> {
        &mut self.register
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
        let cell = *self.get_cell();
        if cell < 0 {
            panic!("Dereferencing negative cell with value {cell:?}");
        }
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
    fn call(&mut self, code: &StandardProgram) -> Result<(), String> {
        // If the function has been defined
        if self.functions.len() > self.reg_scalar() as usize {
            // Push the current instruction pointer to the call stack
            self.calls.push(self.i);
            self.i = self.functions[self.reg_scalar() as usize];
            Ok(())
        } else {
            // If the function hasn't been defined yet, we'll have to find it.

            // Push the return address onto the call stack.
            self.calls.push(self.i);
            // Scan all the function definitions from the start of the program until we find it.
            self.i = 0;
            let mut count = -1;
            while count < self.reg_scalar() {
                // Every time we find a function, it will increment `count`.
                match self.fetch(code) {
                    Some(StandardOp::CoreOp(CoreOp::Function)) => {
                        count += 1;
                    }
                    Some(_) => {}
                    None => return Err(format!("function {} not defined", self.reg_scalar())),
                }
                // If `count` hasn't reached the function we want,
                // keep going.
                if count < self.reg_scalar() {
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
    fn get_cell(&mut self) -> &mut i64 {
        while self.pointer >= self.cells.len() {
            self.cells.extend(vec![0; 1000]);
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
                    CoreOp::Set(n) => {
                        *self.reg_mut_vector() = n.clone()
                    }
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
                        if self.reg_scalar() == 0 {
                            self.jmp_to_end(code)
                        }
                    }
                    CoreOp::If => {
                        if self.reg_scalar() == 0 {
                            self.jmp_to_else(code)
                        }
                    }
                    CoreOp::Else => self.jmp_to_end(code),
                    CoreOp::End => {
                        if self.reg_scalar() != 0 {
                            if let Some(StandardOp::CoreOp(CoreOp::While)) =
                                self.get_matching_for_end(code)
                            {
                                self.jmp_back_to_matching(code)
                            }
                        }
                    }

                    CoreOp::Load(n) => {
                        while self.pointer + n >= self.cells.len() {
                            self.cells.extend(vec![0; 1000]);
                        }

                        self.reg_mut_vector().clear();

                        for i in 0..*n {
                            let val = self.cells[self.pointer + i];
                            self.reg_mut_vector().push(val);
                        }
                    }

                    CoreOp::Store(n) => {
                        while self.pointer + n >= self.cells.len() {
                            self.cells.extend(vec![0; 1000]);
                        }

                        for i in 0..*n {
                            let val = self.reg_vector()[i];
                            self.cells[self.pointer + i] = val;
                        }
                    }
                    // CoreOp::Load(n) => *self.get_cell() = self.reg_scalar(),
                    // CoreOp::Store(n) => self.register = *self.get_cell(),
                    CoreOp::Move(n) => {
                        if *n >= 0 {
                            self.pointer += *n as usize
                        } else {
                            if self.pointer < -*n as usize {
                                return Err(format!(
                                    "Instruction #{} tried to move the pointer to a negative index.",
                                    self.i
                                ));
                            }
                            self.pointer -= -*n as usize
                        }
                    }


                    CoreOp::Where => *self.reg_mut_scalar() = self.pointer as i64,
                    CoreOp::Offset(n, size) => {
                        for i in 0..*size {
                            self.reg_mut_vector()[i] += *n as i64;
                        }
                    }
                    CoreOp::Deref => self.deref(),
                    CoreOp::Refer => self.refer()?,
    
                    CoreOp::Index(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] += self.cells[self.pointer + i];
                        }
                    }
                    CoreOp::BitwiseNand(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] &= !self.cells[self.pointer + i];
                        }
                    }
                    CoreOp::BitwiseAnd(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] &= self.cells[self.pointer + i];
                        }
                    }
                    CoreOp::BitwiseOr(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] |= self.cells[self.pointer + i];
                        }
                    }
                    CoreOp::BitwiseXor(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] ^= self.cells[self.pointer + i];
                        }
                    }
                    CoreOp::BitwiseNot(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] = !self.reg_vector()[i];
                        }
                    }
                    CoreOp::LeftShift(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] <<= self.cells[self.pointer + i];
                        }
                    }
                    // CoreOp::LogicalRightShift => {
                    //     *self.reg_mut_scalar() = (self.reg_scalar() as u64 >> *self.get_cell() as u64) as i64
                    // }
                    CoreOp::LogicalRightShift(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] = (self.reg_vector()[i] as u64 >> self.cells[self.pointer + i] as u64) as i64;
                        }
                    }

                    CoreOp::ArithmeticRightShift(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] >>= self.cells[self.pointer + i];
                        }
                    }
                    
                    CoreOp::Add(n) => {
                        for i in 0..*n {
                            let val = self.cells[self.pointer + i];
                            self.reg_mut_vector()[i] += val;
                        }
                    }
                    CoreOp::Sub(n) => {
                        for i in 0..*n {
                            let val = self.cells[self.pointer + i];
                            self.reg_mut_vector()[i] -= val;
                        }
                    }
                    CoreOp::Mul(n) => {
                        for i in 0..*n {
                            let val = self.cells[self.pointer + i];
                            self.reg_mut_vector()[i] *= val;
                        }
                    }
                    CoreOp::Div(n) => {
                        for i in 0..*n {
                            let val = self.cells[self.pointer + i];
                            if val != 0 {
                                self.reg_mut_vector()[i] /= val;
                            }
                        }
                    }
                    CoreOp::Rem(n) => {
                        for i in 0..*n {
                            let val = self.cells[self.pointer + i];
                            if val != 0 {
                                self.reg_mut_vector()[i] %= val;
                            }
                        }
                    }
                    CoreOp::Neg(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] *= -1;
                        }
                    }

                    CoreOp::And(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] = i64::from(self.reg_vector()[i] != 0 && self.cells[self.pointer + i] != 0);
                        }
                    }
                    CoreOp::Or(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] = i64::from(self.reg_vector()[i] != 0 || self.cells[self.pointer + i] != 0);
                        }
                    }
                    CoreOp::Not(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] = i64::from(self.reg_vector()[i] == 0);
                        }
                    }
    
                    CoreOp::Inc(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] += 1;
                        }
                    }
                    CoreOp::Dec(n) => {
                        for i in 0..*n {
                            self.reg_mut_vector()[i] -= 1;
                        }
                    }
                    
                    /*
                    CoreOp::CompareEqual => *self.reg_mut_scalar() = i64::from(self.reg_scalar() == *self.get_cell()),
                    CoreOp::CompareGreater => *self.reg_mut_scalar() = i64::from(self.reg_scalar() > *self.get_cell()),
                    CoreOp::CompareLess => *self.reg_mut_scalar() = i64::from(self.reg_scalar() < *self.get_cell()),
                    CoreOp::CompareGreaterEqual => *self.reg_mut_scalar() = i64::from(self.reg_scalar() >= *self.get_cell()),
                    CoreOp::CompareLessEqual => *self.reg_mut_scalar() = i64::from(self.reg_scalar() <= *self.get_cell()),
                    */
    
                    CoreOp::Swap => {
                        let temp = self.reg_scalar();
                        *self.reg_mut_scalar() = *self.get_cell();
                        *self.get_cell() = temp;
                    }

                    CoreOp::IsNonNegative => {
                        *self.reg_mut_scalar() = i64::from(self.reg_scalar() >= 0)
                    }
                    CoreOp::Get(i) => *self.reg_mut_scalar() = self.device.get(i.clone())?,
                    CoreOp::Put(o) => self.device.put(self.reg_scalar(), o.clone())?,
                },

                // StandardOp::Set(n) => *self.reg_mut_scalar() = as_int(*n),
                StandardOp::Set(n) => {
                    *self.reg_mut_vector() = n.iter().map(|n| as_int(*n)).collect()
                }

                StandardOp::ToInt => {
                    // self.register = f64::from_bits(self.register as u64) as i64
                    *self.reg_mut_scalar() = as_float(self.reg_scalar()) as i64;
                }
                StandardOp::ToFloat => {
                    // self.register = (self.register as f64).to_bits() as i64
                    *self.reg_mut_scalar() = as_int(self.reg_scalar() as f64);
                }
                // let cell = f64::from_bits(*self.get_cell() as u64);
                // self.register = (f64::from_bits(self.register as u64) + cell).to_bits() as i64;
                StandardOp::Add(n) => {
                    // let a = as_float(self.reg_scalar());
                    // let b = as_float(*self.get_cell());
                    // *self.reg_mut_scalar() = as_int(a + b)
                    for i in 0..*n {
                        let a = as_float(self.reg_vector()[i]);
                        let b = as_float(self.cells[self.pointer + i]);
                        self.reg_mut_vector()[i] = as_int(a + b);
                    }
                }
                StandardOp::Sub(n) => {
                    // let a = as_float(self.reg_scalar());
                    // let b = as_float(*self.get_cell());
                    // *self.reg_mut_scalar() = as_int(a - b)
                    for i in 0..*n {
                        let a = as_float(self.reg_vector()[i]);
                        let b = as_float(self.cells[self.pointer + i]);
                        self.reg_mut_vector()[i] = as_int(a - b);
                    }
                }
                StandardOp::Mul(n) => {
                    // let a = as_float(self.reg_scalar());
                    // let b = as_float(*self.get_cell());
                    // *self.reg_mut_scalar() = as_int(a * b)
                    for i in 0..*n {
                        let a = as_float(self.reg_vector()[i]);
                        let b = as_float(self.cells[self.pointer + i]);
                        self.reg_mut_vector()[i] = as_int(a * b);
                    }
                }
                StandardOp::Div(n) => {
                    // let a = as_float(self.reg_scalar());
                    // let b = as_float(*self.get_cell());
                    // *self.reg_mut_scalar() = as_int(a / b)
                    for i in 0..*n {
                        let a = as_float(self.reg_vector()[i]);
                        let b = as_float(self.cells[self.pointer + i]);
                        self.reg_mut_vector()[i] = as_int(a / b);
                    }
                }
                StandardOp::Rem(n) => {
                    for i in 0..*n {
                        let a = as_float(self.reg_vector()[i]);
                        let b = as_float(self.cells[self.pointer + i]);
                        self.reg_mut_vector()[i] = as_int(a % b);
                    }
                }

                StandardOp::Neg(n) => {
                    for i in 0..*n {
                        self.reg_mut_vector()[i] = as_int(-as_float(self.reg_vector()[i]));
                    }
                }

                StandardOp::IsNonNegative => {
                    *self.reg_mut_scalar() = i64::from(self.reg_scalar() >= 0)
                }
                StandardOp::Sin => {
                    *self.reg_mut_scalar() = as_int(as_float(self.reg_scalar()).sin())
                }
                StandardOp::Cos => {
                    *self.reg_mut_scalar() = as_int(as_float(self.reg_scalar()).cos())
                }
                StandardOp::Tan => {
                    *self.reg_mut_scalar() = as_int(as_float(self.reg_scalar()).tan())
                }
                StandardOp::ASin => {
                    *self.reg_mut_scalar() = as_int(as_float(self.reg_scalar()).asin())
                }
                StandardOp::ACos => {
                    *self.reg_mut_scalar() = as_int(as_float(self.reg_scalar()).acos())
                }
                StandardOp::ATan => {
                    *self.reg_mut_scalar() = as_int(as_float(self.reg_scalar()).atan())
                }
                StandardOp::Pow => {
                    *self.reg_mut_scalar() =
                        as_int(as_float(self.reg_scalar()).powf(as_float(*self.get_cell())))
                }

                StandardOp::Poke => {
                    self.device.poke(self.reg_scalar())?;
                }
                StandardOp::Peek => {
                    *self.reg_mut_scalar() = self.device.peek()?;
                }

                StandardOp::Alloc => {
                    // If the virtual machine doesn't have a thousand cells,
                    // allocate some.
                    if self.cells.len() < 30000 {
                        self.cells.extend(vec![0; 30000]);
                    }
                    // Save the address of where the new cells will start.
                    let result = self.cells.len() - 1;
                    // Allocate new space at the end of the type.
                    self.cells.extend(vec![0; self.reg_scalar() as usize]);
                    // Store the address of the new space in the register.
                    *self.reg_mut_scalar() = result as i64;
                }
                StandardOp::Free => {}
                StandardOp::Call(binding) => {
                    self.device.ffi_call(binding, Some(&mut self.cells))?;
                }
            }
            self.i += 1
        } else {
            self.done = true
        }
        Ok(())
    }
}
