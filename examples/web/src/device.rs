use sage::vm::Device;
use std::collections::VecDeque;

/// A device used for testing the compiler. This simply keeps a buffer
/// of sample input to supply to the virtual machine, and keeps an output
/// buffer to keep track of the output of the virtual machine.
///
/// The tests interpret the program and populate the device with output.
/// Then, we check the devices output against the correct output.
#[derive(Debug, Default)]
pub struct WasmDevice {
    pub input: VecDeque<isize>,
    pub output: Vec<isize>,
}

impl WasmDevice {
    /// Create a new testing device with some given sample input.
    pub fn new(sample_input: impl ToString) -> Self {
        Self {
            input: sample_input
                .to_string()
                .chars()
                .map(|ch| ch as isize)
                .collect(),
            output: vec![],
        }
    }

    pub fn new_raw(input: Vec<isize>) -> Self {
        Self {
            input: input.into(),
            output: vec![],
        }
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
impl Device for WasmDevice {
    fn get(&mut self) -> Result<isize, String> {
        if let Some(n) = self.input.pop_front() {
            Ok(n)
        } else {
            Ok(0)
        }
    }

    fn put(&mut self, val: isize) -> Result<(), String> {
        self.output.push(val);
        Ok(())
    }

    fn put_char(&mut self, ch: char) -> Result<(), String> {
        self.output.push(ch as usize as isize);
        Ok(())
    }

    fn put_int(&mut self, val: isize) -> Result<(), String> {
        for ch in val.to_string().chars() {
            self.put_char(ch)?
        }
        Ok(())
    }

    fn put_float(&mut self, val: f64) -> Result<(), String> {
        for ch in format!("{val:?}").chars() {
            self.put_char(ch)?
        }
        Ok(())
    }

    fn get_char(&mut self) -> Result<char, String> {
        self.get().map(|n| n as u8 as char)
    }

    fn get_int(&mut self) -> Result<isize, String> {
        let mut result: isize = 0;
        loop {
            if self.input.is_empty() { break }
            let ch = self.input[0] as u8 as char;
            if ch.is_ascii_whitespace() {
                self.get_char()?;
            } else {
                break
            }
        }

        loop {
            if self.input.is_empty() { break }
            let n = self.input[0] as u8;
            let ch = n as char;
            if ch.is_ascii_digit() {
                result *= 10;
                result += (n - b'0') as isize;
                self.input.pop_front();
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn get_float(&mut self) -> Result<f64, String> {
        let whole_part = self.get_int()? as f64;

        if self.input.is_empty() { return Ok(whole_part) }
        
        let n = self.input[0] as u8;
        let ch = n as char;
        if ch == '.' {
            self.get_char()?;
            let fractional_part = self.get_int()? as f64;
            let digits = fractional_part.log10() as i32 + 1;
            Ok(whole_part + if digits > 1 {
                fractional_part / 10.0_f64.powi(digits)
            } else {
                0.0
            })

        } else {
            Ok(whole_part)
        }
    }
}
