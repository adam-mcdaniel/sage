//! # The Interpreter Module
//!
//! This module implements two interpreters for the virtual machine: one for each variant.
//! Both virtual machines are supplied with a `Device` object, which acts as a generic frontend
//! of the machine to interact with the world. The `Device` object is responsible for
//! supplying the input and handling the output of the program. For testing the compiler,
//! assembler, and virtual machine, we use a `TestingDevice` object to supply sample input
//! and capture the output to test against the predicted output.

mod core;
pub use self::core::*;
mod std;
pub use self::std::*;

use ::std::{
    collections::VecDeque,
    io::{stdin, stdout, Read, Write},
};

/// Create an input / output device for the virtual machine interpreter
/// to operate on. The method `get` retrieves the device's input, and the
/// function `put` writes to the devices output.
///
/// TODO: Make a trait for a device with the standard variant, which requires
/// `get_char`, `put_char`, `get_int`, `put_int`, `get_float`, and `put_float` methods.
pub trait Device {
    fn get(&mut self) -> Result<isize, String>;
    fn put(&mut self, val: isize) -> Result<(), String>;

    fn get_char(&mut self) -> Result<char, String>;
    fn put_char(&mut self, val: char) -> Result<(), String>;

    fn get_int(&mut self) -> Result<isize, String>;
    fn put_int(&mut self, val: isize) -> Result<(), String>;
    fn get_float(&mut self) -> Result<f64, String>;
    fn put_float(&mut self, val: f64) -> Result<(), String>;
}

/// A device used for testing the compiler. This simply keeps a buffer
/// of sample input to supply to the virtual machine, and keeps an output
/// buffer to keep track of the output of the virtual machine.
///
/// The tests interpret the program and populate the device with output.
/// Then, we check the devices output against the correct output.
#[derive(Debug, Default)]
pub struct TestingDevice {
    pub input: VecDeque<isize>,
    pub output: Vec<isize>,
}

impl TestingDevice {
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
impl Device for TestingDevice {
    fn get(&mut self) -> Result<isize, String> {
        if let Some(n) = self.input.pop_front() {
            Ok(n)
        } else {
            Err(String::from("ran out of input"))
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
            let ch= self.get_char()?;
            if !ch.is_whitespace() {
                if ch.is_ascii_digit() {
                    result = (ch as u8 - b'0') as isize;
                }
                break;
            }
        }

        loop {
            let ch= self.get_char()?;
            if ch.is_ascii_digit() {
                result *= 10;
                result += (ch as u8 - b'0') as isize
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn get_float(&mut self) -> Result<f64, String> {
        let whole_part = self.get_int()?;
        
        Ok(whole_part as f64 + if self.get_char()? == '.' {
            let fractional_part = self.get_int()? as f64;
            fractional_part / 10.0_f64.powi(fractional_part.log10() as i32 + 1)
        } else {
            0.0
        })
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
        if stdout().flush().is_err() {
            Err(String::from("could not flush output"))
        } else {
            Ok(())
        }
    }

    fn get_char(&mut self) -> Result<char, String> {
        let mut buf = [0];
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }
        if stdin().read(&mut buf).is_err() {
            return Err("Could not get user input".to_string());
        }
        Ok(buf[0] as char)
    }
    fn put_char(&mut self, ch: char) -> Result<(), String> {
        print!("{}", ch);
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }
        Ok(())
    }

    fn get_int(&mut self) -> Result<isize, String> {
        let mut buf = [0];
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }

        while stdin().read(&mut buf).is_ok() && (buf[0] as char).is_whitespace() {}

        let mut result = if buf[0].is_ascii_digit() {
            (buf[0] - b'0') as isize
        } else {
            0
        };

        while stdin().read(&mut buf).is_ok() {
            if buf[0].is_ascii_digit() {
                result *= 10;
                result += (buf[0] - b'0') as isize
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn put_int(&mut self, val: isize) -> Result<(), String> {
        print!("{:?}", val);
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }
        Ok(())
    }

    fn get_float(&mut self) -> Result<f64, String> {
        let mut buf = String::new();
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }
        if stdin().read_line(&mut buf).is_err() {
            return Err("Could not get user input".to_string());
        }
        Ok(buf.trim().parse::<f64>().unwrap_or(0.0))
    }

    fn put_float(&mut self, val: f64) -> Result<(), String> {
        print!("{:?}", val);
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }
        Ok(())
    }
}
