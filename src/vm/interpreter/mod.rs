//! # The Interpreter Module
//!
//! This module implements two interpreters for the virtual machine: one for each variant.
//! Both virtual machines are supplied with a `Device` object, which acts as a generic frontend
//! of the machine to interact with the world. The `Device` object is responsible for
//! supplying the input and handling the output of the program. For testing the compiler,
//! assembler, and virtual machine, we use a `TestingDevice` object to supply sample input
//! and capture the output to test against the predicted output.
use crate::InputMode;
use crate::{Input, Output, OutputMode};

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
    /// Get the next input (from a given input source).
    fn get(&mut self, src: Input) -> Result<isize, String>;
    /// Put the given value to the given output destination.
    fn put(&mut self, val: isize, dst: Output) -> Result<(), String>;

    /// Peek a value from the side-effecting device wrapping
    /// the virtual machine.
    fn peek(&mut self) -> Result<isize, String>;
    /// Poke a value into the side-effecting device wrapping
    /// the virtual machine.
    fn poke(&mut self, val: isize) -> Result<(), String>;
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
    pub output: Vec<(isize, Output)>,
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

    // Get the next character from the input buffer.
    fn get_char(&mut self) -> Result<char, String> {
        self.get(Input::stdin()).map(|n| n as u8 as char)
    }

    fn put_char(&mut self, ch: char) -> Result<(), String> {
        self.output.push((ch as usize as isize, Output::stdout()));
        Ok(())
    }

    /// Get the output of the testing device as a string (ascii).
    pub fn output_str(&self) -> String {
        let mut result = String::new();
        for (ch, _) in &self.output {
            result.push(*ch as u8 as char)
        }
        result
    }
}

/// Make the testing device work with the interpreter.
impl Device for TestingDevice {
    fn get(&mut self, src: Input) -> Result<isize, String> {
        if let Some(n) = self.input.pop_front() {
            Ok(n)
        } else {
            Err(String::from("ran out of input"))
        }
    }

    fn put(&mut self, val: isize, dst: Output) -> Result<(), String> {
        self.output.push((val, dst));
        Ok(())
    }

    fn peek(&mut self) -> Result<isize, String> {
        println!("peeking");
        Ok(0)
    }

    fn poke(&mut self, val: isize) -> Result<(), String> {
        println!("poking {}", val);
        Ok(())
    }
}

/// A device used for standard input and output.
/// This simply retrieves a character from standard-in with `get`,
/// and writes a character to standard-out with `put`.
pub struct StandardDevice;

impl Device for StandardDevice {
    fn get(&mut self, src: Input) -> Result<isize, String> {
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

    fn put(&mut self, val: isize, dst: Output) -> Result<(), String> {
        // Print the character without a newline
        match dst.mode {
            OutputMode::Stdout => print!("{}", val as u8 as char),
            OutputMode::StdoutInt => print!("{}", val),
            OutputMode::StdoutFloat => print!("{}", as_float(val)),
            OutputMode::Stderr => eprint!("{}", val as u8 as char),
            OutputMode::StderrInt => eprint!("{}", val),
            OutputMode::StderrFloat => eprint!("{}", as_float(val)),
            _ => return Err(String::from("invalid output mode")),
        }
        if stdout().flush().is_err() {
            Err(String::from("could not flush output"))
        } else {
            Ok(())
        }
    }

    fn peek(&mut self) -> Result<isize, String> {
        println!("peeking");
        Ok(0)
    }

    fn poke(&mut self, val: isize) -> Result<(), String> {
        println!("poking {}", val);
        Ok(())
    }
}
