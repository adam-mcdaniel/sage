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
