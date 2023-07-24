//! # The Interpreter Module
//!
//! This module implements two interpreters for the virtual machine: one for each variant.
//! Both virtual machines are supplied with a `Device` object, which acts as a generic frontend
//! of the machine to interact with the world. The `Device` object is responsible for
//! supplying the input and handling the output of the program. For testing the compiler,
//! assembler, and virtual machine, we use a `TestingDevice` object to supply sample input
//! and capture the output to test against the predicted output.
use crate::side_effects::{FFIBinding, Input, InputMode, Output, OutputMode};

mod core;
pub use self::core::*;
mod std;
pub use self::std::*;

use ::std::{
    collections::{VecDeque, HashMap},
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
    fn get(&mut self, src: Input) -> Result<i64, String>;
    /// Put the given value to the given output destination.
    fn put(&mut self, val: i64, dst: Output) -> Result<(), String>;

    /// Peek a value from the side-effecting device wrapping
    /// the virtual machine.
    fn peek(&mut self) -> Result<i64, String>;
    /// Poke a value into the side-effecting device wrapping
    /// the virtual machine.
    fn poke(&mut self, val: i64) -> Result<(), String>;

    /// FFI call to the device.
    fn ffi_call(&mut self, ffi: &FFIBinding) -> Result<(), String>;
}

/// A device used for testing the compiler. This simply keeps a buffer
/// of sample input to supply to the virtual machine, and keeps an output
/// buffer to keep track of the output of the virtual machine.
///
/// The tests interpret the program and populate the device with output.
/// Then, we check the devices output against the correct output.
#[derive(Debug, Default)]
pub struct TestingDevice {
    pub ffi: HashMap<FFIBinding, fn(&mut VecDeque<i64>)>,
    pub ffi_channel: VecDeque<i64>,
    pub input: VecDeque<i64>,
    pub output: Vec<(i64, Output)>,
}

impl TestingDevice {
    /// Create a new testing device with some given sample input.
    pub fn new(sample_input: impl ToString) -> Self {
        Self {
            ffi: HashMap::new(),
            ffi_channel: VecDeque::new(),
            input: sample_input
                .to_string()
                .chars()
                .map(|ch| ch as i64)
                .collect(),
            output: vec![],
        }
    }

    pub fn new_raw(input: Vec<i64>) -> Self {
        Self {
            ffi: HashMap::new(),
            ffi_channel: VecDeque::new(),
            input: input.into(),
            output: vec![],
        }
    }

    fn put_char(&mut self, ch: char) -> Result<(), String> {
        self.output.push((ch as u64 as i64, Output::stdout_char()));
        Ok(())
    }

    fn put_int(&mut self, val: i64) -> Result<(), String> {
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
        self.get(Input::stdin_char()).map(|n| n as u8 as char)
    }

    fn get_int(&mut self) -> Result<i64, String> {
        let mut result: i64 = 0;
        loop {
            if self.input.is_empty() {
                break;
            }
            let ch = self.input[0] as u8 as char;
            if ch.is_ascii_whitespace() {
                self.get_char()?;
            } else {
                break;
            }
        }

        loop {
            if self.input.is_empty() {
                break;
            }
            let n = self.input[0] as u8;
            let ch = n as char;
            if ch.is_ascii_digit() {
                result *= 10;
                result += (n - b'0') as i64;
                self.input.pop_front();
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn get_float(&mut self) -> Result<f64, String> {
        let whole_part = self.get_int()? as f64;

        if self.input.is_empty() {
            return Ok(whole_part);
        }

        let n = self.input[0] as u8;
        let ch = n as char;
        if ch == '.' {
            self.get_char()?;
            let fractional_part = self.get_int()? as f64;
            let digits = fractional_part.log10() as i32 + 1;
            Ok(whole_part
                + if digits > 1 {
                    fractional_part / 10.0_f64.powi(digits)
                } else {
                    0.0
                })
        } else {
            Ok(whole_part)
        }
    }

    /// Get the output of the testing device as a string (ascii).
    pub fn output_str(&self) -> String {
        let mut result = String::new();
        for (ch, _) in &self.output {
            result.push(*ch as i8 as u8 as char)
        }
        result
    }

    pub fn output_vals(&self) -> Vec<i64> {
        self.output.iter().map(|(val, _)| *val).collect()
    }
}

/// Make the testing device work with the interpreter.
impl Device for TestingDevice {
    fn get(&mut self, src: Input) -> Result<i64, String> {
        match src.mode {
            InputMode::StdinChar => {
                if let Some(n) = self.input.pop_front() {
                    Ok(n)
                } else {
                    Err("input is empty".to_string())
                }
            }
            InputMode::StdinInt => self.get_int(),
            InputMode::StdinFloat => self.get_float().map(as_int),
            _ => {
                eprintln!("Requested input mode: {}", src.mode);
                Ok(0)
            }
        }
    }

    fn put(&mut self, val: i64, dst: Output) -> Result<(), String> {
        match dst.mode {
            OutputMode::StdoutChar => {
                self.output.push((val, dst));
                Ok(())
            }
            OutputMode::StdoutInt => self.put_int(val),
            OutputMode::StdoutFloat => self.put_float(as_float(val)),
            _ => {
                eprintln!("Requested output mode: {} (with output={val})", dst.mode);
                Ok(())
            }
        }
    }

    fn peek(&mut self) -> Result<i64, String> {
        // println!("peeking");
        // Ok(0)
        if let Some(n) = self.ffi_channel.pop_front() {
            Ok(n)
        } else {
            Err("ffi channel is empty".to_string())
        }
    }

    fn poke(&mut self, val: i64) -> Result<(), String> {
        self.ffi_channel.push_back(val);
        Ok(())
    }

    fn ffi_call(&mut self, ffi: &FFIBinding) -> Result<(), String> {
        if let Some(f) = self.ffi.get(ffi) {
            f(&mut self.ffi_channel);
            Ok(())
        } else {
            Err(format!("ffi call not found: {:?}", ffi))
        }
    }
}

/// A device used for standard input and output.
/// This simply retrieves a character from standard-in with `get`,
/// and writes a character to standard-out with `put`.
#[derive(Debug, Clone)]
pub struct StandardDevice {
    ffi: HashMap<FFIBinding, fn(&mut VecDeque<i64>)>,
    ffi_channel: VecDeque<i64>,
}

impl Default for StandardDevice {
    fn default() -> Self {
        let mut result = Self {
            ffi: HashMap::new(),
            ffi_channel: VecDeque::new(),
        };
        
        result.add_binding(FFIBinding::new("square_root".to_string(), 1, 1), |channel| {
            let val = as_float(channel.pop_front().unwrap());
            channel.push_back(as_int(val.sqrt()));
        });

        result.add_binding(FFIBinding::new("add".to_string(), 2, 1), |channel| {
            let a = as_float(channel.pop_front().unwrap());
            let b = as_float(channel.pop_front().unwrap());
            channel.push_back(as_int(a + b));
        });

        result
    }
}

impl StandardDevice {
    pub fn add_binding(&mut self, ffi: FFIBinding, f: fn(&mut VecDeque<i64>)) {
        self.ffi.insert(ffi, f);
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

    fn get_int(&mut self) -> Result<i64, String> {
        let mut buf = [0];
        if stdout().flush().is_err() {
            return Err("Could not flush output".to_string());
        }

        while stdin().read(&mut buf).is_ok() && (buf[0] as char).is_whitespace() {}

        let mut result = if buf[0].is_ascii_digit() {
            (buf[0] - b'0') as i64
        } else {
            0
        };

        while stdin().read(&mut buf).is_ok() {
            if buf[0].is_ascii_digit() {
                result *= 10;
                result += (buf[0] - b'0') as i64
            } else {
                break;
            }
        }

        Ok(result)
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
}

impl Device for StandardDevice {
    fn get(&mut self, src: Input) -> Result<i64, String> {
        Ok(match src.mode {
            InputMode::StdinChar => self.get_char()? as i64,
            InputMode::StdinInt => self.get_int()?,
            InputMode::StdinFloat => as_int(self.get_float()?),
            InputMode::Thermometer => as_int(295.15),
            _ => {
                eprintln!(
                    "Requested input mode: {} (on channel #{})",
                    src.mode, src.channel
                );
                0
            }
        })
    }

    fn put(&mut self, val: i64, dst: Output) -> Result<(), String> {
        // Print the character without a newline
        match dst.mode {
            OutputMode::StdoutChar => print!("{}", val as u8 as char),
            OutputMode::StdoutInt => print!("{}", val),
            OutputMode::StdoutFloat => print!("{:?}", as_float(val)),
            OutputMode::StderrChar => eprint!("{}", val as u8 as char),
            OutputMode::StderrInt => eprint!("{}", val),
            OutputMode::StderrFloat => eprint!("{:?}", as_float(val)),
            _ => {
                eprintln!(
                    "Requested output mode: {} (on channel #{}) with output={val}",
                    dst.mode, dst.channel
                );
            }
        }
        if stdout().flush().is_err() {
            Err(String::from("could not flush output"))
        } else {
            Ok(())
        }
    }

    fn peek(&mut self) -> Result<i64, String> {
        // println!("peeking");
        // Ok(0)
        if let Some(n) = self.ffi_channel.pop_front() {
            Ok(n)
        } else {
            Err("ffi channel is empty".to_string())
        }
    }

    fn poke(&mut self, val: i64) -> Result<(), String> {
        // println!("poking {}", val);
        // Ok(())
        self.ffi_channel.push_back(val);
        Ok(())
    }

    fn ffi_call(&mut self, ffi: &FFIBinding) -> Result<(), String> {
        // println!("ffi call: {:?}", ffi);
        // Ok(())
        if let Some(f) = self.ffi.get(ffi) {
            f(&mut self.ffi_channel);
            Ok(())
        } else {
            Err(format!("ffi call not found: {:?}", ffi))
        }
    }
}
