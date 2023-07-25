use sage::{side_effects::*, vm::*};
use std::collections::{VecDeque, HashMap};
use super::{as_float, as_int, alert, eval};

/// A device used for testing the compiler. This simply keeps a buffer
/// of sample input to supply to the virtual machine, and keeps an output
/// buffer to keep track of the output of the virtual machine.
///
/// The tests interpret the program and populate the device with output.
/// Then, we check the devices output against the correct output.
#[derive(Debug, Default)]
pub struct WasmDevice {
    pub ffi: HashMap<FFIBinding, fn(&mut VecDeque<i64>, Option<&mut Vec<i64>>)>,
    pub ffi_channel: VecDeque<i64>,
    pub input: VecDeque<i64>,
    pub output: Vec<i64>,
}

impl WasmDevice {
    /// Create a new testing device with some given sample input.
    pub fn new(sample_input: impl ToString) -> Self {
        let mut result = Self {
            ffi: HashMap::new(),
            ffi_channel: VecDeque::new(),
            input: sample_input
                .to_string()
                .chars()
                .map(|ch| ch as i64)
                .collect(),
            output: vec![],
        };
        result.add_binding(FFIBinding::new("alert".to_string(), 1, 0), |ffi_channel, tape| {
            let str_addr = ffi_channel.pop_front().unwrap();
            
            if let Some(tape) = tape {
                let mut str_len = 0;
                while tape[str_addr as usize + str_len] != 0 {
                    str_len += 1;
                }
                let mut str_buf = Vec::with_capacity(str_len);
                for i in 0..str_len {
                    str_buf.push(tape[str_addr as usize + i] as u8);
                }
                let str_buf = String::from_utf8(str_buf).unwrap();
                alert(&str_buf);
            } else {
                alert(&format!("alert: {}", str_addr));
            }
        });

        result.add_binding(FFIBinding::new("eval".to_string(), 2, 0), |ffi_channel, tape| {
            // Read the input string from the buffer and then write the result to the buffer.
            let buf_addr = ffi_channel.pop_front().unwrap();
            let buf_size = ffi_channel.pop_front().unwrap();

            if let Some(tape) = tape {
                let mut str_len = 0;
                while tape[buf_addr as usize + str_len] != 0 {
                    str_len += 1;
                }
                let mut str_buf = Vec::with_capacity(str_len);
                for i in 0..str_len {
                    str_buf.push(tape[buf_addr as usize + i] as i8 as u8);
                }
                let str_buf = String::from_utf8(str_buf).unwrap();
                let js_value = eval(&str_buf);
                let js_value = format!("{js_value:?}");
                let js_value = js_value[8..js_value.len()-1].to_string();
                let js_value = js_value.as_bytes();
                for i in 0..std::cmp::min(js_value.len(), (buf_size - 1) as usize) {
                    tape[buf_addr as usize + i] = (js_value[i] % 0xff) as i64;
                }
                tape[buf_addr as usize + std::cmp::min(js_value.len(), (buf_size - 1) as usize)] = 0;
            }
        });
        result
    }    

    pub fn add_binding(&mut self, ffi: FFIBinding, f: fn(&mut VecDeque<i64>, Option<&mut Vec<i64>>)) {
        self.ffi.insert(ffi, f);
    }

    fn put_char(&mut self, ch: char) -> Result<(), String> {
        self.output.push(ch as usize as i64);
        Ok(())
    }

    fn put_int(&mut self, val: i64) -> Result<(), String> {
        for ch in val.to_string().chars() {
            self.put_char(ch)?
        }
        Ok(())
    }

    fn put_float(&mut self, val: f32) -> Result<(), String> {
        for ch in format!("{val}").chars() {
            self.put_char(ch)?
        }
        Ok(())
    }

    fn get_char(&mut self) -> Result<char, String> {
        Ok(self.input.pop_front().map(|n| n as u8 as char).unwrap_or('\0'))
    }

    fn get_int(&mut self) -> Result<i64, String> {
        let mut result: i64 = 0;
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
                result += (n - b'0') as i64;
                self.input.pop_front();
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn get_float(&mut self) -> Result<f32, String> {
        let whole_part = self.get_int()? as f32;

        if self.input.is_empty() { return Ok(whole_part) }
        
        let n = self.input[0] as u8;
        let ch = n as char;
        if ch == '.' {
            self.get_char()?;
            let fractional_part = self.get_int()? as f32;
            let digits = fractional_part.log10() as i32 + 1;
            Ok(whole_part + if digits > 1 {
                fractional_part / 10.0_f32.powi(digits)
            } else {
                0.0
            })

        } else {
            Ok(whole_part)
        }
    }
}

/// Make the testing device work with the interpreter.
impl Device for WasmDevice {
    fn get(&mut self, src: Input) -> Result<i64, String> {
        match src.mode {
            InputMode::StdinChar => {
                Ok(if let Some(n) = self.input.pop_front() {
                    n
                } else {
                    0
                })
            }
            InputMode::StdinInt => self.get_int(),
            InputMode::StdinFloat => self.get_float().map(as_int),
            _ => Ok(0),
        }
    }

    fn put(&mut self, val: i64, dst: Output) -> Result<(), String> {
        match dst.mode {
            OutputMode::StdoutInt => self.put_int(val),
            OutputMode::StdoutFloat => self.put_float(as_float(val)),
            OutputMode::StderrInt => self.put_int(val),
            OutputMode::StderrFloat => self.put_float(as_float(val)),
            _ => {
                self.output.push(val as i64);
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

    fn ffi_call(&mut self, ffi: &FFIBinding, tape: Option<&mut Vec<i64>>) -> Result<(), String> {
        if let Some(f) = self.ffi.get(ffi) {
            f(&mut self.ffi_channel, tape);
            Ok(())
        } else {
            Err(format!("ffi call not found: {:?}", ffi))
        }
    }
}
