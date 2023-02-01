use sage::{io::{Input, InputMode, Output, OutputMode}, vm::{Device, *}};
use std::collections::{HashMap, VecDeque};
/// A device used for testing the compiler. This simply keeps a buffer
/// of sample input to supply to the virtual machine, and keeps an output
/// buffer to keep track of the output of the virtual machine.
///
/// The tests interpret the program and populate the device with output.
/// Then, we check the devices output against the correct output.
#[derive(Clone, Debug)]
pub struct WasmDevice {
    pub inputs: HashMap<Input, VecDeque<i64>>,
    pub outputs: HashMap<Output, VecDeque<i64>>,
}


impl Default for WasmDevice {
    fn default() -> Self {
        let mut inputs = HashMap::new();
        let mut outputs = HashMap::new();
        inputs.insert(Input::stdin_char(), VecDeque::new());
        inputs.insert(Input::stdin_int(), VecDeque::new());
        inputs.insert(Input::stdin_float(), VecDeque::new());
        outputs.insert(Output::stdout_char(), VecDeque::new());
        outputs.insert(Output::stdout_int(), VecDeque::new());
        outputs.insert(Output::stdout_float(), VecDeque::new());
        outputs.insert(Output::stderr_char(), VecDeque::new());
        outputs.insert(Output::stderr_int(), VecDeque::new());
        outputs.insert(Output::stderr_float(), VecDeque::new());
        Self { inputs, outputs }
    }
}

impl WasmDevice {
    /// Create a new testing device from a list of input sources
    /// and a list of output destinations.
    pub fn new(inputs: impl IntoIterator<Item=Input>, outputs: impl IntoIterator<Item=Output>) -> Self {
        let mut result = Self {
            inputs: HashMap::new(),
            outputs: HashMap::new(),
        };

        result.add_inputs(inputs);
        result.add_outputs(outputs);

        result
    }

    fn get_input(&mut self, input: &Input) -> Option<&mut VecDeque<i64>> {
        self.inputs.get_mut(input)
    }

    fn get_output(&mut self, output: &Output) -> Option<&mut VecDeque<i64>> {
        self.outputs.get_mut(output)
    }

    pub fn add_input(&mut self, src: Input) {
        if self.inputs.contains_key(&src) {
            return;
        }
        self.inputs.insert(src, VecDeque::new());
    }

    fn add_inputs(&mut self, srcs: impl IntoIterator<Item=Input>) {
        for src in srcs {
            self.add_input(src);
        }
    }

    pub fn set_inputs(&mut self, srcs: impl IntoIterator<Item=Input>) {
        self.inputs.clear();
        self.add_inputs(srcs);
    }

    pub fn add_output(&mut self, dst: Output) {
        if self.outputs.contains_key(&dst) {
            return;
        }
        self.outputs.insert(dst, VecDeque::new());
    }

    pub fn add_outputs(&mut self, dsts: impl IntoIterator<Item=Output>) {
        for dst in dsts {
            self.add_output(dst);
        }
    }

    pub fn set_outputs(&mut self, dsts: impl IntoIterator<Item=Output>) {
        self.outputs.clear();
        self.add_outputs(dsts);
    }

    pub fn update_input_as_char(&mut self, src: &Input, ch: char) {
        if let Some(input) = self.get_input(src) {
            input.push_back(ch as u8 as i64);
        }
    }

    pub fn update_input_as_int(&mut self, src: &Input, val: i64) {
        if let Some(input) = self.get_input(src) {
            input.push_back(val);
        }
    }

    pub fn update_input_as_float(&mut self, src: &Input, val: f64) {
        if let Some(input) = self.get_input(src) {
            let n = super::as_int(val as f32);
            crate::console_log!("converted {val} to {n}");
            input.push_back(n);
        }
    }

    pub fn update_input_as_bytes(&mut self, src: &Input, val: &[u8]) {
        if let Some(input) = self.get_input(src) {
            for ch in val {
                input.push_back(*ch as i64);
            }
        }
    }

    pub fn update_input_as_string(&mut self, src: &Input, val: impl ToString) {
        if let Some(input) = self.get_input(src) {
            for ch in val.to_string().chars() {
                input.push_back(ch as u8 as i64);
            }
        }
    }

    pub fn update_input_as_value(&mut self, src: &Input, val: i64) {
        self.update_input_as_int(src, val)
    }

    pub fn get_outputs(&self) -> &HashMap<Output, VecDeque<i64>> {
        &self.outputs
    }

    pub fn get_output_to(&mut self, output: Output) -> &VecDeque<i64> {
        self.outputs.entry(output).or_default()
    }

    pub fn get_input_from(&mut self, input: Input) -> &VecDeque<i64> {
        self.inputs.entry(input).or_default()
    }

    fn put_to(&mut self, dst: &Output, val: i64) -> Result<(), String> {
        if let Some(output) = self.get_output(dst) {
            output.push_back(val);
        } else {
            panic!("No output channel for {}", dst)
        }
        Ok(())
    }

    fn get_from(&mut self, src: &Input) -> Result<i64, String> {
        if let Some(input) = self.get_input(&src) {
            if input.len() == 1 && ![
                Input::stdin_char(),
                Input::stdin_int(),
                Input::stdin_float(),
            ].contains(&src) {
                Ok(input[0])
            } else {
                Ok(input.pop_front().unwrap_or(0))
            }
        } else {
            panic!("No input channel for {}", src)
        }
    }

    fn put_char(&mut self, ch: char) -> Result<(), String> {
        self.put_to(&Output::stdout_char(), ch as u8 as i64)
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
        Ok(self.get_from(&Input::stdin_char()).map(|n| n as u8 as char).unwrap_or('\0'))
    }

    fn get_int(&mut self) -> Result<i64, String> {
        let mut result: i64 = 0;

        if let Some(input) = self.get_input(&Input::stdin_char()) {
            loop {
                if input.is_empty() { break }
                let ch = input[0] as u8 as char;
                if ch.is_ascii_whitespace() {
                    input.pop_front();
                } else {
                    break
                }
            }

            loop {
                if input.is_empty() { break }
                let n = input[0] as u8;
                let ch = n as char;
                if ch.is_ascii_digit() {
                    result *= 10;
                    result += (n - b'0') as i64;
                    input.pop_front();
                } else {
                    break;
                }
            }
        }

        Ok(result)
    }

    fn get_float(&mut self) -> Result<f64, String> {
        let whole_part = self.get_int()? as f64;

        if let Some(input) = self.get_input(&Input::stdin_char()) {
            if input.is_empty() { return Ok(whole_part) }
            
            let n = input[0] as u8;
            let ch = n as char;
            if ch == '.' {
                self.get_char()?;
                let fractional_part = self.get_int()? as f64;
                let digits = fractional_part.log10() as i32 + 1;
                return Ok(whole_part + if digits > 1 {
                    fractional_part / 10.0_f64.powi(digits)
                } else {
                    0.0
                })
            }
        }
        Ok(whole_part)
    }
}

/// Make the testing device work with the interpreter.
impl Device for WasmDevice {
    fn peek(&mut self) -> Result<i64, String> { Ok(0) }
    fn poke(&mut self, _val: i64) -> Result<(), String> { Ok(()) }

    fn get(&mut self, src: Input) -> Result<i64, String> {
        match src.mode {
            InputMode::StdinChar => self.get_char().map(|ch| ch as u8 as i64),
            InputMode::StdinInt => self.get_int(),
            InputMode::StdinFloat => Ok(super::as_int(self.get_float()? as f32) as i64),
            _ => self.get_from(&src),
        }
    }

    fn put(&mut self, val: i64, dst: Output) -> Result<(), String> {
        match dst.mode {
            OutputMode::StdoutChar | OutputMode::StderrChar => self.put_char(val as u8 as char),
            OutputMode::StdoutInt | OutputMode::StderrInt => self.put_int(val),
            OutputMode::StdoutFloat | OutputMode::StderrFloat => self.put_float(super::as_float(val as i64) as f64),
            _ => self.put_to(&dst, val),
        }
    }
}
