use wasm_bindgen::prelude::*;
use super::{WasmDevice, WasmInterpreter};

use sage::{lir::Compile, targets::CompiledTarget, io::{Input, Output}};
use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    convert::TryFrom
};

#[wasm_bindgen]
pub struct Compiler {
    program: String,
    asm_code: String,
    vm_code: String,
    c_code: String,
    exec: sage::vm::StandardProgram,
    default_device: WasmDevice,
    interpreter: WasmInterpreter<WasmDevice>
}


impl Default for Compiler {
    fn default() -> Self {
        Self {
            program: String::new(),
            asm_code: String::new(),
            vm_code: String::new(),
            c_code: String::new(),
            exec: sage::vm::StandardProgram::default(),
            default_device: WasmDevice::default(),
            interpreter: WasmInterpreter::new(WasmDevice::default())
        }
    }
}

#[wasm_bindgen]
impl Compiler {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Result<Compiler, JsValue> {
        Ok(Compiler::default())
    }

    pub fn clear_inputs(&mut self) {
        self.default_device.inputs.clear();
    }


    #[wasm_bindgen]
    pub fn clear_output(&mut self) {
        for output in self.interpreter.device.outputs.values_mut() {
            output.clear();
        }
    }

    #[wasm_bindgen]
    pub fn read_stdout(&mut self) -> String {
        self.interpreter.device.get_output_to(Output::stdout_char())
            .iter()
            .map(|n| *n as u8 as char)
            .collect()
    }

    #[wasm_bindgen]
    pub fn write_stdin(&mut self, text: &str) {
        self.interpreter.device.update_input_as_string(&Input::stdin_char(), text);
    }

    pub fn clear_input_channels(&mut self) {
        self.default_device.inputs.clear();
        self.interpreter = WasmInterpreter::new(self.default_device.clone());
    }

    pub fn clear_output_channels(&mut self) {
        self.default_device.outputs.clear();
        self.interpreter = WasmInterpreter::new(self.default_device.clone());
    }

    #[wasm_bindgen]
    pub fn add_input_channel(&mut self, input: &str) {
        let input = Input::try_from(input).unwrap();
        self.default_device.add_input(input);
        self.interpreter = WasmInterpreter::new(self.default_device.clone());
    }

    #[wasm_bindgen]
    pub fn add_output_channel(&mut self, output: &str) {
        let output = Output::try_from(output).unwrap();
        self.default_device.add_output(output);
        self.interpreter = WasmInterpreter::new(self.default_device.clone());
    }

    #[wasm_bindgen]
    pub fn run(&mut self, program: &str) -> Result<(), String> {
        self.compile(program)?;
        self.interpreter
            .run(&self.exec)
            .unwrap();
        Ok(())
    }

    #[wasm_bindgen]
    pub fn compile(&mut self, program: &str) -> Result<(), String> {
        self.clear_output();
        self.program = program.to_string();
        self.interpreter = WasmInterpreter::new(self.default_device.clone());
        let asm_code = sage::parse::parse_lir(program)
            .unwrap()
            .compile()
            .unwrap();
        match asm_code {
            Ok(core) => {
                self.asm_code = core.to_string();
                let vm = core.assemble(8192).unwrap();
                self.c_code = sage::targets::C.build_core(&vm).unwrap_or_else(|e| e.to_string());
                self.vm_code = vm.to_string();
                self.exec = vm.into();
            },
            Err(std) => {
                self.asm_code = std.to_string();
                let vm = std.assemble(8192).unwrap();
                self.c_code = sage::targets::C.build_std(&vm).unwrap_or_else(|e| e.to_string());
                self.vm_code = vm.to_string();
                self.exec = vm;
            }
        };

        Ok(())
    }

    #[wasm_bindgen]
    pub fn step(&mut self) -> Result<(), String> {
        if self.interpreter.done {
            return Ok(())
        }
        self.interpreter.step(&self.exec).map_err(|e| e.to_string())
    }

    #[wasm_bindgen]
    pub fn get_output_as_vals(&mut self, output: &str) -> Vec<i32> {
        let output = Output::try_from(output).unwrap();
        self.interpreter.device.get_output_to(output).iter().map(|n| *n as i32).collect()
    }

    #[wasm_bindgen]
    pub fn get_input_as_vals(&mut self, input: &str) -> Vec<i32> {
        let input = Input::try_from(input).unwrap();
        self.interpreter.device.get_input_from(input).iter().map(|n| *n as i32).collect()
    }

    #[wasm_bindgen]
    pub fn get_output_as_string(&mut self, output: &str) -> String {
        let output = Output::try_from(output).unwrap();
        self.interpreter.device.get_output_to(output).iter().map(|n| *n as u8 as char).collect()
    }

    #[wasm_bindgen]
    pub fn get_output_as_bytes(&mut self, output: &str) -> Vec<u8> {
        let output = Output::try_from(output).unwrap();
        self.interpreter.device.get_output_to(output).into_iter().map(|n| *n as u8).collect()
    }

    #[wasm_bindgen]
    pub fn update_input_as_char(&mut self, input: &str, value: char) {
        let input = Input::try_from(input).unwrap();
        self.interpreter.device.update_input_as_char(&input, value).clone()
    }

    #[wasm_bindgen]
    pub fn update_input_as_int(&mut self, input: &str, value: i32) {
        let input = Input::try_from(input).unwrap();
        self.interpreter.device.update_input_as_value(&input, value as i64).clone()
    }

    #[wasm_bindgen]
    pub fn update_input_as_float(&mut self, input: &str, value: f64) {
        let input = Input::try_from(input).unwrap();
        crate::console_log!("adding {value} to input {input}");
        self.interpreter.device.update_input_as_float(&input, value).clone()
    }

    #[wasm_bindgen]
    pub fn update_input_as_string(&mut self, input: &str, value: &str) {
        let input = Input::try_from(input).unwrap();
        self.interpreter.device.update_input_as_string(&input, value).clone()
    }

    #[wasm_bindgen]
    pub fn update_input_as_bytes(&mut self, input: &str, value: Vec<u8>) {
        let input = Input::try_from(input).unwrap();
        self.interpreter.device.update_input_as_bytes(&input, &value).clone()
    }

    #[wasm_bindgen]
    pub fn get_c_code(&self) -> String {
        self.c_code.clone()
    }

    #[wasm_bindgen]
    pub fn get_vm_code(&self) -> String {
        self.vm_code.clone()
    }

    #[wasm_bindgen]
    pub fn get_asm_code(&self) -> String {
        self.asm_code.clone()
    }

    #[wasm_bindgen]
    pub fn get_program(&self) -> String {
        self.program.clone()
    }

}