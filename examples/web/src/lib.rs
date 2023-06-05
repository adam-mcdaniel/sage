use wasm_bindgen::JsCast;
mod utils;
mod interpreter;
mod device;
use interpreter::WasmInterpreter;
use device::WasmDevice;
use wasm_bindgen::prelude::*;
use sage::{lir::Compile, targets::{self, CompiledTarget}};

/// A function to reinterpret the bits of an integer as a float.
pub fn as_float(n: i64) -> f32 {
    f32::from_bits(n as u32)
}

/// A function to reinterpret the bits of a float as an integer.
pub fn as_int(n: f32) -> i64 {
    n.to_bits() as i64
}

// Called by our JS entry point to run the example
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    utils::set_panic_hook();
    compile_and_run()
}

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    // The `console.log` is quite polymorphic, so we can bind it with multiple
    // signatures. Note that we need to use `js_name` to ensure we always call
    // `log` in JS.
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_u32(a: u32);

    // Multiple arguments too!
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_many(a: &str, b: &str);
}

macro_rules! console_log {
    // Note that this is using the `log` function imported above during
    // `bare_bones`
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}
#[wasm_bindgen(js_name = compile_and_run)]
pub fn compile_and_run() -> Result<(), JsValue> {
    // Use `web_sys`'s global `window` function to get a handle on the global
    // window object.
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");

    let source_code = document.get_element_by_id("source").unwrap().dyn_into::<web_sys::HtmlTextAreaElement>().unwrap().value();
    let input = document.get_element_by_id("input").unwrap().dyn_into::<web_sys::HtmlTextAreaElement>().unwrap().value();
    console_log!("input `{input:?}` + code `{source_code:?}`");
    // Manufacture the element we're gonna append
    let output = document.get_element_by_id("output").unwrap().dyn_into::<web_sys::HtmlTextAreaElement>().unwrap();
    output.set_value("");
    let device = WasmDevice::new(input);
    console_log!("input device `{device:?}`...");

    let target = document.get_element_by_id("target").unwrap().dyn_into::<web_sys::HtmlSelectElement>().unwrap().value();
    let contents = match sage::parse::parse_lir(source_code) {
        Ok(asm_code) => {
            match asm_code.compile() {
                Ok(asm_code) => {
                    match target.as_str() {
                        "run" => {
                            let device = WasmInterpreter::new(device)
                            .run(&match asm_code {
                                Ok(core) => core.into(),
                                Err(std) => std }
                                .assemble(8192)
                                .unwrap())
                            .unwrap();
                            String::from_utf8(device.output
                                .into_iter()
                                .map(|n| n as u8)
                                .collect()).unwrap()
                        },
                        "asm" => {
                            match asm_code {
                                // If we got back a valid program, assemble it and return the result.
                                Ok(asm_code) => asm_code.to_string(),
                                Err(asm_code) => asm_code.to_string(),
                            }
                        }
                        "vm" => {
                            match asm_code {
                                // If we got back a valid program, assemble it and return the result.
                                Ok(asm_code) => asm_code.assemble(8192).unwrap().to_string(),
                                Err(asm_code) => asm_code.assemble(8192).unwrap().to_string(),
                            }
                        }
                        "c" => {
                            match asm_code {
                                // If we got back a valid program, assemble it and return the result.
                                Ok(asm_code) => targets::C::default().build_core(&asm_code.assemble(8192).unwrap()).unwrap(),
                                Err(asm_code) => targets::C::default().build_std(&asm_code.assemble(8192).unwrap()).unwrap(),
                            }
                        }
                        "x86" => {
                            match asm_code {
                                // If we got back a valid program, assemble it and return the result.
                                Ok(asm_code) => targets::X86::default().build_core(&asm_code.assemble(8192).unwrap()).unwrap(),
                                Err(asm_code) => targets::X86::default().build_std(&asm_code.assemble(8192).unwrap()).unwrap(),
                            }
                        }
                        otherwise => {
                            console_log!("unknown target `{otherwise:?}`");
                            unreachable!()
                            // panic!("Unknown target `{otherwise:?}`");
                        },
                    }
                }
                Err(e) => e.to_string(),
            }
        }
        Err(e) => e.to_string(),
    };

    console_log!("output `{contents:?}`");
    output.set_value(&contents);

    Ok(())
}
