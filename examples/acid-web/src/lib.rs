use wasm_bindgen::JsCast;
mod utils;
mod interpreter;
mod device;
use interpreter::WasmInterpreter;
use device::WasmDevice;
use wasm_bindgen::prelude::*;
use acid::{lir::Compile, vm::*};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

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
    let body = document.body().expect("document should have a body");

    let source_code = document.get_element_by_id("source").unwrap().dyn_into::<web_sys::HtmlTextAreaElement>().unwrap().value();
    let input = document.get_element_by_id("input").unwrap().dyn_into::<web_sys::HtmlTextAreaElement>().unwrap().value();
    console_log!("input `{input:?}` + code `{source_code:?}`");
    // Manufacture the element we're gonna append
    let output = document.get_element_by_id("output").unwrap();
    output.set_text_content(Some(""));
    let device = WasmDevice::new(input);
    console_log!("input device `{device:?}`...");
    
    // Parse the lower intermediate representation code.
    let device = match acid::parse::parse_lir(source_code)
        .unwrap()
        .compile()
        .unwrap()
    {
        // If we got back a valid program, assemble it and return the result.
        Ok(asm_code) => 
            CoreInterpreter::new(device)
                .run(&asm_code
                    .assemble(8192)
                    .unwrap())
                .unwrap(),
        Err(asm_code) => WasmInterpreter::new(device)
            .run(&asm_code
                .assemble(8192)
                .unwrap())
            .unwrap(),
    };

    console_log!("output device `{device:?}`...");
    let contents = String::from_utf8(device.output
        .into_iter()
        .map(|n| n as u8)
        .collect()).unwrap();

    output.set_text_content(Some(&contents));

    Ok(())
}
