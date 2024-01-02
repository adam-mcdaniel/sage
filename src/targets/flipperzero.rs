//! # C Target
//!
//! An implementation of the virtual machine for the C language.
//!
//! This allows the virtual machine to target C programs.
use super::{Architecture, CompiledTarget};
use crate::{
    side_effects::{Input, InputMode, Output, OutputMode},
    vm::{CoreOp, StandardOp},
};

/// The type for the C target which implements the `Target` trait.
/// This allows the compiler to target the C language.
#[derive(Default)]
pub struct FlipperZero;

impl Architecture for FlipperZero {
    fn supports_input(&self, i: &Input) -> bool {
        matches!(
            i.mode,
            InputMode::StdinChar | InputMode::StdinFloat | InputMode::StdinInt
        )
    }

    fn supports_output(&self, o: &Output) -> bool {
        matches!(
            o.mode,
            OutputMode::StdoutChar | OutputMode::StdoutFloat | OutputMode::StdoutInt
        )
    }

    fn op(&mut self, op: &CoreOp) -> String {
        match op {
            CoreOp::Comment(text) => {
                format!("// {}", text.replace('\n', "\n// ").replace('\r', ""))
                // let mut comment = String::new();
                // for line in n.split('\n') {
                //     comment += &format!("// {}", line.trim());
                // }
                // comment
            }
            CoreOp::While => "while (reg.i) {".to_string(),
            CoreOp::If => "if (reg.i) {".to_string(),
            CoreOp::Else => "} else {".to_string(),
            CoreOp::Set(n) => format!("reg.i = {};", n),
            CoreOp::Call => "funs[reg.i]();".to_string(),
            CoreOp::Return => "return;".to_string(),
            CoreOp::Save => "*ptr = reg;".to_string(),
            CoreOp::Restore => "reg = *ptr;".to_string(),
            CoreOp::Move(n) => format!("ptr += {};", n),
            CoreOp::Where => "reg.p = ptr;".to_string(),
            CoreOp::Deref => "*ref++ = ptr; ptr = ptr->p;".to_string(),
            CoreOp::Refer => "ptr = *--ref;".to_string(),
            CoreOp::Index => "reg.p += ptr->i;".to_string(),
            CoreOp::BitwiseNand => "reg.i = ~(reg.i & ptr->i);".to_string(),
            CoreOp::Add => "reg.i += ptr->i;".to_string(),
            CoreOp::Sub => "reg.i -= ptr->i;".to_string(),
            CoreOp::Mul => "reg.i *= ptr->i;".to_string(),
            CoreOp::Div => "reg.i /= ptr->i;".to_string(),
            CoreOp::Rem => "reg.i %= ptr->i;".to_string(),
            CoreOp::IsNonNegative => "reg.i = reg.i >= 0;".to_string(),
            _ => unreachable!("Invalid op for C target {op:?}"),
        }
    }

    fn std_op(&mut self, op: &StandardOp) -> Result<String, String> {
        Ok(match op {
            StandardOp::Call(ffi) => format!("__{}();", ffi.name),
            StandardOp::Peek => self.peek()?,
            StandardOp::Poke => self.poke()?,
            StandardOp::Set(n) => format!("reg.f = {};", n),
            StandardOp::ToInt => "reg.i = reg.f;".to_string(),
            StandardOp::ToFloat => "reg.f = reg.i;".to_string(),
            StandardOp::ACos => "reg.f = acos(reg.f);".to_string(),
            StandardOp::ASin => "reg.f = asin(reg.f);".to_string(),
            StandardOp::ATan => "reg.f = atan(reg.f);".to_string(),
            StandardOp::Sin => "reg.f = sin(reg.f);".to_string(),
            StandardOp::Cos => "reg.f = cos(reg.f);".to_string(),
            StandardOp::Tan => "reg.f = tan(reg.f);".to_string(),
            StandardOp::Add => "reg.f += ptr->f;".to_string(),
            StandardOp::Sub => "reg.f -= ptr->f;".to_string(),
            StandardOp::Mul => "reg.f *= ptr->f;".to_string(),
            StandardOp::Div => "reg.f /= ptr->f;".to_string(),
            StandardOp::Rem => "reg.f = fmod(reg.f, ptr->f);".to_string(),
            StandardOp::Pow => "reg.f = pow(reg.f, ptr->f);".to_string(),
            StandardOp::IsNonNegative => "reg.i = reg.f >= 0;".to_string(),
            StandardOp::Alloc => "reg.p = malloc(reg.i * sizeof(reg));".to_string(),
            StandardOp::Free => "free(reg.p);".to_string(),
            _ => return Err(format!("Invalid standard op for C target {op:?}")),
        })
    }

    fn end(&mut self, matching: &CoreOp, fun: Option<usize>) -> String {
        match (matching, fun) {
            (CoreOp::Function | CoreOp::While | CoreOp::If | CoreOp::Else, _) => "}".to_string(),
            _ => unreachable!("Invalid matching op for end"),
        }
    }

    fn declare_proc(&mut self, label_id: usize) -> String {
        format!("void f{label_id}() {{")
    }

    fn name(&self) -> &str {
        "C"
    }
    fn version(&self) -> &str {
        "1.0"
    }

    fn supports_floats(&self) -> bool {
        true
    }

    fn get(&mut self, src: &Input) -> Result<String, String> {
        let ch = src.channel.0;
        match src.mode {
            InputMode::StdinChar => Ok("reg.i = getchar();".to_string()),
            InputMode::StdinInt => Ok("scanf(\"%ld\", &reg.i);".to_string()),
            InputMode::StdinFloat => Ok("scanf(\"%lf\", &reg.f);".to_string()),
            InputMode::Thermometer => Ok("reg.f = 293.15;".to_string()),
            InputMode::Clock => Ok("reg.i = time(NULL);".to_string()),
            InputMode::Random => Ok("reg.i = rand();".to_string()),
            InputMode::Button => Ok(format!(
                "printf(\"Button #{ch}: \"); reg.i = getchar() == 'y'; while (getchar() != 10);"
            )),
            _ => Err("Input not supported by this target".to_string()),
        }
    }

    fn put(&mut self, dst: &Output) -> Result<String, String> {
        match dst.mode {
            OutputMode::StdoutChar => Ok("putchar(reg.i);".to_string()),
            OutputMode::StdoutInt => Ok("printf(\"%ld\", reg.i);".to_string()),
            OutputMode::StdoutFloat => Ok("printf(\"%lf\", reg.f);".to_string()),
            OutputMode::StderrChar => Ok("fprintf(stderr, \"%c\", reg.i);".to_string()),
            OutputMode::StderrInt => Ok("fprintf(stderr, \"%lld\", reg.i);".to_string()),
            OutputMode::StderrFloat => Ok("fprintf(stderr, \"%lf\", reg.f);".to_string()),
            OutputMode::Heater => Ok("printf(\"Heating...\");".to_string()),
            OutputMode::Cooler => Ok("printf(\"Cooling...\");".to_string()),
            _ => Err("Output not supported by this target".to_string()),
        }
    }
    fn peek(&mut self) -> Result<String, String> {
        Ok("reg = *(ffi_ptr--);".to_string())
    }
    fn poke(&mut self) -> Result<String, String> {
        Ok("*(++ffi_ptr) = reg;".to_string())
    }
    fn prelude(&self, is_core: bool) -> Option<String> {
        let mut result = r#"#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

typedef union cell {
    int64_t i;
    double f;
    union cell *p;
} cell;

cell tape[6000], *refs[128], *ptr = tape, **ref = refs, reg, ffi_channel[256], *ffi_ptr = ffi_channel;

unsigned int ref_ptr = 0;
void (*funs[500])(void);

/*****************************************************************
 * Flip.x0 Tutorial
 *                                                     by M1ch3al
 * 0x02 - Keypad_app
 * 
 * This example shows you how to interact with the keypad on 
 * the Flip.x0, managing the all events stacked in the 
 * FuriMessageQueue. There's also an example of FuriTimer and 
 * how to use them to interact with the GUI.
 *****************************************************************
    */

#include <stdio.h>
#include <furi.h>
#include <gui/gui.h>
#include <input/input.h>
#include <notification/notification_messages.h>

// (*1) This will be the label that tells you which keypad-button is pressed.

static void do_nothing() {}
static void (*sage_draw_callback)() = do_nothing;
static void (*sage_input_callback)() = do_nothing;
static void (*sage_timer_callback)() = do_nothing;

static FuriMessageQueue* event_queue = NULL;
static Canvas *canvas = NULL;

static void draw_callback(Canvas* current_canvas, void* ctx) {
    UNUSED(ctx);
    canvas = current_canvas;

    // canvas_clear(canvas);
    // canvas_set_font(canvas, FontPrimary);
    // canvas_draw_str(canvas, 34, 12, "First app");

    // canvas_set_font(canvas, FontSecondary);
    // canvas_draw_str(canvas, 9, 40, "Key pressed - ");

    // canvas_draw_line(canvas, 0, 16, 127, 16);

    // // Reference to (*1)
    // canvas_set_font(canvas, FontSecondary);
    // canvas_draw_str(canvas, 71, 40, "Test");

    sage_draw_callback();
}

void __set_font_primary() {
    if (!canvas) {
        return;
    }
    canvas_set_font(canvas, FontPrimary);
}

void __set_font_secondary() {
    if (!canvas) {
        return;
    }
    canvas_set_font(canvas, FontSecondary);
}

void __clear() {
    if (!canvas) {
        return;
    }
    canvas_clear(canvas);
}

// (*2) Here we can define a callback for the Timer: every 2 seconds
// the furi-timer system will call our defined callback
static void timer_callback(FuriMessageQueue* event_queue) {
    if (!event_queue) {
        return;
    }

    sage_timer_callback();
}

static void input_callback(InputEvent* input_event, void* ctx) {
    if (!input_event || !event_queue) {
        return;
    }
    furi_assert(ctx);
    FuriMessageQueue* event_queue = ctx;
    furi_message_queue_put(event_queue, input_event, FuriWaitForever);

    sage_input_callback();
}

void __set_draw_callback() {
    // Get the pointer from the FFI channel
    sage_draw_callback = funs[ffi_ptr[0].i];
    ffi_ptr--;
}

void __set_input_callback() {
    // Get the pointer from the FFI channel
    sage_input_callback = funs[ffi_ptr[0].i];
    ffi_ptr--;
}

void __set_timer_callback() {
    // Get the pointer from the FFI channel
    sage_timer_callback = funs[ffi_ptr[0].i];
    ffi_ptr--;
}

void __exit() {
    exit(0);
}

void __get_key_pressed() {
    // Get the pointer from the FFI channel
    InputEvent event;
    furi_check(furi_message_queue_get(event_queue, &event, FuriWaitForever) == FuriStatusOk);
    ffi_ptr++;
    ffi_ptr[0].i = 0;

    if(event.key == InputKeyLeft) {
        ffi_ptr[0].i = 1;
    }
    if(event.key == InputKeyRight) {
        ffi_ptr[0].i |= 1 << 1;
    }
    if(event.key == InputKeyUp) {
        ffi_ptr[0].i |= 1 << 2;
    }
    if(event.key == InputKeyDown) {
        ffi_ptr[0].i |= 1 << 3;
    }
    if(event.key == InputKeyOk) {
        ffi_ptr[0].i |= 1 << 4;
    }
    if(event.key == InputKeyBack) {
        ffi_ptr[0].i |= 1 << 5;
    }
}

void __draw_line() {
    if (!canvas) {
        return;
    }
    // Get the pointer from the FFI channel
    canvas_draw_line(canvas, ffi_ptr[-3].i, ffi_ptr[-2].i, ffi_ptr[-1].i, ffi_ptr[0].i);
    ffi_ptr -= 4;
}

void __draw_str() {
    if (!canvas) {
        return;
    }
    // Get the pointer from the FFI channel
    char buffer[1024];
    for (uint32_t i = 0; i < sizeof(buffer); i++) {
        buffer[i] = (ffi_ptr[0].p)[i].i;
    }
    buffer[sizeof(buffer) - 1] = '\0';
    canvas_draw_str(canvas, ffi_ptr[-2].i, ffi_ptr[-1].i, buffer);
    ffi_ptr -= 3;
}
"#
        .to_string();

        if !is_core {
            result = "#include <stdlib.h>\n".to_string() + &result;
        }

        Some(result)
    }

    fn post_funs(&self, funs: Vec<i32>) -> Option<String> {
        let mut result = String::from(r#"
int32_t sage_flipper_output_app(void* p) {
    UNUSED(p);

    // tape = malloc(10 * 1024 * sizeof(cell));
    memset(tape, 0, sizeof(tape));
    // ptr = tape;
    // Initialization of (*1)
    // corrected by an meitwouldseem, thank you man !

    event_queue = furi_message_queue_alloc(8, sizeof(InputEvent));

    ViewPort* view_port = view_port_alloc();
    view_port_draw_callback_set(view_port, draw_callback, NULL);
    view_port_input_callback_set(view_port, input_callback, event_queue);

    Gui* gui = furi_record_open(RECORD_GUI);
    gui_add_view_port(gui, view_port, GuiLayerFullscreen);

    // Creates a furi-timer and associate the proper callback defined in (*2)
    FuriTimer* timer = furi_timer_alloc(timer_callback, FuriTimerTypePeriodic, event_queue);
    // Starts the timer - expiration time in milliseconds (in this case, 2 seconds)
    furi_timer_start(timer, 2000);
"#);
        for fun in funs {
            result += &format!("\tfuns[{fun}] = f{fun};\n", fun = fun)
        }
        Some(result)
    }

    fn postop(&self) -> Option<String> {
        Some("\n".to_string())
    }

    fn postlude(&self, _is_core: bool) -> Option<String> {
        Some(r#"

    // When you use the timer, remember to stop the timer
    furi_timer_stop(timer);
    // and free up the memory allocated for the timer
    furi_timer_free(timer);

    // Freeing up memory from all the unused resources.
    furi_message_queue_free(event_queue);
    gui_remove_view_port(gui, view_port);
    view_port_free(view_port);
    furi_record_close(RECORD_GUI);
    return 0;
}"#.to_string())
    }
}

impl CompiledTarget for FlipperZero {}
