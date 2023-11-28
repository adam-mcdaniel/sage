//! # C Target
//!
//! An implementation of the virtual machine for the C language.
//!
//! This allows the virtual machine to target C programs.
//!
//! ## Portability
//!
//! Right now, this target only supports GCC due to a quirk
//! with the way this implementation compiles functions.
//! For some reason, Clang doesn't like nested functions,
//! even though the function's addresses can still be known
//! as labels at compile time. I'm really not sure why Clang
//! *chooses* not to compile nested functions. This can be
//! fixed by this implementations by just moving function definitions
//! code outside of the `main` function, since the virtual machine
//! does not depend on defining functions at runtime.
use super::{Architecture, CompiledTarget};
use crate::{
    side_effects::{Input, InputMode, Output, OutputMode},
    vm::{CoreOp, StandardOp},
};

/// The type for the C target which implements the `Target` trait.
/// This allows the compiler to target the C language.
#[derive(Default)]
pub struct MyOS;

impl Architecture for MyOS {
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
                // format!("// {}", text.replace('\n', "\n// ").replace('\r', ""))
                let mut comment = String::new();
                for line in text.split('\n') {
                    comment += &format!("// {}", line.trim());
                }
                comment
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
            _ => unreachable!("Invalid op for MyOS target {op:?}"),
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
            StandardOp::Alloc => "reg.p = (cell*)salloc(reg.i * sizeof(reg));".to_string(),
            StandardOp::Free => "sfree((void*)reg.p);".to_string(),
            _ => return Err(format!("Invalid standard op for MyOS target {op:?}")),
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
            InputMode::StdinChar => Ok("reg.i = sgetchar();".to_string()),
            InputMode::StdinInt => Ok("scanf(\"%lld\", &reg.i);".to_string()),
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
        /*
        let mut result = r#"#include <stdlib.h>
        #include <stdio.h>
        #include <stdlib.h>
#include <math.h>
#include <string.h>

union cell {
long long int i;
double f;
union cell *p;
} tape[200000], *refs[1024], *ptr = tape, **ref = refs, reg, ffi_channel[256], *ffi_ptr = ffi_channel;

void __unsafe_memcpy() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p;
    long long int n = ffi_ptr[0].i;
    memcpy(dst, src, n * sizeof(union cell));
}

unsigned int ref_ptr = 0;
void (*funs[10000])(void);
"#
        .to_string();
    */
        let mut result = r#"#include <stdio.h>
#include <string.h>
#include <stdint.h>

typedef union cell {
    int64_t i;
    double f;
    union cell *p;
} cell;

union cell *tape, *refs, *ptr, **ref, reg, *ffi_channel, *ffi_ptr;
uint8_t *heap_start, *heap_end;
unsigned int ref_ptr = 0;
void (**funs)(void);

void __memcpy() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p;
    long long int n = ffi_ptr[0].i;
	ffi_ptr -= 3;
    memcpy(dst, src, n * sizeof(union cell));
}

void __memrev() {
    union cell *dst = ffi_ptr[-1].p, tmp;
    long long int n = ffi_ptr[0].i;
    ffi_ptr -= 2;
    size_t i, j;
    for (i = 0, j = n - 1; i < j; i++, j--) {
        tmp = dst[i];
        dst[i] = dst[j];
        dst[j] = tmp;
    }
}

void __memset() {
    union cell *dst = ffi_ptr[-2].p, v = ffi_ptr[-1];
    long long int n = ffi_ptr[0].i;
    for (int i = 0; i < n; i++) {
        dst[i] = v;
    }
	ffi_ptr -= 3;
    // memset(dst, v, n * sizeof(union cell));
}

void __strncpy() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p;
    long long int n = ffi_ptr[0].i;
	ffi_ptr -= 3;
    for (int i = 0; i < n; i++) {
        if (src[i].i == 0) {
            dst[i].i = 0;
            return;
        }
        dst[i] = src[i];
    }
	dst[n].i = 0;
}

void __strncat() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p;
    long long int n = ffi_ptr[0].i;
	ffi_ptr -= 3;
	for (; dst->i; dst++);
	for (int i = 0; i < n; i++) {
		if (src[i].i == 0) {
			dst[i].i = 0;
			return;
		}
		dst[i] = src[i];
	}
	dst[n].i = 0;
}

void __strcmp() {
    union cell *dst = ffi_ptr[-2].p, *src = ffi_ptr[-1].p;
    long long int n = ffi_ptr[0].i;
    for (int i = 0; i < n; i++) {
        if (dst[i].i != src[i].i) {
            ffi_ptr[-2].i = dst[i].i - src[i].i;
			ffi_ptr -= 2;
            return;
        }
    }
    ffi_ptr[-2].i = 0;
	ffi_ptr -= 2;
}

void __strlen() {
    union cell *src = ffi_ptr[0].p;
	for (ffi_ptr[0].i = 0; src[ffi_ptr[0].i].i; ffi_ptr[0].i++);
}

void __puts() {
    union cell *src = ffi_ptr[0].p;
	ffi_ptr--;
    while (src->i) {
        putchar(src->i);
        src++;
    }
}

void __putint() {
    int64_t num = ffi_ptr[-1].i;
    int64_t base = ffi_ptr[0].i;
    if (base == 10) {
        printf("%ld", num);
    }
    else if (base == 16) {
        printf("%lx", num);
    }
    else if (base == 8) {
        printf("%lo", num);
    }
    else if (base == 2) {
        printf("%lx", num);
    }
    else {
        printf("%ld", num);
    }
}

void __putaddr() {
    union cell *src = ffi_ptr[0].p;
    printf("%p", src);
}

void salloc_init(uint8_t *start, uint8_t *end) {
    heap_start = start;
    heap_end = end;
    // printf("salloc_init: %p - %p\n", heap_start, heap_end);
}

void *salloc(uint32_t bytes) {
	// printf("salloc: %d\n", bytes);
	uint64_t words = (bytes + 128) / 8;
    uint64_t *ret = (uint64_t*)heap_start;
    while (*ret != 0) {
		// printf("salloc: %p taken with reserved words=%ld, bytes=%ld\n", ret, *ret, *ret * 8);
        ret += *ret;
        if ((uintptr_t)ret >= (uintptr_t)heap_end) {
            // printf("salloc: out of memory\n");
            // printf("salloc: tried to allocate %d bytes\n", bytes);
            return NULL;
        }
    }
    *ret = words;
    ret++;
	// printf("salloc: %p (remaining=%d)\n", ret, (uint32_t)(heap_end - ((uint8_t*)ret + bytes)));
    return ret;
}

void sfree(void *ptr) {
	// printf("sfree: %p\n", ptr);
	if ((uintptr_t)ptr < (uintptr_t)heap_start || (uintptr_t)ptr >= (uintptr_t)heap_end) {
		// printf("sfree: invalid pointer\n");
		return;
	}

    uint64_t *p = ptr;
    uint64_t words = *(p - 1);
    memset(p - 1, 0, words / 8);

    return;
}

void __malloc() {
    long long int size = ffi_ptr[0].i;
    ffi_ptr[0].p = (union cell*)salloc(size);
}

void __free() {
    union cell *ptr = ffi_ptr[0].p;
	ffi_ptr--;
    sfree(ptr);
}

char *line;
int at, len, max;

void sgetchar_init() {
    line = (char*)salloc(1024);
    at = 0;
    len = 0;
    max = 1024;
}

void readline() {
    uint32_t c = '\0';
    int at = 0;
    do {
        c = getchar();
        if (c == '\r' || c == '\n') {
            line[at++] = '\n';
            line[at] = '\0';
            putchar('\r');
            putchar('\n');
            len = at;
            return;
        }
        else if (c == '\b' || c == 127) {
            // Backspace or "delete"
            if (at > 0) {
                // Erase character 
                printf("\b \b");
                at--;
            }
        }
        else if (c == 0x1B) {
            // Escape sequence
            char esc1 = getchar();
            char esc2 = getchar();
            if (esc1 == 0x5B) {
                switch (esc2) {
                    case 0x41:
                        printf("UP\n");
                    break;
                    case 0x42:
                        printf("DOWN\n");
                    break;
                    case 0x43:
                        printf("RIGHT\n");
                    break;
                    case 0x44:
                        printf("LEFT\n");
                    break;
                }
            }
        }
        else if (c == 4) {
            // EOF
            printf("\nEOF\n");
            break;
        }
        else if (c != 255) {
            if (at < 255) {
                line[at++] = c;
                // Echo it back out to the user
                putchar(c);
            }
        }
        // else {
        // 	// sleep(10);
        // }
    } while (1);
}

int cursor = 0;

char sgetchar() {
    if (len == 0 || cursor >= len || line[at] == '\0') {
        readline();
        cursor = 0;
    }

    if (cursor >= len) {
        return '\n';
    }

    if (line[cursor]) {
        return line[cursor++];
    }

    return '\0';
}

int isalpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int isdigit(char c) {
    return c >= '0' && c <= '9';
}

int isalnum(char c) {
    return isalpha(c) || isdigit(c);
}

int isws(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

int64_t sgetint(uint64_t radix) {
    int64_t num = 0;
    int64_t sign = 1;
    int64_t c = sgetchar();
    while (isws(c)) {
        c = sgetchar();
    }
    if (c == '-') {
        sign = -1;
        c = sgetchar();
    }
    else if (c == '+') {
        c = sgetchar();
    }
    while (isalnum(c)) {
        int64_t digit = 0;
        if (isdigit(c)) {
            digit = c - '0';
        }
        else if (isalpha(c)) {
            digit = c - 'a' + 10;
        }

        num = num * radix + digit;
        c = sgetchar();
    }
    return num * sign;
}

void __getchar() {
    ffi_ptr++;
    ffi_ptr[0].i = (int64_t)sgetchar();
}

void __getint() {
    uint64_t radix = ffi_ptr[0].i;
    ffi_ptr[0].i = sgetint(radix);
}

void __putchar() {
    putchar(ffi_ptr[0].i);
    ffi_ptr--;
}
"#.to_string();

        // if !is_core {
        //     result = "#include <stdlib.h>\n".to_string() + &result;
        // }

        Some(result)
    }

    fn post_funs(&self, funs: Vec<i32>) -> Option<String> {
        let mut result = String::from(r#"int main () {
    uint8_t buf[0x80000] = {0};
    salloc_init(buf, buf + sizeof(buf));

    funs = (void(**)(void))salloc(200 * sizeof(void*));
    ffi_channel = (cell*)salloc(256 * sizeof(cell));
    tape = (cell*)salloc(30000 * sizeof(cell));
    refs = (cell*)salloc(128 * sizeof(cell));

    sgetchar_init();

    ptr = tape;
    ref = (cell**)refs;
    reg.i = 0;
    ffi_ptr = ffi_channel;
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
        Some("return 0;\n}".to_string())
    }
}

impl CompiledTarget for MyOS {}
