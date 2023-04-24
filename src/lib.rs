//! # The Sage Programming Language
//!
//! 🚧 🏗️ ⚠️ This language is under construction! ⚠️ 🏗️ 🚧
//!
//! ```text
//!   █████   ██████    ███████  ██████   `-.        _.-'
//!  ███░░   ░░░░░███  ███░░███ ███░░███   \ `,    .'/.'
//! ░░█████   ███████ ░███ ░███░███████     \`.`. :.-'.-= .-'/
//!  ░░░░███ ███░░███ ░███ ░███░███░░░       `-.:/o  .'-'/ .'
//!  ██████ ░░████████░░███████░░██████         o\o / ._/.'
//! ░░░░░░   ░░░░░░░░  ░░░░░███ ░░░░░░            \| /o|\`.
//!                    ███ ░███                    |'o `.`.'.
//!                   ░░██████                           `--'
//!                    ░░░░░░            
//! ```
//!
//! <embed type="text/html" src="web/index.html" title="Compiler" width="100%" height="940em"></embed>
//! ***(The sage compiler itself can be compiled to web assembly to be executed on the web. This allows a sage compiler + interpreter to be hosted on a static web page and run embded sage scripts. This web implementation compiles sage LIR code into sage virtual machine code, and then feeds it to a builtin virtual machine interpreter. The compiler can also switch to various backends, such as the C source code generator, or assembly output.)***
//!
//! This crate implements a compiler for the sage programming language
//! and its low level virtual machine.
//!
//! ## Index
//!
//! 1. [The Lower Intermediate Representation](./lir/index.html)
//! 2. [The Assembly Language](./asm/index.html)
//! 3. [The Virtual Machine](./vm/index.html)
//! 4. [Target Backends](./targets/index.html)
//!
//! ## Stages of IR
//!
//! Here are the current stages of IR:
//!
//! 1. LIR (Lower Intermediate Representation)
//! 2. Core / Standard Assembly (an assembly language for the VM)
//! 3. Core / Standard Virtual Machine Code (which is built for a given target)
//!
//! ## Backend
//!
//! This compiler is unique in the way that it handles portability.
//! The language's backend is split in two: the Core variant, and the
//! Standard variant. The Core variant is guaranteed to compile to all
//! targets, and the Standard variant is akin to a standard library of
//! instructions (which are implemented for most targets).
//!
//! This makes sage uniquely equipped to compile to very limited arcitectures:
//! **the Core instruction set for the virtual machine is *almost guaranteed*
//! to be a subset of most CPU architectures.**
//!
//! The Core instructions are required to be implemented by *every target*. These instructions are guaranteed to be supported by every target.
//!
//! |              | The              | Twenty  | and        | Four       | Canonical | Instructions  |
//! |--------------|------------------|---------|------------|------------|-----------|---------------|
//! | Memory       | [`Move(n: int)`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#move)   | [`Index`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#index) | [`Where?`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#where)   | [`Deref`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#deref-and-refer)       | [`Refer`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#deref-and-refer)   | [`BitwiseNand`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#bitwisenand) |
//! | Control Flow | [`While`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#while-if-and-else)         | [`If`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#while-if-and-else)       | [`Else`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#while-if-and-else)      | [`Function`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#function-call-and-return) | [`Call`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#function-call-and-return)     | [`Return`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#function-call-and-return)           |
//! | Arithmetic   | [`IsNonNegative?`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#isnonnegative) | [`Add`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#add-and-subtract)   | [`Subtract`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#add-and-subtract) | [`Multiply`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#multiply-and-divide) | [`Divide`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#multiply-and-divide)  | [`Remainder`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#remainder)   |
//! | Fundamental  | [`Set(n: int)`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#set)   | [`Save`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#save-and-restore)  | [`Restore`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#save-and-restore)  | [`Get`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#get-and-put)      | [`Put`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#put-and-put)     | [`End`](https://github.com/adam-mcdaniel/sage/blob/main/CORE.md#end)         |
//!
//! The standard instructions are not guaranteed) to be wholly implemented by every target, or at all. A target for Linux on x86 will certainly implement all the standard instructions, but a thermostat might implement only a few or none.
//!
//! |                        | The              | Twenty    | and             | Four       | Standard    | Instructions |
//! |------------------------|------------------|-----------|-----------------|------------|-------------|--------------|
//! | Memory and Fundamental | `Allocate`       | `Free`    | `Set(n: float)` | `ToInt`    | `ToFloat`   | `Power`      |
//! | Trigonometry           | `Sine`           | `Cosine`  | `Tangent`       | `ArcSine`  | `ArcCosine` | `ArcTangent` |
//! | Arithmetic             | `IsNonNegative?` | `Add`     | `Subtract`      | `Multiply` | `Divide`    | `Remainder`  |
//! | Worldly                | `GetChar`        | `PutChar` | `GetInt`        | `PutInt`   | `GetFloat`  | `PutFloat`   |
pub mod asm;
pub mod io;
pub mod lir;
pub mod parse;
pub mod targets;
pub mod vm;

/// The value of the NULL pointer constant.
///
/// I've chosen to use the smallest value that can be expressed by an 8-bit signed integer.
/// This is because I want to make sure that this works with 8-bit machines as well.
/// The value of this constant might change in the future though.
pub const NULL: isize = i8::MIN as isize;

/// The UNICODE character art for the logo of the language.
pub const LOGO: &str = r#"
   █████   ██████    ███████  ██████   `-.        _.-'
  ███░░   ░░░░░███  ███░░███ ███░░███   \ `,    .'/.'
 ░░█████   ███████ ░███ ░███░███████     \`.`. :.-'.-= .-'/
  ░░░░███ ███░░███ ░███ ░███░███░░░       `-.:/o  .'-'/ .'
  ██████ ░░████████░░███████░░██████         o\o / ._/.'
 ░░░░░░   ░░░░░░░░  ░░░░░███ ░░░░░░            \| /o|\`.
                    ███ ░███                    |'o `.`.'.
                   ░░██████                           `--'
                    ░░░░░░            "#;

/// The UNICODE character art for the logo of the language, using ANSI escape codes for color.
pub const LOGO_WITH_COLOR: &str = "\x1b[32m
   █████   ██████    ███████  ██████   `-.        _.-'
  ███░░   ░░░░░███  ███░░███ ███░░███   \\ `,    .'/.'
 ░░█████   ███████ ░███ ░███░███████     \\`.`. :.-'.-= .-'/
  ░░░░███ ███░░███ ░███ ░███░███░░░       `-.:/o  .'-'/ .'
  ██████ ░░████████░░███████░░██████         o\\o / ._/.'
 ░░░░░░   ░░░░░░░░  ░░░░░███ ░░░░░░            \\| /o|\\`.
                    ███ ░███                    |'o `.`.'.
                   ░░██████                           `--'
                    ░░░░░░            \x1b[0m";
