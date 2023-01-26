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
//! | Memory       | `Move(n: int)`   | `Index` | `Where?`   | `Deref`    | `Refer`   | `BitwiseNand` |
//! | Control Flow | `While`          | `If`    | `Else`     | `Function` | `Call`    | `Return`      |
//! | Arithmetic   | `IsNonNegative?` | `Add`   | `Subtract` | `Multiply` | `Divide`  | `Remainder`   |
//! | Fundamental  | `Set(n: int)`    | `Save`  | `Restore`  | `Get`      | `Put`     | `End`         |
//!
//! The standard instructions are not guaranteed to be wholly implemented by every target, or at all. A target for Linux on x86 will certainly implement all the standard instructions, but a thermostat might implement only a few or none.
//!
//! |                        | The              | Twenty    | and             | Four       | Standard    | Instructions |
//! |------------------------|------------------|-----------|-----------------|------------|-------------|--------------|
//! | Memory and Fundamental | `Allocate`       | `Free`    | `Set(n: float)` | `ToInt`    | `ToFloat`   | `Power`      |
//! | Trigonometry           | `Sine`           | `Cosine`  | `Tangent`       | `ArcSine`  | `ArcCosine` | `ArcTangent` |
//! | Arithmetic             | `IsNonNegative?` | `Add`     | `Subtract`      | `Multiply` | `Divide`    | `Remainder`  |
//! | Worldly                | `GetChar`        | `PutChar` | `GetInt`        | `PutInt`   | `GetFloat`  | `PutFloat`   |

pub mod asm;
pub mod lir;
pub mod parse;
pub mod targets;
pub mod vm;


/// The different axes an input or output might use.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Axis { X, Y, Z, }

/// The different types of input modes a program might use.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum InputMode {
   /// Standard input (ASCII character)
   Stdin,
   /// Standard input (integer)
   StdinInt,
   /// Standard input (float)
   StdinFloat,
   /// Input from a barometer (pressure in atmospheres)
   Barometer,
   /// Input from a D-Pad (up=0, down=1, left=2, right=3)
   DPad,
   /// Input from a button (0=not pressed, 1=pressed)
   Button,
   /// Input from keyboard (ASCII character)
   Keyboard,
   /// Input from a JoyStick the degree of displacement in a given axis (from -128 to 128).
   JoyStick(Axis),
   /// Time (in seconds) since the program started
   Time,
   /// Input from a thermometer (degrees K)
   Thermometer,
   /// Input from an accelerometer (in meters per second per second) in a given axis
   Accelerometer(Axis),
   /// Input from a speedometer (in meters per second) in a given axis
   Speedometer(Option<Axis>),
   /// Input from an odometer (in meters)
   Odometer,
   /// Input from a gyroscope (in radians per second) around a given axis
   Gyroscope(Axis),
   /// Input from a magnetometer (in teslas) in a given axis
   Magnetometer(Axis),
   /// Input from a microphone (frequency in hertz)
   Microphone,
   /// Input from a position sensor in a given axis (x, y, z)
   Position(Axis),
   /// Input from a compass (degrees)
   Compass,
   /// Input from a distance sensor (in meters)
   Distance,
   /// Input from a light sensor (in lux)
   Light,
   /// Input from a humidity sensor (in percent)
   Humidity,
   /// Input from an analog input (in volts)
   AnalogPin,
   /// Input from a digital input (0=low, 1=high)
   DigitalPin,
   /// Input from a rain gauge (in millimeters)
   RainGauge,
   /// Input from a wind speed sensor (in meters per second)
   WindSpeed,
   /// Input from altitude sensor (in meters)
   Altitude,
   /// Input from depth sensor (in meters)
   Depth,
   /// A custom input mode (for use with a custom output device)
   Custom(String),
}

/// The different types of output modes a program might use.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum OutputMode {
   /// Standard output
   Stdout,
   /// Standard output (integer)
   StdoutInt,
   /// Standard output (float)
   StdoutFloat,
   /// Standard error
   Stderr,
   /// Standard error (integer)
   StderrInt,
   /// Standard error (float)
   StderrFloat,
   /// Set the temperature of a given heating/cooling device (degrees K)
   Temperature,
   /// Set the pressure of a given vacuum/pressurizer (atmospheres)
   Pressure,
   /// Set the speed of a given motor (in revolutions per minute)
   MotorSpeed,
   /// Set the position of a given servo (in radians)
   ServoPosition,
   /// Set the brightness of a given light (in percent)
   LightBrightness,
   /// Set the volume of a given speaker (in percent)
   SpeakerVolume,
   /// Set the frequency of a given speaker (in hertz)
   SpeakerFrequency,
   /// Set the voltage of a given analog output
   AnalogPin,
   /// Set the state of a given digital output (0=low, 1=high)
   DigitalPin,
   /// Set the position of a given stepper motor (in radians)
   StepperPosition,
   /// A custom output mode (for use with a custom output device)
   Custom(String),
}

/// The channel to use for a given I/O mode.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Channel(pub usize);

/// An input source for a program.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Input {
   /// The mode of the input.
   pub mode: InputMode,
   /// The channel to use for the input.
   pub channel: Channel
}

impl Input {
   /// Create a new input source.
   pub const fn new(mode: InputMode, channel: usize) -> Self {
      Self { mode, channel: Channel(channel) }
   }

   /// Input from STDIN (ASCII character)
   pub const fn stdin() -> Self {
      Self::new(InputMode::Stdin, 0)
   }
   /// Input from STDIN (integer)
   pub const fn stdin_int() -> Self {
      Self::new(InputMode::StdinInt, 0)
   }
   /// Input from STDIN (float)
   pub const fn stdin_float() -> Self {
      Self::new(InputMode::StdinFloat, 0)
   }
}

/// An output destination for a program.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Output {
   /// The mode of the output.
   pub mode: OutputMode,
   /// The channel to use for the output.
   pub channel: Channel
}

impl Output {
   /// Create a new output destination.
   pub const fn new(mode: OutputMode, channel: usize) -> Self {
      Self { mode, channel: Channel(channel) }
   }

   /// Output to STDOUT (ASCII character)
   pub const fn stdout() -> Self {
      Self::new(OutputMode::Stdout, 0)
   }

   /// Output to STDOUT (integer)
   pub const fn stdout_int() -> Self {
      Self::new(OutputMode::StdoutInt, 0)
   }

   /// Output to STDOUT (float)
   pub const fn stdout_float() -> Self {
      Self::new(OutputMode::StdoutFloat, 0)
   }

   /// Output to STDERR (ASCII character)
   pub const fn stderr() -> Self {
      Self::new(OutputMode::Stderr, 0)
   }

   /// Output to STDERR (integer)
   pub const fn stderr_int() -> Self {
      Self::new(OutputMode::StderrInt, 0)
   }

   /// Output to STDERR (float)
   pub const fn stderr_float() -> Self {
      Self::new(OutputMode::StderrFloat, 0)
   }
}

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
