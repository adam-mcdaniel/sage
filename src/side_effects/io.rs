//! # I/O Module
//!
//! This module implements all the types related to the I/O operations available
//! to the core instruction set. This allows the core VM to communicate with various
//! instruments connected to the VM, without directly calling foreign functions.
//! This is a standardized interface for various useful types of I/O, that each
//! platform can implement in their own way, if they so choose.

use core::fmt::{Display, Formatter, Result as FmtResult};

use serde_derive::{Deserialize, Serialize};

/// The different axes an input or output might use.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum Axis {
    X,
    Y,
    Z,
}

/// The different directions a D-Pad a might use.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

/// The different output colors a program might use.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum Color {
    /// Black
    Black,
    /// White
    White,
    /// Red
    Red,
    /// Green
    Green,
    /// Blue
    Blue,
    /// Yellow
    Yellow,
    /// Cyan
    Cyan,
    /// Magenta
    Magenta,
    /// Orange
    Orange,
    /// RGB    
    RGB(u8, u8, u8),
}

/// The different types of input modes a program might use.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum InputMode {
    ///////////////////////////////////////////////////////////////
    /// Standard input modes
    /// (The standard interface is typically used for command line programs,
    /// and for all pipelines between programs)
    ///////////////////////////////////////////////////////////////
    /// Standard input (ASCII character)
    StdinChar,
    /// Standard input (integer)
    StdinInt,
    /// Standard input (float)
    StdinFloat,

    ///////////////////////////////////////////////////////////////
    /// Special input modes
    ///////////////////////////////////////////////////////////////
    /// A random number
    Random,

    ///////////////////////////////////////////////////////////////
    /// User input modes
    /// (These should typically be used for games or other interactive programs)
    ///////////////////////////////////////////////////////////////
    /// Input from a D-Pad
    DPad(Direction),
    /// Input from a button (0=not pressed, 1=pressed)
    Button,
    /// Input from keyboard (ASCII character)
    Keyboard,
    /// Input from a JoyStick the degree of displacement in a given direction (from -128 to 128).
    JoyStick(Direction),

    ///////////////////////////////////////////////////////////////
    /// Physical sensor input modes
    /// (These should typically be implemented for devices that measure
    /// the physical world, like a smart watch or a smart phone)
    ///////////////////////////////////////////////////////////////
    /// Time (in seconds) since the program started
    Clock,
    /// Input from an accelerometer (in meters per second per second) in a given axis
    Accelerometer(Axis),
    /// Input from a gyroscope (in degrees per second) around a given axis
    Gyroscope(Axis),
    /// Input from a microphone (frequency in hertz)
    Microphone,

    ///////////////////////////////////////////////////////////////
    /// Environment sensor input modes
    /// (These should typically be implemented for devices that measure
    /// the environment, like a weather station or a smart thermostat)
    ///////////////////////////////////////////////////////////////
    /// Red light intensity (in lux)
    RedLight,
    /// Green light intensity (in lux)
    GreenLight,
    /// Blue light intensity (in lux)
    BlueLight,
    /// Input from a light sensor (in lux)
    Brightness,
    /// Input from a humidity sensor (in percent)
    Humidity,
    /// Input from a barometer (pressure in atmospheres)
    Barometer,
    /// Input from a magnetometer (in teslas) in a given axis
    Magnetometer(Axis),
    /// Input from a thermometer (degrees K)
    Thermometer,
    /// Input from a rain gauge (in millimeters)
    RainGauge,
    /// Input from a UV sensor (in watts per square meter)
    UVSensor,
    /// Input from a wind speed sensor (in meters per second)
    WindSpeed,
    /// Input from a wind direction sensor (in degrees)
    WindDirection,

    ///////////////////////////////////////////////////////////////
    /// Engineering / Science sensor input modes
    ///////////////////////////////////////////////////////////////
    /// Input from a pressure gauge (in atmospheres)
    PressureGauge,
    /// Input from a flow sensor (in liters per second)
    FlowSensor,
    /// Input from a volume sensor (in liters)
    VolumeSensor,
    /// Input from a weight sensor (in kilograms)
    WeightSensor,
    /// Input from a pH sensor (in pH)
    PHSensor,
    /// Input from a conductivity sensor (in siemens per meter)
    ConductivitySensor,

    ///////////////////////////////////////////////////////////////
    /// Navigation input modes
    /// (These should typically be implemented for devices that use GPS,
    /// or for robots)
    ///////////////////////////////////////////////////////////////
    /// Input from a speedometer (in meters per second) in a given axis
    Speedometer(Option<Axis>),
    /// Input from an odometer (in meters)
    Odometer,
    /// Input from a position sensor in a given axis (x, y, z)
    Position(Axis),
    /// Input from a compass (degrees)
    Compass,
    /// Input from a distance sensor (in meters)
    Proximity,
    /// Input from altitude sensor (in meters)
    Altimeter,
    /// Input from depth sensor (in meters)
    DepthSensor,

    ///////////////////////////////////////////////////////////////
    /// Electrical device input modes
    /// (These should typically be used for programs using a GPIO interface,
    /// like a Raspberry Pi or Arduino)
    ///////////////////////////////////////////////////////////////
    /// Input from an analog input (in volts)
    AnalogPin,
    /// Input from a digital input (0=low, 1=high)
    DigitalPin,

    ///////////////////////////////////////////////////////////////
    /// Custom output modes
    ///////////////////////////////////////////////////////////////
    /// A custom input mode (for use with a custom output device)
    Custom(String),
}

/// The different types of output modes a program might use.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum OutputMode {
    ///////////////////////////////////////////////////////////////
    /// Standard output modes
    ///////////////////////////////////////////////////////////////
    /// Standard output (ASCII character)
    StdoutChar,
    /// Standard output (integer)
    StdoutInt,
    /// Standard output (float)
    StdoutFloat,
    /// Standard error (ASCII character)
    StderrChar,
    /// Standard error (integer)
    StderrInt,
    /// Standard error (float)
    StderrFloat,

    ///////////////////////////////////////////////////////////////
    /// Alternative output modes for standard output
    ///////////////////////////////////////////////////////////////
    /// Printer (ASCII character)
    PrinterChar,
    /// Printer (integer)
    PrinterInt,
    /// Printer (float)
    PrinterFloat,

    ///////////////////////////////////////////////////////////////
    /// Lighting device output modes
    ///////////////////////////////////////////////////////////////
    /// Set the brightness of a given light (in percent)
    Brightness,

    ///////////////////////////////////////////////////////////////
    /// Electrical device output modes
    ///////////////////////////////////////////////////////////////
    /// Set the voltage of a given analog output
    AnalogPin,
    /// Set the state of a given digital output (0=low, 1=high)
    DigitalPin,

    ///////////////////////////////////////////////////////////////
    /// Robotics device output modes
    ///////////////////////////////////////////////////////////////
    /// Set the position of a given stepper motor (in radians)
    StepperMotor,
    /// Set the polarity of a solenoid (0=off, 1=on)
    Solenoid,
    /// Set the position of a given valve (0=closed, 1=open)
    Valve,
    /// Set the speed of a given motor (in revolutions per minute)
    MotorSpeed,
    /// Set the position of a given servo (in radians)
    Servo,
    /// Set the temperature of a given heating/cooling device (degrees K)
    Temperature,
    /// Set the pressure of a given pump (in atmospheres)
    Pump,
    /// Set the pressure of a given fan (in atmospheres)
    Fan,
    /// Set the pressure of a given blower (in atmospheres)
    Blower,
    /// Turn a heater on or off (0=off, 1=on)
    Heater,
    /// Turn a cooler on or off (0=off, 1=on)
    Cooler,
    /// Set the pressure of a given vacuum/pressurizer (atmospheres)
    Pressure,

    ///////////////////////////////////////////////////////////////
    /// Sound output modes
    ///////////////////////////////////////////////////////////////
    /// Ring a given buzzer (in hertz)
    Buzzer,
    /// Ring a bell (in hertz)
    Bell,
    /// Play a given note (in hertz)
    Note,
    /// Set the volume of a given speaker (in percent)
    SpeakerVolume,
    /// Set the frequency of a given speaker (in hertz)
    SpeakerFrequency,

    ///////////////////////////////////////////////////////////////
    /// Display output modes
    ///////////////////////////////////////////////////////////////
    /// Update the display
    UpdateDisplay,
    /// Clear the display
    ClearDisplay,
    /// Set the cursor row on the display
    SetCursorRow,
    /// Set the cursor column on the display
    SetCursorColumn,
    /// Move the cursor up on the display
    MoveCursorUp,
    /// Move the cursor down on the display
    MoveCursorDown,
    /// Move the cursor left on the display
    MoveCursorLeft,
    /// Move the cursor right on the display
    MoveCursorRight,
    /// Write a character to the display
    SetCursorChar(Color),
    /// Set the color of a given pixel on the display
    SetCursorPixel(Color),

    ///////////////////////////////////////////////////////////////
    /// Custom output modes
    ///////////////////////////////////////////////////////////////
    /// A custom output mode (for use with a custom output device)
    Custom(String),
}

/// The channel to use for a given I/O mode.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Channel(pub usize);

/// An input source for a program.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Input {
    /// The mode of the input.
    pub mode: InputMode,
    /// The channel to use for the input.
    pub channel: Channel,
}

impl Input {
    /// Create a new input source.
    pub const fn new(mode: InputMode, channel: usize) -> Self {
        Self {
            mode,
            channel: Channel(channel),
        }
    }

    /// Input from STDIN (ASCII character)
    pub const fn stdin_char() -> Self {
        Self::new(InputMode::StdinChar, 0)
    }
    /// Input from STDIN (integer)
    pub const fn stdin_int() -> Self {
        Self::new(InputMode::StdinInt, 0)
    }
    /// Input from STDIN (float)
    pub const fn stdin_float() -> Self {
        Self::new(InputMode::StdinFloat, 0)
    }

    /// A random number
    pub const fn random() -> Self {
        Self::new(InputMode::Random, 0)
    }

    /// The time (in seconds) since the program started
    pub const fn clock() -> Self {
        Self::new(InputMode::Clock, 0)
    }
}

/// An output destination for a program.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Output {
    /// The mode of the output.
    pub mode: OutputMode,
    /// The channel to use for the output.
    pub channel: Channel,
}

impl Output {
    /// Create a new output destination.
    pub const fn new(mode: OutputMode, channel: usize) -> Self {
        Self {
            mode,
            channel: Channel(channel),
        }
    }

    /// Output to STDOUT (ASCII character)
    pub const fn stdout_char() -> Self {
        Self::new(OutputMode::StdoutChar, 0)
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
    pub const fn stderr_char() -> Self {
        Self::new(OutputMode::StderrChar, 0)
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

impl Display for Input {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{} #{}", self.mode, self.channel.0)
    }
}

impl Display for Output {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{} #{}", self.mode, self.channel.0)
    }
}

impl Display for InputMode {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ///////////////////////////////////////////////////////////////
            // Standard input modes
            // (The standard interface is typically used for command line programs,
            // and for all pipelines between programs)
            ///////////////////////////////////////////////////////////////
            // Standard input (ASCII character)
            InputMode::StdinChar => write!(f, "stdin.char"),
            // Standard input (integer)
            InputMode::StdinInt => write!(f, "stdin.int"),
            // Standard input (float)
            InputMode::StdinFloat => write!(f, "stdin.float"),

            ///////////////////////////////////////////////////////////////
            // Special input modes
            ///////////////////////////////////////////////////////////////
            // A random number
            InputMode::Random => write!(f, "random"),

            ///////////////////////////////////////////////////////////////
            // User input modes
            // (These should typically be used for games or other interactive programs)
            ///////////////////////////////////////////////////////////////
            // Input from a D-Pad
            InputMode::DPad(dir) => write!(f, "dpad.{dir}"),
            // Input from a button (0=not pressed, 1=pressed)
            InputMode::Button => write!(f, "button"),
            // Input from keyboard (ASCII character)
            InputMode::Keyboard => write!(f, "keyboard"),
            // Input from a JoyStick the degree of displacement in a given direction (from -128 to 128).
            InputMode::JoyStick(dir) => write!(f, "joystick.{dir}"),

            ///////////////////////////////////////////////////////////////
            // Physical sensor input modes
            // (These should typically be implemented for devices that measure
            // the physical world, like a smart watch or a smart phone)
            ///////////////////////////////////////////////////////////////
            // Time (in seconds) since the program started
            InputMode::Clock => write!(f, "clock"),
            // Input from an accelerometer (in meters per second per second) in a given axis
            InputMode::Accelerometer(axis) => write!(f, "accelerometer.{axis}"),
            // Input from a gyroscope (in degrees per second) around a given axis
            InputMode::Gyroscope(axis) => write!(f, "gyroscope.{axis}"),
            // Input from a microphone (frequency in hertz)
            InputMode::Microphone => write!(f, "microphone"),

            ///////////////////////////////////////////////////////////////
            // Environment sensor input modes
            // (These should typically be implemented for devices that measure
            // the environment, like a weather station or a smart thermostat)
            ///////////////////////////////////////////////////////////////
            // Red light intensity (in lux)
            InputMode::RedLight => write!(f, "redlight"),
            // Green light intensity (in lux)
            InputMode::GreenLight => write!(f, "greenlight"),
            // Blue light intensity (in lux)
            InputMode::BlueLight => write!(f, "bluelight"),
            // Input from a light sensor (in lux)
            InputMode::Brightness => write!(f, "brightness"),
            // Input from a humidity sensor (in percent)
            InputMode::Humidity => write!(f, "humidity"),
            // Input from a barometer (pressure in atmospheres)
            InputMode::Barometer => write!(f, "barometer"),
            // Input from a magnetometer (in teslas) in a given axis
            InputMode::Magnetometer(axis) => write!(f, "magnetometer.{axis}"),
            // Input from a thermometer (degrees K)
            InputMode::Thermometer => write!(f, "thermometer"),
            // Input from a rain gauge (in millimeters)
            InputMode::RainGauge => write!(f, "raingauge"),
            // Input from a UV sensor (in watts per square meter)
            InputMode::UVSensor => write!(f, "uvsensor"),
            // Input from a wind speed sensor (in meters per second)
            InputMode::WindSpeed => write!(f, "windspeed"),
            // Input from a wind direction sensor (in degrees)
            InputMode::WindDirection => write!(f, "winddirection"),

            ///////////////////////////////////////////////////////////////
            // Engineering / Science sensor input modes
            ///////////////////////////////////////////////////////////////
            // Input from a pressure gauge (in atmospheres)
            InputMode::PressureGauge => write!(f, "pressuregauge"),
            // Input from a flow sensor (in liters per second)
            InputMode::FlowSensor => write!(f, "flowsensor"),
            // Input from a volume sensor (in liters)
            InputMode::VolumeSensor => write!(f, "volumesensor"),
            // Input from a weight sensor (in kilograms)
            InputMode::WeightSensor => write!(f, "weightsensor"),
            // Input from a pH sensor (in pH)
            InputMode::PHSensor => write!(f, "phsensor"),
            // Input from a conductivity sensor (in siemens per meter)
            InputMode::ConductivitySensor => write!(f, "conductivitysensor"),

            ///////////////////////////////////////////////////////////////
            // Navigation input modes
            // (These should typically be implemented for devices that use GPS,
            // or for robots)
            ///////////////////////////////////////////////////////////////
            // Input from a speedometer (in meters per second) in a given axis
            InputMode::Speedometer(Some(axis)) => write!(f, "speedometer.{axis}"),
            InputMode::Speedometer(None) => write!(f, "speedometer"),
            // Input from an odometer (in meters)
            InputMode::Odometer => write!(f, "odometer"),
            // Input from a position sensor in a given axis (x, y, z)
            InputMode::Position(axis) => write!(f, "position.{axis}"),
            // Input from a compass (degrees)
            InputMode::Compass => write!(f, "compass"),
            // Input from a distance sensor (in meters)
            InputMode::Proximity => write!(f, "proximity"),
            // Input from altitude sensor (in meters)
            InputMode::Altimeter => write!(f, "altimeter"),
            // Input from depth sensor (in meters)
            InputMode::DepthSensor => write!(f, "depthsensor"),

            ///////////////////////////////////////////////////////////////
            // Electrical device input modes
            // (These should typically be used for programs using a GPIO interface,
            // like a Raspberry Pi or Arduino)
            ///////////////////////////////////////////////////////////////
            // Input from an analog input (in volts)
            InputMode::AnalogPin => write!(f, "analogpin"),
            // Input from a digital input (0=low, 1=high)
            InputMode::DigitalPin => write!(f, "digitalpin"),

            ///////////////////////////////////////////////////////////////
            // Custom output modes
            ///////////////////////////////////////////////////////////////
            // A custom input mode (for use with a custom output device)
            InputMode::Custom(name) => write!(f, "{name}"),
        }
    }
}

impl Display for OutputMode {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ///////////////////////////////////////////////////////////////
            // Standard output modes
            ///////////////////////////////////////////////////////////////
            // Standard output (ASCII character)
            OutputMode::StdoutChar => write!(f, "stdout.char"),
            // Standard output (integer)
            OutputMode::StdoutInt => write!(f, "stdout.int"),
            // Standard output (float)
            OutputMode::StdoutFloat => write!(f, "stdout.float"),
            // Standard error (ASCII character)
            OutputMode::StderrChar => write!(f, "stderr.char"),
            // Standard error (integer)
            OutputMode::StderrInt => write!(f, "stderr.int"),
            // Standard error (float)
            OutputMode::StderrFloat => write!(f, "stderr.float"),

            ///////////////////////////////////////////////////////////////
            // Alternative output modes for standard output
            ///////////////////////////////////////////////////////////////
            // Printer (ASCII character)
            OutputMode::PrinterChar => write!(f, "printer.char"),
            // Printer (integer)
            OutputMode::PrinterInt => write!(f, "printer.int"),
            // Printer (float)
            OutputMode::PrinterFloat => write!(f, "printer.float"),

            ///////////////////////////////////////////////////////////////
            // Lighting device output modes
            ///////////////////////////////////////////////////////////////
            // Set the brightness of a given light (in percent)
            OutputMode::Brightness => write!(f, "brightness"),

            ///////////////////////////////////////////////////////////////
            // Electrical device output modes
            ///////////////////////////////////////////////////////////////
            // Set the voltage of a given analog output
            OutputMode::AnalogPin => write!(f, "analogpin"),
            // Set the state of a given digital output (0=low, 1=high)
            OutputMode::DigitalPin => write!(f, "digitalpin"),

            ///////////////////////////////////////////////////////////////
            // Robotics device output modes
            ///////////////////////////////////////////////////////////////
            // Set the position of a given stepper motor (in radians)
            OutputMode::StepperMotor => write!(f, "steppermotor"),
            // Set the polarity of a solenoid (0=off, 1=on)
            OutputMode::Solenoid => write!(f, "solenoid"),
            // Set the position of a given valve (0=closed, 1=open)
            OutputMode::Valve => write!(f, "valve"),
            // Set the speed of a given motor (in revolutions per minute)
            OutputMode::MotorSpeed => write!(f, "motorspeed"),
            // Set the position of a given servo (in radians)
            OutputMode::Servo => write!(f, "servo"),
            // Set the temperature of a given heating/cooling device (degrees K)
            OutputMode::Temperature => write!(f, "temperature"),
            // Set the pressure of a given pump (in atmospheres)
            OutputMode::Pump => write!(f, "pump"),
            // Set the pressure of a given fan (in atmospheres)
            OutputMode::Fan => write!(f, "fan"),
            // Set the pressure of a given blower (in atmospheres)
            OutputMode::Blower => write!(f, "blower"),
            // Turn a heater on or off (0=off, 1=on)
            OutputMode::Heater => write!(f, "heater"),
            // Turn a cooler on or off (0=off, 1=on)
            OutputMode::Cooler => write!(f, "cooler"),
            // Set the pressure of a given vacuum/pressurizer (atmospheres)
            OutputMode::Pressure => write!(f, "pressure"),

            ///////////////////////////////////////////////////////////////
            // Sound output modes
            ///////////////////////////////////////////////////////////////
            // Ring a given buzzer (in hertz)
            OutputMode::Buzzer => write!(f, "buzzer"),
            // Ring a bell (in hertz)
            OutputMode::Bell => write!(f, "bell"),
            // Play a given note (in hertz)
            OutputMode::Note => write!(f, "note"),
            // Set the volume of a given speaker (in percent)
            OutputMode::SpeakerVolume => write!(f, "speakervolume"),
            // Set the frequency of a given speaker (in hertz)
            OutputMode::SpeakerFrequency => write!(f, "speakerfrequency"),

            ///////////////////////////////////////////////////////////////
            // Display output modes
            ///////////////////////////////////////////////////////////////
            // Update the display
            OutputMode::UpdateDisplay => write!(f, "updatedisplay"),
            // Clear the display
            OutputMode::ClearDisplay => write!(f, "cleardisplay"),
            // Set the cursor row on the display
            OutputMode::SetCursorRow => write!(f, "setcursorrow"),
            // Set the cursor column on the display
            OutputMode::SetCursorColumn => write!(f, "setcursorcolumn"),
            // Move the cursor up on the display
            OutputMode::MoveCursorUp => write!(f, "movecursorup"),
            // Move the cursor down on the display
            OutputMode::MoveCursorDown => write!(f, "movecursordown"),
            // Move the cursor left on the display
            OutputMode::MoveCursorLeft => write!(f, "movecursorleft"),
            // Move the cursor right on the display
            OutputMode::MoveCursorRight => write!(f, "movecursorright"),
            // Write a character to the display
            OutputMode::SetCursorChar(c) => write!(f, "setcursorchar.{c}"),
            // Set the color of a given pixel on the display
            OutputMode::SetCursorPixel(c) => write!(f, "setcursorpixel.{c}"),

            ///////////////////////////////////////////////////////////////
            // Custom output modes
            ///////////////////////////////////////////////////////////////
            // A custom output mode (for use with a custom output device)
            OutputMode::Custom(name) => write!(f, "{name}"),
        }
    }
}

impl Display for Channel {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(
            f,
            "{}",
            match self {
                Direction::Up => "up",
                Direction::Down => "down",
                Direction::Left => "left",
                Direction::Right => "right",
            }
        )
    }
}

impl Display for Axis {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(
            f,
            "{}",
            match self {
                Axis::X => "x",
                Axis::Y => "y",
                Axis::Z => "z",
            }
        )
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(
            f,
            "{}",
            match self {
                Color::Red => "red",
                Color::Green => "green",
                Color::Blue => "blue",
                Color::Yellow => "yellow",
                Color::Magenta => "magenta",
                Color::Cyan => "cyan",
                Color::White => "white",
                Color::Black => "black",
                Color::Orange => "orange",
                Color::RGB(r, g, b) => return write!(f, "rgb({},{},{})", r, g, b),
            }
        )
    }
}
