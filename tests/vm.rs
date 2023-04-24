use sage::{io::Output, vm::*};

#[test]
fn test_add() {
    let a = 32;
    let b = 35;
    let program = CoreProgram(vec![
        CoreOp::Set(a),                     // Set register to `a`
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(b),  // Set register to `b`
        CoreOp::Add,     // Add the tape value to the register
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(0),  // Set register to 0s
        CoreOp::Restore, // Restore register from the current position on the turing tape
        CoreOp::Put(Output::stdout_char()), // Print the tape value
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![a + b]);
}

#[test]
fn test_subtract() {
    let a = 32;
    let b = 35;
    let program = CoreProgram(vec![
        CoreOp::Set(a),                     // Set register to `a`
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(b),  // Set register to `b`
        CoreOp::Sub,     // Subtract the tape value to the register
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(0),  // Set register to 0s
        CoreOp::Restore, // Restore register from the current position on the turing tape
        CoreOp::Put(Output::stdout_char()), // Print the tape value
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![b - a]);
}

#[test]
fn test_multiply() {
    let a = 32;
    let b = 35;
    let program = CoreProgram(vec![
        CoreOp::Set(a),                     // Set register to `a`
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(b),  // Set register to `b`
        CoreOp::Mul,     // Multiply the tape value to the register
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(0),  // Set register to 0s
        CoreOp::Restore, // Restore register from the current position on the turing tape
        CoreOp::Put(Output::stdout_char()), // Print the tape value
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![a * b]);
}

#[test]
fn test_divide() {
    let a = 32;
    let b = 123432;
    let program = CoreProgram(vec![
        CoreOp::Set(a),                     // Set register to `a`
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(b),  // Set register to `b`
        CoreOp::Div,     // Divide the tape value to the register
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(0),  // Set register to 0s
        CoreOp::Restore, // Restore register from the current position on the turing tape
        CoreOp::Put(Output::stdout_char()), // Print the tape value
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![b / a]);
}

#[test]
fn test_remainder() {
    let a = 24;
    let b = 100;
    let program = CoreProgram(vec![
        CoreOp::Set(a),                     // Set register to `a`
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(b),  // Set register to `b`
        CoreOp::Rem,     // Modulo the tape value to the register
        CoreOp::Save,    // Save register to the current position on the turing tape
        CoreOp::Set(0),  // Set register to 0s
        CoreOp::Restore, // Restore register from the current position on the turing tape
        CoreOp::Put(Output::stdout_char()), // Print the tape value
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![b % a]);
}

#[test]
fn test_pointers() {
    let a = 2;
    let b = 100;

    let program = CoreProgram(vec![
        CoreOp::Move(1),
        CoreOp::Set(a),
        CoreOp::Save,
        CoreOp::Move(100),
        CoreOp::Set(b),
        CoreOp::Save,
        CoreOp::Where,
        CoreOp::Move(-101),
        CoreOp::Save,
        CoreOp::Deref,
        CoreOp::Restore,
        CoreOp::Put(Output::stdout_char()),
        CoreOp::Refer,
        CoreOp::Move(1),
        CoreOp::Restore,
        CoreOp::Put(Output::stdout_char()),
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![b, a]);
}
