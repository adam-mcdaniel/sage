use sage::{side_effects::Output, vm::*};

#[test]
fn test_add() {
    let a = 32;
    let b = 35;
    let program = CoreProgram(vec![
        CoreOp::Set(vec![a]),                     // Set register to `a`
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![b]),  // Set register to `b`
        CoreOp::Add(1),     // Add the tape value to the register
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![0]),  // Set register to 0s
        CoreOp::Load(1), // Restore register from the current position on the turing tape
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
        CoreOp::Set(vec![a]),                     // Set register to `a`
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![b]),  // Set register to `b`
        CoreOp::Sub(1),     // Subtract the tape value to the register
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![0]),  // Set register to 0s
        CoreOp::Load(1), // Restore register from the current position on the turing tape
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
        CoreOp::Set(vec![a]),                     // Set register to `a`
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![b]),  // Set register to `b`
        CoreOp::Mul(1),     // Multiply the tape value to the register
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![0]),  // Set register to 0s
        CoreOp::Load(1), // Restore register from the current position on the turing tape
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
        CoreOp::Set(vec![a]),                     // Set register to `a`
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![b]),  // Set register to `b`
        CoreOp::Div(1),     // Divide the tape value to the register
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![0]),  // Set register to 0s
        CoreOp::Load(1), // Restore register from the current position on the turing tape
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
        CoreOp::Set(vec![a]),                     // Set register to `a`
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![b]),  // Set register to `b`
        CoreOp::Rem(1),     // Modulo the tape value to the register
        CoreOp::Store(1),    // Save register to the current position on the turing tape
        CoreOp::Set(vec![0]),  // Set register to 0s
        CoreOp::Load(1), // Restore register from the current position on the turing tape
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
        CoreOp::Set(vec![a]),
        CoreOp::Store(1),
        CoreOp::Move(100),
        CoreOp::Set(vec![b]),
        CoreOp::Store(1),
        CoreOp::Where,
        CoreOp::Move(-101),
        CoreOp::Store(1),
        CoreOp::Deref,
        CoreOp::Load(1),
        CoreOp::Put(Output::stdout_char()),
        CoreOp::Refer,
        CoreOp::Move(1),
        CoreOp::Load(1),
        CoreOp::Put(Output::stdout_char()),
    ]);

    let i = CoreInterpreter::new(TestingDevice::default());
    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![b, a]);
}
