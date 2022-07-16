use asm::{asm::*, vm::{Interpreter, TestingDevice}};

#[test]
fn test_putint() {
    use CoreOp::*;
    
    let program = CoreProgram(vec![
        Fn(String::from("print_alphabet")),
            Move { src: FP, dst: A },
            Prev(A, Some(25)),
            Set(B, 26),
            While(B),
                Put(A.deref()),
                Next(A, None),
                Dec(B),
            End,
        End,
        Set(A, 26),
        Set(B, 'A' as isize),
        While(A),
            Push(B, 1), Inc(B),
            Dec(A),
        End,
        CallLabel(String::from("print_alphabet")),
        Prev(SP, Some(26)),
        Set(A, '\n' as isize), Put(A),
    ]).assemble(32).unwrap();

    let i = Interpreter::new(TestingDevice::default());

    let device = i.run(&program).unwrap();

    assert_eq!(device.output_str(), "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n");
}