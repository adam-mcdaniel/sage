use asm::{
    asm::*,
    vm::{CoreInterpreter, TestingDevice},
};

#[test]
fn test_putint() {
    use CoreOp::*;

    let program = CoreProgram(vec![
        Set(A, 'a' as isize),
        Push(A, 1),
        Set(A, 'b' as isize),
        Push(A, 1),
        Set(A, 'c' as isize),
        Push(A, 1),
        Pop(Some(B), 1),
        Put(B),
        Set(A, 'd' as isize),
        Push(A, 1),
        Pop(Some(B), 1),
        Put(B),
        Pop(Some(B), 1),
        Put(B),
        Pop(Some(B), 1),
        Put(B),
    ])
    .assemble(32)
    .unwrap();
    eprintln!("{:?}", program);

    let i = CoreInterpreter::new(TestingDevice::default());

    let device = i.run(&program).unwrap();

    assert_eq!(device.output_str(), "cdba");
}
