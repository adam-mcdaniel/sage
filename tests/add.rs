use asm::{
    asm::*,
    vm::{CoreInterpreter, TestingDevice},
};

#[test]
fn test_putint() {
    use CoreOp::*;
    let add = Many(vec![
        Fn(String::from("add")),
        Add {
            src: SP.deref(),
            dst: SP.deref().offset(-1),
        },
        Pop(None, 1),
        End,
    ]);

    let program = CoreProgram(vec![
        add.clone(),
        Set(A, 32),
        Push(A, 1),
        Set(A, 1),
        Push(A, 1),
        CallLabel(String::from("add")),
        Put(SP.deref()),
    ])
    .assemble(32)
    .unwrap();
    let i = CoreInterpreter::new(TestingDevice::default());

    let device = i.run(&program).unwrap();

    assert_eq!(device.output, vec![33]);
}
