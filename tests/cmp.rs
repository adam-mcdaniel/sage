use asm::{asm::*, vm::{Interpreter, TestingDevice}};

#[test]
fn test_cmp() {
    use CoreOp::*;

    let program = CoreProgram(vec![
        Fn(String::from("cmp")),
            Compare {
                dst: C,
                a: A,
                b: B,
            },
            Set(D, '=' as isize),
            Add { dst: C, src: D },
            Put(A),
            Set(A, ' ' as isize),
            Put(A),
            Put(C),
            Set(A, ' ' as isize),
            Put(A),
            Put(B),
            Set(A, 10),
            Put(A),
        End,

        Set(A, 'a' as isize),
        Put(A),
        Set(A, '=' as isize),
        Put(A),
        Get(A),
        Get(TMP),
        Set(B, 'b' as isize),
        Put(B),
        Set(B, '=' as isize),
        Put(B),
        Get(B),
        Get(TMP),
        CallLabel(String::from("cmp")),
    ]).assemble(32).unwrap();

    let i = Interpreter::new(TestingDevice::new("5\n6\n"));
    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "a=b=5 < 6\n");

    let i = Interpreter::new(TestingDevice::new("3\n3\n"));
    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "a=b=3 = 3\n");

    let i = Interpreter::new(TestingDevice::new("6\n5\n"));
    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "a=b=6 > 5\n");

}