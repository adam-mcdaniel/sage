use asm::{
    asm::*,
    vm::{CoreInterpreter, TestingDevice},
};

#[test]
fn test_add() {
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

#[test]
fn test_alphabet() {
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
        Push(B, 1),
        Inc(B),
        Dec(A),
        End,
        CallLabel(String::from("print_alphabet")),
        Prev(SP, Some(26)),
        Set(A, '\n' as isize),
        Put(A),
    ])
    .assemble(32)
    .unwrap();

    let i = CoreInterpreter::new(TestingDevice::default());

    let device = i.run(&program).unwrap();

    assert_eq!(device.output_str(), "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n");
}

#[test]
fn test_cmp() {
    use CoreOp::*;

    let program = CoreProgram(vec![
        Fn(String::from("cmp")),
        Compare { dst: C, a: A, b: B },
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
        Get(C),
        Set(B, 'b' as isize),
        Put(B),
        Set(B, '=' as isize),
        Put(B),
        Get(B),
        Get(C),
        CallLabel(String::from("cmp")),
    ])
    .assemble(32)
    .unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("5\n6\n"));
    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "a=b=5 < 6\n");

    let i = CoreInterpreter::new(TestingDevice::new("3\n3\n"));
    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "a=b=3 = 3\n");

    let i = CoreInterpreter::new(TestingDevice::new("6\n5\n"));
    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "a=b=6 > 5\n");
}

#[test]
fn test_stack() {
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

#[test]
fn test_str() {
    use CoreOp::*;

    let putint = Many(vec![
        Fn(String::from("putint")),
        Move {
            src: FP.deref(),
            dst: A,
        },
        Set(B, 10),
        DivRem { src: B, dst: A },
        If(A),
        Push(B, 1),
        Push(A, 1),
        CallLabel(String::from("putint")),
        Pop(None, 1),
        Pop(Some(B), 1),
        End,
        Set(A, '0' as isize),
        Add { src: A, dst: B },
        Put(B),
        End,
    ]);

    let putstr = Many(vec![
        Fn(String::from("putstr")),
        Move {
            src: FP.deref(),
            dst: C,
        },
        While(C.deref()),
        Put(C.deref()),
        Next(C, None),
        End,
        End,
    ]);

    let getstr = Many(vec![
        Fn(String::from("getstr")),
        Move {
            src: FP.deref(),
            dst: C,
        },
        Move { src: C, dst: B },
        Get(C.deref()),
        Set(D, '\n' as isize),
        IsNotEqual {
            dst: E,
            a: C.deref(),
            b: D,
        },
        While(E),
        Next(C, None),
        Get(C.deref()),
        Set(D, '\n' as isize),
        IsNotEqual {
            dst: E,
            a: C.deref(),
            b: D,
        },
        End,
        Set(C.deref(), '\0' as isize),
        Move {
            src: B,
            dst: FP.deref(),
        },
        End,
    ]);

    let strlen = Many(vec![
        Fn(String::from("strlen")),
        Move {
            src: FP.deref(),
            dst: C,
        },
        Set(D, 0),
        While(C.deref()),
        Next(C, None),
        Inc(D),
        End,
        Move {
            src: D,
            dst: FP.deref(),
        },
        End,
    ]);

    let strrev = Many(vec![
        Fn(String::from("strrev")),
        Move {
            src: FP.deref(),
            dst: C,
        },
        Move { src: C, dst: D },
        While(D.deref()),
        Next(D, None),
        End,
        Prev(D, None),
        Move { src: C, dst: B },
        IsLess { dst: B, a: C, b: D },
        While(B),
        Swap(C.deref(), D.deref()),
        Next(C, None),
        Prev(D, None),
        Move { src: C, dst: B },
        IsLess { dst: B, a: C, b: D },
        End,
        End,
    ]);

    let program = CoreProgram(vec![
        Fn(String::from("return-test")),
        CoreOp::put_string("return test 1!\n"),
        Return,
        CoreOp::put_string("return test 2!\n"),
        End,
        putint.clone(),
        putstr.clone(),
        getstr.clone(),
        strlen.clone(),
        strrev.clone(),
        Comment(String::from(
            r#"Hey jude!
                                Hello world!
                                Testing!
                                This is a comment!"#,
        )),
        CoreOp::stack_alloc_string(A, "Hello world!\n"),
        CoreOp::stack_alloc_string(B, "Dontshowme!!\n"),
        // Copy A to B
        Copy {
            src: A.deref(),
            dst: B.deref(),
            size: 14,
        },
        // Overwrite A
        CoreOp::push_string("abcdefghijk?\n"),
        Pop(Some(A.deref()), 14),
        Set(D, 4),
        While(D),
        // Print A
        Push(A, 1),
        CallLabel(String::from("putstr")),
        Pop(None, 1),
        // Print B
        Push(B, 1),
        CallLabel(String::from("putstr")),
        Pop(None, 1),
        Dec(D),
        End,
        CoreOp::stack_alloc_cells(C, vec![0].repeat(1024)),
        CoreOp::put_string(">> "),
        Push(C, 1),
        CallLabel(String::from("getstr")),
        Pop(Some(F), 1),
        CoreOp::put_string("you entered: `"),
        Push(F, 1),
        CallLabel(String::from("strrev")),
        CallLabel(String::from("putstr")),
        CoreOp::put_string("`\nwhich is "),
        CallLabel(String::from("strlen")),
        CallLabel(String::from("putint")),
        Pop(None, 1),
        CoreOp::put_string(" characters long!\n"),
    ])
    .assemble(16)
    .unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));

    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "abcdefghijk?\nHello world!\nabcdefghijk?\nHello world!\nabcdefghijk?\nHello world!\nabcdefghijk?\nHello world!\n>> you entered: `gnitset`\nwhich is 7 characters long!\n")
}
