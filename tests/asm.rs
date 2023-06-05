use sage::{
    asm::*,
    io::{Input, Output},
    parse::parse_asm,
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

    let program = CoreProgram::new(vec![
        add.clone(),
        Set(A, 32),
        Push(A, 1),
        Set(A, 1),
        Push(A, 1),
        CallLabel(String::from("add")),
        Put(SP.deref(), Output::stdout_char()),
    ])
    .assemble(32)
    .unwrap();
    let i = CoreInterpreter::new(TestingDevice::default());

    let device = i.run(&program).unwrap();

    assert_eq!(device.output_vals(), vec![33]);
}

#[test]
fn test_alphabet() {
    use CoreOp::*;

    let program = CoreProgram::new(vec![
        Fn(String::from("print_alphabet")),
        Move { src: FP, dst: A },
        Prev(A, Some(25)),
        Set(B, 26),
        While(B),
        Put(A.deref(), Output::stdout_char()),
        Next(A, None),
        Dec(B),
        End,
        End,
        Set(A, 26),
        Set(B, 'A' as i64),
        While(A),
        Push(B, 1),
        Inc(B),
        Dec(A),
        End,
        CallLabel(String::from("print_alphabet")),
        Prev(SP, Some(26)),
        Set(A, '\n' as i64),
        Put(A, Output::stdout_char()),
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

    let program = CoreProgram::new(vec![
        Fn(String::from("cmp")),
        Compare { dst: D, a: A, b: B },
        Set(C, '=' as i64),
        Add { dst: C, src: D },
        Put(A, Output::stdout_char()),
        Set(A, ' ' as i64),
        Put(A, Output::stdout_char()),
        Put(C, Output::stdout_char()),
        Set(A, ' ' as i64),
        Put(A, Output::stdout_char()),
        Put(B, Output::stdout_char()),
        Set(A, 10),
        Put(A, Output::stdout_char()),
        End,
        Set(A, 'a' as i64),
        Put(A, Output::stdout_char()),
        Set(A, '=' as i64),
        Put(A, Output::stdout_char()),
        Get(A, Input::stdin_char()),
        Get(C, Input::stdin_char()),
        Set(B, 'b' as i64),
        Put(B, Output::stdout_char()),
        Set(B, '=' as i64),
        Put(B, Output::stdout_char()),
        Get(B, Input::stdin_char()),
        Get(C, Input::stdin_char()),
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

    let program = CoreProgram::new(vec![
        Set(A, 'a' as i64),
        Push(A, 1),
        Set(A, 'b' as i64),
        Push(A, 1),
        Set(A, 'c' as i64),
        Push(A, 1),
        Pop(Some(B), 1),
        Put(B, Output::stdout_char()),
        Set(A, 'd' as i64),
        Push(A, 1),
        Pop(Some(B), 1),
        Put(B, Output::stdout_char()),
        Pop(Some(B), 1),
        Put(B, Output::stdout_char()),
        Pop(Some(B), 1),
        Put(B, Output::stdout_char()),
    ])
    .assemble(32)
    .unwrap();

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
        Set(A, '0' as i64),
        Add { src: A, dst: B },
        Put(B, Output::stdout_char()),
        End,
    ]);

    let putstr = Many(vec![
        Fn(String::from("putstr")),
        Move {
            src: FP.deref(),
            dst: C,
        },
        While(C.deref()),
        Put(C.deref(), Output::stdout_char()),
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
        Get(C.deref(), Input::stdin_char()),
        Set(D, '\n' as i64),
        IsNotEqual {
            dst: E,
            a: C.deref(),
            b: D,
        },
        While(E),
        Next(C, None),
        Get(C.deref(), Input::stdin_char()),
        Set(D, '\n' as i64),
        IsNotEqual {
            dst: E,
            a: C.deref(),
            b: D,
        },
        End,
        Set(C.deref(), '\0' as i64),
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
        IsLess { dst: B, a: C, b: D },
        While(B),
        Swap(C.deref(), D.deref()),
        Next(C, None),
        Prev(D, None),
        IsLess { dst: B, a: C, b: D },
        End,
        End,
    ]);

    let program = CoreProgram::new(vec![
        Fn(String::from("return-test")),
        CoreOp::put_string("return test 1!\n", Output::stdout_char()),
        Return,
        CoreOp::put_string("return test 2!\n", Output::stdout_char()),
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
        CoreOp::put_string(">> ", Output::stdout_char()),
        Push(C, 1),
        CallLabel(String::from("getstr")),
        Pop(Some(F), 1),
        CoreOp::put_string("you entered: `", Output::stdout_char()),
        Push(F, 1),
        CallLabel(String::from("strrev")),
        CallLabel(String::from("putstr")),
        CoreOp::put_string("`\nwhich is ", Output::stdout_char()),
        CallLabel(String::from("strlen")),
        CallLabel(String::from("putint")),
        Pop(None, 1),
        CoreOp::put_string(" characters long!\n", Output::stdout_char()),
    ])
    .assemble(512)
    .unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));

    let device = i.run(&program).unwrap();
    assert_eq!(device.output_str(), "abcdefghijk?\nHello world!\nabcdefghijk?\nHello world!\nabcdefghijk?\nHello world!\nabcdefghijk?\nHello world!\n>> you entered: `gnitset`\nwhich is 7 characters long!\n")
}

#[test]
fn test_factorial() {
    let factorial = r#"
    fun fact
        if [FP]
            mov [FP], A
            dec A
            push A
            call fact
            mul [FP + 1], [FP]
            pop
        else
            set [FP], 1
        end
    end

    set A, 10 push A
    call fact
    pop A
    put-int A
    "#;

    let asm_core = parse_asm(factorial).unwrap().unwrap();
    let vm_code = asm_core.assemble(5000).unwrap();

    let device = CoreInterpreter::new(TestingDevice::new(""))
        .run(&vm_code)
        .unwrap();

    assert_eq!(&device.output_str(), "3628800")
}
