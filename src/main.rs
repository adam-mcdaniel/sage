use asm::{
    asm::*,
    targets,
    vm::{Interpreter, TestingDevice},
    CompilerTarget,
};

fn main() {
    use CoreOp::*;

    let putint = CoreOp::Many(vec![
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
            src: var(0),
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
            src: A,
            dst: B,
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
    // eprintln!("{:?}", program);
    // let i = Interpreter::default();
    let i = Interpreter::new(TestingDevice::new("testing\n"));
    let device = i.run(&program).unwrap();
    println!("{:?}", device.input);
    println!("{:?}", device.output_str());

    // let program = StandardProgram(vec![
    //     StandardOp::CoreOp(Fn),
    //         StandardOp::CoreOp(Constant(33)),
    //         StandardOp::CoreOp(PutChar),
    //         StandardOp::CoreOp(Constant(10)),
    //         StandardOp::CoreOp(PutChar),
    //     StandardOp::CoreOp(End),

    //     StandardOp::CoreOp(Fn),
    //         StandardOp::CoreOp(Move(-2)),
    //         StandardOp::CoreOp(Restore),
    //         StandardOp::CoreOp(Move(1)),
    //         StandardOp::CoreOp(Add),
    //         StandardOp::PutInt,
    //     StandardOp::CoreOp(End),

    //     StandardOp::CoreOp(Constant('Z' as usize)),
    //     StandardOp::CoreOp(Save),
    //     StandardOp::CoreOp(Move(1)),

    //     StandardOp::CoreOp(Constant(3)),
    //     StandardOp::CoreOp(Save),
    //     StandardOp::CoreOp(Move(1)),

    //     StandardOp::CoreOp(Constant(1)),
    //     StandardOp::CoreOp(Call),
    // ]);
    println!("{}", targets::C.compile_core(&program).unwrap());
}
