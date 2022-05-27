use asm::{CompilerTarget, asm::*, targets};


fn main() {
    use CoreOp::*;
    // let program = CoreProgram(vec![
    //     Fn(String::from("cmp")),
    //         Compare {
    //             dst: C,
    //             a: A,
    //             b: B,
    //         },
    //         Set(D, '=' as isize),
    //         Add { dst: C, src: D },
    //         PutChar(A),
    //         Set(A, ' ' as isize),
    //         PutChar(A),
    //         PutChar(C),
    //         Set(A, ' ' as isize),
    //         PutChar(A),
    //         PutChar(B),
    //         Set(A, 10),
    //         PutChar(A),
    //     End,

    //     Set(A, 'a' as isize),
    //     PutChar(A),
    //     Set(A, '=' as isize),
    //     PutChar(A),
    //     GetChar(A),
    //     GetChar(TMP),
    //     Set(B, 'b' as isize),
    //     PutChar(B),
    //     Set(B, '=' as isize),
    //     PutChar(B),
    //     GetChar(B),
    //     GetChar(TMP),
    //     CallLabel(String::from("cmp")),
    // ]).assemble().unwrap();

    // let program = CoreProgram(vec![
    //     Set(A, 'a' as isize), Push(A),
    //     Set(A, 'b' as isize), Push(A),
    //     Set(A, 'c' as isize), Push(A),
    //     Pop(Some(B)), PutChar(B),
    //     Set(A, 'd' as isize), Push(A),
    //     Pop(Some(B)), PutChar(B),
    //     Pop(Some(B)), PutChar(B),
    //     Pop(Some(B)), PutChar(B),
    //     Set(A, '\n' as isize), Push(A),
    // ]).assemble().unwrap();
    // eprintln!("{:?}", program);

    // let program = CoreProgram(vec![
    //     Fn(String::from("print_alphabet")),
    //         Move { src: FP, dst: A },
    //         Prev(A, Some(26)),
    //         Set(B, 26),
    //         While(B),
    //             PutChar(A.deref()),
    //             Next(A, None),
    //             Dec(B),
    //         End,
    //     End,
    //     Set(A, 26),
    //     Set(B, 'A' as isize),
    //     While(A),
    //         Push(B), Inc(B),
    //         Dec(A),
    //     End,
    //     CallLabel(String::from("print_alphabet")),
    //     Prev(SP, Some(26)),
    //     Set(A, '\n' as isize), PutChar(A),
    // ]).assemble().unwrap();
    // eprintln!("{:?}", program);
    

    // let program = CoreProgram(vec![
    //     Fn(String::from("putint")),
    //         Move { src: FP.deref().offset(-1), dst: A },

    //         Set(B, 10),
    //         DivRem { src: B, dst: A },
    //         If(A),
    //             Push(B),
    //             Push(A),
    //             CallLabel(String::from("putint")),
    //             Pop(None),
    //             Pop(Some(B)),
    //         End,
    //         Set(A, '0' as isize),
    //         Add { src: A, dst: B},
    //         PutChar(B),
    //     End,
        
    //     PutLiteral(String::from("ladies and gentlemen, one million ints!\n")),
    //     Set(F, 1000000),
    //     While(F),
    //         Push(F),
    //         CallLabel(String::from("putint")),
    //         Pop(None),

    //         Set(E, '\n' as isize), PutChar(E),
    //         Dec(F),
    //     End,
    // ]).assemble().unwrap();
    // eprintln!("{:?}", program);


    

    let program = CoreProgram(vec![
        Fn(String::from("return-test")),
            CoreOp::put_string("return test 1!\n"),
            Return,
            CoreOp::put_string("return test 2!\n"),
        End,
        
        Fn(String::from("putint")),
            Move { src: var(0), dst: A },

            Set(B, 10),
            DivRem { src: B, dst: A },
            If(A),
                Push(B),
                Push(A),
                CallLabel(String::from("putint")),
                Pop(None),
                Pop(Some(B)),
            End,
            Set(A, '0' as isize),
            Add { src: A, dst: B},
            PutChar(B),
        End,
        
        Comment(String::from(r#"Hey jude!
                                Hello world!
                                Testing!
                                This is a comment!"#)),

        Fn(String::from("putstr")),
            Move { src: var(0), dst: C },
            While(C.deref()),
                PutChar(C.deref()),
                Next(C, None),
            End,
        End,

        Fn(String::from("getstr")),
            Move { src: var(0), dst: C },
            Move { src: C, dst: B },
            GetChar(C.deref()),
            Set(D, '\n' as isize),
            IsNotEqual { dst: E, a: C.deref(), b: D },
            While(E),
                PutChar(C.deref()),
                Next(C, None),
                GetChar(C.deref()),
                
                Set(D, '\n' as isize),
                IsNotEqual { dst: E, a: C.deref(), b: D },
            End,
            Set(C.deref(), '\0' as isize),
            
            Move { src: B, dst: var(0) },
        End,

        Fn(String::from("strlen")),
            Move { src: var(0), dst: C },
            Set(D, 0),
            While(C.deref()),
                Next(C, None),
                Inc(D),
            End,
            Move { src: D, dst: var(0) },
        End,

        Fn(String::from("strrev")),
            Move { src: var(0), dst: C },
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


        CoreOp::stack_alloc_string(A, "Hello world!\n"),
        CoreOp::stack_alloc_string(B, "Dontshowme!!\n"),

        // Copy A to B
        Copy { src: A, dst: B, size: 14 },
        
        // Overwrite A
        CoreOp::push_string("abcdefghijk?\n"),
        Store(A, 14),
        
        Set(D, 4),
        While(D),
            // Print A
            Push(A),
            CallLabel(String::from("putstr")),
            Pop(None),

            // Print B
            Push(B),
            CallLabel(String::from("putstr")),
            Pop(None),

            Dec(D),
        End,

        StackAllocateLiteral(C, vec![0].repeat(1024)),
        
        
        CoreOp::put_string(">> "),
        Push(C),
        CallLabel(String::from("getstr")),
        Pop(Some(F)),

        CoreOp::put_string("you entered: `"),

        Push(F),
        CallLabel(String::from("strrev")),
        CallLabel(String::from("putstr")),
        CoreOp::put_string("`\nwhich is "),
        CallLabel(String::from("strlen")),
        CallLabel(String::from("putint")),
        Pop(None),
        
        CoreOp::put_string(" characters long!\n"),
    ]).assemble().unwrap();
    eprintln!("{:?}", program);



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