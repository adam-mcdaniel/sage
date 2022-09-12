use acid::{
    asm::{CoreOp, StandardOp, SP, A, B, C},
    lir::*,
    targets, targets::Target,
    parse::*,
    vm::{CoreInterpreter, StandardInterpreter, TestingDevice, StandardDevice},
};


fn main() {
    let alloc = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "alloc".to_string(),
        args: vec![("size".to_string(), Type::Int)],
        ret: Type::Pointer(Box::new(Type::Any)),
        body: vec![StandardOp::Alloc(SP.deref())],
    });
    let free = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "free".to_string(),
        args: vec![("ptr".to_string(), Type::Pointer(Box::new(Type::Any)))],
        ret: Type::None,
        body: vec![
            StandardOp::Free(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });
    let put_float = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "put_float".to_string(),
        args: vec![("x".to_string(), Type::Float)],
        ret: Type::None,
        body: vec![
            StandardOp::PutFloat(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });
    
    let get_char = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "get_char".to_string(),
        args: vec![],
        ret: Type::Char,
        body: vec![
            StandardOp::GetChar(A),
            StandardOp::CoreOp(CoreOp::Push(A, 1)),
        ],
    });
    let put_char = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Char)],
        ret: Type::None,
        body: vec![
            StandardOp::PutChar(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });

    let get_int = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "get_int".to_string(),
        args: vec![],
        ret: Type::Int,
        body: vec![
            StandardOp::GetInt(A),
            StandardOp::CoreOp(CoreOp::Push(A, 1)),
        ],
    });

    let get_float = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "get_float".to_string(),
        args: vec![],
        ret: Type::Float,
        body: vec![
            StandardOp::GetFloat(A),
            StandardOp::CoreOp(CoreOp::Push(A, 1)),
        ],
    });

    let put_int = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "put_int".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            StandardOp::PutInt(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });

    let put = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref()),
            CoreOp::Pop(None, 1),
        ],
    });

    let swap = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "swap".to_string(),
        args: vec![
            ("x".to_string(), Type::Pointer(Box::new(Type::Int))),
            ("y".to_string(), Type::Pointer(Box::new(Type::Int))),
        ],
        ret: Type::None,
        body: vec![
            CoreOp::Swap(SP.deref().deref(), SP.deref().offset(-1).deref()),
            CoreOp::Pop(None, 2),
        ],
    });

    let min = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "min".to_string(),
        args: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::IsGreater { dst: A, a: SP.deref().offset(-1), b: SP.deref() },
            CoreOp::If(A),
            CoreOp::Pop(Some(SP.deref().offset(-1)), 1),
            CoreOp::Else,
            CoreOp::Pop(None, 1),
            CoreOp::End,
        ],
    });
    let max = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "max".to_string(),
        args: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::IsGreater { dst: A, a: SP.deref().offset(-1), b: SP.deref() },
            CoreOp::If(A),
            CoreOp::Pop(None, 1),
            CoreOp::Else,
            CoreOp::Pop(Some(SP.deref().offset(-1)), 1),
            CoreOp::End,
        ],
    });
    let lt = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "lt".to_string(),
        args: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        ret: Type::Bool,
        body: vec![
            CoreOp::IsLess { a: SP.deref().offset(-1), b: SP.deref(), dst: A },
            CoreOp::Pop(None, 1),
            CoreOp::Move { src: A, dst: SP.deref() }
        ],
    });
    let lte = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "lte".to_string(),
        args: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        ret: Type::Bool,
        body: vec![
            CoreOp::IsLessEqual { a: SP.deref().offset(-1), b: SP.deref(), dst: A },
            CoreOp::Pop(None, 1),
            CoreOp::Move { src: A, dst: SP.deref() }
        ],
    });
    let inc = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "inc".to_string(),
        args: vec![("x".to_string(), Type::Pointer(Box::new(Type::Int)))],
        ret: Type::None,
        body: vec![
            CoreOp::Inc(SP.deref().deref()),
            CoreOp::Pop(None, 1)
        ],
    });
    let dec = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "dec".to_string(),
        args: vec![("x".to_string(), Type::Pointer(Box::new(Type::Int)))],
        ret: Type::None,
        body: vec![
            CoreOp::Dec(SP.deref().deref()),
            CoreOp::Pop(None, 1)
        ],
    });


    let testing = r#"
    proc sum(x: &Int, n: Int) -> Int = {
        let result = 0 in {
            while n {
                n = n - 1;
                putint(x[n]); putchar('\n');
                x[n] = 1000;
                result = result + x[n];
            };
            result
        }
    } in
    let i = 3, x = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9], result = sum((&x) as &Int, sizeofexpr(x)) in {
        sum((&x) as &Int, sizeofexpr(x));
        result
    }
    "#;

    let union_buster = r#"
type Point = struct { x: Int, y: Int } in
type Test = union { scalar: Int, point: Point } in
const HMM = 500 in

let x = union {
    point = struct { x=3, y=7 },
    Test..
}, z = 6 in {
    x.point.x = z;
    if true {
        z = x.point.x + x.point.y;
    };
    putchar('\n');
    const TEST = HMM in z + TEST
}
"#;

    let type_stuff = r#"
type List = let T = Int in struct { data: T, next: &List } in

let x = struct { data=5, next=Null },
    y = struct { data=6, next=&x },
    y: List = struct { data=7, next=&y },
    in y.next->next->data
"#;

    let option = r#"
type Option = let T = Point in struct {
    tag: enum {Some, Nothing},
    data: union { some: T, none: None }
}, Point = struct { x: Int, y: Int } in

proc some(p: Point) -> let T = Point in struct {
    tag: enum {Some, Nothing},
    data: union { some: T, none: None }
} = {struct { tag = Some of enum {Some, Nothing}, data = union {none: None, some: Point = p} }} in

some(struct{ x = 5, y = 6}).data.some.y as Int
"#;

    let square = "let x = getfloat() in x * x";
    let factorial = "proc fact(n: Float) -> Float = { if (n as Int) (n * fact(n - 1)) else 1.0 } in { putfloat(fact(50.0)); putchar('\n'); 0 }";
    let fn_ptr = "proc test(order: proc(Int, Int) -> Int, x: Int, y: Int) -> Int = { order(x, y) } in test(proc(x: Int, y: Int) -> Int = {x}, 5, 6)";


    let list = r#"
type List = struct { data: Int, next: &List } in

proc sort(node: &List) -> None = {
    if (node->next as Cell as Int + 128) {
        sort(node->next);
        let a = min(node->data, node->next->data),
            b = max(node->data, node->next->data)
            in {
            if (b - node->data) {
                sort(node->next);
            };
            node->data = a;
            node->next->data = b;
        };
    }
} in

proc len(node: &List) -> Int = {
    if (node as Cell as Int + 128) {
        len(node->next) + 1
    } else {
        0
    }
} in

proc neq(a: Int, b: Int) -> Bool = {
    (if ((a - b) as Bool) 1 else 0) as Bool
} in

proc index(node: &List, n: Int) -> &List = {
    while n {
        node = node->next;
        n = n - 1;
    };
    node
} in

proc swapi(a: &Int, b: &Int) -> None = {
    let tmp = (*a) in {
        (*a) = *b;
        (*b) = tmp;
    }
} in

proc partition_arr(arr: &Int, low: Int, high: Int) -> Int = {
    let pivot = arr[high],
        i = low - 1,
        j = low in {
        while lt(j, high) {
            if (lte(arr[j], pivot)) {
                inc(&i);
                swapi(&arr[j], &arr[i]);
            };
            inc(&j);
        };
        swapi(&arr[i + 1], &arr[high]);
        i + 1
    }
} in

proc quicksort_arr(arr: &Int, low: Int, high: Int) -> None = {
    if (lt(low, high)) {
        let pi = partition_arr(arr, low, high) in {
            quicksort_arr(arr, low, pi - 1);
            quicksort_arr(arr, pi + 1, high);
        }
    }
} in

const SIZE = 200 in
let arr: &Int = alloc(SIZE * 2), i = 0, a = 5, b = 6 in {
    while lt(i, SIZE) {
        arr[i] = if (i % 2) i else (SIZE - i);
        putint(arr[i]); putchar(';');
        inc(&i);
    };
    quicksort_arr(arr, 0, SIZE - 1);
    putchar('\n');
    i = 0;
    while lt(i, SIZE) {
        putint(arr[i]); putchar(';');
        inc(&i);
    }
}
"#;


    let while_loop = r#"
    let n = 9999, ret = 0 in {
        while n {
            ret = ret + n;
            n = n - 1;
        };
        ret
    }
    "#;

    let collatz = r#"
    proc step(n: Int) -> Int = {if (n % 2) (3 * n + 1) else n / 2} in
    proc collatz(n: Int) -> Int = {
        let i = 0 in
            while n - 1 {
                i = i + 1;
                putint(i); putchar(':'); putchar(' '); putint(n); putchar('\n');
                n = step(n);
            };
        return n
    } in { putchar('>'); collatz(129) }
    "#;

    let test_asm = r#"
    fun test
        set-f A, 32.058
        put-float A
        set B, 10 put-char B
        to-int A
        put-int A
        set B, 10 put-char B
        to-float A
        set-f B, 10.5 add-f B, A
        put-float A
        set B, 10 put-char B
    end

    call test
    call test
    "#;

    let fact_asm = r#"
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

    set A, 5 push A
    call fact
    pop A
    put-int A
    "#;

    let string_asm = r#"
    set A, 100
    alloc A
    array [A], "Hello world!", B
    array [B], "How are you?", C
    array [C], "I am good!", D

    fun putstr
        mov [FP], F
        while [F]
            put-char [F]
            next F
        end
    end

    fun putstrln
        call putstr
        set F, 10 put-char F
    end

    push A
    put-int [SP]
    call putstrln
    push B
    put-int [SP]
    call putstrln
    push C
    put-int [SP]
    call putstrln

    free A
    "#;

    let simple_fun_test = r#"
    proc C() -> None = { putint(6) } in
    proc B() -> None = { putint(5) } in
    proc A() -> None = { if false { B(); C() }; C(); B() } in
    proc D() -> None = { if false { B(); C() }; A(); } in {
        A();
        D();
    }
    "#;

    let bitwise_test = r#"
    set A, 10
    set B, 7

    mov A, C
    bitwise-and B, C
    put-int C

    set D, 10 put-char D

    set B, 100
    bitwise-not B
    inc B
    put-int B

    set D, 10 put-char D

    set A, 100
    set B, 7

    bitwise-xor B, A

    fun put_bits
        set A, 128 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 64 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 32 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 16 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 8 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 4 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 2 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        set A, 1 bitwise-and [SP], A
        if A
            set A, 49
        else
            set A, 48
        end
        put-char A
        pop
    end

    push A
    call put_bits
    "#;

    match parse_asm(bitwise_test) {
        Ok(asm_code) => {
            let device = match asm_code {
                Ok(asm_core) => {
                    // eprintln!("{:?}", asm_core);
                    let vm_code = asm_core.assemble(5000).unwrap();
                    // eprintln!("{:?}", vm_code);
                    println!("{}", targets::C.build_core(&vm_code).unwrap());
                    // eprintln!("{:?}", vm_code.0.len());
                    CoreInterpreter::new(StandardDevice)
                        .run(&vm_code)
                        .unwrap()
                }
                Err(asm_std) => {
                    // eprintln!("{:?}", asm_std);
                    let vm_code = asm_std.assemble(5000).unwrap();
                    // eprintln!("{:?}", vm_code);
                    println!("{}", targets::C.build_std(&vm_code).unwrap());
                    // eprintln!("{:?}", vm_code.0.len());
                    StandardInterpreter::new(StandardDevice)
                        .run(&vm_code)
                        .unwrap()
                }
            };
        }
        Err(e) => {
            eprintln!("{e}");
        }
    }
    return;

    let type_test = r#"
    type List = let B = let T = Int in (T, &B) in B in

    proc hmm() -> List = {
        ('~' as Int, Null)
    } in

    proc hmm2() -> List = {
        let ptr: &List = alloc(sizeof(List)) in {
            (*ptr) = hmm();
            ('?' as Int, ptr)
        }
    } in

    let x: let A = (Int, &A) in A
        = hmm2()
        in putchar(x.1->0 as Char)
    "#;
    match parse_lir(list) {
        Ok(expr) => {
            // eprintln!("{expr:?}");
            
            let expr = Expr::let_consts(vec![("inc", inc.clone()), ("dec", dec.clone()), ("lte", lte.clone()), ("lt", lt.clone()), ("swap", swap.clone()), ("free", free.clone()), ("alloc", alloc.clone()), ("max", max.clone()), ("min", min.clone()), ("putchar", put_char.clone()), ("getchar", get_char.clone()), ("putint", put_int.clone()), ("getint", get_int), ("putfloat", put_float), ("getfloat", get_float), ("put", put.clone())], expr);
            // let expr = put_float.clone().app(vec![Expr::let_consts(vec![("putint", put_int.clone()), ("getint", get_int), ("putfloat", put_float), ("getfloat", get_float)], expr.as_type(Type::Cell).as_type(Type::Float))]);
            // Compile the program to assembly
            let asm_code = expr.clone().compile().unwrap();
        
            let device = match asm_code {
                Ok(asm_core) => {
                    eprintln!("{:#?}", asm_core);
                    let vm_code = asm_core.assemble(100000).unwrap();
                    // println!("{:?}", vm_code);
                    println!("{}", targets::C.build_core(&vm_code).unwrap());
                    // eprintln!("{:?}", expr);
                    CoreInterpreter::new(StandardDevice)
                        .run(&vm_code)
                        .unwrap()
                }
                Err(asm_std) => {
                    eprintln!("{:#?}", asm_std);
                    let vm_code = asm_std.assemble(100000).unwrap();
                    let vm_code = vm_code.flatten();
                    // eprintln!("AFTER:\n{:?}", vm_code);
                    println!("{}", targets::C.build_std(&vm_code).unwrap());
                    // eprintln!("{:?}", expr);
                    StandardInterpreter::new(StandardDevice)
                        .run(&vm_code)
                        .unwrap()
                }
            };
        
            // Run the machine and get its I/O history with the
            // "world" (simulated through the testing device)
            // eprintln!("device: {:?}", device)
        }
        Err(e) => panic!("{e}")
    }
}

/*
fn main() {
    let alloc = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "alloc".to_string(),
        args: vec![("size".to_string(), Type::Int)],
        ret: Type::Pointer(Box::new(Type::Any)),
        body: vec![StandardOp::Alloc(SP.deref())],
    });
    let free = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "free".to_string(),
        args: vec![("ptr".to_string(), Type::Pointer(Box::new(Type::Any)))],
        ret: Type::None,
        body: vec![
            StandardOp::Free(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });
    // let put_float = ConstExpr::StandardBuiltin(StandardBuiltin {
    //     name: "put_float".to_string(),
    //     args: vec![],
    //     ret: Type::None,
    //     body: vec![
    //         StandardOp::PutFloat(SP.deref()),
    //         StandardOp::CoreOp(CoreOp::Pop(None, 1)),
    //     ],
    // });

    let put_int = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "put_int".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            StandardOp::PutInt(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });
    // let get_float = ConstExpr::StandardBuiltin(StandardBuiltin {
    //     name: "get_float".to_string(),
    //     args: vec![],
    //     ret: Type::Float,
    //     body: vec![
    //         StandardOp::CoreOp(CoreOp::Next(SP, None)),
    //         StandardOp::GetFloat(SP.deref()),
    //     ],
    // });

    // use maplit::btreemap;
    // let expr = put.app(vec![Expr::Tuple(vec![
    //     ConstExpr::Char('a').into(),
    //     Expr::Tuple(vec![
    //         ConstExpr::Bool(false).into(),
    //         ConstExpr::Int(2).into(),
    //         Expr::Array(vec![
    //             ConstExpr::Char('c').into(),
    //             ConstExpr::Char('d').into(),
    //             ConstExpr::Char('e').into(),
    //         ]),
    //     ]),
    //     Expr::structure(btreemap! {
    //         "x" => ConstExpr::Char('b').into(),
    //         "y" => ConstExpr::Int(3).into(),
    //         "z" => ConstExpr::Bool(true).into(),
    //     }),
    // ])
    // .field(ConstExpr::Int(1))
    // .field(ConstExpr::Int(2))
    // .idx(ConstExpr::Int(0))]);

    // The program to compile
    // let expr = put.app(vec![Expr::from(ConstExpr::Int(16))
    //     .add(ConstExpr::Float(1.2))
    //     .div(ConstExpr::Int(2))
    //     .mul(get.app(vec![]).as_type(Type::Int).as_type(Type::Float))
    //     .as_type(Type::Int)]);

    // use maplit::btreemap;
    // let expr = put.app(vec![Expr::let_type(
    //     "Point",
    //     Type::Struct(btreemap! {
    //         "x".to_string() => Type::Int,
    //         "y".to_string() => Type::Int,
    //     }),
    //     Expr::let_var(
    //         "a",
    //         Some(Type::Symbol("Point".to_string())),
    //         Expr::structure(btreemap! {
    //             "x" => ConstExpr::Int(1).into(),
    //             "y" => get.app(vec![]).mul(ConstExpr::Int(3)),
    //         }),
    //         Expr::var("a").field(var("y")),
    //     )
    // )]);

    // use maplit::btreemap;
    // let expr = put_int.clone().app(vec![Expr::let_type(
    //     "Node",
    //     Type::Struct(btreemap! {
    //         "data".to_string() => Type::Int,
    //         "next".to_string() => Type::Pointer(Box::new(Type::Symbol("Node".to_string()))),
    //     }),
    //     Expr::let_consts(
    //         btreemap! {
    //             "new" => ConstExpr::proc(
    //                 vec![(
    //                     "val".to_string(),
    //                     Type::Int
    //                 )],
    //                 Type::Symbol("Node".to_string()),
    //                 Expr::structure(btreemap! {
    //                     "data" => Expr::var("val"),
    //                     "next" => ConstExpr::Null.into(),
    //                 }),
    //             ),
    //             "free" => ConstExpr::proc(
    //                 vec![(
    //                     "node".to_string(),
    //                     Type::Symbol("Node".to_string()),
    //                 )],
    //                 Type::None,
    //                 Expr::Many(vec![
    //                     put_int.app(vec![Expr::var("node").field(var("data"))]),
    //                     Expr::var("node").field(var("next"))
    //                         .as_type(Type::Cell)
    //                         .add(ConstExpr::Int(1000))
    //                         .if_then(
    //                             Expr::Many(vec![
    //                                 Expr::var("free").app(vec![Expr::var("node").field(var("next")).deref()]),
    //                                 free.app(vec![Expr::var("node").field(var("next"))]),
    //                             ]),
    //                             ConstExpr::None,
    //                         )
    //                 ])
    //             ),
    //             "next" => ConstExpr::proc(
    //                 vec![(
    //                     "node".to_string(),
    //                     Type::Symbol("Node".to_string()),
    //                 )],
    //                 Type::Symbol("Node".to_string()),
    //                 Expr::var("node").field(var("next")).deref(),
    //             ),
    //             "cons" => ConstExpr::proc(
    //                 vec![
    //                     (
    //                         "head".to_string(),
    //                         Type::Symbol("Node".to_string()),
    //                     ),
    //                     (
    //                         "tail".to_string(),
    //                         Type::Symbol("Node".to_string()),
    //                     ),
    //                 ],
    //                 Type::Symbol("Node".to_string()),
    //                 Expr::let_var(
    //                     "ptr",
    //                     None,
    //                     alloc.app(vec![
    //                         Expr::var("tail").size_of()
    //                     ]),
    //                     Expr::Many(vec![
    //                         Expr::var("ptr").deref_mut(Expr::var("tail")),
    //                         Expr::structure(btreemap! {
    //                             "data" => Expr::var("head").field(var("data")),
    //                             "next" => Expr::var("ptr"),
    //                         })
    //                     ])
    //                 )
    //             ),
    //         },
    //         Expr::Many(vec![
    //             Expr::var("free")
    //                 .app(vec![Expr::var("cons").app(
    //                             vec![
    //                                 Expr::var("new").app(vec![ConstExpr::Int(3).into()]),
    //                                 Expr::var("cons").app(vec![
    //                                     Expr::var("new").app(vec![ConstExpr::Int(5).into()]),
    //                                     Expr::var("new").app(vec![ConstExpr::Int(7).into()]),
    //                                 ]),
    //                             ],
    //                     )
    //                 ]),
    //             ConstExpr::Int(0).into()
    //         ])
    //     ),
    // )]);

    use maplit::btreemap;
    let alloc = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "alloc".to_string(),
        args: vec![("size".to_string(), Type::Int)],
        ret: Type::Pointer(Box::new(Type::Any)),
        body: vec![StandardOp::Alloc(SP.deref())],
    });

    let free = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "free".to_string(),
        args: vec![("ptr".to_string(), Type::Pointer(Box::new(Type::Any)))],
        ret: Type::None,
        body: vec![
            StandardOp::Free(SP.deref()),
            StandardOp::CoreOp(CoreOp::Pop(None, 1)),
        ],
    });

    let put = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_int.clone().app(vec![Expr::let_type(
        "Node",
        Type::Struct(btreemap! {
            "data".to_string() => Type::Int,
            "next".to_string() => Type::Pointer(Box::new(Type::Symbol("Node".to_string()))),
        }),
        Expr::let_procs(
            btreemap! {
                "new" => Procedure::new(
                    vec![(
                        "val".to_string(),
                        Type::Int
                    )],
                    Type::Symbol("Node".to_string()),
                    Expr::structure(btreemap! {
                        "data" => Expr::var("val"),
                        "next" => ConstExpr::Null.into(),
                    }),
                ),
                "free" => Procedure::new(
                    vec![(
                        "node".to_string(),
                        Type::Symbol("Node".to_string()),
                    )],
                    Type::None,
                    Expr::Many(vec![
                        put_int.clone().app(vec![Expr::var("node").field(var("data"))]),
                        Expr::var("node").field(var("next"))
                            .as_type(Type::Cell)
                            .sub(Expr::from(ConstExpr::Null).as_type(Type::Cell))
                            .if_then(
                                Expr::Many(vec![
                                    Expr::var("free").app(vec![Expr::var("node").field(var("next")).deref()]),
                                    free.app(vec![Expr::var("node").field(var("next"))]),
                                ]),
                                ConstExpr::None,
                            )
                    ])
                ),
                "cons" => Procedure::new(
                    vec![
                        (
                            "head".to_string(),
                            Type::Symbol("Node".to_string()),
                        ),
                        (
                            "tail".to_string(),
                            Type::Symbol("Node".to_string()),
                        ),
                    ],
                    Type::Symbol("Node".to_string()),
                    Expr::let_var(
                        "ptr",
                        None,
                        alloc.app(vec![
                            Expr::var("tail").size_of()
                        ]),
                        Expr::Many(vec![
                            Expr::var("ptr").deref_mut(Expr::var("tail")),
                            Expr::structure(btreemap! {
                                "data" => Expr::var("head").field(var("data")),
                                "next" => Expr::var("ptr"),
                            })
                        ])
                    )
                ),
            },
            Expr::Many(vec![
                Expr::var("free")
                    .app(vec![Expr::var("cons").app(
                            vec![
                                Expr::var("new").app(vec![ConstExpr::Int(3).into()]),
                                Expr::var("cons").app(vec![
                                    Expr::var("new").app(vec![ConstExpr::Int(5).into()]),
                                    Expr::var("new").app(vec![ConstExpr::Int(7).into()]),
                                ]),
                            ],
                        )
                    ]),
                ConstExpr::Int(0).into()
            ])
        ),
    )]);

    // let expr = put.app(vec![
    //     Expr::let_var(
    //         "first",
    //         Some(Type::Let(
    //             "List<Int>".to_string(),
    //             Box::new(Type::Tuple(vec![
    //                 Type::Int,
    //                 Type::Union(btreemap! {
    //                     "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
    //                     "None".to_string() => Type::None,
    //                 }),
    //             ])),
    //             Box::new(Type::Symbol("List<Int>".to_string())),
    //         )),
    //         Expr::Tuple(vec![ConstExpr::Int(3).into(), ConstExpr::Union(
    //             Type::Union(btreemap! {
    //                 "Some".to_string() => Type::Pointer(Box::new(Type::Let(
    //                     "List<Int>".to_string(),
    //                     Box::new(Type::Tuple(vec![
    //                         Type::Int,
    //                         Type::Union(btreemap! {
    //                             "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
    //                             "None".to_string() => Type::None,
    //                         }),
    //                     ])),
    //                     Box::new(Type::Symbol("List<Int>".to_string())),
    //                 ))),
    //                 "None".to_string() => Type::None,
    //             }),
    //             "None".to_string(),
    //             Box::new(ConstExpr::None),
    //         ).into()]),
    //         Expr::let_var(
    //             "second",
    //             Some(Type::Let(
    //                 "List<Int>".to_string(),
    //                 Box::new(Type::Tuple(vec![
    //                     Type::Int,
    //                     Type::Union(btreemap! {
    //                         "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
    //                         "None".to_string() => Type::None,
    //                     }),
    //                 ])),
    //                 Box::new(Type::Symbol("List<Int>".to_string())),
    //             )),
    //             Expr::Tuple(vec![ConstExpr::Int(5).into(), Expr::Union(
    //                 Type::Union(btreemap! {
    //                     "Some".to_string() => Type::Pointer(Box::new(Type::Let(
    //                         "List<Int>".to_string(),
    //                         Box::new(Type::Tuple(vec![
    //                             Type::Int,
    //                             Type::Union(btreemap! {
    //                                 "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
    //                                 "None".to_string() => Type::None,
    //                             }),
    //                         ])),
    //                         Box::new(Type::Symbol("List<Int>".to_string())),
    //                     ))),
    //                     "None".to_string() => Type::None,
    //                 }),
    //                 "Some".to_string(),
    //                 Box::new(Expr::var("first").refer()),
    //             ).into()]),
    //             Expr::var("second")
    //                 .field(ConstExpr::Int(1))
    //                 .field(ConstExpr::Symbol("Some".to_string()))
    //                 .deref()
    //                 .field(ConstExpr::Int(0)),
    //         )
    //     ),
    // ]);

    let expr = Expr::let_var(
        "first",
        Some(Type::Let(
            "List<Int>".to_string(),
            Box::new(Type::Tuple(vec![
                Type::Int,
                Type::Union(btreemap! {
                    "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
                    "None".to_string() => Type::None,
                }),
            ])),
            Box::new(Type::Symbol("List<Int>".to_string())),
        )),
        Expr::Tuple(vec![ConstExpr::Int(3).into(), ConstExpr::Union(
            Type::Union(btreemap! {
                "Some".to_string() => Type::Pointer(Box::new(Type::Let(
                    "List<Int>".to_string(),
                    Box::new(Type::Tuple(vec![
                        Type::Int,
                        Type::Union(btreemap! {
                            "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
                            "None".to_string() => Type::None,
                        }),
                    ])),
                    Box::new(Type::Symbol("List<Int>".to_string())),
                ))),
                "None".to_string() => Type::None,
            }),
            "None".to_string(),
            Box::new(ConstExpr::None),
        ).into()]),
        Expr::let_var(
            "second",
            Some(Type::Let(
                "T".to_string(),
                Box::new(Type::Int),
                Box::new(Type::Let(
                    "List<Int>".to_string(),
                    Box::new(Type::Tuple(vec![
                        Type::Symbol("T".to_string()),
                        Type::Union(btreemap! {
                            "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
                            "None".to_string() => Type::None,
                        }),
                    ])),
                    Box::new(Type::Symbol("List<Int>".to_string())),
                ))
            )),
            Expr::Tuple(vec![ConstExpr::Int(5).into(), Expr::Union(
                Type::Union(btreemap! {
                    "Some".to_string() => Type::Pointer(Box::new(Type::Let(
                        "Hmm".to_string(),
                        Box::new(Type::Tuple(vec![
                            Type::Int,
                            Type::Union(btreemap! {
                                "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("Hmm".to_string()))),
                                "None".to_string() => Type::None,
                            }),
                        ])),
                        Box::new(Type::Symbol("Hmm".to_string())),
                    ))),
                    "None".to_string() => Type::None,
                }),
                "Some".to_string(),
                Box::new(Expr::var("first").refer()),
            ).into()]),
            Expr::Many(vec![
                put_int.clone().app(vec![Expr::var("second")
                    .field(ConstExpr::Int(1))
                    .field(ConstExpr::Symbol("Some".to_string()))
                    .deref()
                    .field(ConstExpr::Int(0))]),
                Expr::var("second")
                    .field(ConstExpr::Int(1))
                    .field(ConstExpr::Symbol("Some".to_string()))
                    .refer()
                    .deref_mut(Expr::var("second").refer()),
                put_int.clone().app(vec![Expr::var("second")
                    .field(ConstExpr::Int(1))
                    .field(ConstExpr::Symbol("Some".to_string()))
                    .deref()
                    .field(ConstExpr::Int(1))
                    .field(ConstExpr::Symbol("Some".to_string()))
                    .deref()
                    .field(ConstExpr::Int(0))
                ]),
            ])
        )
    );

    let env = Env::default();

    assert!(Type::Let(
        "A".to_string(),
        Box::new(Type::Tuple(vec![
            Type::Int,
            Type::Union(btreemap! {
                "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("A".to_string()))),
                "None".to_string() => Type::None,
            }),
        ])),
        Box::new(Type::Symbol("A".to_string())),
    ).equals(&Type::Let(
        "T".to_string(),
        Box::new(Type::Int),
        Box::new(Type::Let(
            "C".to_string(),
            Box::new(Type::Tuple(vec![
                Type::Symbol("T".to_string()),
                Type::Union(btreemap! {
                    "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("A".to_string()))),
                    "None".to_string() => Type::None,
                }),
            ])),
            Box::new(Type::Symbol("C".to_string()
        )),
    ))), &env).unwrap());

    // let expr = put_int.app(vec![Expr::let_proc(
    //     "factorial",
    //     Procedure::new(
    //         vec![("n".to_string(), Type::Int)],
    //         Type::Int,
    //         Expr::var("n").if_then(
    //             Expr::var("factorial")
    //                 .app(vec![Expr::var("n").sub(ConstExpr::Int(1))])
    //                 .mul(var("n")),
    //             ConstExpr::Int(1),
    //         )
    //     ),
    //     Expr::var("factorial").app(vec![ConstExpr::Int(5).into()]),
    // )]);

    // use maplit::btreemap;
    // let expr = put.app(vec![Expr::Tuple(vec![
    //     ConstExpr::Char('a').into(),
    //     Expr::Tuple(vec![
    //         ConstExpr::Bool(false).into(),
    //         ConstExpr::Int(2).into(),
    //         Expr::Array(vec![
    //             ConstExpr::Char('c').into(),
    //             ConstExpr::Char('d').into(),
    //             ConstExpr::Char('e').into(),
    //         ]),
    //     ]),
    //     Expr::structure(btreemap! {
    //         "x" => ConstExpr::Char('b').into(),
    //         "y" => ConstExpr::Int(3).into(),
    //         "z" => ConstExpr::Bool(true).into(),
    //     }),
    // ])
    // .field(ConstExpr::Int(1))
    // .field(ConstExpr::Int(2))
    // .idx(ConstExpr::Int(0))]);

    // Compile the program to assembly
    let asm_code = expr.compile().unwrap();

    let input = vec![15];

    let device = match asm_code {
        Ok(asm_core) => {
            eprintln!("{:?}", asm_core);
            let vm_code = asm_core.assemble(16).unwrap();
            eprintln!("{:?}", vm_code);
            println!("{}", C.build_core(&vm_code).unwrap());
            CoreInterpreter::new(TestingDevice::new_raw(input))
                .run(&vm_code)
                .unwrap()
        }
        Err(asm_std) => {
            eprintln!("{:?}", asm_std);
            let vm_code = asm_std.assemble(16).unwrap();
            eprintln!("{:?}", vm_code);
            println!("{}", C.build_std(&vm_code).unwrap());
            StandardInterpreter::new(TestingDevice::new_raw(input))
                .run(&vm_code)
                .unwrap()
        }
    };

    // Run the machine and get its I/O history with the
    // "world" (simulated through the testing device)
    eprintln!("device: {:?}", device)
}
*/
