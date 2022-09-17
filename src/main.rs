use acid::{
    asm::{CoreOp, StandardOp, A, B, C, SP},
    lir::*,
    parse::*,
    targets,
    targets::Target,
    vm::{CoreInterpreter, StandardDevice, StandardInterpreter, TestingDevice},
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
            CoreOp::IsGreater {
                dst: A,
                a: SP.deref().offset(-1),
                b: SP.deref(),
            },
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
            CoreOp::IsGreater {
                dst: A,
                a: SP.deref().offset(-1),
                b: SP.deref(),
            },
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
            CoreOp::IsLess {
                a: SP.deref().offset(-1),
                b: SP.deref(),
                dst: A,
            },
            CoreOp::Pop(None, 1),
            CoreOp::Move {
                src: A,
                dst: SP.deref(),
            },
        ],
    });
    let lte = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "lte".to_string(),
        args: vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)],
        ret: Type::Bool,
        body: vec![
            CoreOp::IsLessEqual {
                a: SP.deref().offset(-1),
                b: SP.deref(),
                dst: A,
            },
            CoreOp::Pop(None, 1),
            CoreOp::Move {
                src: A,
                dst: SP.deref(),
            },
        ],
    });
    let inc = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "inc".to_string(),
        args: vec![("x".to_string(), Type::Pointer(Box::new(Type::Int)))],
        ret: Type::None,
        body: vec![CoreOp::Inc(SP.deref().deref()), CoreOp::Pop(None, 1)],
    });
    let dec = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "dec".to_string(),
        args: vec![("x".to_string(), Type::Pointer(Box::new(Type::Int)))],
        ret: Type::None,
        body: vec![CoreOp::Dec(SP.deref().deref()), CoreOp::Pop(None, 1)],
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
    let fn_ptr = "proc test(order: proc(Int, Int) -> Int, x: Int, y: Int) -> Int = { order(x, y) } in put(test(proc(x: Int, y: Int) -> Int = {x}, 'a' as Int, 'b' as Int))";

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

    set A, 10
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

    // match parse_asm(bitwise_test) {
    //     Ok(asm_code) => {
    //         let device = match asm_code {
    //             Ok(asm_core) => {
    //                 // eprintln!("{:?}", asm_core);
    //                 let vm_code = asm_core.assemble(5000).unwrap();
    //                 // eprintln!("{:?}", vm_code);
    //                 println!("{}", targets::C.build_core(&vm_code).unwrap());
    //                 // eprintln!("{:?}", vm_code.0.len());
    //                 CoreInterpreter::new(StandardDevice)
    //                     .run(&vm_code)
    //                     .unwrap()
    //             }
    //             Err(asm_std) => {
    //                 // eprintln!("{:?}", asm_std);
    //                 let vm_code = asm_std.assemble(5000).unwrap();
    //                 // eprintln!("{:?}", vm_code);
    //                 println!("{}", targets::C.build_std(&vm_code).unwrap());
    //                 // eprintln!("{:?}", vm_code.0.len());
    //                 StandardInterpreter::new(StandardDevice)
    //                     .run(&vm_code)
    //                     .unwrap()
    //             }
    //         };
    //     }
    //     Err(e) => {
    //         eprintln!("{e}");
    //     }
    // }
    // return;

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

    proc hmm3() -> List = {
        let ptr: &List = alloc(sizeof(List)) in {
            (*ptr) = hmm2();
            ('!' as Int, ptr)
        }
    } in

    let x: let A = (Int, &A) in A
        = hmm3()
        in putchar(x.0 as Char)
    "#;

    let bitops_test: &str = r#"
const bitxor = core(a: Int, b: Int) -> Int = {
    bitwise-xor [SP], [SP - 1] pop
}, bitor = core(a: Int, b: Int) -> Int = {
    bitwise-or [SP], [SP - 1] pop
}, bitnand = core(a: Int, b: Int) -> Int = {
    bitwise-nand [SP], [SP - 1] pop
}, bitand = core(a: Int, b: Int) -> Int = {
    bitwise-and [SP], [SP - 1] pop
}, bitnot = core(x: Int) -> Int = {
    bitwise-not [SP]
} in let a = getint(),
         b = getint() in {
    putint(a); putchar(' '); putint(b); putchar(10 as Char);
    putint(bitnand(a, b)); putchar(10 as Char);
    putint(bitand(a, b)); putchar(10 as Char);
    putint(bitor(a, b)); putchar(10 as Char);
    putint(bitxor(a, b)); putchar(10 as Char);
}
    "#;

    let fact_asm_test: &str = r#"
const asm_init = proc() -> None = core {
    fun FACT
        set A, 0
        gt [SP], A, B
        if B
            push [SP]
            dec [SP]
            call FACT
            mul [SP], [SP - 1]
            pop
        else
            set [SP], 1
        end
    end
}, fact = proc(n: Int) -> Int = core {
    call FACT
} in {
    asm_init();

    putint(fact(5)); putchar('\n');
    putint(fact(10)); putchar('\n');
}
    "#;

    let AES_implementation: &str = r#"
const bitxor = proc(a: Int, b: Int) -> Int = core {
    bitwise-xor [SP], [SP - 1] pop
}, bitor = proc(a: Int, b: Int) -> Int = core {
    bitwise-or [SP], [SP - 1] pop
}, bitnand = proc(a: Int, b: Int) -> Int = core {
    bitwise-nand [SP], [SP - 1] pop
}, bitand = proc(a: Int, b: Int) -> Int = core {
    bitwise-and [SP], [SP - 1] pop
}, bitnot = proc(x: Int) -> Int = core {
    bitwise-not [SP]
}, lt = proc(a: Int, b: Int) -> Bool = core {
    lt [SP - 1], [SP], A pop
    mov A, [SP]
}, gt = proc(a: Int, b: Int) -> Bool = core {
    gt [SP - 1], [SP], A pop
    mov A, [SP]
}, eq = proc(a: Int, b: Int) -> Bool = core {
    eq [SP - 1], [SP], A pop
    mov A, [SP]
}, neq = proc(a: Int, b: Int) -> Bool = core {
    neq [SP - 1], [SP], A pop
    mov A, [SP]
}, and = proc(a: Bool, b: Bool) -> Bool = core {
    and [SP], [SP - 1] pop
} in

proc putdec(n: Int) -> None = {
    if (lt(n, 10)) {
        put(n + 48)
    } else {
        putdec(n / 10);
        put(n % 10 + 48)
    }
} in

proc puthex(n: Int, lower: Bool) -> None = {
    if (lt(n, 16)) {
        put(n + if (lt(n, 10)) {
            48
        } else {
            if lower {
                87
            } else {
                55
            }
        })
    } else {
        puthex(n / 16, lower);
        puthex(n % 16, lower)
    }
} in

proc putbyte(n: Int, lower: Bool) -> None = {
    puthex(n / 16, lower);
    puthex(n % 16, lower)
} in

const putint = proc(n: Int) -> None = { putbyte(n, false) } in

const putchar = proc(ch: Char) -> None = {
    put(ch as Int)
} in

proc ln() -> None = {
    putchar('\n')
} in

proc lsh(a: Int, b: Int) -> Int = {
    while b {
        a = a * 2;
        b = b - 1;
    };
    a
} in

proc rsh(a: Int, b: Int) -> Int = {
    while b {
        a = a / 2;
        b = b - 1;
    };
    a
} in

proc xtime(n: Int) -> Int = {
    n = bitand(n, 255) * 2;
    if (lt(n, 256)) n
    else (bitxor(n, 27))
} in

proc mul(a: Int, b: Int) -> Int = {
    a = bitand(a, 255);
    b = bitand(b, 255);
    let result = 0,
        next_term = a,
        i = 0 in {
        while (lt(i, 8)) {
            if (bitand(rsh(b, i), 1)) {
                result = bitxor(result, next_term);
            };
            next_term = xtime(next_term);
            i = i + 1;
        };
        bitand(result, 255)
    }
} in

type State = [Int * 16] in

proc row_major(c0r0: Int, c1r0: Int, c2r0: Int, c3r0: Int,
    c0r1: Int, c1r1: Int, c2r1: Int, c3r1: Int,
    c0r2: Int, c1r2: Int, c2r2: Int, c3r2: Int,
    c0r3: Int, c1r3: Int, c2r3: Int, c3r3: Int
) -> State = {[
    c0r0,
    c0r1,
    c0r2,
    c0r3,

    c1r0,
    c1r1,
    c1r2,
    c1r3,

    c2r0,
    c2r1,
    c2r2,
    c2r3,

    c3r0,
    c3r1,
    c3r2,
    c3r3,
]} in

proc col_major(c0r0: Int, c0r1: Int, c0r2: Int, c0r3: Int,
    c1r0: Int, c1r1: Int, c1r2: Int, c1r3: Int,
    c2r0: Int, c2r1: Int, c2r2: Int, c2r3: Int,
    c3r0: Int, c3r1: Int, c3r2: Int, c3r3: Int,
) -> State = {[
    c0r0,
    c0r1,
    c0r2,
    c0r3,

    c1r0,
    c1r1,
    c1r2,
    c1r3,

    c2r0,
    c2r1,
    c2r2,
    c2r3,

    c3r0,
    c3r1,
    c3r2,
    c3r3,
]} in

proc get(self: &State, x: Int, y: Int) -> Int = {
    self[0][x + y * 4]
} in

proc set(self: &State, x: Int, y: Int, val: Int) -> None = {
    self[0][x + y * 4] = val;
} in

proc putstate(self: State) -> None = {
    let i = 0, j = 0 in {
        while lt(i, 4) {
            while lt(j, 4) {
                putint(get(&self, j, i)); putchar(' ');
                j = j + 1;
            }; putchar('\n');
            j = 0;
            i = i + 1;
        }
    }
} in

proc rot_row(self: State, y: Int, n: Int) -> State = {
    while n {
        let a = get(&self, 0, y),
            b = get(&self, 1, y),
            c = get(&self, 2, y),
            d = get(&self, 3, y) in {
            set(&self, 0, y, b);
            set(&self, 1, y, c);
            set(&self, 2, y, d);
            set(&self, 3, y, a);
        };
        n = n - 1;
    };
    self
} in

proc shift_rows(self: State) -> State = {
    rot_row(rot_row(rot_row(self, 1, 1), 2, 2), 3, 3)
} in

proc inverse_shift_rows(self: State) -> State = {
    rot_row(rot_row(rot_row(self, 1, 3), 2, 2), 3, 1)
} in

proc add_round_key(self: State, round_key: State) -> State = {
    let i = 0 in {
        while lt(i, 16) {
            ((&self) as &Int)[i] = bitxor(((&self) as &Int)[i], ((&round_key) as &Int)[i]);
            i = i + 1;
        };
        self
    }
} in

proc mix_columns(self: State) -> State = {
    let j = 0 in while lt(j, 4) {
        let s0 = bitxor(bitxor(bitxor(mul(get(&self, j, 0), 2), mul(get(&self, j, 1), 3)), get(&self, j, 2)), get(&self, j, 3)),
            s1 = bitxor(bitxor(bitxor(get(&self, j, 0), mul(get(&self, j, 1), 2)), mul(get(&self, j, 2), 3)), get(&self, j, 3)),
            s2 = bitxor(bitxor(bitxor(get(&self, j, 0), get(&self, j, 1)), mul(get(&self, j, 2), 2)), mul(get(&self, j, 3), 3)),
            s3 = bitxor(bitxor(bitxor(mul(get(&self, j, 0), 3), get(&self, j, 1)), get(&self, j, 2)), mul(get(&self, j, 3), 2)) in {
            
            set(&self, j, 0, s0);
            set(&self, j, 1, s1);
            set(&self, j, 2, s2);
            set(&self, j, 3, s3);
        };
        j = j + 1;
    };
    self
} in

proc inverse_mix_columns(self: State) -> State = {
    let j = 0 in while lt(j, 4) {
        let s0 = bitxor(bitxor(bitxor(mul(get(&self, j, 0), 14), mul(get(&self, j, 1), 11)), mul(get(&self, j, 2), 13)), mul(get(&self, j, 3), 9)),
            s1 = bitxor(bitxor(bitxor(mul(get(&self, j, 0), 9),  mul(get(&self, j, 1), 14)), mul(get(&self, j, 2), 11)), mul(get(&self, j, 3), 13)),
            s2 = bitxor(bitxor(bitxor(mul(get(&self, j, 0), 13), mul(get(&self, j, 1), 9)),  mul(get(&self, j, 2), 14)), mul(get(&self, j, 3), 11)),
            s3 = bitxor(bitxor(bitxor(mul(get(&self, j, 0), 11), mul(get(&self, j, 1), 13)), mul(get(&self, j, 2), 9)),  mul(get(&self, j, 3), 14)) in {
            
            set(&self, j, 0, s0);
            set(&self, j, 1, s1);
            set(&self, j, 2, s2);
            set(&self, j, 3, s3);
        };
        j = j + 1;
    };
    self
} in

proc sub_byte(byte: Int) -> Int = {
    let S_BOX = [[99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 254, 215, 171, 118], [202, 130, 201, 125, 250, 89, 71, 240, 173, 212, 162, 175, 156, 164, 114, 192], [183, 253, 147, 38, 54, 63, 247, 204, 52, 165, 229, 241, 113, 216, 49, 21], [4, 199, 35, 195, 24, 150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117], [9, 131, 44, 26, 27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132], [83, 209, 0, 237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207], [208, 239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168], [81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 243, 210], [205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 100, 93, 25, 115], [96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 184, 20, 222, 94, 11, 219], [224, 50, 58, 10, 73, 6, 36, 92, 194, 211, 172, 98, 145, 149, 228, 121], [231, 200, 55, 109, 141, 213, 78, 169, 108, 86, 244, 234, 101, 122, 174, 8], [186, 120, 37, 46, 28, 166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138], [112, 62, 181, 102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158], [225, 248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 223], [140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 84, 187, 22]] in {
        byte = bitand(byte, 255);
        let row = bitand(rsh(byte, 4), 15),
            col = bitand(byte, 15) in
            S_BOX[row][col]
    }
} in

proc inverse_sub_byte(byte: Int) -> Int = {
    let INVERSE_S_BOX = [[82, 9, 106, 213, 48, 54, 165, 56, 191, 64, 163, 158, 129, 243, 215, 251], [124, 227, 57, 130, 155, 47, 255, 135, 52, 142, 67, 68, 196, 222, 233, 203], [84, 123, 148, 50, 166, 194, 35, 61, 238, 76, 149, 11, 66, 250, 195, 78], [8, 46, 161, 102, 40, 217, 36, 178, 118, 91, 162, 73, 109, 139, 209, 37], [114, 248, 246, 100, 134, 104, 152, 22, 212, 164, 92, 204, 93, 101, 182, 146], [108, 112, 72, 80, 253, 237, 185, 218, 94, 21, 70, 87, 167, 141, 157, 132], [144, 216, 171, 0, 140, 188, 211, 10, 247, 228, 88, 5, 184, 179, 69, 6], [208, 44, 30, 143, 202, 63, 15, 2, 193, 175, 189, 3, 1, 19, 138, 107], [58, 145, 17, 65, 79, 103, 220, 234, 151, 242, 207, 206, 240, 180, 230, 115], [150, 172, 116, 34, 231, 173, 53, 133, 226, 249, 55, 232, 28, 117, 223, 110], [71, 241, 26, 113, 29, 41, 197, 137, 111, 183, 98, 14, 170, 24, 190, 27], [252, 86, 62, 75, 198, 210, 121, 32, 154, 219, 192, 254, 120, 205, 90, 244], [31, 221, 168, 51, 136, 7, 199, 49, 177, 18, 16, 89, 39, 128, 236, 95], [96, 81, 127, 169, 25, 181, 74, 13, 45, 229, 122, 159, 147, 201, 156, 239], [160, 224, 59, 77, 174, 42, 245, 176, 200, 235, 187, 60, 131, 83, 153, 97], [23, 43, 4, 126, 186, 119, 214, 38, 225, 105, 20, 99, 85, 33, 12, 125]] in {
        byte = bitand(byte, 255);
        let row = bitand(rsh(byte, 4), 15),
            col = bitand(byte, 15) in
            INVERSE_S_BOX[row][col]
    }
} in


proc sub_bytes(self: State) -> State = {
    let i = 0, j = 0 in {
        while lt(i, 4) {
            while lt(j, 4) {
                set(&self, j, i, sub_byte(get(&self, j, i)));
                j = j + 1;
            };
            j = 0;
            i = i + 1;
        };
    };
    self
} in

proc inverse_sub_bytes(self: State) -> State = {
    let i = 0, j = 0 in {
        while lt(i, 4) {
            while lt(j, 4) {
                set(&self, j, i, inverse_sub_byte(get(&self, j, i)));
                j = j + 1;
            };
            j = 0;
            i = i + 1;
        };
    };
    self
} in

type AESKeyType = enum {AES256, AES192, AES128} in
type AESKeyData = union {
    AES256: [Int * 32],
    AES192: [Int * 24],
    AES128: [Int * 16]
} in
type Key = struct {
    ty: AESKeyType,
    key: AESKeyData
} in

proc get_key_len(key: Key) -> Int = {
    if (eq(key.ty as Int, AES256 of AESKeyType as Int)) 8
    else (if (eq(key.ty as Int, AES192 of AESKeyType as Int)) 6
    else 4)
} in

proc get_key_rounds(key: Key) -> Int = {
    if (eq(key.ty as Int, AES256 of AESKeyType as Int)) 14
    else (if (eq(key.ty as Int, AES192 of AESKeyType as Int)) 12
    else 10)
} in

proc get_key_size(key: Key) -> Int = {
    if (eq(key.ty as Int, AES256 of AESKeyType as Int)) 32
    else (if (eq(key.ty as Int, AES192 of AESKeyType as Int)) 24
    else 16)
} in

proc get_key_data(key: &Key) -> &Int = {
    (&key->key) as &Int
} in

proc putkey(key: Key) -> None = {
    if (eq(key.ty as Int, AES256 of AESKeyType as Int)) {
        putchar('A');
        putchar('E');
        putchar('S');
        putchar('2');
        putchar('5');
        putchar('6');
    } else (if (eq(key.ty as Int, AES192 of AESKeyType as Int)) {
        putchar('A');
        putchar('E');
        putchar('S');
        putchar('1');
        putchar('9');
        putchar('2');
    } else  {
        putchar('A');
        putchar('E');
        putchar('S');
        putchar('1');
        putchar('2');
        putchar('8');
    });

    let i = 0, size = get_key_size(key), data = get_key_data(&key) in 
        while lt(i, size) {
            putchar(' ');
            putint(data[i]);
            i = i + 1;
        }
} in

proc sub_word(word: Int) -> Int = {
    let byte0 = sub_byte(bitand(rsh(word, 24), 255)),
        byte1 = sub_byte(bitand(rsh(word, 16), 255)),
        byte2 = sub_byte(bitand(rsh(word, 8), 255)),
        byte3 = sub_byte(bitand(word, 255)) in
        (lsh(byte0, 24) + lsh(byte1, 16) + lsh(byte2, 8) + byte3)
} in

proc rot_word(word: Int) -> Int = {
    let byte0 = bitand(rsh(word, 24), 255),
        byte1 = bitand(rsh(word, 16), 255),
        byte2 = bitand(rsh(word, 8), 255),
        byte3 = bitand(word, 255) in
        (lsh(byte1, 24) + lsh(byte2, 16) + lsh(byte3, 8) + byte0)
} in

proc word(byte3: Int, byte2: Int, byte1: Int, byte0: Int) -> Int = {
    (lsh(byte3, 24) + lsh(byte2, 16) + lsh(byte1, 8) + byte0)
} in

proc bytes(word: Int) -> [Int * 4] = {
    let byte0 = bitand(rsh(word, 24), 255),
        byte1 = bitand(rsh(word, 16), 255),
        byte2 = bitand(rsh(word, 8), 255),
        byte3 = bitand(word, 255) in
        [byte0, byte1, byte2, byte3]
} in

proc round_key(word0: Int, word1: Int, word2: Int, word3: Int) -> State = {
    let bytes0 = bytes(word0),
        bytes1 = bytes(word1),
        bytes2 = bytes(word2),
        bytes3 = bytes(word3) in
            col_major(bytes0[0], bytes1[0], bytes2[0], bytes3[0],
            bytes0[1], bytes1[1], bytes2[1], bytes3[1],
            bytes0[2], bytes1[2], bytes2[2], bytes3[2],
            bytes0[3], bytes1[3], bytes2[3], bytes3[3])
} in

const alloc = proc(size: Int) -> &Int = core {
    pop B
    set A, 10000 push A
    add B, [[SP]]
    mov [[SP]], [SP]
} in

const free = proc(ptr: &Int) -> None = {} in

proc key_expansion(key: Key, nr: Int, nk: Int) -> &Int = {
    let key_data = get_key_data(&key),
        w: &Int = alloc(4 * (nr + 1) * sizeof(Int)),
        i = 0 in {
        while lt(i, nk) {
            (w[i]) = word(
                key_data[4 * i],
                key_data[4 * i + 1],
                key_data[4 * i + 2],
                key_data[4 * i + 3],
            );
            i = i + 1;
        };

        i = nk;
        let rcon = [1, 2, 4, 8, 16, 32, 64, 128, 27, 54] in {
            while lt(i, 4 * (nr + 1)) {
                let tmp = w[i - 1] in {
                    if (eq(i % nk, 0)) {
                        tmp = bitxor(sub_word(rot_word(tmp)), word(rcon[i / nk - 1], 0, 0, 0));
                    } else {
                        if (and(eq(nk, 8), eq(i % nk, 4))) {
                            tmp = sub_word(tmp);
                        }
                    };
                    w[i] = bitxor(w[i - nk], tmp);
                };
                i = i + 1;
            }
        };
        w
    }
} in

proc aes_256_key(key: [Int * 32]) -> Key = {
    struct {
        ty = AES256 of AESKeyType,
        key = union {
            AES256 = key,
            AESKeyData..
        }
    }
} in

proc aes_192_key(key: [Int * 24]) -> Key = {
    struct {
        ty = AES192 of AESKeyType,
        key = union {
            AES192 = key,
            AESKeyData..
        }
    }
} in

proc aes_128_key(key: [Int * 16]) -> Key = {
    struct {
        ty = AES128 of AESKeyType,
        key = union {
            AES128 = key,
            AESKeyData..
        }
    }
} in

proc cipher(self: State, key: Key) -> State = {
    let result = self,
        i = 4,
        len = get_key_len(key),
        rounds = get_key_rounds(key),
        k = key_expansion(key, rounds, len)
        in {

        result = add_round_key(result, round_key(k[0], k[1], k[2], k[3]));
        while lt(i, 4 * (rounds + 1)) {
            let rk = round_key(k[i], k[i + 1], k[i + 2], k[i + 3]) in {
                result = shift_rows(sub_bytes(result));
                if (neq(i / 4, rounds)) {
                    result = mix_columns(result);
                };
                result = add_round_key(result, rk);
            };
            i = i + 4;
        };
        free(k);
        result
    }
} in

proc inverse_cipher(self: State, key: Key) -> State = {
    let result = self,
        len = get_key_len(key),
        rounds = get_key_rounds(key),
        i = 4 * rounds,
        k = key_expansion(key, rounds, len)
        in {

        while gt(i, 0) {
            let rk = round_key(k[i], k[i + 1], k[i + 2], k[i + 3]) in {
                result = add_round_key(result, rk);
                if (neq(i / 4, rounds)) {
                    result = inverse_mix_columns(result);
                };
                result = inverse_sub_bytes(inverse_shift_rows(result));
            };
            i = i - 4;
        };
        result = add_round_key(result, round_key(k[0], k[1], k[2], k[3]));
        free(k);
        result
    }
} in

let state = row_major(
    0, 1, 2, 3,
    4, 5, 6, 7,
    8, 9, 10, 11,
    12, 13, 14, 15
) in {
    let key = aes_128_key([
        0, 1, 2, 3, 4, 5, 6, 7,
        8, 9, 10, 11, 12, 13, 14, 15,
    ]) in {
        putchar('k');
        putchar('e');
        putchar('y');
        putchar(':');
        putchar(' ');
        putchar('\n');
        putkey(key);
        putchar('\n');

        putchar('i');
        putchar('n');
        putchar('p');
        putchar('u');
        putchar('t');
        putchar(':');
        putchar('\n');
        putstate(state);

        state = cipher(state, key);
        putchar('o');
        putchar('u');
        putchar('t');
        putchar('p');
        putchar('u');
        putchar('t');
        putchar(':');
        putchar('\n');
        putstate(state);
        state = inverse_cipher(state, key);

        putchar('i');
        putchar('n');
        putchar('v');
        putchar('e');
        putchar('r');
        putchar('s');
        putchar('e');
        putchar(':');
        putchar('\n');
        putstate(state);
    };
}
    "#;

    let theorem = r#"

type A = Int,
    B = (Int, Int),
    C = ((Int, Int), Int),
    Test = proc(A, proc(A) -> B, proc(B) -> C) -> C
    in const
        a = 5,
        b = proc(x: Int) -> (Int, Int) = { (x, 6) },
        c = proc(x: (Int, Int)) -> ((Int, Int), Int) = { (x, 7) },
        test = proc(a: A, b: proc(A) -> B, c: proc(B) -> C) -> C = {
            c(b(a))
        }, hmm = proc(f: Test, x: Int) -> ((Int, Int), Int) = {
            f(x, b, c)
        } in {
            let result = hmm(test, a) in {
                putint((result.0).0);
                putint((result.0).1);
                putint(result.1);
            }
        }

    "#;

    let lambda_sim = r#"

type Op = struct {
    apply: proc(struct {x: Int}, Int) -> Int,
    state: struct {x: Int}
} in

proc mul(n: Int) -> Op = {
    struct {
        apply = proc(state: struct{x: Int}, y: Int) -> Int = {
            state.x * y
        },
        state = struct {x=n}
    }
} in

proc add(n: Int) -> Op = {
    struct {
        apply = proc(state: struct{x: Int}, y: Int) -> Int = {
            state.x + y
        },
        state = struct {x=n}
    }
} in

proc app(op: Op, y: Int) -> Int = {
    op.apply(op.state, y)
} in

proc putdec(n: Int) -> None = {
    if (lt(n, 10)) {
        put(n + 48)
    } else {
        putdec(n / 10);
        put(n % 10 + 48)
    }
} in

proc puthex(n: Int, lower: Bool) -> None = {
    if (lt(n, 16)) {
        put(n + if (lt(n, 10)) {
            48
        } else {
            if lower {
                87
            } else {
                55
            }
        })
    } else {
        puthex(n / 16, lower);
        puthex(n % 16, lower)
    }
} in

proc putbyte(n: Int, lower: Bool) -> None = {
    puthex(n / 16, lower);
    puthex(n % 16, lower)
} in

proc ln() -> None = {
    put(10)
} in

let double = mul(2),
    triple = mul(3) in {
    putdec(
        app(add(10), app(triple, app(double, 5)))
    );

    ln();

    putbyte(255, false);
}

    "#;

    // pub fn add(a: Int, b: Int) -> Int {
    //     a ^ b
    // }
    // pub fn xtime(n: Int) -> Int {
    //     if (n as usize) << 1 <= 0xff {
    //         n << 1
    //     } else {
    //         n << 1 ^ 0x1b
    //     }
    // }
    // pub fn mul(a: Int, b: Int) -> Int {
    //     let mut result = 0;
    //     let mut next_term = a;
    //     for i in 0..8 {
    //         if b >> i & 1 == 1 {
    //             result ^= next_term
    //         }
    //         next_term = xtime(next_term);
    //     }
    //     result
    // }

    // , or = core(a: Int, b: Int) -> Int = {
    //     bitwise-or [SP], [SP - 1] pop
    // }, nand = core(a: Int, b: Int) -> Int = {
    //     bitwise-nand [SP], [SP - 1] pop
    // }, and = core(a: Int, b: Int) -> Int = {
    //     bitwise-and [SP], [SP - 1] pop
    // }, not = core(x: Int) -> Int = {
    //     bitwise-not [SP]
    // }
    match parse_lir(AES_implementation) {
        Ok(expr) => {
            eprintln!("{expr:?}");

            let expr = Expr::let_consts(
                vec![
                    ("inc", inc.clone()),
                    ("dec", dec.clone()),
                    ("lte", lte.clone()),
                    ("lt", lt.clone()),
                    ("swap", swap.clone()),
                    ("free", free.clone()),
                    ("alloc", alloc.clone()),
                    ("max", max.clone()),
                    ("min", min.clone()),
                    ("putchar", put_char.clone()),
                    ("getchar", get_char.clone()),
                    ("putint", put_int.clone()),
                    ("getint", get_int),
                    ("putfloat", put_float),
                    ("getfloat", get_float),
                    ("put", put.clone()),
                ],
                expr,
            );
            // let expr = put_float.clone().app(vec![Expr::let_consts(vec![("putint", put_int.clone()), ("getint", get_int), ("putfloat", put_float), ("getfloat", get_float)], expr.as_type(Type::Cell).as_type(Type::Float))]);
            // Compile the program to assembly
            let asm_code = expr.clone().compile().unwrap();

            let device = match asm_code {
                Ok(asm_core) => {
                    eprintln!("{:#?}", asm_core);
                    let vm_code = asm_core.assemble(128).unwrap();
                    // println!("{:#?}", vm_code);
                    println!("{}", targets::C.build_core(&vm_code).unwrap());
                    // eprintln!("{:?}", expr);
                    CoreInterpreter::new(StandardDevice).run(&vm_code).unwrap()
                }
                Err(asm_std) => {
                    // eprintln!("{:#?}", asm_std);
                    let vm_code = asm_std.assemble(128).unwrap();
                    // println!("{:#?}", vm_code);
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
        Err(e) => panic!("{e}"),
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
