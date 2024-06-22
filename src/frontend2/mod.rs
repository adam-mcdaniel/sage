mod lexer;
mod preprocessor;
pub use preprocessor::*;
pub use lexer::*;
use lalrpop_util::lalrpop_mod;
use crate::lir::*;
use std::fmt::Display;
lalrpop_mod!(
    #[allow(clippy::all)]
    frontend_parser, "/frontend2/frontend_parser.rs"
);

#[derive(Debug, PartialEq, Clone, Default)]
pub enum ParseError {
    Preprocessor(String),
    Lexical(LexicalError),
    Syntax(String),
    #[default]
    Unknown
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Lexical(e) => write!(f, "Lexical error: {}", e),
            ParseError::Syntax(e) => write!(f, "Syntax error: {}", e),
            ParseError::Preprocessor(e) => write!(f, "Preprocessor error: {}", e),
            ParseError::Unknown => write!(f, "Unknown error"),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Declaration(crate::lir::Declaration),
    Expr(crate::lir::Expr),
}

/// Parse Core and Standard variants of virtual machine source code.
/// This will return core code by default, but will fallback on standard.
pub fn parse_frontend(
    input: impl ToString,
) -> Result<crate::lir::Expr, String> {
    let code = input
        .to_string()
        .chars()
        .collect::<String>();
    let tokens = lexer::tokenize(&code);
    let just_tokens = tokens.map(|t| t.unwrap().1).collect::<Vec<_>>();
    println!("TOKENS: {:?}\n-----\n", just_tokens);
    let tokens = lexer::tokenize(&code);
    match frontend_parser::ProgramParser::new().parse(tokens) {
        Ok(parsed) => {
            use crate::side_effects::Output;
            let alloc = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
                name: "alloc".to_string(),
                args: vec![("size".to_string(), crate::lir::Type::Int)],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Mutable,
                    Box::new(crate::lir::Type::Any),
                ),
                body: vec![crate::asm::StandardOp::Alloc(crate::asm::SP.deref())],
            });
            let free = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
                name: "free".to_string(),
                args: vec![(
                    "ptr".to_string(),
                    crate::lir::Type::Pointer(
                        crate::lir::Mutability::Any,
                        Box::new(crate::lir::Type::Any),
                    ),
                )],
                ret: crate::lir::Type::None,
                body: vec![
                    crate::asm::StandardOp::Free(crate::asm::SP.deref()),
                    crate::asm::StandardOp::CoreOp(crate::asm::CoreOp::Pop(None, 1)),
                ],
            });
            use crate::asm::CoreOp::*;

            use crate::asm::*;
            // let realloc_fp_stack = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
            //     name: "realloc_fp_stack".to_string(),
            //     args: vec![("size".to_string(), crate::lir::Type::Int)],
            //     ret: crate::lir::Type::None,
            //     body: vec![
            //         // Allocate the new frame pointer stack with the given size
            //         Alloc(SP.deref()),
            //         CoreOp(Many(vec![
            //             Pop(Some(C), 1),
            //             Move {
            //                 src: C,
            //                 dst: D
            //             },
            //             // Copy all the data from the old frame pointer stack to the new one
            //             // Copy the start of the old frame pointer stack to the new one
            //             GetAddress {
            //                 addr: START_OF_FP_STACK,
            //                 dst: A,
            //             },
            //             // Subtract from the current FP_STACK
            //             crate::asm::CoreOp::IsLess {
            //                 a: A,
            //                 b: FP_STACK,
            //                 dst: B,
            //             },
            //             While(B),
            //             Move {
            //                 src: A.deref(),
            //                 dst: C.deref(),
            //             },
            //             Next(A, None),
            //             Next(C, None),
            //             crate::asm::CoreOp::IsLess {
            //                 a: A,
            //                 b: FP_STACK,
            //                 dst: B,
            //             },
            //             End,
            //             Move {
            //                 src: C,
            //                 dst: FP_STACK,
            //             }
            //         ]))
            //     ],
            // });

            // let realloc_stack = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
            //     name: "realloc_stack".to_string(),
            //     args: vec![("size".to_string(), crate::lir::Type::Int)],
            //     ret: crate::lir::Type::None,
            //     body: vec![
            //         // Allocate the new frame pointer stack with the given size
            //         Alloc(SP.deref()),
            //         CoreOp(Many(vec![
            //             Pop(Some(C), 1),
            //             Move {
            //                 src: C,
            //                 dst: D
            //             },
            //             // Copy all the data from the old frame pointer stack to the new one
            //             // Copy the start of the old frame pointer stack to the new one
            //             Move {
            //                 src: STACK_START,
            //                 dst: A,
            //             },
            //             // Subtract from the current FP_STACK
            //             crate::asm::CoreOp::IsLess {
            //                 a: A,
            //                 b: SP,
            //                 dst: B,
            //             },
            //             crate::asm::CoreOp::Set(E, 0),
            //             While(B),
            //             Move {
            //                 src: A.deref(),
            //                 dst: C.deref(),
            //             },
            //             Next(A, None),
            //             Next(C, None),
            //             Inc(E),
            //             crate::asm::CoreOp::IsLess {
            //                 a: A,
            //                 b: SP,
            //                 dst: B,
            //             },
            //             End,
            //             // Index the FP by E
            //             Index {
            //                 src: FP,
            //                 offset: E,
            //                 dst: F,
            //             },
            //             Move {
            //                 src: F,
            //                 dst: FP,
            //             },

            //             Move {
            //                 src: C,
            //                 dst: SP,
            //             },
            //             Move {
            //                 src: D,
            //                 dst: STACK_START,
            //             }
            //         ]))
            //     ],
            // });

            let get_sp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_sp".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Any,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::SP, 1)],
            });

            let set_sp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "set_sp".to_string(),
                args: vec![(
                    "new_sp".to_string(),
                    crate::lir::Type::Pointer(
                        crate::lir::Mutability::Any,
                        Box::new(crate::lir::Type::Cell),
                    ),
                )],
                ret: crate::lir::Type::None,
                body: vec![Pop(Some(A), 1), Move { src: A, dst: SP }],
            });

            let set_fp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "set_fp".to_string(),
                args: vec![(
                    "new_fp".to_string(),
                    crate::lir::Type::Pointer(
                        crate::lir::Mutability::Any,
                        Box::new(crate::lir::Type::Cell),
                    ),
                )],
                ret: crate::lir::Type::None,
                body: vec![Pop(Some(FP), 1)],
            });

            let set_stack_start = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "set_stack_start".to_string(),
                args: vec![(
                    "new_stack_start".to_string(),
                    crate::lir::Type::Pointer(
                        crate::lir::Mutability::Any,
                        Box::new(crate::lir::Type::Cell),
                    ),
                )],
                ret: crate::lir::Type::None,
                body: vec![Pop(Some(STACK_START), 1)],
            });

            let get_fp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_fp".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Any,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::FP, 1)],
            });

            let get_gp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_gp".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Any,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::GP, 1)],
            });
            let set_gp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "set_gp".to_string(),
                args: vec![(
                    "new_gp".to_string(),
                    crate::lir::Type::Pointer(
                        crate::lir::Mutability::Any,
                        Box::new(crate::lir::Type::Cell),
                    ),
                )],
                ret: crate::lir::Type::None,
                body: vec![Pop(Some(GP), 1)],
            });

            let get_fp_stack = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_fp_stack".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Any,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::FP_STACK, 1)],
            });

            let get_stack_start = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_stack_start".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Any,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::STACK_START, 1)],
            });

            let mut debug_body = vec![];
            for ch in "Debug\n".to_string().chars() {
                debug_body.push(crate::asm::CoreOp::Set(crate::asm::TMP, ch as i64));
                debug_body.push(crate::asm::CoreOp::Put(
                    crate::asm::TMP,
                    Output::stdout_char(),
                ));
            }
            for reg in crate::asm::REGISTERS {
                for ch in format!("   {reg} = ").chars() {
                    debug_body.push(crate::asm::CoreOp::Set(crate::asm::TMP, ch as i64));
                    debug_body.push(crate::asm::CoreOp::Put(
                        crate::asm::TMP,
                        Output::stdout_char(),
                    ));
                }
                debug_body.push(crate::asm::CoreOp::Put(reg, Output::stdout_int()));
                debug_body.push(crate::asm::CoreOp::Set(crate::asm::TMP, '\n' as i64));
                debug_body.push(crate::asm::CoreOp::Put(
                    crate::asm::TMP,
                    Output::stdout_char(),
                ));
            }
            // Debug function
            // Prints out stack pointer, frame pointer, and the value at the top of the stack.
            let debug = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "debug".to_string(),
                args: vec![],
                ret: crate::lir::Type::None,
                body: debug_body,
            });

            // body: vec![
            //     crate::asm::StandardOp::CoreOp(
            //         crate::asm::CoreOp::Many(vec![
            //             crate::asm::CoreOp::Set(crate::asm::A, 'S' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, 'P' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, ':' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, ' ' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Put(crate::asm::SP, Output::stdout_int()),
            //             crate::asm::CoreOp::Set(crate::asm::A, '\n' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),

            //             crate::asm::CoreOp::Set(crate::asm::A, 'F' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, 'P' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, ':' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, ' ' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Put(crate::asm::FP, Output::stdout_int()),
            //             crate::asm::CoreOp::Set(crate::asm::A, '\n' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),

            //             crate::asm::CoreOp::Set(crate::asm::A, 'T' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, 'O' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, 'P' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, ':' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Set(crate::asm::A, ' ' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //             crate::asm::CoreOp::Put(crate::asm::SP.deref(), Output::stdout_int()),
            //             crate::asm::CoreOp::Set(crate::asm::A, '\n' as i64),
            //             crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
            //         ])
            //     )
            // ],

            Ok(crate::lir::Expr::let_consts(
                vec![
                    ("free", free),
                    ("alloc", alloc),
                    // ("realloc_fp_stack", realloc_fp_stack),
                    // ("realloc_stack", realloc_stack),
                    ("debug", debug),
                    ("get_sp", get_sp),
                    ("get_fp", get_fp),
                    ("set_sp", set_sp),
                    ("set_fp", set_fp),
                    ("set_gp", set_gp),
                    ("get_fp_stack", get_fp_stack),
                    ("get_stack_start", get_stack_start),
                    ("set_stack_start", set_stack_start),
                    ("get_gp", get_gp),
                ],
                parsed,
            ))
        },
        Err(e) => Err(crate::parse::format_error(&code, e)),
    }
}

#[test]
fn test_preprocessor() {
    // Enable logging
    let mut builder = env_logger::Builder::from_default_env();
    builder.format_timestamp(None);
    builder.filter(None, log::LevelFilter::Trace);
    builder.init();

    rayon::ThreadPoolBuilder::new()
        .num_threads(8)
        .stack_size(512 * 1024 * 1024)
        .build_global()
        .unwrap();

    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(512 * 1024 * 1024)
        .spawn(test_preprocessor_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_preprocessor_helper() {
    println!("{:?}", sage_lisp::Expr::parse("\"hello world!\""));

    match parse_frontend(r#"
    // A curried compile time function that adds two compile time values
    #![defun add (x) (\(y) (+ x y))]
    
    println(
        "constant folding with lisp: ",
        #[add 10]
        12
    );
    
    // A hashmap with a single key value pair
    #![define hashmap #["key" 6]]
    
    // // Some code that returns a string to add to the compiled code
    #!["let x: Int = 5;"]

    fun exit(code: Int): Never {
        println("Exited with code ", code);
        while (true) {pass}
    }
    
    enum Maybe[T] {
        Just(T),
        Nothing
    }
    
    impl Maybe[T] {
        fun unwrap(self: Maybe![T]): T {
            match (self) {
                of Just x => {x},
                of Nothing => {
                    println("Tried to unwrap a Nothing value");
                    exit(1);
                }
            }
        }
    }

    struct Vec[T] {
        data: &mut T,
        length: Int,
        cap: Int
    }

    impl Vec[T] {
        fun make(): Vec![T] {
            return {
                data=alloc(100) as &mut T,
                length=0,
                cap=100  
            };
        }

        fun push(self: &mut Vec![T], value: T) {
            self.data[self.length] = value;
            self.length += 1;
        }

        fun get(self: &Vec![T], index: Int): Maybe![&T] {
            type Output = Maybe![&T];
            if (index < 0 or index >= self.length) {
                return Output of Nothing;
            }
            return Output of Just &self.data[index];
        }

        fun map[U](self: &Vec![T], f: fun(T): U): Vec![U] {
            let mut new_vec = Vec.make![U]();
            for (let mut i=0; i<self.length; i+=1) {
                new_vec.push(f(self.data[i]));
            }
            return new_vec;
        }

        fun reduce[U](self: &Vec![T], f: fun(U, T): U, init: U): U {
            let mut acc = init;
            for (let mut i=0; i<self.length; i+=1) {
                acc = f(acc, self.data[i]);
            }
            return acc;
        }

        fun __println__(self: &Vec![T]) {
            print("[");
            for (let mut i=0; i<self.length; i+=1) {
                print(self.data[i]);
                if (i < self.length - 1) {
                    print(", ");
                }
            }
            println("]");
        }
    }
    
    let x = 5;

    // Add a compile time variable to a runtime variable
    println(
        "adding hashmap value to runtime value: ",
        x + (
            #[hashmap "key"]
        )
    );

    let mut vec = Vec.make![Int]();
    println(vec);

    for (let mut i=0; i<10; i+=1) {
        vec.push(i);
    }

    vec.__println__();

    println("vec[5]: ", *vec.get(5).unwrap());
    // fun square_float(x: Int): Float {
    //     return x * x as Float;
    // }

    // fun sum(x: Int, y: Int): Int {
    //     return x + y;
    // }

    let sum = fun(x: Int, y: Int): Int {
        return x + y;
    };

    let float_vec = vec.map![Float](fun(x: Int): Float {
        return x as Float * 2;
    });
    float_vec.__println__();
    println("float_vec[5]: ", *float_vec.get(5).unwrap());

    println("sum of vec: ", vec.reduce![Int](sum, 0));

    // println("Method: ", x.add(5));
    struct Test[T] {
        x: Vec![T]
    }

    let test: Test![Int] = {
        x = vec
    };

    println(test.x.reduce![Int](sum, 0));

    "#) {
        Ok(expr) => {
            println!("{expr}\n-----\n{expr:?}");
            // Try to compile
            use crate::vm::{StandardInterpreter, CoreInterpreter, StandardDevice};
            let compiled = expr.compile().unwrap();
            println!("\n\nRESULT {:?}", compiled);
            match compiled {
                Ok(core) => {
                    println!("Compiled to core variant");
                    let vm_code = core.assemble(8192).unwrap();
                    println!("------- RUNNING CORE VM CODE -------");
                    let output = CoreInterpreter::new(StandardDevice::default())
                        .run(&vm_code)
                        .unwrap();
                    println!("-------- OUTPUT --------\n{:?}", output);
                },
                Err(std) => {
                    println!("Compiled to standard variant");
                    let vm_code = std.assemble(8192).unwrap();
                    println!("------- RUNNING STANDARD VM CODE -------");
                    let output = StandardInterpreter::new(StandardDevice::default())
                        .run(&vm_code)
                        .unwrap();
                    println!("-------- OUTPUT --------\n{:?}", output);
                }
            }
            // match compile_source_to_vm(filename, src, src_type, call_stack_size)? {
            //     // If the code is core variant virtual machine code
            //     Ok(vm_code) => {
            //         CoreInterpreter::new(StandardDevice::default())
            //             .run(&vm_code)
            //             .map_err(Error::InterpreterError)?;
            //     }
            //     // If the code is standard variant virtual machine code
            //     Err(vm_code) => {
            //         StandardInterpreter::new(StandardDevice::default())
            //             .run(&vm_code) 
            //             .map_err(Error::InterpreterError)?;
            //     }
            // }
        },
        Err(e) => panic!("{e}"),
    }
}

#[test]
fn test_functions() {

}

#[test]
fn test_parser() {
    match parse_frontend("
    // let x = Direction![Int] of North {
    //     radius = 5,
    //     center = {x = 0, y = 0}
    // };
    // return {test=Direction![Int] of North {
    //     radius = 5,
    //     center = {x = 0, y = 0}
    // }};

    // let x = 4;
    // let y = 10;
    // println(x < 5 and y > 6);

    fun testing[T](mut dir: Direction, x: T): Int {
        dir = Direction of North;
        let test = Shape![T] of Circle {
            radius = x,
            center = {x = 0, y = 0}
        };
        5;
    }

    enum Shape[T] {
        Circle {
            radius: T,
            center: Position
        },
        Rectangle {
            width: Int,
            height: Int
        }
    }

    let shape = Shape![Int] of Circle {
        radius=5, center = {x = 5, y = 6}
    };
    println(shape);

    // // def testing2(x: Int, y: Int, z: Int): Int = x + y * z * 2;
    // // let x = 5 && 6;

    // def poly_proc[A, B](a: A, b: B): A = a;

    // // // let x: Int = 1;
    // // // const y = { x = 5, y = {a = 1, b = 2} };

    // // // let static TEST: Int = 5;
    // // // let mut x: Int = 5;
    // // // x = 7;
    // // // let ref = &mut x;
    // // // *ref = 8;
    // // // ref[1] = 9;
    // // // 5;6;7;
    // // // -x.y;

    // struct Rectangle {
    //     width: Int,
    //     height: Int
    // }

    struct Position {
        x: Int,
        y: Int
    }

    enum Direction {
        North, South, East, West
    }

    // // let x = poly_proc![Int, Int](5, 6);

    // struct HashMap[K, V] {
    //     data: Vec![(K, V)]
    // }

    // struct Vec[T] {
    //     data: &mut T,
    //     length: Int,
    //     cap: Int
    // }
    

    // def alloc(n: Int): &mut Cell {
    //     // let data = malloc(n);
    //     // data as &mut Cell
    //     return new 5 as &mut Cell;
    // }

    // def Vec_make[T](): Vec![T] {
    //     return {
    //         data=alloc(100) as &mut T,
    //         length=0,
    //         cap=100  
    //     };
    // }

    // let x = enum {Some Int, None} of Some 5;
    // let y: Option![Int] = x;
    // println(x);
    // let z = y;

    // println(y);
    // enum Option[T] {
    //     Some(T),
    //     None
    // }
    // println(z);


    // // let y = (5);
    // // let x = println(Option![Int] of Some 5);

    // def HashMap_make[K, V](): HashMap![K, V] {
    //     return {
    //         data=Vec_make![(K, V)](),
    //     };
    // }

    // // type Test = HashMap![
    // //     Vec![(Int, Int)],
    // //     Vec![(Int, Int)]
    // // ];
    // let test = 5;
    // let test2 = 6;

    // println(test + test2);

    // test.testing![Float](Direction of North, 5.0);

") {
        Ok(expr) => {
            println!("{expr}\n-----\n{expr:?}");
            // Try to compile
            println!("\n\nRESULT {:?}", expr.compile());
        },
        Err(e) => panic!("{e}"),
    }
}