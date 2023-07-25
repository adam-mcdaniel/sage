use maplit::btreemap;
use sage::{
    asm::{CoreOp, CoreProgram, StandardOp, StandardProgram, A, SP},
    side_effects::{Input, Output},
    lir::*,
    parse::*,
    vm::{as_int, CoreInterpreter, StandardInterpreter, TestingDevice},
};

fn var(name: impl ToString) -> ConstExpr {
    ConstExpr::Symbol(name.to_string())
}

#[test]
fn test_struct() {
    // Executing `test_struct_helper` overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(test_struct_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_struct_helper() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_vars(
        vec![
            ("x", None, ConstExpr::Char('x').into()),
            (
                "y",
                None,
                Expr::structure(btreemap! {
                    "a" => ConstExpr::Char('a').into(),
                    "b" => ConstExpr::Char('b').into(),
                    "c" => ConstExpr::Char('c').into(),
                }),
            ),
            ("z", None, ConstExpr::Char('z').into()),
            (
                "put_char",
                None,
                ConstExpr::proc(
                    None,
                    vec![(
                        "x".to_string(),
                        Type::Struct(btreemap! {
                            "a".to_string() => Type::Char,
                            "b".to_string() => Type::Char,
                            "c".to_string() => Type::Char,
                        }),
                    )],
                    Type::None,
                    Expr::Many(vec![
                        put_char.clone().app(vec![Expr::var("x").field(var("a"))]),
                        put_char.clone().app(vec![Expr::var("x").field(var("b"))]),
                        put_char.app(vec![Expr::var("x").field(var("c"))]),
                    ]),
                )
                .into(),
            ),
        ],
        var("put_char").app(vec![Expr::structure(btreemap! {
            "a" => var("x").into(),
            "b" => Expr::var("y").field(var("b")),
            "c" => var("z").into(),
        })]),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "xbz");
}

#[test]
fn test_scopes() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let add = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "add".to_string(),
        args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::Comment("Add two integers on the stack".to_string()),
            CoreOp::Add {
                src: SP.deref(),
                dst: SP.deref().offset(-1),
            },
            CoreOp::Pop(None, 1),
        ],
    });

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "f",
        None,
        ConstExpr::proc(
            None,
            vec![
                ("x".to_string(), Type::Int),
                ("y".to_string(), Type::Int),
                ("z".to_string(), Type::Int),
            ],
            Type::Int,
            add.clone().app(vec![
                add.app(vec![Expr::var("x"), Expr::var("y")]),
                Expr::var("z"),
            ]),
        ),
        put_char.app(vec![Expr::var("f").app(vec![
            ConstExpr::Int(1).into(),
            ConstExpr::Int(2).into(),
            ConstExpr::Int(3).into(),
        ])]),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "\u{6}");
}

#[test]
fn test_tuples() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let add = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "add".to_string(),
        args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::Comment("Add two integers on the stack".to_string()),
            CoreOp::Add {
                src: SP.deref(),
                dst: SP.deref().offset(-1),
            },
            CoreOp::Pop(None, 1),
        ],
    });

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "test",
        None,
        ConstExpr::proc(
            None,
            vec![(
                "tup".to_string(),
                Type::Tuple(vec![Type::Char, Type::Tuple(vec![Type::Int, Type::Int])]),
            )],
            Type::Int,
            Expr::Many(vec![
                put_char
                    .clone()
                    .app(vec![Expr::var("tup").field(ConstExpr::Int(0))]),
                add.app(vec![
                    Expr::var("tup")
                        .field(ConstExpr::Int(1))
                        .field(ConstExpr::Int(0)),
                    Expr::var("tup")
                        .field(ConstExpr::Int(1))
                        .field(ConstExpr::Int(1)),
                ]),
            ]),
        ),
        put_char.app(vec![Expr::var("test").app(vec![Expr::Tuple(vec![
            ConstExpr::Char('?').into(),
            Expr::Tuple(vec![ConstExpr::Int(2).into(), ConstExpr::Int(3).into()]),
        ])])]),
    );
    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "?\u{5}");
}

#[test]
fn test_array() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let add = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "add".to_string(),
        args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::Comment("Add two integers on the stack".to_string()),
            CoreOp::Add {
                src: SP.deref(),
                dst: SP.deref().offset(-1),
            },
            CoreOp::Pop(None, 1),
        ],
    });

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "i",
        None,
        ConstExpr::Int(0),
        put_char.app(vec![add.app(vec![
            Expr::Array(vec![
                ConstExpr::Char('a').into(),
                ConstExpr::Char('b').into(),
                ConstExpr::Char('c').into(),
            ])
            .idx(ConstExpr::Int(1)),
            Expr::Array(vec![
                ConstExpr::Int(2).into(),
                ConstExpr::Int(4).into(),
                ConstExpr::Int(6).into(),
                ConstExpr::Int(8).into(),
                ConstExpr::Int(10).into(),
            ])
            .idx(Expr::var("i")),
        ])]),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(256).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "d");
}

#[test]
fn test_nested_arrays() {
    // Compiling most examples overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(test_nested_arrays_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_nested_arrays_helper() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "i",
        None,
        ConstExpr::Int(1),
        Expr::let_var(
            "j",
            None,
            ConstExpr::Int(1),
            Expr::let_var(
                "arr",
                None,
                Expr::Array(vec![
                    Expr::Array(vec![
                        ConstExpr::Char('a').into(),
                        ConstExpr::Char('b').into(),
                        ConstExpr::Char('c').into(),
                    ]),
                    Expr::Array(vec![
                        ConstExpr::Char('d').into(),
                        ConstExpr::Char('e').into(),
                        ConstExpr::Char('f').into(),
                    ]),
                    Expr::Array(vec![
                        ConstExpr::Char('g').into(),
                        ConstExpr::Char('h').into(),
                        ConstExpr::Char('i').into(),
                    ]),
                ]),
                Expr::Many(vec![
                    Expr::var("arr")
                        .idx(Expr::var("i"))
                        .idx(Expr::var("j"))
                        .refer()
                        .deref_mut(ConstExpr::Char('!')),
                    put_char.clone().app(vec![Expr::var("arr")
                        .idx(Expr::var("i"))
                        .idx(ConstExpr::Int(0))]),
                    put_char.clone().app(vec![Expr::var("arr")
                        .idx(Expr::var("i"))
                        .idx(ConstExpr::Int(1))]),
                    put_char.app(vec![Expr::var("arr")
                        .idx(Expr::var("i"))
                        .idx(ConstExpr::Int(2))]),
                ]),
            ),
        ),
    );
    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "d!f");
}

#[test]
fn test_nested_structs() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "p",
        None,
        Expr::Struct(btreemap! {
            "a".to_string() => Expr::Struct(btreemap! {
                "a".to_string() => ConstExpr::Int(3).into(),
                "b".to_string() => ConstExpr::Int(4).into(),
                "c".to_string() => ConstExpr::Int(5).into(),
            }),
            "b".to_string() => Expr::Struct(btreemap! {
                "x".to_string() => ConstExpr::Int(1).into(),
                "y".to_string() => ConstExpr::Int(2).into(),
                "z".to_string() => ConstExpr::Int(3).into(),
            }),
        }),
        Expr::Many(vec![
            Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("b".to_string()))
                .refer()
                .deref_mut(ConstExpr::Int(8)),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("a".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("b".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("c".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("x".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("y".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("z".to_string()))]),
            put_char.clone().app(vec![ConstExpr::Char('\n').into()]),
            Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .refer()
                .deref_mut(Expr::Struct(btreemap! {
                    "x".to_string() => ConstExpr::Int(5).into(),
                    "y".to_string() => ConstExpr::Int(4).into(),
                    "z".to_string() => ConstExpr::Int(3).into(),
                })),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("a".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("b".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("c".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("x".to_string()))]),
            put_char.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("y".to_string()))]),
            put_char.app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("z".to_string()))]),
        ]),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(
        device.output_vals(),
        vec![3, 8, 5, 1, 2, 3, 10, 3, 8, 5, 5, 4, 3]
    );
}

#[test]
fn test_union() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "test",
        None,
        ConstExpr::proc(
            None,
            vec![
                (
                    "a".to_string(),
                    Type::Union(btreemap! {
                        "ch".to_string() => Type::Char,
                        "tup".to_string() => Type::Tuple(vec![Type::Int, Type::Int]),
                    }),
                ),
                (
                    "b".to_string(),
                    Type::Union(btreemap! {
                        "ch".to_string() => Type::Char,
                        "tup".to_string() => Type::Tuple(vec![Type::Int, Type::Int]),
                    }),
                ),
            ],
            Type::Int,
            Expr::Many(vec![
                Expr::var("b").field(ConstExpr::Symbol("ch".to_string()))
            ]),
        ),
        put_char.app(vec![Expr::var("test").app(vec![Expr::Tuple(vec![
            Expr::Union(
                Type::Union(btreemap! {
                    "ch".to_string() => Type::Char,
                    "tup".to_string() => Type::Tuple(vec![Type::Int, Type::Int]),
                }),
                "tup".to_string(),
                Box::new(Expr::Tuple(vec![
                    ConstExpr::Int(2).into(),
                    ConstExpr::Int(3).into(),
                ])),
            ),
            Expr::Union(
                Type::Union(btreemap! {
                    "ch".to_string() => Type::Char,
                    "tup".to_string() => Type::Tuple(vec![Type::Int, Type::Int]),
                }),
                "ch".to_string(),
                Box::new(ConstExpr::Char('?').into()),
            ),
        ])])]),
    );
    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "?");
}

#[test]
fn test_struct2() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let add = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "add".to_string(),
        args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::Comment("Add two integers on the stack".to_string()),
            CoreOp::Add {
                src: SP.deref(),
                dst: SP.deref().offset(-1),
            },
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "putpoint",
        None,
        ConstExpr::proc(
            None,
            vec![(
                "p".to_string(),
                Type::Struct(btreemap! {
                    "x".to_string() => Type::Int,
                    "y".to_string() => Type::Int,
                }),
            )],
            Type::None,
            Expr::Many(vec![
                put_char.clone().app(vec![
                    Expr::var("p").field(ConstExpr::Symbol("x".to_string()))
                ]),
                put_char.clone().app(vec![ConstExpr::Char(',').into()]),
                put_char.clone().app(vec![ConstExpr::Char(' ').into()]),
                put_char.app(vec![
                    Expr::var("p").field(ConstExpr::Symbol("y".to_string()))
                ]),
            ]),
        ),
        Expr::let_var(
            "move",
            None,
            ConstExpr::proc(
                None,
                vec![
                    (
                        "p".to_string(),
                        Type::Struct(btreemap! {
                            "x".to_string() => Type::Int,
                            "y".to_string() => Type::Int,
                        }),
                    ),
                    ("dx".to_string(), Type::Int),
                    ("dy".to_string(), Type::Int),
                ],
                Type::Struct(btreemap! {
                    "x".to_string() => Type::Int,
                    "y".to_string() => Type::Int,
                }),
                Expr::Many(vec![
                    // put_char.clone()
                    //     .app(vec![Expr::var("union").field(ConstExpr::Symbol("tup".to_string()))]),
                    Expr::Struct(btreemap! {
                        "x".to_string() => add.clone().app(vec![
                            Expr::var("p")
                                .field(ConstExpr::Symbol("x".to_string())),
                            Expr::var("dx")
                        ]),
                        "y".to_string() => add.app(vec![
                            Expr::var("p")
                                .field(ConstExpr::Symbol("y".to_string())),
                            Expr::var("dy")
                        ]),
                    }),
                ]),
            ),
            Expr::var("putpoint").app(vec![Expr::var("move").app(vec![Expr::Tuple(vec![
                Expr::Struct(btreemap! {
                    "x".to_string() => ConstExpr::Int(3).into(),
                    "y".to_string() => ConstExpr::Int(4).into(),
                }),
                ConstExpr::Int(1).into(),
                ConstExpr::Int(2).into(),
            ])])]),
        ),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "\u{4}, \u{6}");
}

#[test]
fn test_mixed_types() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.app(vec![Expr::Tuple(vec![
        ConstExpr::Char('a').into(),
        Expr::Tuple(vec![
            ConstExpr::Bool(false).into(),
            ConstExpr::Int(2).into(),
            Expr::Array(vec![
                ConstExpr::Char('c').into(),
                ConstExpr::Char('d').into(),
                ConstExpr::Char('e').into(),
            ]),
        ]),
        Expr::structure(btreemap! {
            "x" => ConstExpr::Char('b').into(),
            "y" => ConstExpr::Int(3).into(),
            "z" => ConstExpr::Bool(true).into(),
        }),
    ])
    .field(ConstExpr::Int(1))
    .field(ConstExpr::Int(2))
    .idx(ConstExpr::Int(0))]);

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "c");
}

#[test]
fn test_loop() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let sub = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "sub".to_string(),
        args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        ret: Type::Int,
        body: vec![
            CoreOp::Comment("Add two integers on the stack".to_string()),
            CoreOp::Sub {
                src: SP.deref(),
                dst: SP.deref().offset(-1),
            },
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "a",
        None,
        ConstExpr::Int(6),
        Expr::var("a").while_loop(Expr::Many(vec![
            Expr::var("a")
                .refer()
                .deref_mut(sub.app(vec![Expr::var("a"), ConstExpr::Int(1).into()])),
            put_char.app(vec![Expr::var("a")]),
        ])),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![5, 4, 3, 2, 1, 0]);
}

#[test]
fn test_if() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "a",
        None,
        ConstExpr::Int(6),
        put_char.app(vec![Expr::from(ConstExpr::Int(1)).if_then(
            Expr::from(ConstExpr::Int(0)).if_then(ConstExpr::Char('a'), ConstExpr::Char('b')),
            ConstExpr::Char('c'),
        )]),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "b");
}

#[test]
fn test_int_arithmetic() {
    let mut env = Env::default();
    let mut program = CoreProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let get = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "get".to_string(),
        args: vec![],
        ret: Type::Cell,
        body: vec![
            CoreOp::Next(SP, None),
            CoreOp::Get(SP.deref(), Input::stdin_char()),
        ],
    });

    // The program to compile
    let expr = put_char.app(vec![Expr::from(ConstExpr::Int(16))
        .add(ConstExpr::Int(1))
        .div(ConstExpr::Int(2))
        .mul(get.app(vec![]))
        .rem(ConstExpr::Int(11))]);

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![5]));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![7]);
}

#[test]
fn test_float_arithmetic() {
    let mut env = Env::default();
    let mut program = StandardProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let get = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "get".to_string(),
        args: vec![],
        ret: Type::Cell,
        body: vec![
            CoreOp::Next(SP, None),
            CoreOp::Get(SP.deref(), Input::stdin_char()),
        ],
    });

    // The program to compile
    let expr = put_char.app(vec![Expr::from(ConstExpr::Int(16))
        .add(ConstExpr::Float(1.2))
        .div(ConstExpr::Int(2))
        .mul(get.app(vec![]).as_type(Type::Float))]);

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = StandardInterpreter::new(TestingDevice::new_raw(vec![as_int(2.2)]));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![as_int(18.92)]);
}

#[test]
fn test_as() {
    let mut env = Env::default();
    let mut program = StandardProgram::new(vec![]);

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let get = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "get".to_string(),
        args: vec![],
        ret: Type::Cell,
        body: vec![
            CoreOp::Next(SP, None),
            CoreOp::Get(SP.deref(), Input::stdin_char()),
        ],
    });

    let expr = put_char.app(vec![get
        .clone()
        .app(vec![])
        .as_type(Type::Int)
        .add(get.clone().app(vec![]).as_type(Type::Float))
        .mul(get.app(vec![]).as_type(Type::Float))
        .as_type(Type::Int)]);

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = StandardInterpreter::new(TestingDevice::new_raw(vec![5, as_int(5.0), as_int(2.5)]));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![25]);
}

#[test]
fn test_typecheck() {
    let env = Env::default();

    let expr = Expr::let_var(
        "a",
        None,
        Expr::structure(btreemap! {
            "x" => ConstExpr::Int(1).into(),
            "y" => ConstExpr::Char('5').into(),
        }),
        Expr::from(ConstExpr::Int(5)).add(Expr::var("a").field(var("y"))),
    );
    assert!(matches!(
        expr.type_check(&env),
        Err(Error::InvalidBinaryOp(_, _, _)) | Err(Error::InvalidBinaryOpTypes(_, _, _))
    ));

    let expr = Expr::let_var(
        "a",
        None,
        Expr::structure(btreemap! {
            "x" => ConstExpr::Int(1).into(),
            "y" => ConstExpr::Char('5').into(),
        }),
        Expr::var("a").as_type(Type::Int),
    );
    expr.type_check(&env).unwrap_err();
}

#[test]
fn test_recursive_types() {
    // Executing `test_recursive_types_helper` overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(test_recursive_types_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_recursive_types_helper() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let alloc = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "alloc".to_string(),
        args: vec![("size".to_string(), Type::Int)],
        ret: Type::Pointer(Box::new(Type::Any)),
        body: vec![StandardOp::Alloc(SP.deref())],
    });

    let expr = put_char.app(vec![Expr::let_type(
        "Node",
        Type::Struct(btreemap! {
            "data".to_string() => Type::Int,
            "next".to_string() => Type::Pointer(Box::new(Type::Symbol("Node".to_string()))),
        }),
        Expr::let_vars(
            vec![
                (
                    "node",
                    None,
                    ConstExpr::proc(
                        None,
                        vec![("val".to_string(), Type::Int)],
                        Type::Symbol("Node".to_string()),
                        Expr::structure(btreemap! {
                            "data" => Expr::var("val"),
                            "next" => ConstExpr::Null.into(),
                        }),
                    )
                    .into(),
                ),
                (
                    "next",
                    None,
                    ConstExpr::proc(
                        None,
                        vec![("node".to_string(), Type::Symbol("Node".to_string()))],
                        Type::Symbol("Node".to_string()),
                        Expr::var("node").field(var("next")).deref(),
                    )
                    .into(),
                ),
                (
                    "cons",
                    None,
                    ConstExpr::proc(
                        None,
                        vec![
                            ("head".to_string(), Type::Symbol("Node".to_string())),
                            ("tail".to_string(), Type::Symbol("Node".to_string())),
                        ],
                        Type::Symbol("Node".to_string()),
                        Expr::let_var(
                            "ptr",
                            None,
                            alloc.app(vec![Expr::var("tail").size_of()]),
                            Expr::Many(vec![
                                Expr::var("ptr").deref_mut(Expr::var("tail")),
                                Expr::structure(btreemap! {
                                    "data" => Expr::var("head").field(var("data")),
                                    "next" => Expr::var("ptr"),
                                }),
                            ]),
                        ),
                    )
                    .into(),
                ),
            ],
            Expr::var("next")
                .app(vec![Expr::var("next").app(vec![Expr::var("cons").app(
                    vec![
                        Expr::var("node").app(vec![ConstExpr::Int(3).into()]),
                        Expr::var("cons").app(vec![
                            Expr::var("node").app(vec![ConstExpr::Int(5).into()]),
                            Expr::var("node").app(vec![ConstExpr::Int(7).into()]),
                        ]),
                    ],
                )])])
                .field(var("data")),
        ),
    )]);

    let program = expr.compile().unwrap().unwrap_err();

    let i = StandardInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![7]);
}

#[test]
fn test_alloc_and_free() {
    // Executing `test_alloc_and_free_helper` overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(test_alloc_and_free_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_alloc_and_free_helper() {
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

    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.clone().app(vec![Expr::let_type(
        "Node",
        Type::Struct(btreemap! {
            "data".to_string() => Type::Int,
            "next".to_string() => Type::Pointer(Box::new(Type::Symbol("Node".to_string()))),
        }),
        Expr::let_procs(
            btreemap! {
                "new" => Procedure::new(
                        None,
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
                        None,
                        vec![(
                        "node".to_string(),
                        Type::Symbol("Node".to_string()),
                    )],
                    Type::None,
                    Expr::Many(vec![
                        put_char.app(vec![Expr::var("node").field(var("data"))]),
                        Expr::var("node").field(var("next"))
                            .as_type(Type::Cell)
                            .sub(Expr::from(ConstExpr::Null).as_type(Type::Cell).as_type(Type::Int))
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
                        None,
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
    let program = expr.compile().unwrap().unwrap_err();

    let i = StandardInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(256).unwrap();

    let device = i.run(&vm_code).unwrap();
    assert_eq!(device.output_vals(), vec![3, 5, 7, 0]);
}

#[test]
fn test_recursion() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.app(vec![Expr::let_proc(
        "factorial",
        Procedure::new(
            None,
            vec![("n".to_string(), Type::Int)],
            Type::Int,
            Expr::var("n").if_then(
                Expr::var("factorial")
                    .app(vec![Expr::var("n").sub(ConstExpr::Int(1))])
                    .mul(var("n")),
                ConstExpr::Int(1),
            ),
        ),
        Expr::var("factorial").app(vec![ConstExpr::Int(5).into()]),
    )]);

    let program = expr.compile().unwrap().unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(256).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![120]);
}

#[test]
fn test_inline_let_type() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.app(vec![Expr::let_proc(
        "factorial",
        Procedure::new(
            None,
            vec![("n".to_string(), Type::Int)],
            Type::Let(
                "a".to_string(),
                Box::new(Type::Let(
                    "b".to_string(),
                    Box::new(Type::Let(
                        "c".to_string(),
                        Box::new(Type::Int),
                        Box::new(Type::Symbol("c".to_string())),
                    )),
                    Box::new(Type::Symbol("b".to_string())),
                )),
                Box::new(Type::Symbol("a".to_string())),
            ),
            Expr::var("n").if_then(
                Expr::var("factorial")
                    .app(vec![Expr::var("n").sub(ConstExpr::Int(1))])
                    .mul(var("n")),
                ConstExpr::Int(1),
            ),
        ),
        Expr::var("factorial").app(vec![ConstExpr::Int(5).into()]),
    )]);

    let program = expr.compile().unwrap().unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(256).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![120]);
}

#[test]
fn test_inline_let_recursive_type() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.clone().app(vec![Expr::let_type(
        "hmm",
        Type::Let(
            "data".to_string(),
            Box::new(Type::Int),
            Box::new(Type::Let(
                "Node".to_string(),
                Box::new(Type::Struct(btreemap! {
                    "data".to_string() => Type::Symbol("data".to_string()),
                    "next".to_string() => Type::Pointer(Box::new(Type::Symbol("Node".to_string()))),
                })),
                Box::new(Type::Symbol("Node".to_string())),
            )),
        ),
        Expr::let_var(
            "test",
            Some(Type::Symbol("hmm".to_string())),
            Expr::structure(btreemap! {
                "data" => ConstExpr::Int(3).into(),
                "next" => ConstExpr::Null.into(),
            }),
            ConstExpr::Int(5),
        ),
    )]);

    expr.compile().unwrap().unwrap();

    let expr = put_char.app(vec![Expr::let_type(
        "List<Int>",
        Type::Let(
            "List<Int>".to_string(),
            Box::new(Type::Tuple(vec![
                Type::Int,
                Type::Pointer(Box::new(Type::Symbol("List<Int>".to_string()))),
            ])),
            Box::new(Type::Symbol("List<Int>".to_string())),
        ),
        Expr::let_var(
            "first",
            Some(Type::Symbol("List<Int>".to_string())),
            Expr::Tuple(vec![ConstExpr::Int(3).into(), ConstExpr::Null.into()]),
            ConstExpr::Int(5),
        ),
    )]);

    expr.compile().unwrap().unwrap();
}

#[test]
fn test_inline_let_type_equality() {
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
                    "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("C".to_string()))),
                    "None".to_string() => Type::None,
                }),
            ])),
            Box::new(Type::Symbol("C".to_string()
        )),
    ))), &env).unwrap());

    assert!(!Type::Let(
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
            "D".to_string(),
            Box::new(Type::None),
            Box::new(Type::Let(
                "C".to_string(),
                Box::new(Type::Tuple(vec![
                    Type::Symbol("T".to_string()),
                    Type::Union(btreemap! {
                        "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("C".to_string()))),
                        "None".to_string() => Type::None,
                    }),
                ])),
                Box::new(Type::Symbol("D".to_string()
            )),
        )),
    ))), &env).unwrap());
}

#[test]
fn test_pseudotemplates() {
    // Executing `test_pseudotemplates_helper` overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(test_pseudotemplates_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_pseudotemplates_helper() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = Expr::let_var(
        "first",
        None,
        Expr::Tuple(vec![ConstExpr::Int(3).into(), ConstExpr::Union(
            Type::Union(btreemap! {
                "Some".to_string() => Type::Pointer(Box::new(Type::Let(
                    "A".to_string(),
                    Box::new(Type::Tuple(vec![
                        Type::Int,
                        Type::Union(btreemap! {
                            "Some".to_string() => Type::Pointer(Box::new(Type::Symbol("A".to_string()))),
                            "None".to_string() => Type::None,
                        }),
                    ])),
                    Box::new(Type::Symbol("A".to_string())),
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
                "Some".to_string(),
                Box::new(Expr::var("first").refer()),
            )]),
            Expr::Many(vec![
                put_char.clone().app(vec![Expr::var("second")
                    .field(ConstExpr::Int(1))
                    .field(ConstExpr::Symbol("Some".to_string()))
                    .deref()
                    .field(ConstExpr::Int(0))]),
                Expr::var("second")
                    .field(ConstExpr::Int(1))
                    .field(ConstExpr::Symbol("Some".to_string()))
                    .refer()
                    .deref_mut(Expr::var("second").refer()),
                put_char.app(vec![Expr::var("second")
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

    let program = expr.clone().compile().unwrap().unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(10).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![3, 5]);

    expr.compile().unwrap().unwrap();
}

#[test]
fn test_mutually_recursive_types() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.clone().app(vec![Expr::let_types(
        vec![
            ("A", Type::Pointer(Box::new(Type::Symbol("B".to_string())))),
            ("B", Type::Pointer(Box::new(Type::Symbol("A".to_string())))),
        ],
        Expr::let_vars(
            vec![
                (
                    "a",
                    Some(Type::Symbol("A".to_string())),
                    ConstExpr::Null.into(),
                ),
                (
                    "b",
                    Some(Type::Symbol("B".to_string())),
                    Expr::var("a").refer(),
                ),
            ],
            Expr::var("b")
                .deref()
                .as_type(Type::Cell)
                .as_type(Type::Int),
        ),
    )]);

    let program = expr.compile().unwrap().unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(10).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![sage::NULL]);

    let expr = put_char.app(vec![Expr::let_types(
        vec![
            ("A", Type::Pointer(Box::new(Type::Symbol("B".to_string())))),
            ("B", Type::Pointer(Box::new(Type::Symbol("A".to_string())))),
            ("C", Type::Pointer(Box::new(Type::Symbol("C".to_string())))),
        ],
        Expr::let_vars(
            vec![
                (
                    "a",
                    Some(Type::Symbol("A".to_string())),
                    ConstExpr::Null.into(),
                ),
                (
                    "b",
                    Some(Type::Symbol("B".to_string())),
                    Expr::var("a").refer(),
                ),
                ("c", Some(Type::Symbol("C".to_string())), Expr::var("b")),
            ],
            Expr::var("c")
                .deref()
                .as_type(Type::Cell)
                .as_type(Type::Int),
        ),
    )]);

    expr.compile().unwrap_err();
}

#[test]
fn test_let_multiple_vars() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Comment("Put an integer from the stack".to_string()),
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let expr = put_char.clone().app(vec![Expr::let_vars(
        vec![
            ("x", None, ConstExpr::Int(5).into()),
            ("y", None, Expr::var("x").add(ConstExpr::Int(5))),
            ("z", None, Expr::var("y").mul(ConstExpr::Int(5))),
        ],
        Expr::var("z"),
    )]);

    let program = expr.compile().unwrap().unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(10).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_vals(), vec![50]);

    let expr = put_char.app(vec![Expr::let_vars(
        vec![
            ("y", None, Expr::var("x").add(ConstExpr::Int(5))),
            ("x", None, ConstExpr::Int(5).into()),
            ("z", None, Expr::var("y").mul(ConstExpr::Int(5))),
        ],
        Expr::var("z"),
    )]);

    expr.compile().unwrap_err();
}

#[test]
fn test_quicksort() {
    // Compiling quicksort overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(test_quicksort_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_quicksort_helper() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Put(SP.deref(), Output::stdout_char()),
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

    let alloc = ConstExpr::StandardBuiltin(StandardBuiltin {
        name: "alloc".to_string(),
        args: vec![("size".to_string(), Type::Int)],
        ret: Type::Pointer(Box::new(Type::Any)),
        body: vec![StandardOp::Alloc(SP.deref())],
    });

    let list = r#"
    proc partition_arr(arr: &Int, low: Int, high: Int) -> Int = {
        let pivot = arr[high],
            i = low - 1,
            j = low in {
            while lt(j, high) {
                if (lte(arr[j], pivot)) {
                    inc(&i);
                    swap(&arr[j], &arr[i]);
                };
                inc(&j);
            };
            swap(&arr[i + 1], &arr[high]);
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
    
    const SIZE = 50 in
    let ptr: &Int = alloc(SIZE), i = 0 in {
        while lt(i, SIZE) {
            ptr[i] = SIZE - i;
            inc(&i);
        };
        i = 0;
        quicksort_arr(ptr, 0, SIZE - 1);
        while lt(i, SIZE) {
            put_char(ptr[i]);
            inc(&i);
        }
    }
    "#;

    let expr = parse_lir(list).unwrap();
    let expr = Expr::let_consts(
        vec![
            ("alloc", alloc),
            ("inc", inc),
            ("lte", lte),
            ("lt", lt),
            ("swap", swap),
            ("put_char", put_char),
        ],
        expr,
    );
    let asm_std = expr.compile().unwrap().unwrap_err();
    let vm_code = asm_std.assemble(512).unwrap();
    let device = StandardInterpreter::new(TestingDevice::new_raw(vec![]))
        .run(&vm_code)
        .unwrap();

    assert_eq!(
        device.output_vals(),
        (1..=50).collect::<Vec<_>>()
    );
}

#[test]
fn test_collatz() {
    // Compiling quicksort overflows the tiny stack for tests.
    // So, we spawn a new thread with a larger stack size.
    let child = std::thread::Builder::new()
        .stack_size(4 * 1024 * 1024)
        .spawn(test_collatz_helper)
        .unwrap();

    // Wait for the thread to finish.
    child.join().unwrap();
}

fn test_collatz_helper() {
    let put_char = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "put_char".to_string(),
        args: vec![("x".to_string(), Type::Int)],
        ret: Type::None,
        body: vec![
            CoreOp::Put(SP.deref(), Output::stdout_char()),
            CoreOp::Pop(None, 1),
        ],
    });

    let collatz = r#"
    proc step(n: Int) -> Int = {
        if (n % 2) {
            3 * n + 1
        } else {
            n / 2
        }
    } in
    
    proc collatz(n: Int) -> None = {
        put_char(n);
        while n - 1 {
            n = step(n);
            put_char(n);
        };
    } in collatz(19)
    "#;

    let expr = parse_lir(collatz).unwrap();
    let expr = Expr::let_const("put_char", put_char, expr);
    let asm_std = expr.compile().unwrap().unwrap();
    let vm_code = asm_std.assemble(16).unwrap();
    let device = CoreInterpreter::new(TestingDevice::new(""))
        .run(&vm_code)
        .unwrap();

    assert_eq!(
        device.output_vals(),
        vec![19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
    );
}
