use asm::{
    asm::{CoreOp, CoreProgram, StandardProgram, SP},
    ir::*,
    vm::{as_int, CoreInterpreter, StandardInterpreter, TestingDevice},
};
use maplit::btreemap;

fn var(name: impl ToString) -> ConstExpr {
    ConstExpr::Symbol(name.to_string())
}

#[test]
fn test_struct() {
    let mut env = Env::default();
    let mut program = CoreProgram(vec![]);

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

    let expr = Expr::let_vars(
        btreemap! {
            "x" => (None, ConstExpr::Char('x').into()),
            "y" => (None, Expr::structure(btreemap! {
                "a" => ConstExpr::Char('a').into(),
                "b" => ConstExpr::Char('b').into(),
                "c" => ConstExpr::Char('c').into(),
            })),
            "z" => (None, ConstExpr::Char('z').into()),
            "put" => (None, ConstExpr::proc(
                vec![
                    ("x".to_string(), Type::Struct(btreemap! {
                        "a".to_string() => Type::Char,
                        "b".to_string() => Type::Char,
                        "c".to_string() => Type::Char,
                    })),
                ],
                Type::None,
                Expr::Many(vec![
                    put.clone().app(vec![Expr::var("x").field(var("a"))]),
                    put.clone().app(vec![Expr::var("x").field(var("b"))]),
                    put.clone().app(vec![Expr::var("x").field(var("c"))]),
                ])
            ).into()),
        },
        var("put").app(vec![Expr::structure(btreemap! {
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
    let mut program = CoreProgram(vec![]);

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

    let expr = Expr::let_var(
        "f",
        None,
        ConstExpr::proc(
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
        put.app(vec![Expr::var("f").app(vec![
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
    let mut program = CoreProgram(vec![]);

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

    let expr = Expr::let_var(
        "test",
        None,
        ConstExpr::proc(
            vec![(
                "tup".to_string(),
                Type::Tuple(vec![Type::Char, Type::Tuple(vec![Type::Int, Type::Int])]),
            )],
            Type::Int,
            Expr::Many(vec![
                put.clone()
                    .app(vec![Expr::var("tup").field(ConstExpr::Int(0))]),
                add.clone().app(vec![
                    Expr::var("tup")
                        .field(ConstExpr::Int(1))
                        .field(ConstExpr::Int(0)),
                    Expr::var("tup")
                        .field(ConstExpr::Int(1))
                        .field(ConstExpr::Int(1)),
                ]),
            ]),
        ),
        put.app(vec![Expr::var("test").app(vec![Expr::Tuple(vec![
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
    let mut program = CoreProgram(vec![]);

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

    let expr = Expr::let_var(
        "i",
        None,
        ConstExpr::Int(0),
        put.app(vec![add.app(vec![
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
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output_str(), "d");
}

#[test]
fn test_nested_arrays() {
    let mut env = Env::default();
    let mut program = CoreProgram(vec![]);

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
                    put.clone().app(vec![Expr::var("arr")
                        .idx(Expr::var("i"))
                        .idx(ConstExpr::Int(0))]),
                    put.clone().app(vec![Expr::var("arr")
                        .idx(Expr::var("i"))
                        .idx(ConstExpr::Int(1))]),
                    put.clone().app(vec![Expr::var("arr")
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
    let mut program = CoreProgram(vec![]);

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
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("a".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("b".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("c".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("x".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("y".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("z".to_string()))]),
            put.clone().app(vec![ConstExpr::Char('\n').into()]),
            Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .refer()
                .deref_mut(Expr::Struct(btreemap! {
                    "x".to_string() => ConstExpr::Int(5).into(),
                    "y".to_string() => ConstExpr::Int(4).into(),
                    "z".to_string() => ConstExpr::Int(3).into(),
                })),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("a".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("b".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("a".to_string()))
                .field(ConstExpr::Symbol("c".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("x".to_string()))]),
            put.clone().app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("y".to_string()))]),
            put.app(vec![Expr::var("p")
                .field(ConstExpr::Symbol("b".to_string()))
                .field(ConstExpr::Symbol("z".to_string()))]),
        ]),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output, vec![3, 8, 5, 1, 2, 3, 10, 3, 8, 5, 5, 4, 3]);
}

#[test]
fn test_union() {
    let mut env = Env::default();
    let mut program = CoreProgram(vec![]);

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

    let expr = Expr::let_var(
        "test",
        None,
        ConstExpr::proc(
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
        put.app(vec![Expr::var("test").app(vec![Expr::Tuple(vec![
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
    let mut program = CoreProgram(vec![]);

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
            vec![(
                "p".to_string(),
                Type::Struct(btreemap! {
                    "x".to_string() => Type::Int,
                    "y".to_string() => Type::Int,
                }),
            )],
            Type::None,
            Expr::Many(vec![
                put.clone().app(vec![
                    Expr::var("p").field(ConstExpr::Symbol("x".to_string()))
                ]),
                put.clone().app(vec![ConstExpr::Char(',').into()]),
                put.clone().app(vec![ConstExpr::Char(' ').into()]),
                put.clone().app(vec![
                    Expr::var("p").field(ConstExpr::Symbol("y".to_string()))
                ]),
            ]),
        ),
        Expr::let_var(
            "move",
            None,
            ConstExpr::proc(
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
                    // put.clone()
                    //     .app(vec![Expr::var("union").field(ConstExpr::Symbol("tup".to_string()))]),
                    Expr::Struct(btreemap! {
                        "x".to_string() => add.clone().app(vec![
                            Expr::var("p")
                                .field(ConstExpr::Symbol("x".to_string())),
                            Expr::var("dx")
                        ]),
                        "y".to_string() => add.clone().app(vec![
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
    let mut program = CoreProgram(vec![]);

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

    let expr = put.app(vec![Expr::Tuple(vec![
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
    let mut program = CoreProgram(vec![]);

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
            put.app(vec![Expr::var("a")]),
        ])),
    );

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output, vec![5, 4, 3, 2, 1, 0]);
}

#[test]
fn test_if() {
    let mut env = Env::default();
    let mut program = CoreProgram(vec![]);

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

    let expr = Expr::let_var(
        "a",
        None,
        ConstExpr::Int(6),
        put.app(vec![Expr::from(ConstExpr::Int(1)).if_then(
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
    let mut program = CoreProgram(vec![]);

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

    let get = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "get".to_string(),
        args: vec![],
        ret: Type::Cell,
        body: vec![CoreOp::Next(SP, None), CoreOp::Get(SP.deref())],
    });

    // The program to compile
    let expr = put.app(vec![Expr::from(ConstExpr::Int(16))
        .add(ConstExpr::Int(1))
        .div(ConstExpr::Int(2))
        .mul(get.app(vec![]))
        .rem(ConstExpr::Int(11))]);

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = CoreInterpreter::new(TestingDevice::new_raw(vec![5]));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output, vec![7]);
}

#[test]
fn test_float_arithmetic() {
    let mut env = Env::default();
    let mut program = StandardProgram(vec![]);

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

    let get = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "get".to_string(),
        args: vec![],
        ret: Type::Cell,
        body: vec![CoreOp::Next(SP, None), CoreOp::Get(SP.deref())],
    });

    // The program to compile
    let expr = put.app(vec![Expr::from(ConstExpr::Int(16))
        .add(ConstExpr::Float(1.2))
        .div(ConstExpr::Int(2))
        .mul(get.app(vec![]).as_type(Type::Float))]);

    expr.compile_expr(&mut env, &mut program).unwrap();

    let i = StandardInterpreter::new(TestingDevice::new_raw(vec![as_int(2.2)]));
    let vm_code = program.assemble(16).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output, vec![as_int(18.92)]);
}

#[test]
fn test_as() {
    let mut env = Env::default();
    let mut program = StandardProgram(vec![]);

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

    let get = ConstExpr::CoreBuiltin(CoreBuiltin {
        name: "get".to_string(),
        args: vec![],
        ret: Type::Cell,
        body: vec![CoreOp::Next(SP, None), CoreOp::Get(SP.deref())],
    });

    let expr = put.app(vec![get
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

    assert_eq!(device.output, vec![25]);
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
    assert_eq!(
        expr.type_check(&env),
        Err(Error::InvalidBinop(
            Expr::from(ConstExpr::Int(5)).add(Expr::var("a").field(var("y")))
        ))
    );

    let expr = Expr::let_var(
        "a",
        None,
        Expr::structure(btreemap! {
            "x" => ConstExpr::Int(1).into(),
            "y" => ConstExpr::Char('5').into(),
        }),
        Expr::var("a").as_type(Type::Int),
    );
    assert_eq!(
        expr.type_check(&env),
        Err(Error::InvalidAs(Expr::var("a").as_type(Type::Int)))
    );
}
