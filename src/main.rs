use asm::{
    asm::{CoreOp, StandardOp, SP},
    lir::*,
    targets::*,
    vm::{CoreInterpreter, StandardInterpreter, TestingDevice},
};

fn var(name: impl ToString) -> ConstExpr {
    ConstExpr::Symbol(name.to_string())
}

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

    let expr = put.clone().app(vec![Expr::let_type(
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
                        put.app(vec![Expr::var("node").field(var("data"))]),
                        Expr::var("node").field(var("next"))
                            .as_type(Type::Cell)
                            .add(ConstExpr::Int(1000))
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
    let program = expr.clone().compile().unwrap().unwrap_err();

    let i = StandardInterpreter::new(TestingDevice::new_raw(vec![]));
    let vm_code = program.assemble(256).unwrap();
    let device = i.run(&vm_code).unwrap();

    assert_eq!(device.output, vec![3, 5, 7, 0]);
    
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
