use asm::{
    asm::{CoreOp, StandardOp, SP},
    ir::*,
    targets::*,
    vm::{CoreInterpreter, StandardInterpreter, TestingDevice},
};

fn main() {
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
    // let put_float = ConstExpr::StandardBuiltin(StandardBuiltin {
    //     name: "put_float".to_string(),
    //     args: vec![],
    //     ret: Type::None,
    //     body: vec![
    //         StandardOp::PutFloat(SP.deref()),
    //         StandardOp::CoreOp(CoreOp::Pop(None, 1)),
    //     ],
    // });

    // let put_int = ConstExpr::StandardBuiltin(StandardBuiltin {
    //     name: "put_int".to_string(),
    //     args: vec![],
    //     ret: Type::None,
    //     body: vec![
    //         StandardOp::PutInt(SP.deref()),
    //         StandardOp::CoreOp(CoreOp::Pop(None, 1)),
    //     ],
    // });
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
    let expr = put.app(vec![Expr::from(ConstExpr::Int(16))
        .add(ConstExpr::Float(1.2))
        .div(ConstExpr::Int(2))
        .mul(get.app(vec![]).as_type(Type::Int).as_type(Type::Float))
        .as_type(Type::Int)]);

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

    let input = vec![10];

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
