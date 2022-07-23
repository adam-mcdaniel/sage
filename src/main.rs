use asm::{
    asm::{CoreOp, CoreProgram, SP},
    ir::*,
    vm::{CoreInterpreter, TestingDevice},
};
use maplit::btreemap;

fn main() {
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
    .idx(ConstExpr::Int(0))]); //Symbol("x".to_string()));

    expr.compile(&mut env, &mut program).unwrap();
    eprintln!("{:?}", program);

    let i = CoreInterpreter::new(TestingDevice::new("testing\n"));
    let vm_code = program.assemble(16).unwrap();
    println!("{:?}", vm_code);
    let device = i.run(&vm_code).unwrap();

    eprintln!("{:?}", device.output_str());
}
