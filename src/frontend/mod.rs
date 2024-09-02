mod parse;
use parse::parse_source;
use crate::lir::Expr;

pub fn parse(input: impl ToString, filename: Option<&str>) -> Result<Expr, String> {
    use no_comment::{IntoWithoutComments, languages};
    let input = input
        .to_string();

    let input = include_str!("std_lib.sg").to_string() + &input;

    let input = input
        .chars()
        .without_comments(languages::rust())
        .collect::<String>();


    let expr = parse_source(&input, filename.map(|x| x.to_owned()))?;
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
        expr,
    ))
}