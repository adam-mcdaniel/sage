mod parse;
use crate::side_effects::Output;
use no_comment::{languages, IntoWithoutComments};
use parse::*;

pub fn parse(code: impl ToString, filename: Option<&str>) -> Result<crate::lir::Expr, String> {
    let code = code
        .to_string()
        .chars()
        .without_comments(languages::rust())
        .collect::<String>();

    match parse_frontend(code.as_ref(), filename) {
        Ok(result) => {
            let alloc = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
                name: "alloc".to_string(),
                args: vec![("size".to_string(), crate::lir::Type::Int)],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Mutable,
                    Box::new(crate::lir::Type::Any),
                ),
                body: vec![crate::asm::StandardOp::Alloc(crate::asm::SP.deref())],
            });

            let get_sp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_sp".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Immutable,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::SP, 1)],
            });

            let get_fp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_fp".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Immutable,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::FP, 1)],
            });

            let get_gp = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_gp".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Immutable,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::GP, 1)],
            });
            let get_fp_stack = crate::lir::ConstExpr::CoreBuiltin(crate::lir::CoreBuiltin {
                name: "get_fp_stack".to_string(),
                args: vec![],
                ret: crate::lir::Type::Pointer(
                    crate::lir::Mutability::Immutable,
                    Box::new(crate::lir::Type::Cell),
                ),
                body: vec![crate::asm::CoreOp::Push(crate::asm::FP_STACK, 1)],
            });

            let mut debug_body = vec![];
            for ch in format!("Debug\n").chars() {
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
                    ("alloc", alloc),
                    ("debug", debug),
                    ("get_sp", get_sp),
                    ("get_fp", get_fp),
                    ("get_fp_stack", get_fp_stack),
                    ("get_gp", get_gp),
                ],
                result,
            ))
        }
        Err(e) => Err(e.to_string()),
    }
}
