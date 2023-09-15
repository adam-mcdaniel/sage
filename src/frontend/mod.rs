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
                ret: crate::lir::Type::Pointer(crate::lir::Mutability::Mutable, Box::new(crate::lir::Type::Any)),
                body: vec![crate::asm::StandardOp::Alloc(crate::asm::SP.deref())],
            });

            // Debug function
            // Prints out stack pointer, frame pointer, and the value at the top of the stack.
            let debug = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
                name: "debug".to_string(),
                args: vec![],
                ret: crate::lir::Type::None,
                body: vec![
                    crate::asm::StandardOp::CoreOp(
                        crate::asm::CoreOp::Many(vec![
                            crate::asm::CoreOp::Set(crate::asm::A, 'S' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, 'P' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, ':' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, ' ' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Put(crate::asm::SP, Output::stdout_int()),
                            crate::asm::CoreOp::Set(crate::asm::A, '\n' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            
                            crate::asm::CoreOp::Set(crate::asm::A, 'F' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, 'P' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, ':' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, ' ' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Put(crate::asm::FP, Output::stdout_int()),
                            crate::asm::CoreOp::Set(crate::asm::A, '\n' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
        
                            crate::asm::CoreOp::Set(crate::asm::A, 'T' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, 'O' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, 'P' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, ':' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Set(crate::asm::A, ' ' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                            crate::asm::CoreOp::Put(crate::asm::SP.deref(), Output::stdout_int()),
                            crate::asm::CoreOp::Set(crate::asm::A, '\n' as i64),
                            crate::asm::CoreOp::Put(crate::asm::A, Output::stdout_char()),
                        ])
                    )
                ],
            });

            Ok(crate::lir::Expr::let_consts(
                vec![
                    ("alloc", alloc),
                    ("debug", debug),
                ],
                result,
            ))
        }
        Err(e) => Err(e.to_string()),
    }
}
