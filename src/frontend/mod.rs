//! # Frontend Module
//!
//! This module implements the frontend of the compiler, which is responsible for parsing
//! the source code and converting it into the LIR.

mod parse;
mod nom_parse;
use crate::side_effects::Output;
use no_comment::{languages, IntoWithoutComments};
use parse::*;

pub fn parse(code: impl ToString, filename: Option<&str>, add_builtins: bool) -> Result<crate::lir::Expr, String> {
    let code = code
        .to_string()
        .chars()
        .without_comments(languages::rust())
        .collect::<String>();

    match parse_frontend(code.as_ref(), filename) {
        Ok(result) => {
            if add_builtins {
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
                    result,
                ))
            } else {
                Ok(result)
            }
        }
        Err(e) => Err(e.to_string()),
    }
}
