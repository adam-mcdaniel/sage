mod parse;
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
            // eprintln!("Parsed LIR: {:#?}", result);
            let alloc = crate::lir::ConstExpr::StandardBuiltin(crate::lir::StandardBuiltin {
                name: "alloc".to_string(),
                args: vec![("size".to_string(), crate::lir::Type::Int)],
                ret: crate::lir::Type::Pointer(Box::new(crate::lir::Type::Any)),
                body: vec![crate::asm::StandardOp::Alloc(crate::asm::SP.deref())],
            });

            Ok(crate::lir::Expr::LetConst(
                "alloc".to_string(),
                alloc,
                Box::new(result),
            ))
        }
        Err(e) => Err(e.to_string()),
    }
}
