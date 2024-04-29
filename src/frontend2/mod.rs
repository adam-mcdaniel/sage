mod lexer;
pub use lexer::*;
use lalrpop_util::lalrpop_mod;
use crate::lir::*;
use crate::parse;
lalrpop_mod!(
    #[allow(clippy::all)]
    frontend_parser, "/frontend2/frontend_parser.rs"
);

#[derive(Debug)]
pub enum Statement {
    Declaration(crate::lir::Declaration),
    Expr(crate::lir::Expr),
}

/// Parse Core and Standard variants of virtual machine source code.
/// This will return core code by default, but will fallback on standard.
pub fn parse_frontend(
    input: impl ToString,
) -> Result<crate::lir::Expr, String> {
    let code = input
        .to_string()
        .chars()
        .collect::<String>();
    let tokens = lexer::tokenize(&code);
    let just_tokens = tokens.map(|t| t.unwrap().1).collect::<Vec<_>>();
    println!("TOKENS: {:?}\n-----\n", just_tokens);
    let tokens = lexer::tokenize(&code);
    match frontend_parser::ProgramParser::new().parse(tokens) {
        Ok(parsed) => Ok(parsed),
        Err(e) => Err(crate::parse::format_error(&code, e)),
    }

    // let tokens = lexer::tokenize(&code);
    // match frontend_parser::ProgramParser::new().parse(tokens) {
    //     Ok(parsed) => Ok(parsed),
    //     Err(e) => Err(crate::parse::format_error(&code, e)),
    // }
}

#[test]
fn test_parser() {
    match parse_frontend("
    fun testing(mut dir: Direction): Int {
        dir = Direction of North;
        let test = Shape of Circle {
            radius = 5,
            center = {x = 0, y = 0}
        };
        5;
    }

    enum Shape {
        Circle {
            radius: Int,
            center: Position
        },
        Rectangle {
            width: Int,
            height: Int
        }
    }

    // def testing2(x: Int, y: Int, z: Int): Int = x + y * z * 2;

    def poly_proc<A, B>(a: A, b: B): A = a;

    // // let x: Int = 1;
    // // const y = { x = 5, y = {a = 1, b = 2} };

    // // let static TEST: Int = 5;
    // // let mut x: Int = 5;
    // // x = 7;
    // // let ref = &mut x;
    // // *ref = 8;
    // // ref[1] = 9;
    // // 5;6;7;
    // // -x.y;

    // struct Rectangle {
    //     width: Int,
    //     height: Int
    // }

    struct Position {
        x: Int,
        y: Int
    }

    enum Direction {
        North, South, East, West
    }

    let x = poly_proc<Int, Int>(5, 6);

    struct Vec<T> {
        data: &mut T,
        length: Int,
        cap: Int
    }
    
    struct HashMap<K, V> {
        data: Vec<(K, V)>
    }

    let test = 5;
    let test2 = 6;

    println(test + test2);

    // testing(Direction of North);

") {
        Ok(expr) => {
            println!("{expr}\n-----\n{expr:?}");
            // Try to compile
            println!("\n\nRESULT {:?}", expr.compile());
        },
        Err(e) => panic!("{e}"),
    }
}