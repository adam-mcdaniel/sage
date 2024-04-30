mod lexer;
mod preprocessor;
pub use preprocessor::*;
pub use lexer::*;
use lalrpop_util::lalrpop_mod;
use crate::lir::*;
use std::fmt::Display;
lalrpop_mod!(
    #[allow(clippy::all)]
    frontend_parser, "/frontend2/frontend_parser.rs"
);

#[derive(Debug, PartialEq, Clone, Default)]
pub enum ParseError {
    Preprocessor(String),
    Lexical(LexicalError),
    Syntax(String),
    #[default]
    Unknown
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Lexical(e) => write!(f, "Lexical error: {}", e),
            ParseError::Syntax(e) => write!(f, "Syntax error: {}", e),
            ParseError::Preprocessor(e) => write!(f, "Preprocessor error: {}", e),
            ParseError::Unknown => write!(f, "Unknown error"),
        }
    }
}

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
}

#[test]
fn test_preprocessor() {
    println!("{:?}", sage_lisp::Expr::parse("\"hello world!\""));

    match parse_frontend(r#"
    // A curried compile time function that adds two compile time values
    #![defun add (x) (\(y) (+ x y))]
    
    println(
        // Add a value to an expression at compile time
        #[add 10]
        12
    );
    
    // A hashmap with a single key value pair
    #![define hashmap #["key" 6]]
    
    // Some code that returns a string to add to the compiled code
    #!["let x = 5;"]
    
    // Add a compile time variable to a runtime variable
    println(
        x + (
            #[hashmap "key"]
        )
    );
    "#) {
        Ok(expr) => {
            println!("{expr}\n-----\n{expr:?}");
            // Try to compile
            println!("\n\nRESULT {:?}", expr.compile());
        },
        Err(e) => panic!("{e}"),
    }
}

#[test]
fn test_functions() {

}

#[test]
fn test_parser() {
    match parse_frontend("
    // let x = Direction![Int] of North {
    //     radius = 5,
    //     center = {x = 0, y = 0}
    // };
    // return {test=Direction![Int] of North {
    //     radius = 5,
    //     center = {x = 0, y = 0}
    // }};

    // let x = 4;
    // let y = 10;
    // println(x < 5 and y > 6);

    fun testing[T](mut dir: Direction, x: T): Int {
        dir = Direction of North;
        let test = Shape![T] of Circle {
            radius = x,
            center = {x = 0, y = 0}
        };
        5;
    }

    enum Shape[T] {
        Circle {
            radius: T,
            center: Position
        },
        Rectangle {
            width: Int,
            height: Int
        }
    }

    let shape = Shape![Int] of Circle {
        radius=5, center = {x = 5, y = 6}
    };
    println(shape);

    // // def testing2(x: Int, y: Int, z: Int): Int = x + y * z * 2;
    // // let x = 5 && 6;

    // def poly_proc[A, B](a: A, b: B): A = a;

    // // // let x: Int = 1;
    // // // const y = { x = 5, y = {a = 1, b = 2} };

    // // // let static TEST: Int = 5;
    // // // let mut x: Int = 5;
    // // // x = 7;
    // // // let ref = &mut x;
    // // // *ref = 8;
    // // // ref[1] = 9;
    // // // 5;6;7;
    // // // -x.y;

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

    // // let x = poly_proc![Int, Int](5, 6);

    // struct HashMap[K, V] {
    //     data: Vec![(K, V)]
    // }

    // struct Vec[T] {
    //     data: &mut T,
    //     length: Int,
    //     cap: Int
    // }
    

    // def alloc(n: Int): &mut Cell {
    //     // let data = malloc(n);
    //     // data as &mut Cell
    //     return new 5 as &mut Cell;
    // }

    // def Vec_make[T](): Vec![T] {
    //     return {
    //         data=alloc(100) as &mut T,
    //         length=0,
    //         cap=100  
    //     };
    // }

    // let x = enum {Some Int, None} of Some 5;
    // let y: Option![Int] = x;
    // println(x);
    // let z = y;

    // println(y);
    // enum Option[T] {
    //     Some(T),
    //     None
    // }
    // println(z);


    // // let y = (5);
    // // let x = println(Option![Int] of Some 5);

    // def HashMap_make[K, V](): HashMap![K, V] {
    //     return {
    //         data=Vec_make![(K, V)](),
    //     };
    // }

    // // type Test = HashMap![
    // //     Vec![(Int, Int)],
    // //     Vec![(Int, Int)]
    // // ];
    // let test = 5;
    // let test2 = 6;

    // println(test + test2);

    // test.testing![Float](Direction of North, 5.0);

") {
        Ok(expr) => {
            println!("{expr}\n-----\n{expr:?}");
            // Try to compile
            println!("\n\nRESULT {:?}", expr.compile());
        },
        Err(e) => panic!("{e}"),
    }
}