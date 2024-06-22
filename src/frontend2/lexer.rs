use log::trace;
use logos::Logos;
use std::{fmt::Display, ops::Range};
use super::*;

pub struct Lexer<'input> {
    input: &'input str,
}

pub fn symbol(token: Token) -> String {
    match token {
        Token::Symbol(s) => s,
        _ => unreachable!(),
    }
}

pub fn integer(token: Token) -> i64 {
    match token {
        Token::Integer(i) => i,
        _ => unreachable!(),
    }
}

pub fn float(token: Token) -> f64 {
    match token {
        Token::Float(f) => f,
        _ => unreachable!(),
    }
}

pub fn string(token: Token) -> String {
    match token {
        Token::String(s) => s,
        _ => unreachable!(),
    }
}

pub fn character(token: Token) -> char {
    match token {
        Token::Char(c) => c,
        _ => unreachable!(),
    }
}

pub fn boolean(token: Token) -> bool {
    match token {
        Token::Bool(b) => b,
        _ => unreachable!(),
    }
}

pub fn decorator(token: Token) -> sage_lisp::Expr {
    match token {
        Token::Decorator(d) => d,
        _ => unreachable!(),
    }
}

pub fn attribute(token: Token) -> sage_lisp::Expr {
    match token {
        Token::Attribute(a) => a,
        _ => unreachable!(),
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexicalError {
    Unexpected {
        range: Range<usize>,
    },
    InvalidLisp {
        error: String,
        range: Range<usize>,
    },
    // UnexpectedCharacter(char),
    UnexpectedEndOfInput,

    #[default]
    Unknown
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalError::Unexpected { range } => write!(f, "Unexpected token at {:?}", range),
            LexicalError::InvalidLisp { error, range } => write!(f, "Invalid Lisp expression at {:?}: {}", range, error),
            LexicalError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            LexicalError::Unknown => write!(f, "Unknown error"),
        }
    }
}

pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = Result<(usize, Token, usize), ParseError>> + 'a {
    let mut lexer = Token::lexer(input);
    std::iter::from_fn(move || {
        let token = lexer.next();
        let range = lexer.span();
        trace!("TOKEN: {}", input[range.clone()].to_string());
        match token {
            Some(Ok(token)) => {
                let triple = Token::to_lalr_triple((token, range.clone()));
                // Some(triple)
                if let Ok(triple) = triple {
                    Some(Ok(triple))
                } else {
                    trace!("SOME ERROR: {:?} at {:?}: {}", triple, range.clone(), &input[range]);
                    Some(Err(ParseError::Lexical(LexicalError::Unknown)))
                    // Some(Err("Unexpected token".to_string()))
                }
            }
            Some(Err(e)) => {
                // println!("ERR ERROR: {:?} at {:?}: {}", e, range.clone(), &input[range]);
                let next_token = lexer.next().unwrap().unwrap();
                trace!("Next token: {:?} = {}", next_token, &input[lexer.span()]);
                Some(Err(ParseError::Lexical(e)))
            },
            None => None,
        }
    })
}

// pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[test]
fn test_lexer() {

    let code = r#"// Create a C-like enum
    enum Direction {
        North, South, East, West
    }
    
    // Pattern match over a tuple of a Direction, Int, and struct
    match (Direction of South, 2, {x = 5, y = -6}) {
        (of North, _, _)
        | (of East, _, _)
        | (of West, _, _)
        | (of South, 3, _) => print("Incorrect!\n"),
        (of South, 2, {x = 5, y = -6}) => {
            // This will be the branch that matches
            print("Correct!\n");
        },
        _ => print("Incorrect!\n")
    }
    
    // Create a polymorphic Rust-like enum
    enum Option<T> {
        Some(T),
        Nothing
    }
    
    // Define a fallible division operation
    def divide(n: Int, d: Int): Option<Int> {
        if (d == 0) {
            return Option<Int> of Nothing;
        } else {
            return Option<Int> of Some(n / d);
        }
    }
    
    // Match over a division operation with an if-let statement
    if let of Some(n) = divide(6, 2) {
        print("6 / 2 = ", n, "\n");
    } else {
        print("6 / 2 = undefined\n");
    }
"#;

    let mut lexer = Token::lexer(code);

    while let Some(token) = lexer.next() {
        println!("{:?}", token);
    }
}

fn extract_char(lexer: &mut logos::Lexer<Token>) -> Option<char> {
    let slice = lexer.slice();
    // let slice = &slice[1..slice.len()-1];
    if let Ok(c) = snailquote::unescape(&format!("\"{}\"", slice)) {
        c.chars().nth(1)
    } else {
        None
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[\t\r ]+")]
#[logos(error=LexicalError)]
pub enum Token {
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
    #[regex(r"\n", logos::skip)]
    Newline,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |tok| tok.slice().to_string(), priority=1)]
    Symbol(String),
    /// Parse a floating point number.
    #[regex(r"[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?", |tok| tok.slice().parse::<f64>().unwrap(), priority=2)]
    Float(f64),
    #[regex(r"-?([0-9]+|0x[a-f0-9]+|0b[01]+|0o[0-7]+)", |tok| tok.slice().parse::<i64>().unwrap(), priority=3)]
    Integer(i64),
    /// Parse a string literal
    #[regex(r#""(?:[^"]|\\")*""#, |tok| snailquote::unescape(tok.slice()).unwrap(), priority=4)]
    String(String),
    #[regex(r"'(:?[^']|\\')*'", extract_char)]
    Char(char),
    #[regex("false|False", |_| false, priority=5)]
    #[regex("true|True", |_| true, priority=5)]
    Bool(bool),
    #[regex("None|none|()|pass", priority=5)]
    None,
    #[regex("Null|null|NULL", priority=5)]
    Null,
    #[regex(r"case", priority=5)]
    Case,

    // #[regex(r"#\[[^(\]\n)]*\]\n", |tok| {
    //     println!("DECORATOR: {:?}", tok.slice());
    //     sage_lisp::Expr::parse(&tok.slice()[2..tok.slice().len()-2]).map_err(|error| LexicalError::InvalidLisp {
    //         error,
    //         range: tok.span(),
    //     })
    // }, priority=99999999)]
    // Make decorator beat all other symbols
    #[regex(r"#\[[^\n]*\n", |tok| {
        println!("DECORATOR: {:?}", tok.slice());
        if tok.slice().trim().chars().last() != Some(']') {
            return Err(LexicalError::InvalidLisp {
                error: "Expected ']'".to_string(),
                range: tok.span(),
            });
        }
        let code = &tok.slice()[2..tok.slice().len()-2];
        println!("CODE: {}", code);
        match sage_lisp::Expr::parse(&code) {
            Ok(expr) => Ok(expr),
            Err(error) => {
                if let Ok(expr) = sage_lisp::Expr::parse(&format!("({})", &code)) {
                    return Ok(expr);
                }
                println!("ERROR: {:?}", error);
                Err(LexicalError::InvalidLisp {
                    error,
                    range: tok.span(),
                })
            }
        }
    }, priority=1000)]
    Decorator(sage_lisp::Expr),

    #[regex(r"#!\[[^\n]*\n", |tok| {
        println!("ATTRIBUTE: {:?}", tok.slice());
        if tok.slice().trim().chars().last() != Some(']') {
            return Err(LexicalError::InvalidLisp {
                error: "Expected ']'".to_string(),
                range: tok.span(),
            });
        }
        let code = &tok.slice()[3..tok.slice().len()-2];
        println!("CODE: {}", code);
        match sage_lisp::Expr::parse(&code) {
            Ok(expr) => Ok(expr),
            Err(error) => {
                if let Ok(expr) = sage_lisp::Expr::parse(&format!("({})", &code)) {
                    return Ok(expr);
                }
                println!("ERROR: {:?}", error);
                Err(LexicalError::InvalidLisp {
                    error,
                    range: tok.span(),
                })
            }
        }
    }, priority=1000)]
    Attribute(sage_lisp::Expr),


    #[regex(r"=")]
    Assign,
    #[regex(r"\+=")]
    PlusAssign,
    #[regex(r"-=")]
    MinusAssign,
    #[regex(r"\*=")]
    StarAssign,
    #[regex(r"/=")]
    DivideAssign,
    #[regex(r"&=")]
    AndAssign,
    #[regex(r"|=")]
    OrAssign,
    #[regex(r"\^=")]
    XorAssign,
    #[regex(r"%=")]
    ModAssign,
    #[regex(r"<<=")]
    LeftShiftAssign,
    #[regex(r">>=")]
    RightShiftAssign,


    #[regex(r"==")]
    Equal,
    #[regex(r"!=")]
    NotEqual,
    #[regex(r"<=")]
    LessEqual,
    #[regex(r">=")]
    GreaterEqual,
    // #[regex(r"<")]
    // LAngle,
    // #[regex(r">")]
    // RAngle,

    // Parse template parameters
    // #[regex(r"<+[^ ]([^>]*>+\,[ ]*)*[^>]*>+", |tok| {
    //     // Parse the template parameters
    //     println!("TEMPLATE: {:?}", tok.slice());
    //     let inner = &tok.slice()[1..tok.slice().len()-1];
    //     let tokens = Token::lexer(inner);
    //     let result = tokens.into_iter().map(|t| t.unwrap()).collect::<Vec<_>>();
    //     result
    // }, priority=5)]
    // Template(Vec<Token>),


    #[regex(r"<")]
    Less,
    #[regex(r">")]
    Greater,
    #[regex(r"<<")]
    LeftShift,
    #[regex(r">>")]
    RightShift,
    #[regex(r"\+")]
    Plus,
    #[regex(r"-")]
    Minus,
    #[regex(r"\*")]
    Star,
    #[regex(r"%")]
    Mod,
    #[regex(r"/")]
    Divide,
    #[regex(r"&")]
    Ampersand,
    #[regex(r"\^")]
    Xor,
    #[regex(r"|")]
    Bar,
    #[regex(r"~")]
    Negate,

    #[regex(r"and|&&", priority=5)]
    And,
    #[regex(r"\|\||or", priority=5)]
    Or,
    #[regex(r"!")]
    Bang,
    #[regex(r"not")]
    Not,


    #[regex(r":")]
    Specifier,
    #[regex(r"\.")]
    Dot,


    #[regex(r"\(", priority=0)]
    LParen,
    #[regex(r"\)", priority=0)]
    RParen,
    #[regex(r"\{", priority=0)]
    LBrace,
    #[regex(r"\}", priority=0)]
    RBrace,
    #[regex(r"\[", priority=0)]
    LBracket,
    #[regex(r"\]", priority=0)]
    RBracket,

    #[regex(r",")]
    Comma,
    #[regex(r";")]
    Semicolon,
    
    #[regex(r"type")]
    Type,
    #[regex(r"struct")]
    Struct,
    #[regex(r"enum")]
    Enum,
    #[regex(r"match")]
    Match,
    #[regex(r"of")]
    Of,
    #[regex(r"static")]
    Static,
    #[regex(r"when")]
    When,
    #[regex(r"if")]
    If,
    #[regex(r"else")]
    Else,
    #[regex(r"as")]
    As,
    #[regex(r"for")]
    For,
    #[regex(r"while")]
    While,
    #[regex(r"return")]
    Return,
    #[regex(r"def|fun")]
    Function,
    #[regex(r"let")]
    Let,
    #[regex(r"mut")]
    Mut,
    #[regex(r"const")]
    Const,
    #[regex(r"impl")]
    Impl,
    #[regex(r"extern")]
    Extern,
    #[regex(r"import")]
    Import,
    #[regex(r"->")]
    Arrow,
    #[regex(r"=>")]
    FatArrow,

    #[regex(r"\?")]
    Try,
    #[regex(r"new")]
    New,
    #[regex(r"del")]
    Delete,
    #[regex(r"sizeof")]
    SizeOf,
    
}

impl Token {
    pub fn to_lalr_triple(
        (t, range): (Self, Range<usize>),
    ) -> Result<(usize, Self, usize), LexicalError> {
        // if t == LexicalError::UnexpectedEndOfInput {
        //     Err(Self::Error::Unexpected { range: r })
        // } else {
        //     Ok((r.start, t, r.end))
        // }
        Ok((range.start, t, range.end))
    }
}
