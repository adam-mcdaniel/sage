use lalrpop_util::lalrpop_mod;
use no_comment::{languages, IntoWithoutComments};

// lalrpop_mod!(
//     #[allow(clippy::all)]
//     frontend_parser
// );
// Use Logos to create a lexer for the frontend.
use logos::Logos;
use std::ops::Range;

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

#[derive(Debug)]
pub enum LexicalError {
    Unexpected {
        range: Range<usize>,
    },
    // UnexpectedCharacter(char),
    UnexpectedEndOfInput,
}

pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = Result<(usize, Token, usize), String>> + 'a {
    let mut lexer = Token::lexer(input);
    std::iter::from_fn(move || {
        let token = lexer.next();
        let range = lexer.span();
        println!("TOKEN: {}", input[range.clone()].to_string());
        match token {
            Some(Ok(token)) => {
                let triple = Token::to_lalr_triple((token, range.clone()));
                // Some(triple)
                if let Ok(triple) = triple {
                    Some(Ok(triple))
                } else {
                    println!("SOME ERROR: {:?} at {:?}", triple, &range);
                    Some(Err("Unexpected token".to_string()))
                }
            }
            Some(Err(e)) => {
                // Some(Err(LexicalError::Unexpected { range }))
                println!("ERR ERROR: {:?} at {:?}", e, range);
                Some(Err("Unexpected".to_string()))
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
    #[token("false|False", |_| false)]
    #[token("true|True", |_| true)]
    Bool(bool),
    #[regex("None|()")]
    None,
    #[regex("Null|null|NULL")]
    Null,

    #[token("=")]
    Assign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    DivideAssign,
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    XorAssign,
    #[token("%=")]
    ModAssign,
    #[token("<<=")]
    LeftShiftAssign,
    #[token(">>=")]
    RightShiftAssign,


    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    // #[token("<")]
    // LAngle,
    // #[token(">")]
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


    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<<")]
    LeftShift,
    #[token(">>")]
    RightShift,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("%")]
    Mod,
    #[token("/")]
    Divide,
    #[token("&")]
    Ampersand,
    #[token("^")]
    Xor,
    #[token("|")]
    Bar,
    #[token("~")]
    Negate,

    #[regex(r"and|&&", priority=5)]
    And,
    #[regex(r"\|\||or", priority=5)]
    Or,
    #[token("!")]
    Bang,
    #[token("not")]
    Not,


    #[token(":")]
    Specifier,
    #[token(".")]
    Dot,


    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    
    #[token("type")]
    Type,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("match")]
    Match,
    #[token("of")]
    Of,
    #[token("static")]
    Static,
    #[token("when")]
    When,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("as")]
    As,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[regex(r"def|fun")]
    Function,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("impl")]
    Impl,
    #[token("extern")]
    Extern,
    #[token("import")]
    Import,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,

    #[token("?")]
    Try,
    #[token("new")]
    New,
    #[token("del")]
    Delete,
    #[token("sizeof")]
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
