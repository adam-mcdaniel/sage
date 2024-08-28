use std::{
    collections::{BTreeMap, HashMap}, hash::Hash, result, sync::RwLock
};

use nom::{
    branch::alt, bytes::complete::{escaped_transform, tag, take_while, take_while1}, character::complete::{char, crlf, digit1, multispace0, multispace1, one_of}, combinator::{all_consuming, cut, map, map_res, not, opt, recognize, verify}, error::{context, ContextError, ParseError}, multi::{many0, many0_count, many1}, sequence::{delimited, pair, preceded, terminated}, IResult
};


use nom::{
    bytes::complete::{escaped, }, character::complete::{alpha1, alphanumeric0, alphanumeric1, anychar, none_of}, combinator::{value}, error::{convert_error, ErrorKind, VerboseError}
};
use crate::{lir::{self, *}, parse::SourceCodeLocation};
const KEYWORDS: &[&str] = &[
    "def",
    "fun",
    "struct",
    "enum",
    "mut",
    "let",
    "if",
    "else",
    "while",
    "for",
    "return",
    "match",
    "True",
    "False",
    "Null",
    "None",
    "sizeof",
    "Int",
    "Float",
    "Char",
    "Bool",
    "Cell",
    "Never",
    "!",
];

use lazy_static::lazy_static;
enum Associativity {
    Left,
    Right,
}

lazy_static! {
    // Declare a map of binary operators, their precedence, associativity, and their operator value
    static ref BIN_OPS: RwLock<BTreeMap<String, (u8, Associativity, fn(Expr, Expr) -> Expr)>> = {

        let mut result: BTreeMap<String, (u8, Associativity, fn(Expr, Expr) -> Expr)> = BTreeMap::new();
        result.insert("+".to_owned(), (10, Associativity::Left, |a, b| a.add(b)));
        result.insert("-".to_owned(), (10, Associativity::Left, |a, b| a.sub(b)));
        result.insert("*".to_owned(), (20, Associativity::Left, |a, b| a.mul(b)));
        result.insert("/".to_owned(), (20, Associativity::Left, |a, b| a.div(b)));
        result.insert("%".to_owned(), (20, Associativity::Left, |a, b| a.rem(b)));
        result.insert("==".to_owned(), (5, Associativity::Left, |a, b| a.eq(b)));
        result.insert("!=".to_owned(), (5, Associativity::Left, |a, b| a.neq(b)));
        result.insert("<".to_owned(), (5, Associativity::Left, |a, b| a.lt(b)));
        result.insert("<=".to_owned(), (5, Associativity::Left, |a, b| a.le(b)));
        result.insert(">".to_owned(), (5, Associativity::Left, |a, b| a.gt(b)));
        result.insert(">=".to_owned(), (5, Associativity::Left, |a, b| a.ge(b)));
        result.insert("&&".to_owned(), (3, Associativity::Left, |a, b| a.and(b)));
        result.insert("and".to_owned(), (3, Associativity::Left, |a, b| a.and(b)));
        result.insert("||".to_owned(), (2, Associativity::Left, |a, b| a.or(b)));
        result.insert("or".to_owned(), (2, Associativity::Left, |a, b| a.or(b)));
        result.insert("&".to_owned(), (4, Associativity::Left, |a, b| a.bitand(b)));
        result.insert("|".to_owned(), (2, Associativity::Left, |a, b| a.bitor(b)));
        result.insert("^".to_owned(), (3, Associativity::Left, |a, b| a.bitxor(b)));

        // result.insert("-".to_owned(), (10, Associativity::Left, Box::new(crate::lir::Arithmetic::Subtract)));
        // result.insert("*".to_owned(), (20, Associativity::Left, Box::new(crate::lir::Arithmetic::Multiply)));
        // result.insert("/".to_owned(), (20, Associativity::Left, Box::new(crate::lir::Arithmetic::Divide)));
        // result.insert("%".to_owned(), (20, Associativity::Left, Box::new(crate::lir::Arithmetic::Remainder)));
        // result.insert("==".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::Equal)));
        // result.insert("!=".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::NotEqual)));
        // result.insert("<".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::LessThan)));
        // result.insert("<=".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::LessThanOrEqual)));
        // result.insert(">".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::GreaterThan)));
        // result.insert(">=".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::GreaterThanOrEqual)));
        // result.insert("&&".to_owned(), (3, Associativity::Left, Box::new(crate::lir::And)));
        // result.insert("||".to_owned(), (2, Associativity::Left, Box::new(crate::lir::Or)));
        // result.insert("&".to_owned(), (4, Associativity::Left, Box::new(crate::lir::BitwiseAnd)));
        // result.insert("|".to_owned(), (2, Associativity::Left, Box::new(crate::lir::BitwiseOr)));
        // result.insert("^".to_owned(), (3, Associativity::Left, Box::new(crate::lir::BitwiseXor)));

        RwLock::new(result)
    };

    static ref UN_OPS: RwLock<BTreeMap<String, (u8, fn(Expr) -> Expr)>> = {
        let mut result: BTreeMap<String, (u8, fn(Expr) -> Expr)> = BTreeMap::new();
        
        result.insert("!".to_owned(), (11, |x| x.not()));
        result.insert("not".to_owned(), (11, |x| x.not()));
        result.insert("new".to_owned(), (1, |x| x.unop(New)));
        result.insert("-".to_owned(), (11, |x| x.neg()));
        result.insert("~".to_owned(), (11, |x| x.bitnot()));
        result.insert("*".to_owned(), (11, |x| x.deref()));
        result.insert("&".to_owned(), (11, |x| x.refer(Mutability::Immutable)));
        result.insert("&mut".to_owned(), (11, |x| x.refer(Mutability::Mutable)));

        RwLock::new(result)
    };
}

fn get_max_precedence() -> u8 {
    BIN_OPS.read().unwrap().values().map(|x| x.0).max().unwrap()
}

fn whitespace<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while(|c: char| c.is_whitespace())(input)
}

fn til_eol<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while(|c: char| c != '\n')(input)
}

fn til_first_non_ws_line<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    // Consume whitespace until we find a non-whitespace character
    let mut n = 0;
    while input.chars().nth(n).unwrap_or('\0').is_whitespace() {
        n += 1;
    }

    // Now retreat until we find a newline
    while input.chars().nth(n).unwrap_or('\n') != '\n' && n > 0 {
        n -= 1;
    }

    Ok((&input[n..], &input[..n]))
}

fn get_indentation(ws: &str) -> u8 {
    let spaces = ws.chars().filter(|c| *c == ' ').count() as u8;
    let tabs = ws.chars().filter(|c| *c == '\t').count() as u8;
    println!("Spaces: {spaces}, Tabs: {tabs}");
    spaces + tabs * 4
}

fn parse_whitespace_sensitive_block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(indentation: u8, mut input: &'a str) -> IResult<&'a str, Expr, E> {
    println!("Parsing block with indentation {indentation}");
    let mut input_updater = input;
    let mut is_first = true;

    let mut result = vec![];
    loop {
        input = input_updater;
        let (input, _) = til_first_non_ws_line(input)?;
        let beginning_of_line = input;
        println!("Parsing line: {input}");
        // Get new indentation level
        let (input, new_indentation_ws) = whitespace(input)?;
        if input.is_empty() {
            println!("Empty line, breaking out of block");
            break;
        }

        println!("Parsing line: {input}");
        let new_indentation = get_indentation(new_indentation_ws);
        println!("New indentation: {new_indentation}, `{new_indentation_ws}`");
        // Confirm that the new indentation is 4 spaces greater than the current indentation
        if new_indentation != indentation && is_first {
            // Throw an error
            println!("Indentation error: {new_indentation} != {indentation}");
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Verify)));
        } else if new_indentation != indentation {
            println!("Indentation error: {new_indentation} != {indentation}");
            // If the remainder of the line is empty, skip it
            if let Ok((input, _)) = crlf::<&str, E>(input) {
                input_updater = input;
                continue;
            }
            println!("Non-indented line, breaking out of block");
            break;
        }

        // Parse the expression
        println!("Parsing expression: {input}");
        if let Ok((input, expr)) = parse_expr::<E>(input) {
            println!("Parsed expression: {expr}");
            input_updater = input;
            result.push(expr);
        } else {
            if beginning_of_line == input {
                return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Verify)));
            }

            let (input, expr) = parse_suite(indentation, beginning_of_line)?;
            println!("Parsed expression: {expr}");
            input_updater = input;
            result.push(expr);
        }
        is_first = false;
    }

    Ok((input, Expr::Many(result)))
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(crate::lir::Declaration),
    Expr(crate::lir::Expr),
}


fn stmts_to_expr(stmts: Vec<Statement>) -> Expr {
    use std::collections::VecDeque;
    let rev_stmts = stmts.into_iter().rev().collect::<Vec<_>>();
    let mut body = Expr::NONE;
    let mut result = VecDeque::new();
    let mut decls = VecDeque::new();
    for stmt in rev_stmts {
        match stmt {
            Statement::Expr(e) => {
                result.push_front(e);
            },
            Statement::Declaration(decl) => {
                if decl.is_compile_time_declaration() {
                    decls.push_front(decl);
                } else {
                    if !result.is_empty() {
                        if body != Expr::NONE {
                            result.push_back(body);
                            body = Expr::Many(result.into());
                        } else if result.len() > 1 {
                            body = Expr::Many(result.into());
                        } else {
                            body = result.pop_front().unwrap();
                        }
                        result = VecDeque::new();
                    }
                    body = body.with(decl);
                }
            },
        }
    }
    if !result.is_empty() {
        if body != Expr::NONE {
            result.push_back(body);
        }
        body = Expr::Many(result.into());
    }

    if decls.is_empty() {
        body
    } else {
        body.with(Declaration::Many(decls.into()))
    }
}

pub fn parse(input: &str) -> Result<Expr, String> {
    fn parse_helper<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
        let (input, _) = whitespace(input)?;
        let (input, stmts) = many0(context("statement", parse_stmt))(input)?;
        // let (input, stmts) = many0(terminated(cut(context("statement", parse_stmt)), whitespace))(input)?;
        let (input, _) = whitespace(input)?;
        Ok((input, stmts_to_expr(stmts)))
    }

    match all_consuming(parse_helper::<VerboseError<&str>>)(input) {
        Ok((_, expr)) => Ok(expr),
        Err(nom::Err::Error(e)) => {
            println!("Error: {e}");
            Err(format!("{}", convert_error(input, e)))
        },
        Err(nom::Err::Failure(e)) => {
            println!("Failure: {e}");
            Err(format!("{}", convert_error(input, e)))
        },
        Err(nom::Err::Incomplete(e)) => {
            unreachable!()
        },
    }
    // let (input, _) = whitespace(input)?;
    // let (input, stmts) = many0(terminated(parse_stmt, whitespace))(input)?;
    // Ok((input, stmts_to_expr(stmts)))
}

fn parse_impl_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "impl" <name: Symbol> <args: Tuple<(<(<Symbol> ":")?> <Type>)>> <body: Block> => {
    //     let args: Vec<_> = args.into_iter().map(|(_name, ty)| ty).collect();
    //     Statement::Declaration(Declaration::Impl(name, args, body))
    // },
    println!("Parsing impl");
    let (input, _) = tag("impl")(input)?;

    let (input, ty) = cut(parse_type)(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    let (mut input, _) = whitespace(input)?;
    let mut impl_items = vec![];
    while let Ok((i, item)) = parse_impl_item::<E>(input, &ty) {
        println!("Parsed impl item: {item:?}");
        impl_items.push(item);
        let (i, _) = whitespace(i)?;
        input = i;
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;


    Ok((input, Statement::Declaration(Declaration::Impl(ty, impl_items))))
}

fn parse_impl_item<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str, ty: &Type) -> IResult<&'a str, (String, ConstExpr), E> {
    // let (input, _) = whitespace(input)?;
    if let Ok((input, method)) = parse_impl_method::<E>(input, ty) {
        return Ok((input, method));
    }

    let (input, item) = alt((
        context("function", parse_impl_fun),
        context("const", parse_impl_const),
    ))(input)?;
    Ok((input, item))
}

fn parse_impl_fun<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, (String, ConstExpr), E> {
    let (input, _) = tag("fun")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // Check if there are any template args
    let (input, template_args) = cut(opt(parse_type_params))(input)?;
    // Get the function parameters with mutability
    let (input, (params, ret)) = cut(parse_fun_params)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_block)(input)?;
    // Ok((input, (name.to_owned(), ConstExpr::Proc(Procedure::new(name, params, ret, body))))
    lazy_static! {
        static ref IMPL_FUN_COUNTER: RwLock<usize> = RwLock::new(0);
    }

    let mut count = *IMPL_FUN_COUNTER.read().unwrap();
    count += 1;
    *IMPL_FUN_COUNTER.write().unwrap() = count;

    if let Some(args) = template_args {
        Ok((input, (name.to_owned(), ConstExpr::PolyProc(PolyProcedure::new(name.to_owned(), args.into_iter().map(|x| x.to_owned()).collect(), params, ret, body)))))
    } else {
        Ok((input, (name.to_owned(), ConstExpr::Proc(Procedure::new(None, params, ret, body)))))
    }
}

fn parse_impl_const<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, (String, ConstExpr), E> {
    let (input, _) = tag("const")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // let (input, ty) = cut(parse_type)(input)?;
    // let (input, _) = whitespace(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = cut(parse_const)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(";")(input)?;
    Ok((input, (name.to_owned(), value)))
}

fn parse_impl_method<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str, ty: &Type) -> IResult<&'a str, (String, ConstExpr), E> {
    // "fun" <name: Symbol> <args: Tuple<(<(<Symbol> ":")?> <Type>)>> ":" <ret: Type> <body: Block> => {
    //     let args: Vec<_> = args.into_iter().map(|(_name, ty)| ty).collect();
    //     Statement::Declaration(Declaration::Proc(name.clone(), Procedure::new(name, args, ret, body)))
    // },
    println!("Parsing impl method");
    let (input, _) = tag("fun")(input)?;
    println!("Parsing method");
    let (input, _) = whitespace(input)?;
    println!("Parsing method name");
    let (input, name) = cut(parse_symbol)(input)?;
    println!("Parsed method name: {name}");
    let (input, _) = whitespace(input)?;
    // Check if there are any template args
    let (input, template_args) = cut(opt(parse_type_params))(input)?;
    println!("Parsed template args: {template_args:#?}");
    // Get the function parameters with mutability
    let (input, (params, ret)) = parse_method_params(input, ty)?;
    println!("Parsed method parameters: {params:#?}, {ret:#?}");
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_block)(input)?;
    // Ok((input, Statement::Declaration(Declaration::Proc(name.to_owned(), Procedure::new(Some(name.to_owned()), params, ret, body))))
    if let Some(args) = template_args {
        Ok((input, (name.to_owned(), ConstExpr::PolyProc(PolyProcedure::new(name.to_owned(), args.into_iter().map(|x| x.to_owned()).collect(), params, ret, body)))))
    } else {
        Ok((input, (name.to_owned(), ConstExpr::Proc(Procedure::new(Some(name.to_owned()), params, ret, body)))))
    }
}

fn parse_match_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "match" <expr: Expr> "{" <branches: Tuple<(<pattern: Pattern> "=>" <body: Block>)>> "}" => Statement::Match(expr, branches.into_iter().map(|(pat, body)| (pat, body)).collect()),
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("match")(input)?;
    println!("Parsing match");
    let (input, _) = whitespace(input)?;
    let (input, expr) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut branches) = many0(terminated(
        pair(
            parse_pattern,
            preceded(
                pair(whitespace, tag("=>")),
                cut(alt((parse_block, parse_expr)))
            )
        ),
        preceded(tag(","), whitespace)
    ))(input)?;
    println!("Parsed branches: {input}");
    println!("Parsed branches: {branches:#?}");
    let (input, branch) = opt(terminated(
        pair(
            parse_pattern,
            preceded(
                pair(whitespace, tag("=>")),
                cut(alt((parse_block, parse_expr)))
            )
        ),
        whitespace
    ))(input)?;

    if let Some((pat, body)) = branch {
        branches.push((pat, body));
    }


    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;
    Ok((input, Statement::Expr(Expr::Match(expr.into(), branches.into_iter().map(|(pat, body)| (pat, body)).collect()))))
}

fn parse_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, pattern) = alt((
        context("pointer", parse_pointer_pattern),
        context("struct", parse_struct_pattern),
        context("variant", parse_variant_pattern),
        context("group", delimited(tag("("), cut(parse_pattern), tag(")"))),
        context("tuple", parse_tuple_pattern),
        context("wildcard", map(tag("_"), |_| Pattern::Wildcard)),
        context("mutable symbol", map(preceded(tag("mut"), parse_symbol), |name| Pattern::Symbol(Mutability::Mutable, name.to_owned()))),
        context("symbol", map(parse_symbol, |name| Pattern::Symbol(Mutability::Immutable, name.to_owned()))),
        // context("tuple", map(ma
    ))(input)?;
    Ok((input, pattern))
}

fn parse_tuple_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut patterns) = many0(terminated(parse_pattern, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(parse_pattern)(input)?;
    if let Some(p) = pattern {
        patterns.push(p);
    }

    let (input, _) = tag(")")(input)?;
    Ok((input, Pattern::Tuple(patterns)))
}

fn parse_pointer_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("&")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = parse_pattern(input)?;
    Ok((input, Pattern::Pointer(Box::new(pattern))))
}

fn parse_variant_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, variant) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(parse_pattern)(input)?;
    Ok((input, Pattern::Variant(variant.to_owned(), pattern.map(Box::new))))
}

fn parse_struct_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            opt(preceded(
                pair(whitespace, tag("=")),
                parse_pattern
            ))
        ),
        tag(",")
    ))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(terminated(
        pair(
            parse_symbol,
            opt(preceded(
                pair(whitespace, tag("=")),
                parse_pattern
            ))
        ),
        whitespace
    ))(input)?;
    if let Some((name, pat)) = pattern {
        fields.push((name, pat));
    }

    let (input, _) = tag("}")(input)?;
    Ok((input, Pattern::Struct(fields.into_iter().map(|(name, pat)| (name.to_owned(), pat.unwrap_or(Pattern::Symbol(Mutability::Immutable, name.to_string())))).collect())))
}

fn parse_block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = whitespace(input)?;
    // let (input, expr) = parse_whitespace_sensitive_block(4, input)?;

    let (input, mut stmts) = many0(context("statement", parse_stmt))(input)?;
    
    // Check if there's a trailing expression
    let (input, expr) = opt(parse_expr)(input)?;
    if let Some(e) = expr {
        stmts.push(Statement::Expr(e));
    }
    let (input, _) = whitespace(input)?;

    let (input, _) = cut(tag("}"))(input)?;

    Ok((input, stmts_to_expr(stmts)))
}

fn parse_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;
    let (input, stmt) = alt((
        context("short statement", parse_short_stmt),
        context("long statement", parse_long_stmt),
        // map(parse_expr, Statement::Expr),
        // map(parse_decl, Statement::Declaration),
    ))(input)?;

    Ok((input, stmt))
}

fn parse_long_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;
    let (input, stmt) = alt((
        context("if", parse_if_stmt),
        context("when", parse_when_stmt),
        context("match", parse_match_stmt),
        context("while", parse_while_stmt),
        context("for", parse_for_stmt),
        context("function", parse_fun_stmt),
        context("impl", parse_impl_stmt),
        context("enum", parse_enum_stmt),
        context("struct", parse_struct_stmt),
        map(context("block", parse_block), Statement::Expr),
    ))(input)?;

    let (input, _) = whitespace(input)?;

    Ok((input, stmt))
}

fn parse_if_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "if" <condition: Expr> <then: Block> <else: Option<Block>> => Statement::If(condition, Box::new(then), else.map(Box::new)),
    let (input, _) = tag("if")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (mut input, then) = cut(parse_block)(input)?;

    let mut else_branches = vec![];
    // Try to parse else ifs
    while let Ok((x, _)) = preceded(whitespace::<E>, preceded(tag("else"), preceded(whitespace, tag("if"))))(input) {
        let (x, _) = whitespace(x)?;
        let (x, condition) = cut(parse_expr)(x)?;
        let (x, _) = whitespace(x)?;
        let (x, then) = cut(parse_block)(x)?;
        input = x;
        else_branches.push((condition, then));
    }



    let (input, mut result) = if let Ok((input, _)) = preceded(whitespace::<E>, preceded(tag("else"), whitespace::<E>))(input) {
        // Parse the else block
        cut(parse_block)(input)?
    } else {
        (input, Expr::NONE)
    };

    for (condition, then) in else_branches.into_iter().rev() {
        result = Expr::If(condition.into(), Box::new(then), Box::new(result));
    }

    Ok((input, Statement::Expr(Expr::If(condition.into(), Box::new(then), Box::new(result)))))
}

fn parse_when_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "when" <condition: Expr> <then: Block> <else: Option<Block>> => Statement::If(condition, Box::new(then), else.map(Box::new)),
    let (input, _) = tag("when")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_const)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, then) = cut(parse_block)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, else_) = opt(preceded(tag("else"), cut(parse_block)))(input)?;
    Ok((input, Statement::Expr(Expr::When(condition.into(), Box::new(then), else_.unwrap_or(Expr::NONE).into()))))
}

fn parse_while_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "while" <condition: Expr> <body: Block> => Statement::While(condition, Box::new(body)),
    let (input, _) = tag("while")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_block)(input)?;
    Ok((input, Statement::Expr(Expr::While(condition.into(), Box::new(body)))))
}

fn parse_for_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "for" <init: Option<Statement>> <condition: Option<Expr>> <step: Option<Statement>> <body: Block> => Statement::For(init.map(Box::new), condition.map(Box::new), step.map(Box::new), Box::new(body)),
    let (input, _) = tag("for")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, init) = cut(parse_short_stmt)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, step) = cut(parse_short_stmt)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_block)(input)?;
    // Ok((input, Statement::Expr(Expr::For(init.map(Box::new), condition.map(Box::new), step.map(Box::new), Box::new(body)))))
    let mut init_expr = Expr::NONE;
    let mut init_decl = Declaration::Many(vec![]);
    match init {
        Statement::Declaration(decl) => init_decl = decl,
        Statement::Expr(e) => init_expr = e,
    };

    let mut step_expr = Expr::NONE;
    if let Statement::Expr(e) = step {
        step_expr = e;
    }

    Ok((input, Statement::Expr(Expr::While(condition.into(), Expr::Many(vec![
        init_expr,
        body,
        step_expr,
    ]).into()).with(init_decl))))
}

fn parse_fun_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "fun" <name: Symbol> <args: Tuple<(<(<Symbol> ":")?> <Type>)>> ":" <ret: Type> <body: Block> => {
    //     let args: Vec<_> = args.into_iter().map(|(_name, ty)| ty).collect();
    //     Statement::Declaration(Declaration::Proc(name.clone(), Procedure::new(name, args, ret, body)))
    // },
    let (input, _) = tag("fun")(input)?;
    println!("Parsing function");
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // Check if there are any template args
    let (input, template_args) = cut(opt(parse_type_params))(input)?;
    // Get the function parameters with mutability
    println!("Parsing function parameters");
    println!("Input: {input}");
    let (input, (params, ret)) = cut(parse_fun_params)(input)?;
    println!("Parsed function parameters: {params:#?}, {ret:#?}");
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_block)(input)?;
    println!("Parsed function body: {body}");
    // Ok((input, Statement::Declaration(Declaration::Proc(name.to_owned(), Procedure::new(Some(name.to_owned()), params, ret, body)))))
    if let Some(args) = template_args {
        Ok((input, Statement::Declaration(Declaration::PolyProc(name.to_owned(), PolyProcedure::new(name.to_owned(), args.into_iter().map(|x| x.to_owned()).collect(), params, ret, body)))))
    } else {
        Ok((input, Statement::Declaration(Declaration::Proc(name.to_owned(), Procedure::new(Some(name.to_owned()), params, ret, body)))))
    }
}

fn parse_fun_params<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, (Vec<(String, Mutability, Type)>, Type), E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, params) = many0(terminated(
        pair(
            pair(map(opt(tag("mut")), |x| match x { Some(_) => Mutability::Mutable, None => Mutability::Immutable }), parse_symbol),
            preceded(
                pair(whitespace, tag(":")),
                cut(parse_type)
            )
        ),
        tag(",")
    ))(input)?;
    let (input, last) = opt(
        pair(
            // parse_symbol,
            pair(map(opt(tag("mut")), |x| match x { Some(_) => Mutability::Mutable, None => Mutability::Immutable }), parse_symbol),
            preceded(
                pair(whitespace, tag(":")),
                cut(parse_type)
            )
        )
    )(input)?;

    let mut params: Vec<_> = params.into_iter().map(|((mutability, name), ty)| (name.to_owned(), mutability, ty)).collect();
    if let Some(((mutability, name), ty)) = last {
        params.push((name.to_owned(), mutability, ty));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag(")"))(input)?;
    let (input, _) = whitespace(input)?;
    
    
    let (input, ret) = cut(opt(preceded(tag(":"), parse_type)))(input)?;

    Ok((input, (params, ret.unwrap_or(Type::None))))
}

fn parse_method_params<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str, ty: &Type) -> IResult<&'a str, (Vec<(String, Mutability, Type)>, Type), E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    
    // Get the self parameter, either `self`, `mut self`, `&self`, or `&mut self`
    let (input, self_param) = alt((
        context("self", map(tag("self"), |_| ("self".to_owned(), Mutability::Immutable, ty.clone()))),
        context("mut self", map(delimited(tag("mut"), whitespace, tag("self")), |_| ("self".to_owned(), Mutability::Mutable, ty.clone()))),
        context("&self", map(delimited(tag("&"), whitespace, tag("self")), |_| ("self".to_owned(), Mutability::Immutable, Type::Pointer(Mutability::Immutable, Box::new(ty.clone()))))),
        context("&mut self", map(delimited(delimited(tag("&"), whitespace, tag("mut")), whitespace, tag("self")), |_| ("self".to_owned(), Mutability::Immutable, Type::Pointer(Mutability::Mutable, Box::new(ty.clone()))))),
    ))(input)?;
    println!("Parsed self parameter: {self_param:#?}");
    let (input, _) = whitespace(input)?;
    let (input, _) = opt(tag(","))(input)?;

    let (input, params) = many0(terminated(
        pair(
            pair(map(opt(tag("mut")), |x| match x { Some(_) => Mutability::Mutable, None => Mutability::Immutable }), parse_symbol),
            preceded(
                pair(whitespace, tag(":")),
                cut(parse_type)
            )
        ),
        tag(",")
    ))(input)?;
    println!("Parsed self parameter: {params:#?}");
    let (input, last) = opt(
        pair(
            // parse_symbol,
            pair(map(opt(tag("mut")), |x| match x { Some(_) => Mutability::Mutable, None => Mutability::Immutable }), parse_symbol),
            preceded(
                pair(whitespace, tag(":")),
                cut(parse_type)
            )
        )
    )(input)?;
    println!("Parsed method parameters: {params:#?}, {last:#?}");
    println!("Parsed method parameters: {input}");

    let mut params: Vec<_> = std::iter::once(self_param).chain(params.into_iter().map(|((mutability, name), ty)| (name.to_owned(), mutability, ty))).collect();
    if let Some(((mutability, name), ty)) = last {
        params.push((name.to_owned(), mutability, ty));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag(")"))(input)?;
    let (input, _) = whitespace(input)?;
    
    
    let (input, ret) = cut(opt(preceded(tag(":"), parse_type)))(input)?;

    Ok((input, (params, ret.unwrap_or(Type::None))))
}

fn parse_extern_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "extern" <name: Symbol> <args: Tuple<(<(<Symbol> ":")?> <Type>)>> ":" <ret: Type> => {
    //     let args: Vec<_> = args.into_iter().map(|(_name, ty)| ty).collect();
    //     Statement::Declaration(Declaration::ExternProc(name.clone(), FFIProcedure::new(name, args, ret)))
    // },
    let (input, _) = tag("extern")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("fun"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) =cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    
    let (input, _) = whitespace(input)?;
    // let (input, _) = tag(":")(input)?;
    // let (input, ret) = parse_type(input)?;
    let (input, (params, ret)) = cut(parse_fun_params)(input)?;
    
    let args: Vec<_> = params.into_iter().map(|(_name, _mutability, ty)| ty).collect();
    Ok((input, Statement::Declaration(Declaration::ExternProc(name.to_owned(), FFIProcedure::new(name.to_owned(), args, ret)))))
}

fn parse_const_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "const" <name: Symbol> "=" <value: ConstExpr> => Statement::Declaration(Declaration::Const(name, value)),
    let (input, _) = tag("const")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("="))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = cut(parse_const)(input)?;
    Ok((input, Statement::Declaration(Declaration::Const(name.to_owned(), value))))
}

fn parse_var_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "let" <name: Symbol> ":" <ty: Type> "=" <value: Expr> => Statement::Declaration(Declaration::Var(name, Mutability::Immutable, Some(ty), value)),
    // "let" <name: Symbol> "=" <value: Expr> => Statement::Declaration(Declaration::Var(name, Mutability::Immutable, None, value)),
    let (input, _) = tag("let")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mutability) = opt(tag("mut"))(input)?;
    let mutability = if mutability.is_some() {
        Mutability::Mutable
    } else {
        Mutability::Immutable
    };

    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;

    let (input, _) = whitespace(input)?;
    let (input, ty) = opt(terminated(
        preceded(
            pair(whitespace, tag(":")),
            parse_type
        ),
        whitespace
    ))(input)?;

    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = parse_expr(input)?;

    Ok((input, Statement::Declaration(Declaration::Var(name.to_owned(), mutability, ty, value))))
}

fn parse_static_var_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "let" "static" <name: Symbol> ":" <ty: Type> "=" <value: ConstExpr> => Statement::Declaration(Declaration::StaticVar(name, Mutability::Immutable, ty, value)),
    // "let" "static" "mut" <name: Symbol> ":" <ty: Type> "=" <value: ConstExpr> => Statement::Declaration(Declaration::StaticVar(name, Mutability::Mutable, ty, value)),
    let (input, _) = tag("let")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("static")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mutability) = opt(tag("mut"))(input)?;
    let mutability = if mutability.is_some() {
        Mutability::Mutable
    } else {
        Mutability::Immutable
    };
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;

    let (input, _) = whitespace(input)?;
    let (input, ty) = opt(terminated(
        preceded(
            pair(whitespace, tag(":")),
            parse_type
        ),
        whitespace
    ))(input)?;

    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = parse_const(input)?;

    let ty = ty.unwrap_or(Type::None);
    Ok((input, Statement::Declaration(Declaration::StaticVar(name.to_owned(), mutability, ty, value))))
}

fn parse_type_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "type" <name: Symbol> "=" <ty: Type> => Statement::Declaration(Declaration::Type(name, ty)),
    let (input, _) = tag("type")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;
    Ok((input, Statement::Declaration(Declaration::Type(name.to_owned(), ty))))
}

fn parse_struct_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "struct" <name: Symbol> <fields: Tuple<(<Symbol> ":" <Type>)>> => {
    //     let fields: Vec<_> = fields.into_iter().map(|(name, ty)| (name, ty)).collect();
    //     Statement::Declaration(Declaration::Struct(name, fields))
    // },
    let (input, _) = tag("struct")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // Parse the template params
    let (input, template_params) = opt(parse_type_params)(input)?;

    let (input, _) = whitespace(input)?;

    let (input, _) = tag("{")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, fields) = many1(terminated(
        pair(
            parse_symbol,
            preceded(
                pair(whitespace, tag(":")),
                parse_type
            )
        ),
        tag(",")
    ))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("}"))(input)?;
    let fields = fields.into_iter().map(|(name, ty)| (name.to_owned(), ty)).collect();

    // Check if there are any template params
    if let Some(params) = template_params {
        Ok((input, Statement::Declaration(Declaration::Type(name.to_owned(), Type::Poly(params.into_iter().map(|x| x.to_owned()).collect(), Type::Struct(fields).into())))))
    } else {
        Ok((input, Statement::Declaration(Declaration::Type(name.to_owned(), Type::Struct(fields)))))
    }
}

fn parse_enum_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    let (input, _) = tag("enum")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    // Get the template params
    let (input, template_params) = opt(parse_type_params)(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    // Parse a comma separated list of symbols, optionally followed by a type
    let (input, mut fields) = many0(terminated(
        preceded(whitespace, pair(
            parse_symbol,
            opt(parse_type)
        )),
        terminated(tag(","), whitespace)
    ))(input)?;

    let (input, last) = opt(
        preceded(whitespace, pair(
            parse_symbol,
            opt(parse_type)
        ))
    )(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    // For all the fields that don't have a type, assign them the "None" type
    let fields = fields.into_iter().map(|(k, v)| (k.to_owned(), v.unwrap_or(Type::None))).collect();
    // println!("Fields: {fields}");
    // println!("Template params: {template_params}");
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    if let Some(params) = template_params {
        Ok((input, Statement::Declaration(Declaration::Type(name.to_owned(), Type::Poly(params.into_iter().map(|x| x.to_owned()).collect(), Type::EnumUnion(fields).into())))))
    } else {
        Ok((input, Statement::Declaration(Declaration::Type(name.to_owned(), Type::EnumUnion(fields)))))
    }
}



fn parse_return_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    // "return" <value: Expr> => Statement::Expr(Expr::Return(value.into())),
    let (input, _) = tag("return")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = cut(parse_expr)(input)?;
    Ok((input, Statement::Expr(Expr::Return(value.into()))))
}

fn parse_assign_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    let (input, dst) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    // First, try just the regular assignment
    match tag::<&str, &str, E>("=")(input) {
        Ok((input, _)) => {
            let (input, _) = whitespace(input)?;
            let (input, val) = cut(parse_expr)(input)?;
            // Ok((input, dst.assign(val)))

            let result = match dst {
                Expr::Deref(e) => Statement::Expr(e.deref_mut(val)),
                Expr::Index(e, idx) => Statement::Expr(e.idx(*idx).refer(Mutability::Mutable).deref_mut(val)),
                Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                    Statement::Expr(
                        Expr::var(name).refer(Mutability::Mutable)
                            .deref_mut(val)
                    )
                },
                Expr::Member(e, field) => {
                    Statement::Expr(e.field(field).refer(Mutability::Mutable).deref_mut(val))
                }
                _ => unreachable!(),
            };

            Ok((input, result))
        },
        Err(_) => {
            // If that fails, try the compound assignment
            let (input, op) = alt((
                value(Assign::new(Arithmetic::Add), tag("+=")),
                value(Assign::new(Arithmetic::Subtract), tag("-=")),
                value(Assign::new(Arithmetic::Multiply), tag("*=")),
                value(Assign::new(Arithmetic::Divide), tag("/=")),
                value(Assign::new(Arithmetic::Remainder), tag("%=")),
                value(Assign::new(BitwiseXor), tag("^=")),
                value(Assign::new(BitwiseAnd), tag("&=")),
                value(Assign::new(BitwiseOr), tag("|=")),
                // value(Assign::new(Shl), tag("<<=")),
                // value(Assign::new(Shr), tag(">>=")),

            ))(input)?;

            let (input, _) = whitespace(input)?;
            let (input, val) = cut(parse_expr)(input)?;
            Ok((input, Statement::Expr(dst.refer(Mutability::Mutable).assign_op(op, val))))
        }
    }


    // let (input, op) = alt((
    //     value(Assign::new(Arithmetic::Add), tag("+=")),
    //     value(Assign::new(Arithmetic::Subtract), tag("-=")),
    //     value(Assign::new(Arithmetic::Multiply), tag("*=")),
    //     value(Assign::new(Arithmetic::Divide), tag("/=")),
    //     value(Assign::new(Arithmetic::Remainder), tag("%=")),
    //     value(Assign::new(BitwiseXor), tag("^=")),
    //     value(Assign::new(BitwiseAnd), tag("&=")),
    //     value(Assign::new(BitwiseOr), tag("|=")),
    //     // value(Assign::new(Shl), tag("<<=")),
    //     // value(Assign::new(Shr), tag(">>=")),

    // ))(input)?;

    // let (input, _) = whitespace(input)?;
    // let (input, val) = parse_expr(input)?;
    // Ok((input, dst.assign_op(op, val)))
}

fn parse_short_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;

    // Check if there's just a semicolon
    if let Ok((input, _)) = tag::<&str, &str, E>(";")(input) {
        return Ok((input, Statement::Expr(Expr::NONE)));
    }

    let (input, stmt) = alt((
        context("extern", parse_extern_stmt),
        context("const", parse_const_stmt),
        context("let", parse_var_stmt),
        context("let static", parse_static_var_stmt),
        context("type", parse_type_stmt),
        context("return", parse_return_stmt),
        context("assignment", parse_assign_stmt),
        context("expression", map(parse_expr, Statement::Expr)),
    ))(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, stmt))
}

fn parse_type_pointer<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("&")(input)?;
    let (input, _) = whitespace(input)?;
    // Check for "mut" keyword
    let (input, mutability) = opt(tag("mut"))(input)?;
    let mutability = if mutability.is_some() {
        Mutability::Mutable
    } else {
        Mutability::Immutable
    };

    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;
    Ok((input, Type::Pointer(mutability, Box::new(ty))))
}

fn parse_type_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("*")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, size) = parse_const(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("]")(input)?;
    Ok((input, Type::Array(Box::new(ty), Box::new(size))))
}

fn parse_type_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut tys) = many1(terminated(parse_type, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_ty) = opt(parse_type)(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_ty) = last_ty {
        tys.push(last_ty);
    }

    Ok((input, Type::Tuple(tys)))
}

fn parse_type_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    // Optionally check for "struct" keyword
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            preceded(
                pair(whitespace, tag(":")),
                parse_type
            )
        ),
        tag(",")
    ))(input)?;

    let (input, last) = opt(
        pair(
            parse_symbol,
            preceded(
                pair(whitespace, tag(":")),
                parse_type
            )
        )
    )(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;
    
    Ok((input, Type::Struct(fields.into_iter().map(|(k, v)| (k.to_owned(), v)).collect())))
}

fn parse_type_enum<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    // Optionally check for "enum" keyword
    let (input, _) = opt(tag("enum"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    // Parse a comma separated list of symbols, optionally followed by a type
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            opt(parse_type)
        ),
        tag(",")
    ))(input)?;

    let (input, last) = opt(
        pair(
            parse_symbol,
            opt(parse_type)
        )
    )(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    // For all the fields that don't have a type, assign them the "None" type
    let fields = fields.into_iter().map(|(k, v)| (k.to_owned(), v.unwrap_or(Type::None))).collect();

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Type::EnumUnion(fields)))
}

fn parse_type_params<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<String>, E> {
    let (input, _) = tag("<")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut params) = many0(terminated(parse_symbol, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_param) = opt(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(">")(input)?;

    if let Some(last_param) = last_param {
        params.push(last_param);
    }

    Ok((input, params.into_iter().map(|x| x.to_string()).collect()))
}

fn parse_type_function<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("fun")(input)?;
    let (input, _) = whitespace(input)?;
    // Try to parse any polymorphic type parameters
    let (input, poly_params) = opt(parse_type_params)(input)?;

    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut args) = many0(terminated(parse_type, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_arg) = opt(parse_type)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ret) = parse_type(input)?;

    if let Some(last_arg) = last_arg {
        args.push(last_arg);
    }

    // Ok((input, Type::Proc(args, Box::new(ret))))

    match poly_params {
        Some(poly_params) => {
            Ok((input, Type::Poly(poly_params, Box::new(Type::Proc(args, Box::new(ret))))))
        }
        None => {
            Ok((input, Type::Proc(args, Box::new(ret))))
        }
    }
}

fn parse_type_apply<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, ty) = parse_type_atom(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("<")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut args) = many0(terminated(parse_type, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_arg) = opt(parse_type)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(">")(input)?;

    if let Some(last_arg) = last_arg {
        args.push(last_arg);
    }

    Ok((input, Type::Apply(Box::new(ty), args)))
}

fn parse_type_primitive<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, ty) = alt((
        value(Type::Int, tag("Int")),
        value(Type::Float, tag("Float")),
        value(Type::Char, tag("Char")),
        value(Type::Bool, tag("Bool")),
        value(Type::None, tag("None")),
        value(Type::Cell, tag("Cell")),
        value(Type::Never, tag("Never")),
        value(Type::Never, tag("!")),
        value(Type::None, tag("None")),
        value(Type::None, pair(
            tag("("),
            preceded(whitespace, tag(")"))
        )),
    ))(input)?;

    Ok((input, ty))
}



fn parse_type_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = whitespace(input)?;
    let (input, ty) = alt((
        parse_type_primitive,
        parse_type_pointer,
        parse_type_array,
        parse_type_tuple,
        parse_type_struct,
        parse_type_enum,
        parse_type_function,
        map(parse_symbol, |x| Type::Symbol(x.to_string())),
        parse_type_group,
    ))(input)?;

    Ok((input, ty))
}

fn parse_type_group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, ty))
}

fn parse_type<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, ty) = alt((
        parse_type_apply,
        parse_type_atom,
    ))(input)?;

    Ok((input, ty))
}

fn parse_indentation<'a, E: ParseError<&'a str> + ContextError<&'a str>>(indentation: u8, input: &'a str) -> IResult<&'a str, u8, E> {
    println!("Checking for indentation against {indentation}...");
    let (input, ws) = whitespace(input)?;
    let new_indentation = get_indentation(ws);
    if new_indentation != indentation && !input.is_empty() {
        println!("Failed");
        return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Verify)));
    }
    Ok((input, new_indentation))
}

fn parse_suite<'a, E: ParseError<&'a str> + ContextError<&'a str>>(indentation: u8, mut input: &'a str) -> IResult<&'a str, Expr, E> {
    println!("Parsing suite with indentation {indentation}");
    let mut input_updater = input;
    let mut result = vec![];
    loop {
        input = input_updater;
        // Try to match an `if`
        til_first_non_ws_line(input)?;
        println!("Checking for indentation against {indentation}...");
        println!("Input: {input}");
        let Ok((input, _)) = parse_indentation::<E>(indentation, input) else {
            break;
        };
        
        match alt((
            tag::<&str, &str, E>("if"),
            tag::<&str, &str, E>("while"),
            tag::<&str, &str, E>("struct"),
            tag::<&str, &str, E>("enum")
        ))(input) {
            Ok((input, "if")) => {
                println!("Parsing if");
                let (input, _) = whitespace(input)?;
                let (input, cond) = parse_expr(input)?;
                let (input, _) = whitespace(input)?;
                let (input, _) = tag(":")(input)?;
                let (input, body) = parse_whitespace_sensitive_block(indentation+4, input)?;
                input_updater = input;
                result.push(Expr::If(cond.into(), body.into(), Expr::NONE.into()))
            },
            Ok((input, "while")) => {
                println!("Parsing while");
                let (input, _) = whitespace(input)?;
                let (input, cond) = parse_expr(input)?;
                let (input, _) = whitespace(input)?;
                let (input, _) = tag(":")(input)?;
                let (input, body) = parse_whitespace_sensitive_block(indentation+4, input)?;
                input_updater = input;
                result.push(Expr::While(cond.into(), body.into()))
            },
            Ok((input, "enum")) => {
                let (input, _) = whitespace(input)?;
                let (input, name) = parse_symbol(input)?;
                
                let (input, _) = whitespace(input)?;
                let (input, template) = opt(parse_type_params)(input)?;
                let (mut input, _) = tag(":")(input)?;
                let mut variants = BTreeMap::new();
                input_updater = input;
                loop {
                    println!("Parsing enum variant");
                    input = input_updater;
                    let Ok((input, _)) = parse_indentation::<E>(indentation + 4, input) else {
                        println!("Indentation error");
                        break;
                    };
                    if input.is_empty() {
                        break;
                    }
                    let (input, name) = parse_symbol(input)?;
                    let (input, ty) = opt(alt((
                        parse_type_struct,
                        parse_type_group
                    )))(input)?;
                    let ty = ty.unwrap_or(Type::None);
                    variants.insert(name.to_string(), ty);
                    input_updater = input;
                }
                println!("Parsed enum: {name} {variants:?}");
                let mut ty = Type::EnumUnion(variants);
                if let Some(template) = template {
                    ty = Type::Poly(template, Box::new(ty));
                }
                result = vec![Expr::Many(result).with(Declaration::Type(name.to_owned(), ty))];
            },
            Ok((input, "struct")) => {
                let (input, _) = whitespace(input)?;
                let (input, name) = parse_symbol(input)?;
                
                let (input, _) = whitespace(input)?;
                let (input, template) = opt(parse_type_params)(input)?;
                let (input, _) = whitespace(input)?;

                let (mut input, _) = tag(":")(input)?;
                let mut members = BTreeMap::new();
                input_updater = input;
                loop {
                    println!("Parsing struct variant");
                    input = input_updater;
                    let Ok((input, _)) = parse_indentation::<E>(indentation + 4, input) else {
                        println!("Indentation error");
                        break;
                    };
                    if input.is_empty() {
                        break;
                    }
                    let (input, name) = parse_symbol(input)?;
                    let (input, _) = whitespace(input)?;
                    let (input, _) = tag(":")(input)?;
                    let (input, ty) = parse_type(input)?;
                    members.insert(name.to_string(), ty);
                    input_updater = input;
                }
                println!("Parsed enum: {name} {members:?}");
                let mut ty = Type::Struct(members);
                if let Some(template) = template {
                    ty = Type::Poly(template, Box::new(ty));
                }
                result = vec![Expr::Many(result).with(Declaration::Type(name.to_owned(), ty))];
            },
            _ => {
                println!("Parsing block");
                if input.is_empty() || input.chars().all(|c| c.is_whitespace()) {
                    break;
                }

                let (input, expr) = parse_whitespace_sensitive_block(indentation, input)?;
                println!("Parsed block: {expr}");
                input_updater = input;
                result.push(expr);
                break;
            }
        };

    }
    input = input_updater;

    Ok((input, Expr::Many(result)))
}


fn parse_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    // let (input, c) = parse_expr_atom(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_expr_prec(input, 0)?;
    // Ok((input, Expr::ConstExpr(c)))
    Ok((input, expr))
}

fn parse_expr_prec<'a, E: ParseError<&'a str> + ContextError<&'a str>>(mut input: &'a str, prec: u8) -> IResult<&'a str, Expr, E> {
    // if prec <= 0 {
    //     return parse_expr_atom(input);
    // }

    // println!("Parsing at prec={prec}");
    let (mut input, _) = whitespace(input)?;
    if prec >= get_max_precedence() {
        // println!("Max prec reached, falling back to atom");
        return parse_expr_term(input);
    }

    // Get
    let mut has_found_op = false;
    let mut found_op = None;

    for (op, (op_prec, op_expr)) in UN_OPS.read().unwrap().iter().rev() {
        if *op_prec > prec {
            continue;
        }
        match tag::<&str, &str, E>(op.as_str())(input) {
            Ok((i, _)) if is_symbol_char(i.chars().next().unwrap()) && is_symbol_char(op.chars().last().unwrap()) => continue,
            Ok((i, _)) => {
                input = i;
                has_found_op = true;
                found_op = Some(*op_expr);
                break;
            },
            _ => continue,
        }
    }
    
    let (mut input, mut lhs) = if has_found_op {
        let (input, lhs) = parse_expr_prec(input, prec)?;
        (input, found_op.unwrap()(lhs))
    } else {
        parse_expr_prec(input, prec + 1)?
    };

    // println!("Parsed lhs: {lhs}");
    let mut input_updater = input;
    loop {
        input = input_updater;

        // Try to match a binary operator
        let (mut input, _) = whitespace(input)?;
        // Match any of the binary operators
        let mut has_found_op = false;
        let mut found_op = None;
        for (op, (op_prec, _, _op_expr)) in BIN_OPS.read().unwrap().iter().rev() {
            if *op_prec < prec {
                continue;
            }
            match tag::<&str, &str, E>(op.as_str())(input) {
                Ok((i, op)) if i.chars().next().unwrap() != '=' => {
                    println!("FOUND OPERATOR: {op}");
                    input = i;
                    has_found_op = true;
                    found_op = Some(op);
                    break;
                },
                _ => continue,
            }
        }

        if !has_found_op {
            return Ok((input_updater, lhs));
        }

        let found_op = found_op.unwrap();

        let (input, _) = whitespace(input)?;

        // Get precedence of the operator
        let op_prec = BIN_OPS.read().unwrap().get(found_op).unwrap().0;

        if op_prec < prec {
            // println!("Operator precedence is less than current precedence, returning {lhs}");
            return Ok((input_updater, lhs));
        }

        let (input, rhs) = parse_expr_prec(input, prec + 1)?;
        // println!("Parsed rhs: {rhs}");
        // lhs = lhs.binop(BIN_OPS.read().unwrap().get(found_op).unwrap().2.clone(), rhs);
        let f = BIN_OPS.read().unwrap().get(found_op).unwrap().2;
        lhs = f(lhs, rhs);

        input_updater = input;
    }
}


fn parse_expr_term<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (mut input, mut expr) = parse_expr_factor(input)?;

    loop {
        let mut found = false;
        if let Ok((i, member)) = parse_expr_member::<E>(&expr, input) {
            input = i;
            expr = member;
            found = true;
        }
    
        if let Ok((i, index)) = parse_expr_index::<E>(&expr, input) {
            input = i;
            expr = index;
            found = true;
        }
    
        if let Ok((i, cast)) = parse_expr_cast::<E>(&expr, input) {
            input = i;
            expr = cast;
            found = true;
        }
    
        if let Ok((i, call)) = parse_expr_call::<E>(&expr, input) {
            input = i;
            expr = call;
            found = true;
        }

        if !found {
            break;
        }
    }

    Ok((input, expr))
}

fn parse_expr_factor<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, expr) = alt((
        parse_expr_variant,
        map(parse_const_monomorph, Expr::ConstExpr),
        parse_expr_atom,
        map(parse_const, Expr::ConstExpr),
    ))(input)?;

    Ok((input, expr))
}

fn parse_expr_variant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = opt(parse_expr_atom)(input)?;
    Ok((input, Expr::EnumUnion(ty, name.to_string(), Box::new(expr.unwrap_or(Expr::NONE)))))
}

fn parse_expr_index<'a, E: ParseError<&'a str> + ContextError<&'a str>>(expr: &Expr, input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, index) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("]")(input)?;

    Ok((input, expr.clone().idx(index)))
}

fn parse_expr_cast<'a, E: ParseError<&'a str> + ContextError<&'a str>>(expr: &Expr, input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("as")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;

    Ok((input, expr.clone().as_type(ty)))
}

fn parse_expr_call<'a, E: ParseError<&'a str> + ContextError<&'a str>>(expr: &Expr, input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut args) = many0(terminated(parse_expr, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_arg) = opt(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_arg) = last_arg {
        args.push(last_arg);
    }

    match expr {
        Expr::ConstExpr(ConstExpr::Symbol(name)) => {
            // Ok((input, Expr::var(name).app(args)))
            match name.as_str() {
                "print" => {
                    // return Ok((input, args.print()))
                    return Ok((input, Expr::Many(args.into_iter().map(|x| x.print()).collect())))
                },
                "println" => {
                    // return Ok((input, args.println()))
                    return Ok((input, Expr::Many(
                        args.into_iter().chain(vec![Expr::ConstExpr(ConstExpr::Char('\n'))])
                            .map(|x| x.print())
                            .collect()
                    )))
                },
                _ => {}
            }
        },
        _ => {}
    }

    Ok((input, expr.clone().app(args)))
}

fn parse_expr_member<'a, E: ParseError<&'a str> + ContextError<&'a str>>(expr: &Expr, input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let parse_field = |input| {
        let (input, _) = tag(".")(input)?;
        let (input, _) = whitespace(input)?;
        let (input, field) = alt((
            map(parse_symbol, |x| ConstExpr::Symbol(x.to_string())),
            map(parse_int_literal, |x| ConstExpr::Int(x)),
        ))(input)?;
        Ok((input, field))
    };

    let (input, fields) = many1(parse_field)(input)?;
    let mut expr = expr.clone();
    for field in fields {
        expr = expr.field(field);
    }

    Ok((input, expr))
}

fn parse_expr_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, expr) = alt((
        map(parse_const_atom, Expr::ConstExpr),
        parse_expr_group,
        parse_expr_tuple,
        parse_expr_array,
        parse_expr_struct,
        parse_expr_block,
    ))(input)?;

    Ok((input, expr))
}

fn parse_expr_group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, expr))
}

fn parse_expr_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many1(terminated(parse_expr, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_expr)(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, Expr::Tuple(exprs)))
}

fn parse_expr_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many0(terminated(parse_expr, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_expr)(input)?;
    let (input, _) = tag("]")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, Expr::Array(exprs)))
}

fn parse_expr_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        alt((
            pair(
                parse_symbol,
                preceded(
                    pair(whitespace, tag("=")),
                    parse_expr
                )
            ),
            map(parse_symbol, |x| (x, Expr::var(x.to_string()))),
        )),
        tag(",")
    ))(input)?;

    let (input, last) = opt(
        alt((
            pair(
                parse_symbol,
                preceded(
                    pair(whitespace, tag("=")),
                    parse_expr
                )
            ),
            map(parse_symbol, |x| (x, Expr::var(x.to_string()))),
        ))
    )(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Expr::Struct(fields.into_iter().map(|(k, v)| (k.to_owned(), v)).collect())))
}

fn parse_expr_block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    println!("Parsing block");
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = cut(many0(terminated(parse_expr, tag(";"))))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last) = opt(parse_expr)(input)?;
    if let Some(last) = last {
        exprs.push(last);
    }

    let (input, _) = cut(tag("}"))(input)?;

    Ok((input, Expr::Many(exprs)))
}

fn parse_const<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    alt((
        parse_const_monomorph,
        parse_const_term,
    ))(input)
}

fn parse_const_term<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    alt((
        parse_const_variant,
        parse_const_member,
    ))(input)
}

fn parse_const_monomorph<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, expr) = parse_const_term(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("<")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut tys) = many0(terminated(parse_type, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_ty) = opt(parse_type)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(">")(input)?;

    if let Some(last_ty) = last_ty {
        tys.push(last_ty);
    }

    Ok((input, expr.monomorphize(tys)))
}

fn parse_const_variant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_const_atom(input)?;
    Ok((input, ConstExpr::EnumUnion(ty, name.to_string(), Box::new(expr))))
}

fn parse_const_member<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, mut expr) = parse_const_atom(input)?;
    // Handle multiple dots
    let (input, _) = whitespace(input)?;
    let (input, fields) = many0(terminated(
        preceded(
            tag("."),
            alt((map(parse_symbol, |x| ConstExpr::Symbol(x.to_string())), map(parse_int_literal, |x| ConstExpr::Int(x))))
        ),
        whitespace
    ))(input)?;

    for field in fields {
        expr = expr.field(field);
    }

    Ok((input, expr))
}

fn parse_const_group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_const(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, expr))
}

fn parse_const_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = whitespace(input)?;
    alt((
        parse_const_sizeof_expr,
        parse_const_sizeof_type,
        parse_const_tuple,
        parse_const_bool,
        parse_const_null,
        parse_const_none,
        map(parse_float_literal, ConstExpr::Float),
        map(parse_int_literal, ConstExpr::Int),
        map(parse_string_literal, |s| ConstExpr::Array(s.to_string().chars().map(ConstExpr::Char)
            .chain(std::iter::once(ConstExpr::Char('\0')))
            .collect())),
        parse_const_array,
        parse_const_struct,
        parse_const_group,
        map(parse_symbol, |x| ConstExpr::Symbol(x.to_string())),
    ))(input)
}

fn parse_int_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, i64, E> {
    let (input, result) = map(digit1, |s: &str| s.parse().unwrap())(input)?;
    // println!("Got number: {:?}", result);
    // println!("Next char: {:?}", input.chars().next());
    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, result))
}

fn parse_float_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, f64, E> {
    let (input, result) = map(
        pair(digit1, preceded(char('.'), digit1)),
        |(a, b): (&str, &str)| format!("{}.{}", a, b).parse().unwrap(),
    )(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, result))
}

fn parse_inner_str_double<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
    let (a, b) = escaped_transform(
        none_of("\\\""),
        '\\',
        alt((
            value("\n", tag("n")),
            value("\"", tag("\"")),
            value("\\", tag("\\")),
        ))
    )(i)?;

    Ok((a, b))
}
    
fn parse_string_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    map(context(
      "string",
      alt((
            // preceded(char('\''), cut(terminated(parse_inner_str_single, char('\'')))),
            preceded(char('"'), cut(terminated(parse_inner_str_double, char('"')))),
      )),
    ), |s| s.to_string())(input)
}

fn parse_const_bool<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, result) = alt((
        value(true, tag("True")),
        value(false, tag("False")),
    ))(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, ConstExpr::Bool(result)))
}

fn parse_const_null<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("Null")(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, ConstExpr::Null))
}

fn parse_const_none<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("None")(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, ConstExpr::None))
}

fn parse_const_sizeof_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("sizeof")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, ConstExpr::SizeOfExpr(Box::new(expr))))
}

fn parse_const_sizeof_type<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("sizeof")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("<")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(">")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, ConstExpr::SizeOfType(ty)))
}

fn parse_const_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many1(terminated(parse_const, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_const)(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, ConstExpr::Tuple(exprs)))
}

fn parse_const_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many0(terminated(parse_const, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_const)(input)?;
    let (input, _) = tag("]")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, ConstExpr::Array(exprs)))
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn parse_symbol<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input, _) = whitespace(input)?;
    // recognize(all_consuming(pair(
    //     verify(anychar, |&c| c.is_lowercase() || c == '_'),
    //     many0_count(preceded(opt(char('_')), alphanumeric1)),
    // )))(input)
    let (input, result) = recognize(
        pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_"))))
        )
    )(input)?;
    if KEYWORDS.contains(&result) {
        return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Tag)));
    }
    Ok((input, result))
}

fn parse_const_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        alt((
            pair(
            parse_symbol,
                preceded(
                    pair(whitespace, tag("=")),
                    parse_const
                )
            ),
            map(parse_symbol, |x| (x, ConstExpr::Symbol(x.to_string()))),
        )),
        tag(",")
    ))(input)?;

    let (input, last) = opt(
        alt((
            pair(
                parse_symbol,
                preceded(
                    pair(whitespace, tag("=")),
                    parse_const
                )
            ),
            map(parse_symbol, |x| (x, ConstExpr::Symbol(x.to_string()))),
        ))
    )(input)?;
    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, ConstExpr::Struct(fields.into_iter().map(|(k, v)| (k.to_owned(), v)).collect())))
}


#[cfg(test)]
mod tests {
    use rayon::iter::Zip;

    use super::*;

    fn compile_and_run(code: &str, input: &str) -> Result<String, String> {
        // fn helper(code: &str, input: &str) -> Result<String, String> {
        //     use crate::{lir::Compile, parse::*, vm::*};
        //     use std::{
        //         fs::{read_dir, read_to_string},
        //         path::PathBuf,
        //     };
        //     let parsed = parse(code)?;
        //     println!("{parsed}");
        //     let asm_code = parsed.compile();
        //     const CALL_STACK_SIZE: usize = 1024;
        //     if let Err(ref e) = asm_code {
        //         return Err(format!("{e}"));
        //     }
        //     let asm_code = asm_code.unwrap();
    
        //     let vm_code = match asm_code {
        //         Ok(core_asm_code) => core_asm_code.assemble(CALL_STACK_SIZE).map(Ok),
        //         Err(std_asm_code) => std_asm_code.assemble(CALL_STACK_SIZE).map(Err),
        //     }
        //     .unwrap();
    
        //     let device = match vm_code {
        //         Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(input))
        //             .run(&vm_code)?,
        //             // .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
        //         Err(vm_code) => StandardInterpreter::new(TestingDevice::new(input))
        //             .run(&vm_code)?
        //             // .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
        //     };
    
        //     let output_text = device.output_str();
    
        //     Ok(output_text)
        // }


        rayon::ThreadPoolBuilder::new()
            .num_threads(1)
            .stack_size(512 * 1024 * 1024)
            .build_global()
            .unwrap();

        use crate::side_effects::Output;
        use no_comment::{languages, IntoWithoutComments};
        // Compiling most examples overflows the tiny stack for tests.
        // So, we spawn a new thread with a larger stack size.
        std::thread::scope(|s| {
            let child = std::thread::Builder::new()
                    .stack_size(512 * 1024 * 1024)
                    .spawn_scoped(s, move || {
                        use crate::{lir::Compile, parse::*, vm::*};
                        use ::std::{
                            fs::{read_dir, read_to_string},
                            path::PathBuf,
                        };
                        let parsed = parse(code)?;

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

                        let asm_code = crate::lir::Expr::let_consts(
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
                            parsed,
                        ).compile();
                        // let asm_code = parsed.compile();
                        const CALL_STACK_SIZE: usize = 1024;
                        if let Err(ref e) = asm_code {
                            return Err(format!("{e}"));
                        }
                        let asm_code = asm_code.unwrap();
                
                        let vm_code = match asm_code {
                            Ok(core_asm_code) => core_asm_code.assemble(CALL_STACK_SIZE).map(Ok),
                            Err(std_asm_code) => std_asm_code.assemble(CALL_STACK_SIZE).map(Err),
                        }
                        .unwrap();
                
                        let device = match vm_code {
                            Ok(vm_code) => CoreInterpreter::new(TestingDevice::new(input))
                                .run(&vm_code)?,
                                // .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                            Err(vm_code) => StandardInterpreter::new(TestingDevice::new(input))
                                .run(&vm_code)?
                                // .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                        };
                
                        let output_text = device.output_str();
                
                        Ok(output_text)
                    })
                    .unwrap();
            child.join().unwrap()
        })
    }

    fn assert_parse_const(input: &str, expected: Option<ConstExpr>) {
        let result = parse_const::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result.map_err(|e| match e {
            nom::Err::Error(e) => nom::error::convert_error(input, e),
            nom::Err::Failure(e) => nom::error::convert_error(input, e),
            nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e)
        }).unwrap();
        println!("{:?}", result);

        if let Some(expected) = expected {
            assert_eq!(result.1, expected);
        }
    }

    fn assert_parse_type(input: &str, expected: Option<Type>) {
        let result = parse_type::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result.map_err(|e| match e {
            nom::Err::Error(e) => nom::error::convert_error(input, e),
            nom::Err::Failure(e) => nom::error::convert_error(input, e),
            nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e)
        }).unwrap();
        println!("{:?}", result);

        if let Some(expected) = expected {
            assert_eq!(result.1, expected);
        }
    }

    fn unassert_parse_const(input: &str) {
        let result = parse_const::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result.map_err(|e| match e {
            nom::Err::Error(e) => nom::error::convert_error(input, e),
            nom::Err::Failure(e) => nom::error::convert_error(input, e),
            nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e)
        });
        println!("{:?}", result);
        assert!(result.is_err());
    }

    fn assert_parse_expr(input: &str, expr: Option<Expr>) {
        let result = parse_expr::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result.map_err(|e| match e {
            nom::Err::Error(e) => nom::error::convert_error(input, e),
            nom::Err::Failure(e) => nom::error::convert_error(input, e),
            nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e)
        }).unwrap();
        println!("{:?}", result);

        if let Some(expr) = expr {
            assert_eq!(result.1, expr);
        }
    }

    fn unassert_parse_expr(input: &str) {
        let result = parse_expr::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => println!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result.map_err(|e| match e {
            nom::Err::Error(e) => nom::error::convert_error(input, e),
            nom::Err::Failure(e) => nom::error::convert_error(input, e),
            nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e)
        });
        println!("{:?}", result);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_const() {
        assert_parse_const("1", Some(ConstExpr::Int(1)));

        assert_parse_const("1.0", Some(ConstExpr::Float(1.0)));

        assert_parse_const("\"hello\"", Some(ConstExpr::Array(vec![ConstExpr::Char('h'), ConstExpr::Char('e'), ConstExpr::Char('l'), ConstExpr::Char('l'), ConstExpr::Char('o'), ConstExpr::Char('\0')])));
        assert_parse_const("\"hello\\n\"", Some(ConstExpr::Array(vec![ConstExpr::Char('h'), ConstExpr::Char('e'), ConstExpr::Char('l'), ConstExpr::Char('l'), ConstExpr::Char('o'), ConstExpr::Char('\n'), ConstExpr::Char('\0')])));

        assert_parse_const("a", Some(ConstExpr::Symbol("a".to_string())));
        assert_parse_const("_a", Some(ConstExpr::Symbol("_a".to_string())));
        assert_parse_const("_a1", Some(ConstExpr::Symbol("_a1".to_string())));
        unassert_parse_const("1a");
        unassert_parse_const("1_");

        let s = {
            let mut fields = std::collections::BTreeMap::new();
            fields.insert("a".to_string(), ConstExpr::Int(1));
            fields.insert("b".to_string(), ConstExpr::Int(2));
            ConstExpr::Struct(fields)
        };
        assert_parse_const("struct {a = 1, b = 2}", Some(s.clone()));
        assert_parse_const("{a = 1, b = 2}", Some(s.clone()));
        assert_parse_const("{b = 2, a = 1}", Some(s.clone()));
        assert_parse_const("struct {b = 2, a = 1}", Some(s));

        let t = {
            let mut fields = std::collections::BTreeMap::new();
            fields.insert("a".to_string(), ConstExpr::Int(1));
            fields.insert("b".to_string(), ConstExpr::Int(2));
            ConstExpr::Tuple(vec![ConstExpr::Struct(fields.clone()), ConstExpr::Struct(fields)])
        };
        assert_parse_const("({a=1, b=2,}, {b=2, a=1})", Some(t.clone()));
        assert_parse_const("(1,)", Some(ConstExpr::Tuple(vec![ConstExpr::Int(1)])));

        let a = ConstExpr::Array(vec![ConstExpr::Int(1), ConstExpr::Int(2), ConstExpr::Int(3)]);
        assert_parse_const("[1, 2, 3]", Some(a.clone()));
        
        assert_parse_const("std.Option<Int>", Some(ConstExpr::Symbol("std".to_string()).field(ConstExpr::Symbol("Option".to_string())).monomorphize(vec![Type::Int])));
        assert_parse_const("sizeof([1, 2, 3])", Some(ConstExpr::SizeOfExpr(Expr::from(a.clone()).into())));
        assert_parse_const("sizeof<Int>()", Some(ConstExpr::SizeOfType(Type::Int)));
        unassert_parse_const("sizeof<Int>(5)");
        assert_parse_const("Result<Int, String> of Ok(5)", Some(
            ConstExpr::EnumUnion(Type::Symbol("Result".to_string()).apply(vec![
                Type::Int,
                Type::Symbol("String".to_string()),
            ]), "Ok".to_string(), Box::new(ConstExpr::Int(5)))
        ));
    }

    #[test]
    fn test_parse_type() {
        assert_parse_type("Cell", Some(Type::Cell));
        assert_parse_type("Int", Some(Type::Int));
        assert_parse_type("Float", Some(Type::Float));
        assert_parse_type("Never", Some(Type::Never));
        assert_parse_type("!", Some(Type::Never));
        assert_parse_type("()", Some(Type::None));
        assert_parse_type("( )", Some(Type::None));


        assert_parse_type("&Int", Some(Type::Pointer(Mutability::Immutable, Box::new(Type::Int))));
        assert_parse_type("&mut Int", Some(Type::Pointer(Mutability::Mutable, Box::new(Type::Int))));
        assert_parse_type("& mut Int", Some(Type::Pointer(Mutability::Mutable, Box::new(Type::Int))));
        assert_parse_type("&mut (Int, Char)", Some(Type::Pointer(Mutability::Mutable, Box::new(Type::Tuple(vec![
            Type::Int,
            Type::Char,
        ])))));

        assert_parse_type("[Int * 5]", Some(Type::Array(Box::new(Type::Int), Box::new(ConstExpr::Int(5)))));

        assert_parse_type("enum {Ok Int, Err &Char}", Some(Type::EnumUnion(vec![
            ("Ok".to_string(), Type::Int),
            ("Err".to_string(), Type::Pointer(Mutability::Immutable, Box::new(Type::Char))),
        ].into_iter().collect())));

        assert_parse_type("struct {a: Int, b: Char}", Some(Type::Struct(vec![
            ("a".to_string(), Type::Int),
            ("b".to_string(), Type::Char),
        ].into_iter().collect())));

        assert_parse_type("enum {Ok {x: Int, y: Int}, Err &Char}", Some(Type::EnumUnion(vec![
            ("Ok".to_string(), Type::Struct(
                vec![
                    ("x".to_string(), Type::Int),
                    ("y".to_string(), Type::Int),
                ].into_iter().collect()
            )),
            ("Err".to_string(), Type::Pointer(Mutability::Immutable, Box::new(Type::Char))),
        ].into_iter().collect())));

        assert_parse_type("Option<Int>", Some(Type::Apply(Box::new(Type::Symbol("Option".to_string())), vec![Type::Int])));
        assert_parse_type("fun(Option<Int>) -> Bool", Some(Type::Proc(vec![
            Type::Apply(Box::new(Type::Symbol("Option".to_string())), vec![Type::Int])
        ], Box::new(Type::Bool))));
        assert_parse_type("fun<T>(Option<T>) -> Bool", Some(Type::Poly(vec!["T".to_string()], Type::Proc(vec![
            Type::Apply(Box::new(Type::Symbol("Option".to_string())), vec![Type::Symbol("T".to_string())])
        ], Box::new(Type::Bool)).into())));

        assert_parse_type("fun<T>(Option<T>, Int) -> Bool", Some(Type::Poly(vec!["T".to_string()], Type::Proc(vec![
            Type::Apply(Box::new(Type::Symbol("Option".to_string())), vec![Type::Symbol("T".to_string())]),
            Type::Int,
        ], Box::new(Type::Bool)).into())));
    }

    #[test]
    fn test_parse_operator_precedence() {
        // Basic addition and subtraction
        assert_parse_expr("a - b", Some(Expr::var("a").sub(Expr::var("b"))));
        assert_parse_expr("a + b - c", Some(Expr::var("a").add(Expr::var("b")).sub(Expr::var("c"))));

        // Multiplication and division with addition and subtraction
        assert_parse_expr("a * b - c", Some(Expr::var("a").mul(Expr::var("b")).sub(Expr::var("c"))));
        assert_parse_expr("a / b + c", Some(Expr::var("a").div(Expr::var("b")).add(Expr::var("c"))));
        assert_parse_expr("a * b / c", Some(Expr::var("a").mul(Expr::var("b")).div(Expr::var("c"))));

        // Mixed operations with parentheses
        assert_parse_expr("a * (b + c)", Some(Expr::var("a").mul(Expr::var("b").add(Expr::var("c")))));
        assert_parse_expr("(a + b) * c", Some(Expr::var("a").add(Expr::var("b")).mul(Expr::var("c"))));
        assert_parse_expr("(a + b) * (c - d)", Some(Expr::var("a").add(Expr::var("b")).mul(Expr::var("c").sub(Expr::var("d")))));

        // Multiple operators with parentheses
        assert_parse_expr("a + (b * c) / d", Some(Expr::var("a").add(Expr::var("b").mul(Expr::var("c")).div(Expr::var("d")))));
        assert_parse_expr("a + b * (c - d) / e", Some(Expr::var("a").add(Expr::var("b").mul(Expr::var("c").sub(Expr::var("d"))).div(Expr::var("e")))));

        // Nested parentheses
        assert_parse_expr("a + (b * (c + d))", Some(Expr::var("a").add(Expr::var("b").mul(Expr::var("c").add(Expr::var("d"))))));
        assert_parse_expr("((a + b) * c) - d", Some(Expr::var("a").add(Expr::var("b")).mul(Expr::var("c")).sub(Expr::var("d"))));
    }

    #[test]
    fn test_parse_expr() {
        match compile_and_run(r#"
            fun memcpy<T>(dst: &mut T, src: &T, n: Int) {
                for let mut i = 0; i < n; i += 1; {
                    dst[i] = src[i];
                }
            }

            let x = 5;
            println(x);

            if False {
                println("Testing");
            } else if False {
                println("Not testing");
            } else {
                println("Testing again");
            }

            struct Point {
                x: Int,
                y: Int,
            }
            let p: Point = {x = 5, y = 10};

            fun add(a: Int, b: Int): Int {
                a + b
            }

            println(add(5, 10));

            enum Option<T> {
                Some(T),
                Nothing,
            }
            
            let o: Option<Int> = Option<Int> of Some(5);

            enum List<T> {
                Cons(T, &List<T>),
                Nil,
            }

            let l = List<Int> of Cons(5, 
                new List<Int> of Cons(10, 
                    new List<Int> of Nil));

            fun print_list<T>(l: &List<T>) {
                match l {
                    &of Cons(x, xs) => {
                        println(x);
                        print_list<T>(xs);
                    },
                    _ => {}
                }
            }

            print_list<Int>(&l);

            struct Vec<T> {
                len: Int,
                cap: Int,
                data: &mut T,
            }

            impl Vec<T> {
                fun new(): Vec<T> {
                    const START_CAP = 8;
                    return {
                        len = 0,
                        cap = START_CAP,
                        data = alloc(START_CAP * sizeof<T>()),
                    };
                }

                fun push(&mut self, x: T) {
                    if self.len == self.cap {
                        self.cap *= 2;
                        let new_data = alloc(self.cap * sizeof<T>()) as &mut T;
                        memcpy<Cell>(new_data as &mut Cell, self.data as &mut Cell, self.len * sizeof<T>());
                        free(self.data);
                    }
                    self.data[self.len] = x;
                    self.len += 1;
                }

                fun print(&self) {
                    print("[");
                    for let mut i = 0; i < self.len; i += 1; {
                        print(self.data[i]);
                        if i < self.len - 1 {
                            print(", ");
                        }
                    }
                    println("]");
                }
            }

            let mut v = Vec.new<Int>();

            for let mut i = 0; i < 10; i += 1; {
                v.push(i * i);
            }

            v.print();


            const std = {
                Vec = Vec,
                Option = Option,
                List = List,
            };

            const X = std.Vec;

            let mut x = X.new<Float>();
            x.push(3.14159);
            x.print();
            "#, "hello!") {
            Ok(expr) => {
                // println!("{:#?}", expr)
                // Compile and run
                println!("{}", expr)
            },
            Err(e) => println!("Error: {}", e),
        }
        return;
        assert_parse_expr("1", Some(Expr::ConstExpr(ConstExpr::Int(1))));
        assert_parse_expr("1.0", Some(Expr::ConstExpr(ConstExpr::Float(1.0))));

        assert_parse_expr("(1.0, {x, y})", Some(Expr::ConstExpr(ConstExpr::Tuple(vec![
            ConstExpr::Float(1.0),
            ConstExpr::Struct({
                let mut fields = std::collections::BTreeMap::new();
                fields.insert("x".to_string(), ConstExpr::Symbol("x".to_string()));
                fields.insert("y".to_string(), ConstExpr::Symbol("y".to_string()));
                fields
            })
        ]))));

        assert_parse_expr("{a;b}", Some(Expr::Many(vec![
            Expr::ConstExpr(ConstExpr::Symbol("a".to_string())),
            Expr::ConstExpr(ConstExpr::Symbol("b".to_string())),
        ])));
        assert_parse_expr("{a}", Some(Expr::ConstExpr(ConstExpr::Struct({
            let mut fields = std::collections::BTreeMap::new();
            fields.insert("a".to_string(), ConstExpr::var("a"));
            fields
        }))));
        
        // Now try calls
        assert_parse_expr("a()", Some(Expr::var("a").app(vec![])));
        assert_parse_expr("a(b)", Some(Expr::var("a").app(vec![Expr::var("b")])));
        assert_parse_expr("a.x(b)", Some(Expr::var("a").field(ConstExpr::var("x")).app(vec![Expr::var("b")])));
        assert_parse_expr("std.println<Int>(5)", Some(ConstExpr::var("std").field(ConstExpr::var("println")).monomorphize(vec![Type::Int]).app(vec![ConstExpr::Int(5).into()])));
        assert_parse_expr("True", Some(ConstExpr::Bool(true).into()));
        assert_parse_expr("!True || 1 + 2 >= 3", Some(
            Expr::from(ConstExpr::Bool(true)).not().or(
                Expr::from(ConstExpr::Int(1)).add(Expr::from(ConstExpr::Int(2))).ge(Expr::from(ConstExpr::Int(3)))
            )
        ));

        assert_parse_expr("*&a", Some(Expr::var("a").refer(Mutability::Immutable).deref()));
        assert_parse_expr("&*a", Some(Expr::var("a").deref().refer(Mutability::Immutable)));
        assert_parse_expr("&mut *a", Some(Expr::var("a").deref().refer(Mutability::Mutable)));

        println!("Parsing block");
//         println!("{:#?}", parse_suite::<nom::error::VerboseError<&str>>(0, r#"
// enum Result<T, E>:
//     Ok(T)
//     Err(E)

// struct Point:
//     x: Int
//     y: Int

// if True and 1 > 2 + 3:
//     println(a + b)
//     a - b
//     while not a:
//         println("Testing")
//         std.println<Int>(5)
//         increment(&mut a)

//         Result<Int, String> of Ok(5)
        
//         "#));
        
        // impl Result<T, E>:
        //     def unwrap(self) -> T:
        //         match self:
        //             of Ok(x) => x
        //             of Err(e) => panic(e)
    }


    // #[test]
    // fn test_parse_const_tuple() {
    //     let input = "(a = 1, b = 2)";
    //     let result = parse_const_tuple(input);
    //     assert_eq!(result, Ok(("", ConstExpr::Tuple(vec![ConstExpr::Tuple(vec![ConstExpr::Symbol("a".to_string()), ConstExpr::Int(1)]), ConstExpr::Tuple(vec![ConstExpr::Symbol("b".to_string()), ConstExpr::Int(2)])]))));
    // }

    // #[test]
    // fn test_parse_const_array() {
    //     let input = "[1, 2, 3]";
    //     let result = parse_const_array(input);
    //     assert_eq!(result, Ok(("", ConstExpr::Array(vec![ConstExpr::Int(1), ConstExpr::Int(2), ConstExpr::Int(3)])));
    // }

    // #[test]
    // fn test_parse_const_struct() {
    //     let input = "struct {a = 1, b = 2}";
    //     let result = parse_const_struct(input);
    //     assert_eq!(result, Ok(("", ConstExpr::Struct(vec![("a".to_string(), ConstExpr::Int(1)), ("b".to_string(), ConstExpr::Int(2))].into_iter().collect())));
    // }
}