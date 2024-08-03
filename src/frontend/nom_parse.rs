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
        parse_type_group,
        parse_type_tuple,
        parse_type_struct,
        parse_type_enum,
        parse_type_function,
        map(parse_symbol, |x| Type::Symbol(x.to_string())),
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
            Ok((i, op)) => {
                input = i;
                has_found_op = true;
                found_op = Some(*op_expr);
                break;
            },
            Err(_) => continue,
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
                Ok((i, op)) => {
                    println!("FOUND OPERATOR: {op}");
                    input = i;
                    has_found_op = true;
                    found_op = Some(op);
                    break;
                },
                Err(_) => continue,
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
    let (input, expr) = parse_expr_atom(input)?;
    Ok((input, Expr::EnumUnion(ty, name.to_string(), Box::new(expr))))
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
    let (input, mut exprs) = many0(terminated(parse_expr, tag(";")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last) = opt(parse_expr)(input)?;
    if let Some(last) = last {
        exprs.push(last);
    }

    let (input, _) = tag("}")(input)?;

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
        println!("{:#?}", parse_suite::<nom::error::VerboseError<&str>>(0, r#"
enum Result<T, E>:
    Ok(T)
    Err(E)

struct Point:
    x: Int
    y: Int

if True and 1 > 2 + 3:
    println(a + b)
    a - b
    while not a:
        println("Testing")
        std.println<Int>(5)
        increment(&mut a)

        Result<Int, String> of Ok(5)
        
        "#));
        
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