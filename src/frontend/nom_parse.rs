use std::{
    hash::Hash,
    collections::HashMap,
    sync::RwLock,
};

use nom::{
    IResult,
    character::complete::{char, digit1, multispace0, multispace1, one_of},
    combinator::{map, map_res, opt, cut, recognize, all_consuming, verify},
    error::{ContextError, ParseError, context},
    multi::{many0, many1, many0_count},
    sequence::{delimited, pair, preceded, terminated},
    branch::alt,
    bytes::complete::{tag, take_while, take_while1, escaped_transform},
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
    static ref BIN_OPS: RwLock<HashMap<String, (u8, Associativity, Box<dyn lir::BinaryOp>)>> = {

        let mut result: HashMap<String, (u8, Associativity, Box<dyn lir::BinaryOp>)> = HashMap::new();

        result.insert("+".to_owned(), (10, Associativity::Left, Box::new(crate::lir::Add)));
        result.insert("-".to_owned(), (10, Associativity::Left, Box::new(crate::lir::Arithmetic::Subtract)));
        result.insert("*".to_owned(), (20, Associativity::Left, Box::new(crate::lir::Arithmetic::Multiply)));
        result.insert("/".to_owned(), (20, Associativity::Left, Box::new(crate::lir::Arithmetic::Divide)));
        result.insert("%".to_owned(), (20, Associativity::Left, Box::new(crate::lir::Arithmetic::Remainder)));
        result.insert("==".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::Equal)));
        result.insert("!=".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::NotEqual)));
        result.insert("<".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::LessThan)));
        result.insert("<=".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::LessThanOrEqual)));
        result.insert(">".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::GreaterThan)));
        result.insert(">=".to_owned(), (5, Associativity::Left, Box::new(crate::lir::Comparison::GreaterThanOrEqual)));
        result.insert("&&".to_owned(), (3, Associativity::Left, Box::new(crate::lir::And)));
        result.insert("||".to_owned(), (2, Associativity::Left, Box::new(crate::lir::Or)));
        result.insert("&".to_owned(), (4, Associativity::Left, Box::new(crate::lir::BitwiseAnd)));
        result.insert("|".to_owned(), (2, Associativity::Left, Box::new(crate::lir::BitwiseOr)));
        result.insert("^".to_owned(), (3, Associativity::Left, Box::new(crate::lir::BitwiseXor)));

        RwLock::new(result)
    };
}

fn whitespace<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while(|c: char| c.is_whitespace())(input)
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

fn parse_type_function<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("fun")(input)?;
    let (mut input, _) = whitespace(input)?;
    // Try to parse any polymorphic type parameters
    let mut poly_params = vec![];
    if let Ok((i, _)) = tag::<_, _, E>("<")(input) {
        let (i, _) = whitespace(i)?;
        let (i, mut params) = many0(terminated(parse_symbol, tag(",")))(i)?;
        let (i, _) = whitespace(i)?;
        let (i, last_param) = opt(parse_symbol)(i)?;
        let (i, _) = whitespace(i)?;
        let (i, _) = tag(">")(i)?;
        let (i, _) = whitespace(i)?;

        if let Some(last_param) = last_param {
            params.push(last_param);
        }

        poly_params = params.into_iter().map(|x| x.to_string()).collect();

        input = i;
    }

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

    if poly_params.is_empty() {
        Ok((input, Type::Proc(args, Box::new(ret))))
    } else {
        Ok((input, Type::Poly(poly_params, Box::new(Type::Proc(args, Box::new(ret))))))
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
    ))(input)?;
    let (input, _) = whitespace(input)?;

    Ok((input, ty))
}

fn parse_type<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Type, E> {
    let (input, ty) = alt((
        parse_type_apply,
        parse_type_atom,
    ))(input)?;

    Ok((input, ty))
}


fn parse_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, c) = parse_const(input)?;
    Ok((input, Expr::ConstExpr(c)))
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

fn parse_const_bool<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, bool, E> {
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

    Ok((input, result))
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
    eprintln!("Parsing struct");
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            preceded(
                pair(whitespace, tag("=")),
                parse_const
            )
        ),
        tag(",")
    ))(input)?;

    let (input, last) = opt(
        pair(
            parse_symbol,
            preceded(
                pair(whitespace, tag("=")),
                parse_const
            )
        )
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
    use rayon::vec;

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