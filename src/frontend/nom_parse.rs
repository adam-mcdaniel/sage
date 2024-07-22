use nom::{
    IResult,
    character::complete::{char, digit1, multispace0, multispace1, one_of},
    combinator::{map, map_res, opt},
    error::{ContextError, ParseError},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated},
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
};

use crate::{lir::*, parse::SourceCodeLocation};

fn whitespace<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    take_while(|c: char| c.is_whitespace())(input)
}

fn parse_const<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    // preceded(
    //     tag("const"),
    //     terminated(
    //         map_res(
    //             pair(
    //                 terminated(
    //                     map_res(
    //                         many1(one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")),
    //                         |s: Vec<char>| Ok(s.into_iter().collect::<String>())
    //                     ),
    //                     multispace0
    //                 ),
    //                 delimited(
    //                     char('='),
    //                     map_res(
    //                         many1(digit1),
    //                         |s: Vec<char>| Ok(s.into_iter().collect::<String>())
    //                     ),
    //                     multispace0
    //                 )
    //             ),
    //             |(name, value)| Ok(format!("{} = {}", name, value))
    //         ),
    //         char(';')
    //     )
    // )(input)
    parse_const_atom(input)
}

fn parse_const_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    //
    alt((
        parse_const_tuple,
    ))(input)
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

fn parse_symbol<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    map(many1(one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")), |x| x.into_iter().collect())(input)
}

fn parse_const_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = opt(tag("struct"))(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            preceded(
                tag("="),
                parse_const
            )
        ),
        tag(",")
    ))(input)?;

    let (input, _) = whitespace(input)?;
    
    let (input, _) = tag("}")(input)?;

    Ok((input, ConstExpr::Struct(fields.into_iter().collect())))
}