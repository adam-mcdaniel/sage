//! # Parsing Module
//!
//! This module contains all the related functions for parsing
//! any given source language supported by the compiler.
//!
//! Right now, this module supports parsing:
//! - LIR source
//! - Core Assembly source
//! - Standard Assembly source
//! - Core Virtual Machine code source
//! - Standard Virtual Machine code source
//!
//! ## Stability
//!
//! This module will likely be unstable for a long while,
//! because none of the language syntaxes are stablized.
//!
//! I also hope to replace the parsers with libraries using
//! fewer dependencies. LALRPOP requires a ridiculous number of
//! packages to function. Nom seems to be an ideal candidate.
//!
//! #### Error Types
//!
//! The error types in this module are just strings for now,
//! but I intend to replace these with a set of full blown syntax
//! error enums in the future.

use super::asm::{CoreProgram, StandardProgram};
use super::lir::Expr;
use super::vm;

use lalrpop_util::lalrpop_mod;
use no_comment::{IntoWithoutComments as _, languages};

lalrpop_mod!(
    #[allow(clippy::all)]
    lir_parser
);
lalrpop_mod!(
    #[allow(clippy::all)]
    asm_parser
);
lalrpop_mod!(
    #[allow(clippy::all)]
    vm_parser
);

pub(crate) use asm_parser::{CoreProgramParser, StandardProgramParser};

/// Parse Core and Standard variants of virtual machine source code.
/// This will return core code by default, but will fallback on standard.
pub fn parse_vm(
    input: impl ToString,
) -> Result<Result<vm::CoreProgram, vm::StandardProgram>, String> {
    let code = input.to_string()
        .chars()
        .without_comments(languages::rust())
        .collect::<String>();

    let code = code.trim();
    
    match vm_parser::CoreProgramParser::new().parse(&code.to_string()) {
        Ok(parsed) => Ok(Ok(parsed)),
        Err(_) => match vm_parser::StandardProgramParser::new().parse(&code.to_string()) {
            Ok(parsed) => Ok(Err(parsed)),
            Err(e) => Err(format_error(&code, e)),
        },
    }
}

/// Parse Core and Standard variants of assembly source code.
/// This will return core code by default, but will fallback on standard.
pub fn parse_asm(input: impl ToString) -> Result<Result<CoreProgram, StandardProgram>, String> {
    let code = input.to_string()
        .chars()
        .without_comments(languages::rust())
        .collect::<String>();

    let code = code.trim();

    match asm_parser::CoreProgramParser::new().parse(&code.to_string()) {
        Ok(parsed) => Ok(Ok(parsed)),
        Err(_) => match asm_parser::StandardProgramParser::new().parse(&code.to_string()) {
            Ok(parsed) => Ok(Err(parsed)),
            Err(e) => Err(format_error(&code, e)),
        },
    }
}

/// Parse LIR code as an LIR expression.
pub fn parse_lir(input: impl ToString) -> Result<Expr, String> {
    let code = input.to_string()
        .chars()
        .without_comments(languages::rust())
        .collect::<String>();
        
    let code = code.trim();

    lir_parser::ExprParser::new()
        .parse(&code)
        .map_err(|e| format_error(&code, e))
}

type SyntaxError<'a, T> = lalrpop_util::ParseError<usize, T, &'a str>;

/// This formats an error properly given the line, the `unexpected` token as a string,
/// the line number, and the column number of the unexpected token.
fn make_error(line: &str, unexpected: &str, line_number: usize, column_number: usize) -> String {
    // The string used to underline the unexpected token
    let underline = format!(
        "{}^{}",
        " ".repeat(column_number),
        "-".repeat(unexpected.len() - 1)
    );

    // Format string properly and return
    format!(
        "{WS} |
{line_number} | {line}
{WS} | {underline}
{WS} |
{WS} = unexpected `{unexpected}`",
        WS = " ".repeat(line_number.to_string().len()),
        line_number = line_number,
        line = line,
        underline = underline,
        unexpected = unexpected
    )
}

// Gets the line number, the line, and the column number of the error
fn get_line(script: &str, location: usize) -> (usize, String, usize) {
    if script.is_empty() {
        return (1, "".to_string(), 0);
    }

    // Get the line number from the character location
    let line_number = script[..(location + 1).min(script.len())].lines().count();
    // Get the line from the line number
    let line = match script.lines().nth(line_number - 1) {
        Some(line) => line,
        None => {
            if let Some(line) = script.lines().last() {
                line
            } else {
                ""
            }
        }
    }
    .replace('\t', "    ");

    // Get the column number from the location
    let mut column = {
        let mut current_column = 0;
        // For every character in the script until the location of the error,
        // keep track of the column location
        for ch in script[..location].chars() {
            if ch == '\n' {
                current_column = 0;
            } else if ch == '\t' {
                current_column += 4;
            } else {
                current_column += 1;
            }
        }
        current_column
    };

    // Trim the beginning of the line and subtract the number of spaces from the column
    let trimmed_line = line.trim_start();
    column -= (line.len() - trimmed_line.len()) as i32;

    (line_number, String::from(trimmed_line), column as usize)
}

/// This is used to take an LALRPOP error and convert
/// it into a nicely formatted error message
fn format_error<T: core::fmt::Debug>(script: &str, err: SyntaxError<T>) -> String {
    match err {
        SyntaxError::InvalidToken { location } => {
            let (line_number, line, column) = get_line(script, location);
            make_error(
                &line,
                &(script.as_bytes()[location] as char).to_string(),
                line_number,
                column,
            )
        }
        SyntaxError::UnrecognizedEOF { location, .. } => {
            let (line_number, line, _) = get_line(script, location);
            make_error(&line, "EOF", line_number, line.len())
        }
        SyntaxError::UnrecognizedToken { token, .. } => {
            // The start and end of the unrecognized token
            let start = token.0;
            let end = token.2;

            let (line_number, line, column) = get_line(script, start);
            let unexpected = &script[start..end];
            make_error(&line, unexpected, line_number, column)
        }
        SyntaxError::ExtraToken { token } => {
            // The start and end of the extra token
            let start = token.0;
            let end = token.2;

            let (line_number, line, column) = get_line(script, start);
            let unexpected = &script[start..end];

            make_error(&line, unexpected, line_number, column)
        }
        SyntaxError::User { error } => format!(
            "  |\n? | {}\n  | ^{}\n  |\n  = unexpected compiling error",
            error,
            "-".repeat(error.len() - 1)
        ),
    }
}
