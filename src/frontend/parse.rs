use log::{error, trace};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while1, take_while_m_n},
    character::complete::{char, digit1, hex_digit1, multispace1, oct_digit1},
    combinator::{all_consuming, cut, map, map_opt, opt, recognize, verify},
    error::{context, ContextError, ParseError},
    multi::{fold_many0, many0, many0_count, many1},
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};
use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use crate::{lir::*, parse::SourceCodeLocation};
use nom::{
    character::complete::{alpha1, alphanumeric1},
    combinator::value,
    error::{convert_error, ErrorKind, FromExternalError, VerboseError},
};
const KEYWORDS: &[&str] = &[
    "def", "fun", "struct", "enum", "mut", "let", "if", "else", "while", "for", "return", "match",
    "True", "False", "Null", "None", "sizeof", "Int", "Float", "Char", "Bool", "Cell", "Never",
    "!",
];

fn bin_digit1<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    take_while1(|c: char| c == '0' || c == '1')(input)
}

use lazy_static::lazy_static;

fn precompute_line_starts(s: &str) -> Vec<usize> {
    let mut line_starts = vec![0]; // The first line always starts at index 0
    for (i, c) in s.char_indices() {
        if c == '\n' {
            line_starts.push(i + 1); // Start of the next line
        }
    }
    line_starts
}
fn line_and_column(line_starts: &[usize], index: usize) -> (usize, usize) {
    match line_starts.binary_search(&index) {
        Ok(line) => (line, 0), // The index is exactly at the start of a line
        Err(line) => {
            if line == 0 {
                return (0, 0);
            }
            let line = line - 1; // The index is within this line
            let column = index - line_starts[line];
            (line, column)
        }
    }
}

fn make_env() -> sage_lisp::Env {
    use sage_lisp::{Env, Expr, Symbol};
    use std::io::BufRead;

    let mut env = Env::new();
    env.bind_builtin("env", |env, args| {
        // Get the env as a map
        if args.is_empty() {
            return Expr::Map(env.get_bindings());
        }
        let a = env.eval(args[0].clone());
        env.get(&a).cloned().unwrap_or(Expr::None)
    });

    env.bind_builtin("+", |env, exprs| {
        let mut sum = Expr::default();
        for e in exprs {
            let e = env.eval(e.clone());
            // sum += env.eval(e);
            match (sum, e) {
                (Expr::None, b) => sum = b,
                (Expr::Int(a), Expr::Int(b)) => sum = Expr::Int(a + b),
                (Expr::Float(a), Expr::Float(b)) => sum = Expr::Float(a + b),
                (Expr::Int(a), Expr::Float(b)) => sum = Expr::Float(a as f64 + b),
                (Expr::Float(a), Expr::Int(b)) => sum = Expr::Float(a + b as f64),
                (Expr::String(a), Expr::String(b)) => sum = Expr::String(format!("{}{}", a, b)),
                (Expr::List(a), Expr::List(b)) => {
                    let mut list = a.clone();
                    list.extend(b);
                    sum = Expr::List(list);
                }
                (Expr::List(a), b) => {
                    let mut list = a.clone();
                    list.push(b);
                    sum = Expr::List(list);
                }
                (a, b) => return Expr::error(format!("Invalid expr {} + {}", a, b)),
            }
        }
        sum
    });
    env.alias("+", "add");

    env.bind_builtin("-", |env, exprs| {
        let mut diff = Expr::default();
        for e in exprs {
            let e = env.eval(e.clone());
            match (diff, e) {
                (Expr::None, b) => diff = b,
                (Expr::Int(a), Expr::Int(b)) => diff = Expr::Int(a - b),
                (Expr::Float(a), Expr::Float(b)) => diff = Expr::Float(a - b),
                (Expr::Int(a), Expr::Float(b)) => diff = Expr::Float(a as f64 - b),
                (Expr::Float(a), Expr::Int(b)) => diff = Expr::Float(a - b as f64),
                (a, b) => return Expr::error(format!("Invalid expr {} - {}", a, b)),
            }
        }
        diff
    });
    env.alias("-", "sub");

    env.bind_builtin("*", |env, exprs| {
        let mut product = Expr::default();
        for e in exprs {
            let e = env.eval(e.clone());
            match (product, e) {
                (Expr::None, b) => product = b,
                (Expr::Int(a), Expr::Int(b)) => product = Expr::Int(a * b),
                (Expr::Float(a), Expr::Float(b)) => product = Expr::Float(a * b),
                (Expr::Int(a), Expr::Float(b)) => product = Expr::Float(a as f64 * b),
                (Expr::Float(a), Expr::Int(b)) => product = Expr::Float(a * b as f64),
                (Expr::List(a), Expr::Int(b)) => {
                    let mut list = a.clone();
                    for _ in 0..b {
                        list.extend(a.clone());
                    }
                    product = Expr::List(list);
                }
                (a, b) => return Expr::error(format!("Invalid expr {} * {}", a, b)),
            }
        }
        product
    });
    env.alias("*", "mul");

    env.bind_builtin("/", |env, exprs| {
        let mut quotient = Expr::default();
        for e in exprs {
            let e = env.eval(e.clone());
            match (quotient, e) {
                (Expr::None, b) => quotient = b,
                (Expr::Int(a), Expr::Int(b)) => quotient = Expr::Int(a / b),
                (Expr::Float(a), Expr::Float(b)) => quotient = Expr::Float(a / b),
                (Expr::Int(a), Expr::Float(b)) => quotient = Expr::Float(a as f64 / b),
                (Expr::Float(a), Expr::Int(b)) => quotient = Expr::Float(a / b as f64),
                (a, b) => return Expr::error(format!("Invalid expr {} / {}", a, b)),
            }
        }
        quotient
    });
    env.alias("/", "div");

    env.bind_builtin("%", |env, exprs| {
        let mut quotient = Expr::default();
        for e in exprs {
            let e = env.eval(e.clone());
            match (quotient, e) {
                (Expr::None, b) => quotient = b,
                (Expr::Int(a), Expr::Int(b)) => quotient = Expr::Int(a % b),
                (Expr::Float(a), Expr::Float(b)) => quotient = Expr::Float(a % b),
                (Expr::Int(a), Expr::Float(b)) => quotient = Expr::Float(a as f64 % b),
                (Expr::Float(a), Expr::Int(b)) => quotient = Expr::Float(a % b as f64),
                (a, b) => return Expr::error(format!("Invalid expr {} % {}", a, b)),
            }
        }
        quotient
    });
    env.alias("%", "rem");

    env.bind_builtin("=", |env, exprs| {
        let a = env.eval(exprs[0].clone());
        let b = env.eval(exprs[1].clone());

        Expr::Bool(a == b)
    });
    env.alias("=", "==");

    env.bind_builtin("!=", |env, exprs| {
        let a = env.eval(exprs[0].clone());
        let b = env.eval(exprs[1].clone());

        Expr::Bool(a != b)
    });

    env.bind_builtin("<=", |env, exprs| {
        let a = env.eval(exprs[0].clone());
        let b = env.eval(exprs[1].clone());

        Expr::Bool(a <= b)
    });

    env.bind_builtin(">=", |env, exprs| {
        let a = env.eval(exprs[0].clone());
        let b = env.eval(exprs[1].clone());

        Expr::Bool(a >= b)
    });

    env.bind_builtin("<", |env, exprs| {
        let a = env.eval(exprs[0].clone());
        let b = env.eval(exprs[1].clone());

        Expr::Bool(a < b)
    });

    env.bind_builtin(">", |env, exprs| {
        let a = env.eval(exprs[0].clone());
        let b = env.eval(exprs[1].clone());

        Expr::Bool(a > b)
    });

    env.bind_lazy_builtin("if", |env, exprs| {
        let cond = env.eval(exprs[0].clone());
        let then = exprs[1].clone();
        if exprs.len() < 3 {
            if cond == Expr::Bool(true) {
                then
            } else {
                Expr::None
            }
        } else {
            let else_ = exprs[2].clone();
            if cond == Expr::Bool(true) {
                then
            } else {
                else_
            }
        }
    });

    env.bind_builtin("define", |env, exprs| {
        let name = exprs[0].clone();
        let value = env.eval(exprs[1].clone());
        env.bind(name, value);
        Expr::None
    });

    env.bind_builtin("undefine", |env, exprs| {
        let name = exprs[0].clone();
        env.unbind(&name);
        Expr::None
    });

    env.bind_builtin("defun", |env, args| {
        let name = args[0].clone();
        let params = args[1].clone();
        let body = args[2].clone();
        if let Expr::List(params) = params {
            let f = env.eval(Expr::Function(None, params, Box::new(body)));
            env.bind(name, f);
            Expr::None
        } else {
            Expr::error(format!("Invalid params {:?}", params))
        }
    });

    env.bind_builtin("println", |env, exprs| {
        for e in exprs {
            let e = env.eval(e.clone());

            match e {
                Expr::String(s) => print!("{}", s),
                Expr::Symbol(s) => print!("{}", s.name()),
                _ => print!("{}", e),
            }
        }
        println!();
        Expr::None
    });

    env.bind_lazy_builtin("do", |_env, exprs| Expr::Many(Vec::from(exprs)));

    env.bind_builtin("sqrt", |env, expr| {
        let e = env.eval(expr[0].clone());
        match e {
            Expr::Int(i) => Expr::Float((i as f64).sqrt()),
            Expr::Float(f) => Expr::Float(f.sqrt()),
            e => Expr::error(format!("Invalid expr sqrt {}", e)),
        }
    });

    env.bind_builtin("^", |env, expr| {
        let a = env.eval(expr[0].clone());
        let b = env.eval(expr[1].clone());
        match (a, b) {
            (Expr::Int(a), Expr::Int(b)) => Expr::Float((a as f64).powf(b as f64)),
            (Expr::Float(a), Expr::Float(b)) => Expr::Float(a.powf(b)),
            (Expr::Int(a), Expr::Float(b)) => Expr::Float((a as f64).powf(b)),
            (Expr::Float(a), Expr::Int(b)) => Expr::Float(a.powf(b as f64)),
            (a, b) => Expr::error(format!("Invalid expr {} ^ {}", a, b)),
        }
    });

    env.alias("^", "pow");

    let lambda = |env: &mut Env, expr: &[Expr]| {
        let params = expr[0].clone();
        let body = expr[1].clone();
        if let Expr::List(params) = params {
            Expr::Function(Some(Box::new(env.clone())), params, Box::new(body))
        } else {
            Expr::error(format!("Invalid params {:?}", params))
        }
    };
    env.bind_builtin("lambda", lambda);
    env.bind_builtin("\\", lambda);

    env.bind_builtin("apply", |env, expr| {
        let f = env.eval(expr[0].clone());
        let args = env.eval(expr[1].clone());
        if let Expr::List(args) = args {
            match f {
                Expr::Function(Some(mut env), params, body) => {
                    let mut new_env = env.clone();
                    for (param, arg) in params.iter().zip(args.iter()) {
                        new_env.bind(param.clone(), env.eval(arg.clone()));
                    }
                    new_env.eval(*body.clone())
                }
                Expr::Function(None, params, body) => {
                    let mut new_env = env.clone();
                    for (param, arg) in params.iter().zip(args.iter()) {
                        new_env.bind(param.clone(), env.eval(arg.clone()));
                    }
                    new_env.eval(*body.clone())
                }
                Expr::Builtin(f) => f.apply(&mut env.clone(), &args),
                f => Expr::error(format!("Invalid function {f} apply {}", Expr::from(args))),
            }
        } else {
            Expr::error(format!("Invalid function {f} apply {}", args))
        }
    });

    env.bind_builtin("cons", |env, expr| {
        let a = env.eval(expr[0].clone());
        let b = env.eval(expr[1].clone());
        // Create a new list with a as the head and b as the tail.
        if let Expr::List(b) = b {
            let mut list = vec![a];
            list.extend(b);
            Expr::List(list)
        } else if b == Expr::None {
            Expr::List(vec![a])
        } else {
            Expr::List(vec![a, b])
        }
    });
    let head = |env: &mut Env, expr: &[Expr]| {
        let a = env.eval(expr[0].clone());
        if let Expr::List(a) = a {
            a[0].clone()
        } else {
            Expr::error(format!("Invalid head {a}"))
        }
    };
    let tail = |env: &mut Env, expr: &[Expr]| {
        let a = env.eval(expr[0].clone());
        if let Expr::List(a) = a {
            Expr::List(a[1..].to_vec())
        } else {
            Expr::error(format!("Invalid tail {a}"))
        }
    };

    env.bind_builtin("car", head);
    env.bind_builtin("head", head);
    env.bind_builtin("cdr", tail);
    env.bind_builtin("tail", tail);

    let format = |env: &mut Env, expr: &[Expr]| {
        let format = env.eval(expr[0].clone());
        // Collect the args
        let args = expr[1..].to_vec();

        let mut format = match format {
            Expr::String(s) => s,
            e => return Expr::error(format!("Invalid format {e}")),
        };

        // Find all of the format specifiers.
        let mut specifiers = vec![];
        for (i, c) in format.chars().enumerate() {
            if c == '{' {
                let mut j = i + 1;
                while j < format.len() {
                    if format.chars().nth(j).unwrap() == '}' {
                        break;
                    }
                    j += 1;
                }
                specifiers.push(format[i + 1..j].to_owned());
            }
        }

        // Replace the named specifiers with variables in the scope.
        for name in &specifiers {
            if name.is_empty() {
                continue;
            }
            let name = Expr::Symbol(Symbol::new(name));

            let value = env.eval(name.clone());
            let specifier = format!("{{{name}}}");
            match value {
                Expr::String(s) => {
                    format = format.replacen(&specifier, &s, 1);
                }
                other => {
                    format = format.replacen(&specifier, &other.to_string(), 1);
                }
            }
        }

        // Replace the empty specifiers with the args.
        let mut i = 0;
        for name in &specifiers {
            if !name.is_empty() {
                continue;
            }
            if i >= args.len() {
                return Expr::error("Too few arguments");
            }
            let specifier = "{}".to_string();
            let value = env.eval(args[i].clone());
            match value {
                Expr::String(s) => {
                    format = format.replacen(&specifier, &s, 1);
                }
                other => {
                    format = format.replacen(&specifier, &other.to_string(), 1);
                }
            }
            // format = format.replacen("{}", &args[i].to_string(), 1);
            i += 1;
        }

        if i < args.len() {
            return Expr::error("Too many arguments");
        }

        Expr::String(format)
    };

    env.bind_builtin("format", format);

    env.bind_builtin("list", |env, expr| {
        let mut list = vec![];
        for e in expr {
            list.push(env.eval(e.clone()));
        }
        Expr::List(list)
    });

    env.bind_builtin("append", |env, expr| {
        let mut list = vec![];
        for e in expr {
            let e = env.eval(e.clone());
            if let Expr::List(l) = e {
                list.extend(l);
            } else {
                return Expr::error(format!("Invalid append {e}"));
            }
        }
        Expr::List(list)
    });

    env.bind_builtin("eval", |env, expr| {
        let e = env.eval(expr[0].clone());
        env.eval(e)
    });

    env.bind_builtin("exit", |env, expr| match env.eval(expr[0].clone()) {
        Expr::Int(i) => std::process::exit(i as i32),
        Expr::String(s) => {
            eprintln!("{s}");
            std::process::exit(1);
        }
        e => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    });

    env.bind_builtin("quote", |_env, expr| expr[0].clone());

    env.bind_builtin("or", |env, expr| {
        for e in expr {
            let e = env.eval(e.clone());
            if e == Expr::Bool(true) {
                return Expr::Bool(true);
            }
        }
        Expr::Bool(false)
    });

    env.bind_builtin("and", |env, expr| {
        for e in expr {
            let e = env.eval(e.clone());
            if e == Expr::Bool(false) {
                return Expr::Bool(false);
            }
        }
        Expr::Bool(true)
    });

    env.bind_builtin("not", |env, expr| {
        let e = env.eval(expr[0].clone());
        match e {
            Expr::Bool(b) => Expr::Bool(!b),
            e => Expr::error(format!("Invalid not {e}")),
        }
    });

    env.bind_builtin("len", |env, expr| {
        let e = env.eval(expr[0].clone());
        match e {
            Expr::String(s) => Expr::Int(s.len() as i64),
            Expr::List(l) => Expr::Int(l.len() as i64),
            Expr::Map(m) => Expr::Int(m.len() as i64),
            Expr::Tree(t) => Expr::Int(t.len() as i64),
            e => Expr::error(format!("Invalid len {e}")),
        }
    });

    env.bind_builtin("let", |env, expr| {
        let mut new_env = env.clone();
        let bindings = expr[0].clone();
        let body = expr[1].clone();
        match bindings {
            Expr::List(bindings) => {
                for binding in bindings {
                    if let Expr::List(binding) = binding {
                        let name = binding[0].clone();
                        let value = env.eval(binding[1].clone());
                        new_env.bind(name, value);
                    } else {
                        return Expr::error(format!("Invalid binding {binding}"));
                    }
                }
            }
            Expr::Map(bindings) => {
                for (name, value) in bindings {
                    new_env.bind(name, env.eval(value));
                }
            }
            Expr::Tree(bindings) => {
                for (name, value) in bindings {
                    new_env.bind(name, env.eval(value));
                }
            }
            bindings => return Expr::error(format!("Invalid bindings {bindings}")),
        }
        new_env.eval(body)
    });

    env.bind_builtin("get", |env, expr| {
        let a = env.eval(expr[0].clone());
        let b = env.eval(expr[1].clone());

        match (a, b) {
            (Expr::String(a), Expr::Int(b)) => {
                Expr::String(a.chars().nth(b as usize).unwrap_or('\0').to_string())
            }
            (Expr::List(a), Expr::Int(b)) => a.get(b as usize).cloned().unwrap_or(Expr::None),
            (Expr::Map(a), Expr::Symbol(b)) => {
                // a.get(&b).cloned().unwrap_or(Expr::None)
                a.get(&Expr::Symbol(b.clone())).cloned().unwrap_or_else(|| {
                    a.get(&Expr::String(b.name().to_owned()))
                        .cloned()
                        .unwrap_or(Expr::None)
                })
            }
            (Expr::Map(a), b) => a.get(&b).cloned().unwrap_or(Expr::None),
            (Expr::Tree(a), b) => a.get(&b).cloned().unwrap_or(Expr::None),
            (a, b) => Expr::error(format!("Invalid expr get {} {}", a, b)),
        }
    });
    env.alias("get", "@");

    env.bind_builtin("set", |env, expr| {
        let a = env.eval(expr[0].clone());
        let b = env.eval(expr[1].clone());
        let c = env.eval(expr[2].clone());

        match (a, b) {
            (Expr::String(mut a), Expr::Int(b)) => {
                if b == a.len() as i64 {
                    a.push_str(&c.to_string());
                } else {
                    a = a
                        .chars()
                        .enumerate()
                        .map(|(i, c)| if i == b as usize { c } else { '\0' })
                        .collect();
                }
                Expr::String(a)
            }
            (Expr::List(mut a), Expr::Int(b)) => {
                if b as usize >= a.len() {
                    a.resize(b as usize + 1, Expr::None);
                }
                a[b as usize] = c;
                Expr::List(a)
            }
            (Expr::Map(mut a), b) => {
                a.insert(b, c);
                Expr::Map(a)
            }
            (Expr::Tree(mut a), b) => {
                a.insert(b, c);
                Expr::Tree(a)
            }
            (a, b) => Expr::error(format!("Invalid expr set {} {} {}", a, b, c)),
        }
    });

    env.bind_builtin("zip", |env, expr| {
        let a = env.eval(expr[0].clone());
        let b = env.eval(expr[1].clone());

        match (a, b) {
            (Expr::List(a), Expr::List(b)) => {
                let mut list = vec![];
                for (a, b) in a.into_iter().zip(b.into_iter()) {
                    list.push(Expr::List(vec![a, b]));
                }
                Expr::List(list)
            }
            (a, b) => Expr::error(format!("Invalid expr zip {} {}", a, b)),
        }
    });

    // Convert a list of pairs into a map.
    env.bind_builtin("to-map", |env, expr| {
        let a = env.eval(expr[0].clone());
        match a {
            Expr::List(a) => {
                let mut map = std::collections::HashMap::new();
                for e in a {
                    if let Expr::List(e) = e {
                        if e.len() == 2 {
                            map.insert(e[0].clone(), e[1].clone());
                        } else {
                            return Expr::error(format!("Invalid pair {}", Expr::from(e)));
                        }
                    } else {
                        return Expr::error(format!("Invalid pair {}", e));
                    }
                }
                Expr::Map(map)
            }
            Expr::Map(a) => Expr::Map(a),
            Expr::Tree(a) => Expr::Map(a.into_iter().collect()),
            a => Expr::error(format!("Invalid expr to-map {}", a)),
        }
    });

    // Convert a list of pairs into a tree.
    env.bind_builtin("to-tree", |env, expr| {
        let a = env.eval(expr[0].clone());
        match a {
            Expr::List(a) => {
                let mut tree = std::collections::BTreeMap::new();
                for e in a {
                    if let Expr::List(e) = e {
                        if e.len() == 2 {
                            tree.insert(e[0].clone(), e[1].clone());
                        } else {
                            return Expr::error(format!("Invalid pair {}", Expr::from(e)));
                        }
                    } else {
                        return Expr::error(format!("Invalid pair {}", e));
                    }
                }
                Expr::Tree(tree)
            }
            Expr::Map(a) => Expr::Tree(a.into_iter().collect()),
            Expr::Tree(a) => Expr::Tree(a),
            a => Expr::error(format!("Invalid expr to-tree {}", a)),
        }
    });

    env.bind_builtin("to-list", |env, expr| {
        let a = env.eval(expr[0].clone());
        match a {
            Expr::Map(a) => {
                let mut list = vec![];
                for (k, v) in a {
                    list.push(Expr::List(vec![k, v]));
                }
                Expr::List(list)
            }
            Expr::Tree(a) => {
                let mut list = vec![];
                for (k, v) in a {
                    list.push(Expr::List(vec![k, v]));
                }
                Expr::List(list)
            }
            Expr::List(a) => Expr::List(a),
            a => Expr::error(format!("Invalid expr to-list {}", a)),
        }
    });

    env.bind_builtin("map", |env, expr| {
        let f = env.eval(expr[0].clone());
        let a = env.eval(expr[1].clone());
        match a {
            Expr::List(a) => {
                let mut list = vec![];
                for e in a {
                    list.push(env.eval(Expr::List(vec![f.clone(), e])));
                }
                Expr::List(list)
            }
            Expr::Map(a) => {
                let mut map = std::collections::HashMap::new();
                for (k, v) in a {
                    let pair = env.eval(Expr::List(vec![f.clone(), k.quote(), v.quote()]));
                    if let Expr::List(pair) = pair {
                        map.insert(pair[0].clone(), pair[1].clone());
                    } else {
                        return Expr::error(format!("Invalid pair {}", pair));
                    }
                }
                Expr::Map(map)
            }
            Expr::Tree(a) => {
                let mut tree = std::collections::BTreeMap::new();
                for (k, v) in a {
                    // tree.insert(k.clone(), env.eval(Expr::List(vec![f.clone(), k, v])));
                    let pair = env.eval(Expr::List(vec![f.clone(), k.quote(), v.quote()]));
                    if let Expr::List(pair) = pair {
                        tree.insert(pair[0].clone(), pair[1].clone());
                    } else {
                        return Expr::error(format!("Invalid pair {}", pair));
                    }
                }
                Expr::Tree(tree)
            }
            a => Expr::error(format!("Invalid expr map {}", a)),
        }
    });

    env.bind_builtin("filter", |env, expr| {
        let f = env.eval(expr[0].clone());
        let a = env.eval(expr[1].clone());

        match a {
            Expr::List(a) => {
                let mut list = vec![];
                for e in a {
                    if env.eval(Expr::List(vec![f.clone(), e.clone()])) == Expr::Bool(true) {
                        list.push(e);
                    }
                }
                Expr::List(list)
            }
            Expr::Map(a) => {
                let mut map = std::collections::HashMap::new();
                for (k, v) in a {
                    let x = env.eval(Expr::List(vec![f.clone(), k.quote(), v.quote()]));
                    if x == Expr::Bool(true) {
                        map.insert(k, v);
                    }
                }
                Expr::Map(map)
            }
            Expr::Tree(a) => {
                let mut tree = std::collections::BTreeMap::new();
                for (k, v) in a {
                    let x = env.eval(Expr::List(vec![f.clone(), k.quote(), v.quote()]));
                    if x == Expr::Bool(true) {
                        tree.insert(k, v);
                    }
                }
                Expr::Tree(tree)
            }
            a => Expr::error(format!("Invalid expr filter {}", a)),
        }
    });

    env.bind_builtin("reduce", |env, expr| {
        let f = env.eval(expr[0].clone());
        let a = env.eval(expr[1].clone());
        let b = env.eval(expr[2].clone());

        match a {
            Expr::List(a) => {
                let mut acc = b;
                for e in a {
                    acc = env.eval(Expr::List(vec![f.clone(), acc, e]));
                }
                acc
            }
            Expr::Map(a) => {
                let mut acc = b;
                for (k, v) in a {
                    acc = env.eval(Expr::List(vec![f.clone(), acc, k, v]));
                }
                acc
            }
            Expr::Tree(a) => {
                let mut acc = b;
                for (k, v) in a {
                    acc = env.eval(Expr::List(vec![f.clone(), acc, k, v]));
                }
                acc
            }
            a => Expr::error(format!("Invalid expr reduce {}", a)),
        }
    });

    env.bind_builtin("range", |env, expr| {
        let a = env.eval(expr[0].clone());
        let b = env.eval(expr[1].clone());
        match (a, b) {
            (Expr::Int(a), Expr::Int(b)) => {
                let mut list = vec![];
                for i in a..=b {
                    list.push(Expr::Int(i));
                }
                Expr::List(list)
            }
            (Expr::Int(a), Expr::Float(b)) => {
                let mut list = vec![];
                for i in a..=(b as i64) {
                    list.push(Expr::Int(i));
                }
                Expr::List(list)
            }
            (Expr::Float(a), Expr::Int(b)) => {
                let mut list = vec![];
                for i in (a as i64)..=b {
                    list.push(Expr::Int(i));
                }
                Expr::List(list)
            }
            (Expr::Float(a), Expr::Float(b)) => {
                let mut list = vec![];
                for i in (a as i64)..=(b as i64) {
                    list.push(Expr::Int(i));
                }
                Expr::List(list)
            }
            (Expr::Symbol(a), Expr::Symbol(b)) if a.name().len() == 1 && b.name().len() == 1 => {
                let mut list = vec![];
                let first = a.name().chars().next().unwrap();
                let last = b.name().chars().next().unwrap();
                for i in first..=last {
                    list.push(Expr::Symbol(Symbol::new(&i.to_string())));
                }
                Expr::List(list)
            }
            (Expr::String(a), Expr::String(b)) if a.len() == 1 && b.len() == 1 => {
                let mut list = vec![];
                let first = a.chars().next().unwrap();
                let last = b.chars().next().unwrap();
                for i in first..=last {
                    list.push(Expr::String(i.to_string()));
                }
                Expr::List(list)
            }
            (a, b) => Expr::error(format!("Invalid expr range {} {}", a, b)),
        }
    });

    env.bind_builtin("rev", |env, expr| {
        let a = env.eval(expr[0].clone());
        match a {
            Expr::List(mut a) => {
                a.reverse();
                Expr::List(a)
            }
            a => Expr::error(format!("Invalid expr rev {}", a)),
        }
    });

    use crate::targets::CompiledTarget;
    env.bind_builtin("parse", |env, expr| {
        let a = env.eval(expr[0].clone());
        match a {
            Expr::String(frontend_src) => {
                match crate::parse::parse_frontend_minimal(frontend_src, Some("stdin")) {
                    Ok(frontend_code) => Expr::serialize(frontend_code),
                    Err(e) => Expr::error(e),
                }
            }
            a => Expr::error(format!("Invalid expr parse {}", a)),
        }
    });
    env.bind_builtin("parse-big", |env, expr| {
        let a = env.eval(expr[0].clone());
        match a {
            Expr::String(frontend_src) => {
                match crate::parse::parse_frontend(frontend_src, Some("stdin")) {
                    Ok(frontend_code) => Expr::serialize(frontend_code),
                    Err(e) => Expr::error(e),
                }
            }
            a => Expr::error(format!("Invalid expr parse {}", a)),
        }
    });

    env.bind_builtin("compile", |env, expr| {
        let frontend_code = Expr::deserialize::<crate::lir::Expr>(&env.eval(expr[0].clone()))
            .map_err(|e| Expr::error(format!("Invalid AST: {e}")));

        match frontend_code {
            Err(e) => e,
            Ok(frontend_code) => {
                let asm_code = frontend_code
                    .compile()
                    .map_err(|e| Expr::error(format!("Invalid AST: {e}")));
                if let Err(e) = asm_code {
                    return e;
                }
                let asm_code = asm_code.unwrap();

                let vm_code = match asm_code {
                    Ok(core_asm_code) => core_asm_code.assemble(100000).map(Ok),
                    Err(std_asm_code) => std_asm_code.assemble(100000).map(Err),
                }
                .unwrap();

                let c_code = match vm_code {
                    Ok(vm_code) => crate::targets::C.build_core(&vm_code.flatten()).unwrap(),
                    Err(vm_code) => crate::targets::C.build_std(&vm_code.flatten()).unwrap(),
                };

                Expr::String(c_code)
            }
        }
    });

    env.bind_builtin("asm", |env, expr| {
        let frontend_code = Expr::deserialize::<crate::lir::Expr>(&env.eval(expr[0].clone()))
            .map_err(|e| Expr::error(format!("Invalid AST: {e}")));

        match frontend_code {
            Err(e) => e,
            Ok(frontend_code) => {
                let asm_code = frontend_code
                    .compile()
                    .map_err(|e| Expr::error(format!("Invalid AST: {e}")));
                if let Err(e) = asm_code {
                    return e;
                }
                let asm_code = asm_code.unwrap();
                match asm_code {
                    Ok(core_asm_code) => Expr::serialize(core_asm_code.code),
                    Err(std_asm_code) => Expr::serialize(std_asm_code.code),
                }
            }
        }
    });

    env.bind_builtin("read", |env, expr| {
        // Read a file
        let path = env.eval(expr[0].clone());

        match path {
            Expr::String(path) => {
                let path = std::path::Path::new(&path);
                let file = std::fs::File::open(path).unwrap();
                let reader = std::io::BufReader::new(file);
                let mut code = String::new();
                for line in reader.lines() {
                    code.push_str(&line.unwrap());
                    code.push_str("\n");
                }
                Expr::String(code)
            }
            a => Expr::error(format!("Invalid expr read {}", a)),
        }
    });

    env.bind_builtin("write", |env, expr| {
        // Write a file
        use std::io::Write;
        let path = env.eval(expr[0].clone());

        let content = env.eval(expr[1].clone());

        match (path, content) {
            (Expr::String(path), Expr::String(content)) => {
                let path = std::path::Path::new(&path);
                let mut file = std::fs::File::create(path).unwrap();
                file.write_all(content.as_bytes()).unwrap();
                Expr::None
            }
            (a, b) => Expr::error(format!("Invalid expr write {} {}", a, b)),
        }
    });

    env.bind_builtin("shell", |env, expr| {
        // Run a shell command
        let cmd = env.eval(expr[0].clone());

        match cmd {
            Expr::String(cmd) => {
                let output = std::process::Command::new("sh")
                    .arg("-c")
                    .arg(&cmd)
                    .output()
                    .expect("failed to execute process");
                let stdout = String::from_utf8(output.stdout).unwrap();
                let stderr = String::from_utf8(output.stderr).unwrap();
                Expr::List(vec![Expr::String(stdout), Expr::String(stderr)])
            }
            a => Expr::error(format!("Invalid expr shell {}", a)),
        }
    });

    env
}

lazy_static! {
    static ref LINE_NUMBERS: RwLock<Vec<usize>> = RwLock::new(vec![]);
    static ref FILE_NAME: RwLock<Option<String>> = RwLock::new(None);
    static ref PROGRAM: RwLock<Arc<String>> = RwLock::new(Arc::new(String::new()));
    static ref LISP_ENV: RwLock<sage_lisp::Env> = RwLock::new(make_env());
    static ref FILE_SAVES: RwLock<Vec<(Vec<usize>, Arc<String>, Option<String>)>> =
        RwLock::new(vec![]);
}

fn save_source_code_setup() {
    let mut saves = FILE_SAVES.write().unwrap();

    let line_numbers = LINE_NUMBERS.read().unwrap();
    let file_name = FILE_NAME.read().unwrap();
    let program = PROGRAM.read().unwrap();

    saves.push((line_numbers.clone(), program.clone(), file_name.clone()));
}

fn restore_source_code_setup() {
    let mut saves = FILE_SAVES.write().unwrap();

    let mut line_numbers = LINE_NUMBERS.write().unwrap();
    let mut file_name = FILE_NAME.write().unwrap();
    let mut program = PROGRAM.write().unwrap();

    if let Some((l, p, f)) = saves.pop() {
        *line_numbers = l;
        *program = p;
        *file_name = f;
    }
}

fn obliterate_save() {
    let mut line_numbers = LINE_NUMBERS.write().unwrap();
    let mut file_name = FILE_NAME.write().unwrap();
    let mut program = PROGRAM.write().unwrap();

    *line_numbers = vec![];
    *file_name = None;
    *program = Arc::new(String::new());
    FILE_SAVES.write().unwrap().clear();
    *LISP_ENV.write().unwrap() = make_env();
}

fn setup_source_code_locations(program: &str, filename: Option<String>) {
    *LINE_NUMBERS.write().unwrap() = precompute_line_starts(program);
    *FILE_NAME.write().unwrap() = filename.to_owned();
    *PROGRAM.write().unwrap() = Arc::new(program.to_string());
}

fn get_source_code_location(char_idx: usize, length: Option<usize>) -> SourceCodeLocation {
    let (line, column) = line_and_column(&LINE_NUMBERS.read().unwrap(), char_idx);
    let offset = char_idx;
    let filename = FILE_NAME.read().unwrap().clone();

    SourceCodeLocation {
        line,
        column,
        offset,
        length,
        filename,
    }
}

lazy_static! {
    static ref OFFSETS: RwLock<Vec<usize>> = RwLock::new(vec![]);
}

fn get_current_offset_in_program(remaining_input: &str) -> usize {
    let length = PROGRAM.read().unwrap().len();
    let remaining_length = remaining_input.len();
    if length > remaining_length {
        length - remaining_length
    } else {
        0
    }
}

fn start_source_code_tracking(remaining_input: &str) {
    let offset = get_current_offset_in_program(remaining_input);
    OFFSETS.write().unwrap().push(offset);
}

fn end_source_code_tracking(remaining_input: &str) -> SourceCodeLocation {
    let new_offset = get_current_offset_in_program(remaining_input);
    let old_offset = OFFSETS.write().unwrap().pop().unwrap();
    let program = PROGRAM.read().unwrap();
    trace!("Old offset: {old_offset}, new offset: {new_offset}");
    if new_offset > old_offset {
        trace!("Code: {}", &program[old_offset..new_offset]);
        let length = new_offset - old_offset;
        get_source_code_location(old_offset, Some(length))
    } else {
        get_source_code_location(new_offset, None)
    }
}

lazy_static! {
    // Declare a map of binary operators, their precedence, associativity, and their operator value
    static ref BIN_OPS: RwLock<BTreeMap<String, (u8, fn(Expr, Expr) -> Expr)>> = {

        let mut result: BTreeMap<String, (u8, fn(Expr, Expr) -> Expr)> = BTreeMap::new();
        result.insert("+".to_owned(), (10, |a, b| a.add(b)));
        result.insert("-".to_owned(), (10, |a, b| a.sub(b)));
        result.insert("*".to_owned(), (20, |a, b| a.mul(b)));
        result.insert("/".to_owned(), (20, |a, b| a.div(b)));
        result.insert("%".to_owned(), (20, |a, b| a.rem(b)));
        result.insert("==".to_owned(), (5, |a, b| a.eq(b)));
        result.insert("!=".to_owned(), (5, |a, b| a.neq(b)));
        result.insert("<".to_owned(), (5, |a, b| a.lt(b)));
        result.insert("<=".to_owned(), (5, |a, b| a.le(b)));
        result.insert(">".to_owned(), (5, |a, b| a.gt(b)));
        result.insert(">=".to_owned(), (5, |a, b| a.ge(b)));
        result.insert("&&".to_owned(), (3, |a, b| a.and(b)));
        result.insert("and".to_owned(), (3, |a, b| a.and(b)));
        result.insert("||".to_owned(), (2, |a, b| a.or(b)));
        result.insert("or".to_owned(), (2, |a, b| a.or(b)));
        result.insert("&".to_owned(), (4, |a, b| a.bitand(b)));
        result.insert("|".to_owned(), (2, |a, b| a.bitor(b)));
        result.insert("^".to_owned(), (3, |a, b| a.bitxor(b)));

        // result.insert("-".to_owned(), (10, Box::new(crate::lir::Arithmetic::Subtract)));
        // result.insert("*".to_owned(), (20, Box::new(crate::lir::Arithmetic::Multiply)));
        // result.insert("/".to_owned(), (20, Box::new(crate::lir::Arithmetic::Divide)));
        // result.insert("%".to_owned(), (20, Box::new(crate::lir::Arithmetic::Remainder)));
        // result.insert("==".to_owned(), (5, Box::new(crate::lir::Comparison::Equal)));
        // result.insert("!=".to_owned(), (5, Box::new(crate::lir::Comparison::NotEqual)));
        // result.insert("<".to_owned(), (5, Box::new(crate::lir::Comparison::LessThan)));
        // result.insert("<=".to_owned(), (5, Box::new(crate::lir::Comparison::LessThanOrEqual)));
        // result.insert(">".to_owned(), (5, Box::new(crate::lir::Comparison::GreaterThan)));
        // result.insert(">=".to_owned(), (5, Box::new(crate::lir::Comparison::GreaterThanOrEqual)));
        // result.insert("&&".to_owned(), (3, Box::new(crate::lir::And)));
        // result.insert("||".to_owned(), (2, Box::new(crate::lir::Or)));
        // result.insert("&".to_owned(), (4, Box::new(crate::lir::BitwiseAnd)));
        // result.insert("|".to_owned(), (2, Box::new(crate::lir::BitwiseOr)));
        // result.insert("^".to_owned(), (3, Box::new(crate::lir::BitwiseXor)));

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

fn whitespace<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    take_while(|c: char| c.is_whitespace())(input)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(crate::lir::Declaration, Option<SourceCodeLocation>),
    Expr(crate::lir::Expr),
}

fn stmts_to_expr(stmts: Vec<Statement>, end_of_program: bool) -> Expr {
    use std::collections::VecDeque;
    let rev_stmts = stmts.into_iter().rev().collect::<Vec<_>>();
    let mut body = Expr::NONE;
    let mut result = VecDeque::new();
    let mut decls = VecDeque::new();
    for stmt in rev_stmts {
        match stmt {
            Statement::Expr(e) => {
                result.push_front(e);
            }
            Statement::Declaration(decl, source_code_loc) => {
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
                    if let Declaration::Module(..) = decl {
                        body = body.hard_with(decl);
                    } else {
                        body = body.with(decl);
                    }
                    if let Some(loc) = source_code_loc {
                        body = body.annotate(loc);
                    }
                }
            }
        }
    }

    if end_of_program {
        result.push_back(Expr::NONE);
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
        body.with(Declaration::many(decls))
    }
}

pub fn parse_source(input: &str, filename: Option<String>) -> Result<Expr, String> {
    obliterate_save();
    let old_dir = match &filename {
        Some(_) => std::env::current_dir().unwrap_or_default(),
        None => std::path::PathBuf::new(),
    };

    setup_source_code_locations(input, filename.clone());

    fn parse_helper<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Expr, E> {
        let (input, _) = whitespace(input)?;
        let (input, stmts) = many0(context("statement", parse_stmt))(input)?;
        let (input, _) = whitespace(input)?;
        Ok((input, stmts_to_expr(stmts, true)))
    }

    if let Some(path) = &filename {
        if let Ok(current_dir) = std::env::current_dir() {
            let _ = std::env::set_current_dir(
                current_dir.join(std::path::Path::new(&path).parent().unwrap()),
            );
        }
    }
    let result = all_consuming(parse_helper::<VerboseError<&str>>)(input);
    let _ = std::env::set_current_dir(old_dir);
    match result {
        Err(nom::Err::Error(e)) => {
            trace!("Error: {e}");
            Err(convert_error(input, e).to_string())
        }
        Err(nom::Err::Failure(e)) => {
            trace!("Failure: {e}");
            Err(convert_error(input, e).to_string())
        }
        Err(nom::Err::Incomplete(_e)) => {
            unreachable!()
        }
        Ok((_, expr)) => Ok(expr),
    }
}

pub(crate) fn parse_module(name: &str, input: &str) -> Result<Declaration, String> {
    setup_source_code_locations(input, Some(name.to_owned()));

    match parse_module_contents::<VerboseError<&str>>(name, input) {
        Err(nom::Err::Error(e)) => {
            trace!("Error: {e}");
            Err(convert_error(input, e).to_string())
        }
        Err(nom::Err::Failure(e)) => {
            trace!("Failure: {e}");
            Err(convert_error(input, e).to_string())
        }
        Err(nom::Err::Incomplete(_e)) => {
            unreachable!()
        }
        Ok(("", expr)) => Ok(expr),
        Ok((new_input, _expr)) => {
            let e = VerboseError::<&str>::from_error_kind(new_input, ErrorKind::Verify);
            Err(convert_error(input, e).to_string())
        }
    }
}

fn parse_decorator<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, sage_lisp::Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("#")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("[")(input)?;
    let (input, expr) = sage_lisp::parse_expr(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("]")(input)?;

    Ok((input, expr))
}

fn parse_attribute<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, sage_lisp::Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("#")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("!")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("[")(input)?;
    let (input, expr) = sage_lisp::parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("]")(input)?;

    let mut env = LISP_ENV.write().unwrap();

    Ok((input, env.eval(expr)))
}

fn parse_impl_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = tag("impl")(input)?;

    let (input, ty) = cut(parse_type)(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("{"))(input)?;
    let (mut input, _) = whitespace(input)?;
    let mut impl_items = vec![];
    while let Ok((i, item)) = parse_impl_item::<E>(input, &ty) {
        trace!("Parsed impl item: {item:?}");
        impl_items.push(item);
        let (i, _) = whitespace(i)?;
        input = i;
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("}"))(input)?;

    Ok((
        input,
        Statement::Declaration(Declaration::Impl(ty, impl_items), None),
    ))
}

fn parse_impl_item<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
    ty: &Type,
) -> IResult<&'a str, (String, ConstExpr), E> {
    if let Ok((input, method)) = parse_impl_method::<E>(input, ty) {
        return Ok((input, method));
    }

    let (input, item) = alt((
        context("function", parse_impl_fun),
        context("const", parse_impl_const),
    ))(input)?;
    Ok((input, item))
}

fn parse_impl_fun<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (String, ConstExpr), E> {
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

    if let Some(args) = template_args {
        Ok((
            input,
            (
                name.to_owned(),
                ConstExpr::PolyProc(PolyProcedure::new(
                    name.to_owned(),
                    args.into_iter().map(|x| x.to_owned()).collect(),
                    params,
                    ret,
                    body,
                )),
            ),
        ))
    } else {
        Ok((
            input,
            (
                name.to_owned(),
                ConstExpr::Proc(Procedure::new(None, params, ret, body)),
            ),
        ))
    }
}

fn parse_impl_const<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (String, ConstExpr), E> {
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

fn parse_impl_method<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
    ty: &Type,
) -> IResult<&'a str, (String, ConstExpr), E> {
    trace!("Parsing impl method");
    let (input, _) = tag("fun")(input)?;
    trace!("Parsing method");
    let (input, _) = whitespace(input)?;
    trace!("Parsing method name");
    let (input, name) = cut(parse_symbol)(input)?;
    trace!("Parsed method name: {name}");
    let (input, _) = whitespace(input)?;
    // Check if there are any template args
    let (input, template_args) = cut(opt(parse_type_params))(input)?;
    trace!("Parsed template args: {template_args:#?}");
    // Get the function parameters with mutability
    if let Ok((input, (params, ret))) = parse_method_params::<E>(input, ty) {
        trace!("Parsed method parameters: {params:#?}, {ret:#?}");
        let (input, _) = whitespace(input)?;
        let (input, body) = cut(parse_block)(input)?;
        if let Some(args) = template_args {
            Ok((
                input,
                (
                    name.to_owned(),
                    ConstExpr::PolyProc(PolyProcedure::new(
                        name.to_owned(),
                        args.into_iter().map(|x| x.to_owned()).collect(),
                        params,
                        ret,
                        body,
                    )),
                ),
            ))
        } else {
            Ok((
                input,
                (
                    name.to_owned(),
                    ConstExpr::Proc(Procedure::new(Some(name.to_owned()), params, ret, body)),
                ),
            ))
        }
    } else {
        let (input, (params, ret)) = parse_fun_params(input)?;
        trace!("Parsed method parameters: {params:#?}, {ret:#?}");
        let (input, _) = whitespace(input)?;
        let (input, body) = cut(parse_block)(input)?;
        if let Some(args) = template_args {
            Ok((
                input,
                (
                    name.to_owned(),
                    ConstExpr::PolyProc(PolyProcedure::new(
                        name.to_owned(),
                        args.into_iter().map(|x| x.to_owned()).collect(),
                        params,
                        ret,
                        body,
                    )),
                ),
            ))
        } else {
            Ok((
                input,
                (
                    name.to_owned(),
                    ConstExpr::Proc(Procedure::new(Some(name.to_owned()), params, ret, body)),
                ),
            ))
        }
    }
}

fn parse_match_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    // "match" <expr: Expr> "{" <branches: Tuple<(<pattern: Pattern> "=>" <body: Block>)>> "}" => Statement::Match(expr, branches.into_iter().map(|(pat, body)| (pat, body)).collect()),
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("match")(input)?;
    trace!("Parsing match");
    let (input, _) = whitespace(input)?;
    let (input, expr) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("{"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut branches) = many0(terminated(
        pair(
            parse_pattern,
            preceded(
                delimited(whitespace, cut(tag("=>")), whitespace),
                alt((parse_block, parse_expr)),
            ),
        ),
        preceded(whitespace, tag(",")),
    ))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, branch) = opt(pair(
        parse_pattern,
        preceded(
            delimited(whitespace, cut(tag("=>")), whitespace),
            alt((parse_block, parse_expr)),
        ),
    ))(input)?;

    if let Some((pat, body)) = branch {
        branches.push((pat, body));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;
    Ok((input, Expr::Match(expr.into(), branches)))
}

fn parse_match_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, expr) = parse_match_expr(input)?;
    Ok((input, Statement::Expr(expr)))
}

fn parse_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, pattern) = alt((context("alt", parse_alt_pattern), parse_pattern_atom))(input)?;
    trace!("Got pattern: {pattern:#?}");
    Ok((input, pattern))
}

fn parse_alt_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, mut patterns) = many0(terminated(
        parse_pattern_atom,
        preceded(whitespace, preceded(whitespace, tag("|"))),
    ))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(parse_pattern_atom)(input)?;
    if let Some(p) = pattern {
        patterns.push(p);
    }
    trace!("Got alt pattern: {patterns:#?}");
    if patterns.len() == 1 {
        return Ok((input, patterns.pop().unwrap()));
    }
    Ok((input, Pattern::Alt(patterns)))
}

fn parse_pattern_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, pattern) = alt((
        context("pointer", parse_pointer_pattern),
        context("struct", parse_struct_pattern),
        context("variant", parse_variant_pattern),
        context("wildcard", map(tag("_"), |_| Pattern::Wildcard)),
        context(
            "mutable symbol",
            map(preceded(tag("mut"), parse_symbol), |name| {
                Pattern::Symbol(Mutability::Mutable, name.to_owned())
            }),
        ),
        context(
            "symbol",
            map(parse_symbol, |name| {
                Pattern::Symbol(Mutability::Immutable, name.to_owned())
            }),
        ),
        context("tuple", parse_tuple_pattern),
        context("group", delimited(tag("("), cut(parse_pattern), tag(")"))),
        context("const", map(parse_const, Pattern::ConstExpr)),
    ))(input)?;
    Ok((input, pattern))
}

fn parse_tuple_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut patterns) = many1(terminated(parse_pattern, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(parse_pattern)(input)?;
    if let Some(p) = pattern {
        patterns.push(p);
    }

    let (input, _) = tag(")")(input)?;
    Ok((input, Pattern::Tuple(patterns)))
}

fn parse_pointer_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("&")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = parse_pattern_atom(input)?;
    Ok((input, Pattern::Pointer(Box::new(pattern))))
}

fn parse_variant_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, variant) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(parse_pattern_atom)(input)?;
    Ok((
        input,
        Pattern::Variant(variant.to_owned(), pattern.map(Box::new)),
    ))
}

fn parse_struct_pattern<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Pattern, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            opt(preceded(pair(whitespace, tag("=")), parse_pattern)),
        ),
        tag(","),
    ))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = opt(terminated(
        pair(
            parse_symbol,
            opt(preceded(pair(whitespace, tag("=")), parse_pattern)),
        ),
        whitespace,
    ))(input)?;
    if let Some((name, pat)) = pattern {
        fields.push((name, pat));
    }

    let (input, _) = tag("}")(input)?;
    Ok((
        input,
        Pattern::Struct(
            fields
                .into_iter()
                .map(|(name, pat)| {
                    (
                        name.to_owned(),
                        pat.unwrap_or(Pattern::Symbol(Mutability::Immutable, name.to_string())),
                    )
                })
                .collect(),
        ),
    ))
}

fn parse_block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = whitespace(input)?;
    // let (input, expr) = parse_whitespace_sensitive_block(4, input)?;

    start_source_code_tracking(input);

    let (input, mut stmts) = many0(context("statement", parse_stmt))(input)?;

    let (input, _) = whitespace(input)?;
    // Check if there's a trailing expression
    let (input, expr) = opt(parse_expr)(input)?;
    if let Some(e) = expr {
        stmts.push(Statement::Expr(e));
    }
    let (input, _) = whitespace(input)?;

    let (input, _) = tag("}")(input)?;
    let (input, _) = whitespace(input)?;

    let source_code_loc = end_source_code_tracking(input);

    Ok((input, stmts_to_expr(stmts, false).annotate(source_code_loc)))
}

fn parse_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;
    start_source_code_tracking(input);
    let (input, stmt) = alt((
        context(
            "attribute",
            value(Statement::Expr(Expr::NONE), parse_attribute),
        ),
        context("long statement", parse_long_stmt),
        context("short statement", parse_short_stmt),
    ))(input)?;
    let source_code_loc = end_source_code_tracking(input);

    let stmt = match stmt {
        Statement::Declaration(decl, _) => {
            Statement::Declaration(decl, Some(source_code_loc.clone()))
        }
        Statement::Expr(expr) => Statement::Expr(expr.annotate(source_code_loc.clone())),
    };
    trace!("Annotating {stmt:?} with loc {source_code_loc:?}");
    Ok((input, stmt))
}

fn parse_long_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;

    let (input, stmt) = alt((
        context("if let", parse_if_let_stmt),
        context("if", parse_if_stmt),
        context("when", parse_when_stmt),
        context("match", parse_match_stmt),
        context("while", parse_while_stmt),
        context("for", parse_for_stmt),
        context("function", parse_quick_fun_stmt),
        context("function", parse_fun_stmt),
        context("impl", parse_impl_stmt),
        context("enum", parse_enum_stmt),
        context("struct", parse_struct_stmt),
        context("module", parse_module_stmt),
        context("module", parse_module_file_stmt),
        context("import", parse_import_stmt),
        map(context("block", parse_block), Statement::Expr),
    ))(input)?;

    let (input, _) = whitespace(input)?;

    Ok((input, stmt))
}

fn parse_import_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;
    let (input, stmt) = parse_import_decl(input)?;
    Ok((input, Statement::Declaration(stmt, None)))
}
fn parse_import_decl<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Declaration, E> {
    // "import" <path: String> => Statement::Import(path),
    let (input, _) = tag("from")(input)?;
    let (input, _) = whitespace(input)?;
    // let (input, module_name) = cut(parse_symbol)(input)?;
    // Get the module path, possibly with multiple dots
    // let (input, mut module_path) = many0(terminated(parse_symbol, tag(".")))(input)?;
    // let (input, module_name) = parse_symbol(input)?;
    // module_path.push(module_name);
    let (input, module) = parse_const(input)?;

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("import")(input)?;

    let (mut input, _) = whitespace(input)?;
    // Try to parse a "*"
    if let Ok((input, _)) = tag::<&str, &str, E>("*")(input) {
        let (input, _) = whitespace(input)?;
        let (input, _) = tag(";")(input)?;
        return Ok((input, Declaration::FromImportAll(module)));
    }

    let mut imports = vec![];
    let mut status = input;
    loop {
        input = status;
        let (input, _) = whitespace(input)?;
        let (input, path) = cut(parse_symbol)(input)?;
        // Optionally parse an alias
        let (input, alias) =
            opt(preceded(whitespace, preceded(tag("as"), cut(parse_symbol))))(input)?;

        // If there's a comma, continue parsing
        let (input, _) = whitespace(input)?;

        imports.push((path.to_owned(), alias));
        if let Ok((input, _)) = tag::<&str, &str, E>(",")(input) {
            status = input;
            continue;
        }

        let imports: Vec<(String, Option<String>)> = imports
            .into_iter()
            .map(|(x, y)| (x.to_owned(), y.map(|z| z.to_owned())))
            .collect();

        let (input, _) = whitespace(input)?;
        let (input, _) = tag(";")(input)?;

        return Ok((
            input,
            Declaration::FromImport {
                module,
                names: imports,
            },
        ));
    }
}

fn parse_module_contents<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    name: &str,
    input: &'a str,
) -> IResult<&'a str, Declaration, E> {
    let (input, _) = whitespace(input)?;

    let (input, decls) = many0(context("statement", parse_decl))(input)?;

    let (input, _) = whitespace(input)?;
    Ok((input, Declaration::module(name, decls)))
}

fn parse_module_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;

    let (input, _) = tag("mod")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;

    let (input, _) = tag("{")(input)?;

    let (input, module) = parse_module_contents(name, input)?;

    let (input, _) = cut(tag("}"))(input)?;
    let (input, _) = whitespace(input)?;

    Ok((input, Statement::Declaration(module, None)))
}

fn parse_module_file_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;

    let (input, _) = tag("mod")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = whitespace(input)?;
    trace!("Parsed module file stmt for {name}");
    // Open the file
    if let Ok(contents) = std::fs::read_to_string(format!("{}.sg", name)) {
        save_source_code_setup();
        setup_source_code_locations(&contents.clone(), Some(name.to_string()));
        if let Ok((new_input, module)) =
            parse_module_contents::<VerboseError<&str>>(name, &contents)
        {
            if !new_input.is_empty() {
                return Err(nom::Err::Error(E::from_error_kind(
                    input,
                    ErrorKind::Verify,
                )));
            }
            restore_source_code_setup();
            return Ok((input, Statement::Declaration(module, None)));
        } else {
            restore_source_code_setup();
            return Err(nom::Err::Error(E::from_error_kind(
                input,
                ErrorKind::Verify,
            )));
        }
    }

    restore_source_code_setup();
    Err(nom::Err::Error(E::from_error_kind(
        input,
        ErrorKind::Verify,
    )))
}

fn parse_decl<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Declaration, E> {
    let (input, _) = whitespace(input)?;
    let (input, decl) = alt((
        context("function", parse_fun_stmt),
        context("type", parse_type_stmt),
        context("enum", parse_enum_stmt),
        context("struct", parse_struct_stmt),
        context("extern", parse_extern_stmt),
        context("const", terminated(parse_const_stmt, tag(";"))),
        context("impl", parse_impl_stmt),
        context("import", parse_import_stmt),
        context("module", parse_module_stmt),
        context("module", parse_module_file_stmt),
    ))(input)?;

    match decl {
        Statement::Declaration(decl, _) => Ok((input, decl)),
        _ => unreachable!(),
    }
}

fn parse_if_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (mut input, then) = cut(parse_block)(input)?;

    let mut else_branches = vec![];
    // Try to parse else ifs
    while let Ok((x, _)) = preceded(
        whitespace::<E>,
        preceded(tag("else"), preceded(whitespace, tag("if"))),
    )(input)
    {
        let (x, _) = whitespace(x)?;
        let (x, condition) = cut(parse_expr)(x)?;
        let (x, _) = whitespace(x)?;
        let (x, then) = cut(parse_block)(x)?;
        input = x;
        else_branches.push((condition, then));
    }

    let (input, mut result) = if let Ok((input, _)) =
        preceded(whitespace::<E>, preceded(tag("else"), whitespace::<E>))(input)
    {
        // Parse the else block
        cut(parse_block)(input)?
    } else {
        (input, Expr::NONE)
    };

    for (condition, then) in else_branches.into_iter().rev() {
        result = Expr::If(condition.into(), Box::new(then), Box::new(result));
    }

    Ok((
        input,
        Expr::If(condition.into(), Box::new(then), Box::new(result)),
    ))
}

fn parse_if_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, expr) = parse_if_expr(input)?;
    Ok((input, Statement::Expr(expr)))
}

fn parse_if_let_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("let")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, pattern) = cut(parse_pattern)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, then) = cut(parse_block)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, else_) = opt(preceded(tag("else"), cut(parse_block)))(input)?;
    Ok((
        input,
        Expr::IfLet(
            pattern,
            value.into(),
            Box::new(then),
            else_.unwrap_or(Expr::NONE).into(),
        ),
    ))
}

fn parse_if_let_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, expr) = parse_if_let_expr(input)?;
    Ok((input, Statement::Expr(expr)))
}

fn parse_when_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    // "when" <condition: Expr> <then: Block> <else: Option<Block>> => Statement::If(condition, Box::new(then), else.map(Box::new)),
    let (input, _) = tag("when")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_const)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, then) = cut(parse_block)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, else_) = opt(preceded(tag("else"), cut(parse_block)))(input)?;
    Ok((
        input,
        Statement::Expr(Expr::When(
            condition,
            Box::new(then),
            else_.unwrap_or(Expr::NONE).into(),
        )),
    ))
}

fn parse_while_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = tag("while")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, condition) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_block)(input)?;
    Ok((
        input,
        Statement::Expr(Expr::While(condition.into(), Box::new(body))),
    ))
}

fn parse_for_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
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

    let mut init_expr = Expr::NONE;
    let mut init_decl = Declaration::many(vec![]);
    match init {
        Statement::Declaration(decl, _) => init_decl = decl,
        Statement::Expr(e) => init_expr = e,
    };

    let mut step_expr = Expr::NONE;
    if let Statement::Expr(e) = step {
        step_expr = e;
    }

    Ok((
        input,
        Statement::Expr(
            Expr::While(
                condition.into(),
                Expr::Many(vec![init_expr, body, step_expr]).into(),
            )
            .with(init_decl),
        ),
    ))
}

fn parse_fun_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = tag("fun")(input)?;
    trace!("Parsing function");
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // Check if there are any template args
    let (input, template_args) = cut(opt(parse_type_params))(input)?;
    // Get the function parameters with mutability
    trace!("Parsing function parameters");
    trace!("Input: {input}");
    let (input, (params, ret)) = cut(parse_fun_params)(input)?;
    trace!("Parsed function parameters: {params:#?}, {ret:#?}");
    let (input, _) = whitespace(input)?;
    let (input, body) = parse_block(input)?;
    trace!("Parsed function body: {body}");
    if let Some(args) = template_args {
        Ok((
            input,
            Statement::Declaration(
                Declaration::PolyProc(
                    name.to_owned(),
                    PolyProcedure::new(
                        name.to_owned(),
                        args.into_iter().map(|x| x.to_owned()).collect(),
                        params,
                        ret,
                        body,
                    ),
                ),
                None,
            ),
        ))
    } else {
        Ok((
            input,
            Statement::Declaration(
                Declaration::Proc(
                    name.to_owned(),
                    Procedure::new(Some(name.to_owned()), params, ret, body),
                ),
                None,
            ),
        ))
    }
}

fn parse_quick_fun_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = tag("fun")(input)?;
    trace!("Parsing function");
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // Check if there are any template args
    let (input, template_args) = cut(opt(parse_type_params))(input)?;
    // Get the function parameters with mutability
    trace!("Parsing function parameters");
    trace!("Input: {input}");
    let (input, (params, ret)) = cut(parse_fun_params)(input)?;
    trace!("Parsed function parameters: {params:#?}, {ret:#?}");
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, body) = cut(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag(";"))(input)?;
    trace!("Parsed function body: {body}");

    if let Some(args) = template_args {
        Ok((
            input,
            Statement::Declaration(
                Declaration::PolyProc(
                    name.to_owned(),
                    PolyProcedure::new(
                        name.to_owned(),
                        args.into_iter().map(|x| x.to_owned()).collect(),
                        params,
                        ret,
                        body,
                    ),
                ),
                None,
            ),
        ))
    } else {
        Ok((
            input,
            Statement::Declaration(
                Declaration::Proc(
                    name.to_owned(),
                    Procedure::new(Some(name.to_owned()), params, ret, body),
                ),
                None,
            ),
        ))
    }
}

fn parse_fun_params<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (Vec<(String, Mutability, Type)>, Type), E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, params) = many0(terminated(
        pair(
            pair(
                map(opt(tag("mut")), |x| match x {
                    Some(_) => Mutability::Mutable,
                    None => Mutability::Immutable,
                }),
                parse_symbol,
            ),
            preceded(pair(whitespace, tag(":")), cut(parse_type)),
        ),
        delimited(whitespace, tag(","), whitespace),
    ))(input)?;

    let (input, _) = whitespace(input)?;
    let (input, last) = opt(pair(
        pair(
            map(opt(tag("mut")), |x| match x {
                Some(_) => Mutability::Mutable,
                None => Mutability::Immutable,
            }),
            parse_symbol,
        ),
        preceded(pair(whitespace, tag(":")), cut(parse_type)),
    ))(input)?;

    let mut params: Vec<_> = params
        .into_iter()
        .map(|((mutability, name), ty)| (name.to_owned(), mutability, ty))
        .collect();
    if let Some(((mutability, name), ty)) = last {
        params.push((name.to_owned(), mutability, ty));
    }
    trace!("Parsed function parameters: {params:#?}");

    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag(")"))(input)?;
    let (input, _) = whitespace(input)?;

    let (input, ret) = cut(opt(preceded(tag(":"), parse_type)))(input)?;

    Ok((input, (params, ret.unwrap_or(Type::None))))
}

fn parse_method_params<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
    ty: &Type,
) -> IResult<&'a str, (Vec<(String, Mutability, Type)>, Type), E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;

    // Get the self parameter, either `self`, `mut self`, `&self`, or `&mut self`
    let (input, self_param) = alt((
        context(
            "self",
            map(tag("self"), |_| {
                ("self".to_owned(), Mutability::Immutable, ty.clone())
            }),
        ),
        context(
            "mut self",
            map(delimited(tag("mut"), whitespace, tag("self")), |_| {
                ("self".to_owned(), Mutability::Mutable, ty.clone())
            }),
        ),
        context(
            "&self",
            map(delimited(tag("&"), whitespace, tag("self")), |_| {
                (
                    "self".to_owned(),
                    Mutability::Immutable,
                    Type::Pointer(Mutability::Immutable, Box::new(ty.clone())),
                )
            }),
        ),
        context(
            "&mut self",
            map(
                delimited(
                    delimited(tag("&"), whitespace, tag("mut")),
                    whitespace,
                    tag("self"),
                ),
                |_| {
                    (
                        "self".to_owned(),
                        Mutability::Immutable,
                        Type::Pointer(Mutability::Mutable, Box::new(ty.clone())),
                    )
                },
            ),
        ),
    ))(input)?;
    trace!("Parsed self parameter: {self_param:#?}");
    let (input, _) = whitespace(input)?;
    let (input, _) = opt(tag(","))(input)?;

    let (input, params) = many0(terminated(
        pair(
            pair(
                map(opt(tag("mut")), |x| match x {
                    Some(_) => Mutability::Mutable,
                    None => Mutability::Immutable,
                }),
                parse_symbol,
            ),
            preceded(pair(whitespace, tag(":")), cut(parse_type)),
        ),
        tag(","),
    ))(input)?;
    trace!("Parsed self parameter: {params:#?}");
    let (input, last) = opt(pair(
        // parse_symbol,
        pair(
            map(opt(tag("mut")), |x| match x {
                Some(_) => Mutability::Mutable,
                None => Mutability::Immutable,
            }),
            parse_symbol,
        ),
        preceded(pair(whitespace, tag(":")), cut(parse_type)),
    ))(input)?;
    trace!("Parsed method parameters: {params:#?}, {last:#?}");
    trace!("Parsed method parameters: {input}");

    let mut params: Vec<_> = std::iter::once(self_param)
        .chain(
            params
                .into_iter()
                .map(|((mutability, name), ty)| (name.to_owned(), mutability, ty)),
        )
        .collect();
    if let Some(((mutability, name), ty)) = last {
        params.push((name.to_owned(), mutability, ty));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag(")"))(input)?;
    let (input, _) = whitespace(input)?;

    let (input, ret) = cut(opt(preceded(tag(":"), parse_type)))(input)?;

    Ok((input, (params, ret.unwrap_or(Type::None))))
}

fn parse_extern_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    // "extern" <name: Symbol> <args: Tuple<(<(<Symbol> ":")?> <Type>)>> ":" <ret: Type> => {
    //     let args: Vec<_> = args.into_iter().map(|(_name, ty)| ty).collect();
    //     Statement::Declaration(Declaration::ExternProc(name.clone(), FFIProcedure::new(name, args, ret)))
    // },
    let (input, _) = tag("extern")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("fun"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    // let (input, _) = tag(":")(input)?;
    // let (input, ret) = parse_type(input)?;
    let (input, (params, ret)) = cut(parse_fun_params)(input)?;
    // let (input, _) = cut(tag(";"))(input)?;

    let args: Vec<_> = params
        .into_iter()
        .map(|(_name, _mutability, ty)| ty)
        .collect();
    Ok((
        input,
        Statement::Declaration(
            Declaration::ExternProc(
                name.to_owned(),
                FFIProcedure::new(name.to_owned(), args, ret),
            ),
            None,
        ),
    ))
}

fn parse_const_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    // "const" <name: Symbol> "=" <value: ConstExpr> => Statement::Declaration(Declaration::Const(name, value)),
    let (input, _) = tag("const")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = cut(parse_symbol)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("="))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = cut(parse_const)(input)?;
    Ok((
        input,
        Statement::Declaration(Declaration::Const(name.to_owned(), value), None),
    ))
}

fn parse_pattern_var_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    // "let" <name: Symbol> ":" <ty: Type> "=" <value: Expr> => Statement::Declaration(Declaration::Var(name, Mutability::Immutable, Some(ty), value)),
    // "let" <name: Symbol> "=" <value: Expr> => Statement::Declaration(Declaration::Var(name, Mutability::Immutable, None, value)),
    let (input, _) = tag("let")(input)?;
    let (input, _) = whitespace(input)?;

    let (input, pattern) = parse_pattern(input)?;
    let (input, _) = whitespace(input)?;

    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = parse_expr(input)?;

    Ok((
        input,
        Statement::Declaration(Declaration::VarPat(pattern, value), None),
    ))
}

fn parse_var_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
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
        preceded(pair(whitespace, tag(":")), parse_type),
        whitespace,
    ))(input)?;

    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = parse_expr(input)?;

    Ok((
        input,
        Statement::Declaration(
            Declaration::Var(name.to_owned(), mutability, ty, value),
            None,
        ),
    ))
}

fn parse_static_var_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
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
        preceded(pair(whitespace, tag(":")), parse_type),
        whitespace,
    ))(input)?;

    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = parse_expr(input)?;

    let ty = ty.unwrap_or(Type::None);
    Ok((
        input,
        Statement::Declaration(
            Declaration::static_var(name.to_owned(), mutability, ty, value),
            None,
        ),
    ))
}

fn parse_type_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    // "type" <name: Symbol> "=" <ty: Type> => Statement::Declaration(Declaration::Type(name, ty)),
    let (input, _) = tag("type")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, opt_template_params) = opt(parse_type_params)(input)?;
    let (input, _) = whitespace(input)?;

    let (input, _) = tag("=")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut ty) = parse_type(input)?;
    if let Some(template_params) = opt_template_params {
        ty = Type::Poly(template_params, ty.into());
    }

    Ok((
        input,
        Statement::Declaration(Declaration::Type(name.to_owned(), ty), None),
    ))
}

fn parse_struct_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
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
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            preceded(pair(whitespace, tag(":")), parse_type),
        ),
        preceded(whitespace, tag(",")),
    ))(input)?;
    let (input, _) = whitespace(input)?;

    // Check for the last field
    let (input, last) = opt(pair(
        parse_symbol,
        preceded(pair(whitespace, tag(":")), parse_type),
    ))(input)?;
    if let Some((name, ty)) = last {
        fields.push((name, ty));
    }
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("}"))(input)?;
    let fields = fields
        .into_iter()
        .map(|(name, ty)| (name.to_owned(), ty))
        .collect();

    // Check if there are any template params
    if let Some(params) = template_params {
        Ok((
            input,
            Statement::Declaration(
                Declaration::Type(
                    name.to_owned(),
                    Type::Poly(
                        params,
                        Type::Struct(fields).into(),
                    ),
                ),
                None,
            ),
        ))
    } else {
        Ok((
            input,
            Statement::Declaration(
                Declaration::Type(name.to_owned(), Type::Struct(fields)),
                None,
            ),
        ))
    }
}

fn parse_enum_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
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
        preceded(whitespace, pair(parse_symbol, opt(parse_type))),
        terminated(tag(","), whitespace),
    ))(input)?;

    let (input, last) = opt(preceded(whitespace, pair(parse_symbol, opt(parse_type))))(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    // For all the fields that don't have a type, assign them the "None" type
    let fields = fields
        .into_iter()
        .map(|(k, v)| (k.to_owned(), v.unwrap_or(Type::None)))
        .collect();
    // trace!("Fields: {fields}");
    // trace!("Template params: {template_params}");
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    if let Some(params) = template_params {
        Ok((
            input,
            Statement::Declaration(
                Declaration::Type(
                    name.to_owned(),
                    Type::Poly(
                        params,
                        Type::EnumUnion(fields).into(),
                    ),
                ),
                None,
            ),
        ))
    } else {
        Ok((
            input,
            Statement::Declaration(
                Declaration::Type(name.to_owned(), Type::EnumUnion(fields)),
                None,
            ),
        ))
    }
}

fn parse_return_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    // "return" <value: Expr> => Statement::Expr(Expr::Return(value.into())),
    let (input, _) = tag("return")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, value) = cut(parse_expr)(input)?;
    Ok((input, Statement::Expr(Expr::Return(value.into()))))
}

fn parse_assign_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
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
                Expr::Index(e, idx) => {
                    Statement::Expr(e.idx(*idx).refer(Mutability::Mutable).deref_mut(val))
                }
                Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                    Statement::Expr(Expr::var(name).refer(Mutability::Mutable).deref_mut(val))
                }
                Expr::Member(e, field) => {
                    Statement::Expr(e.field(field).refer(Mutability::Mutable).deref_mut(val))
                }
                Expr::ConstExpr(ConstExpr::Member(e, field)) => Statement::Expr(
                    Expr::from(e.field(*field))
                        .refer(Mutability::Mutable)
                        .deref_mut(val),
                ),
                Expr::Annotated(inner, _) => match *inner {
                    Expr::Deref(e) => Statement::Expr(e.deref_mut(val)),
                    Expr::Index(e, idx) => {
                        Statement::Expr(e.idx(*idx).refer(Mutability::Mutable).deref_mut(val))
                    }
                    Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                        Statement::Expr(Expr::var(name).refer(Mutability::Mutable).deref_mut(val))
                    }
                    Expr::Member(e, field) => {
                        Statement::Expr(e.field(field).refer(Mutability::Mutable).deref_mut(val))
                    }
                    Expr::ConstExpr(ConstExpr::Member(e, field)) => Statement::Expr(
                        Expr::from(e.field(*field))
                            .refer(Mutability::Mutable)
                            .deref_mut(val),
                    ),
                    unexpected => panic!("Unexpected assignment to {unexpected}"),
                },
                unexpected => panic!("Unexpected assignment to {unexpected}"),
            };

            Ok((input, result))
        }
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
            Ok((
                input,
                Statement::Expr(dst.refer(Mutability::Mutable).assign_op(op, val)),
            ))
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

fn parse_short_stmt<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Statement, E> {
    let (input, _) = whitespace(input)?;

    // Check if there's just a semicolon
    if let Ok((input, _)) = tag::<&str, &str, E>(";")(input) {
        return Ok((input, Statement::Expr(Expr::NONE)));
    }

    start_source_code_tracking(input);

    let (input, stmt) = alt((
        context("extern", parse_extern_stmt),
        context("const", parse_const_stmt),
        context("let", parse_var_stmt),
        context("let", parse_pattern_var_stmt),
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

fn parse_type_pointer<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
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

fn parse_type_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
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

fn parse_type_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut tys) = many1(terminated(parse_type, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_ty) = opt(parse_type)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_ty) = last_ty {
        tys.push(last_ty);
    }

    Ok((input, Type::Tuple(tys)))
}

fn parse_type_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
    // Optionally check for "struct" keyword
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        pair(
            parse_symbol,
            preceded(pair(whitespace, tag(":")), parse_type),
        ),
        tag(","),
    ))(input)?;

    let (input, last) = opt(pair(
        parse_symbol,
        preceded(pair(whitespace, tag(":")), parse_type),
    ))(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((
        input,
        Type::Struct(fields.into_iter().map(|(k, v)| (k.to_owned(), v)).collect()),
    ))
}

fn parse_type_enum<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
    // Optionally check for "enum" keyword
    let (input, _) = opt(tag("enum"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    // Parse a comma separated list of symbols, optionally followed by a type
    let (input, mut fields) =
        many0(terminated(pair(parse_symbol, opt(parse_type)), tag(",")))(input)?;

    let (input, last) = opt(pair(parse_symbol, opt(parse_type)))(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    // For all the fields that don't have a type, assign them the "None" type
    let fields = fields
        .into_iter()
        .map(|(k, v)| (k.to_owned(), v.unwrap_or(Type::None)))
        .collect();

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, Type::EnumUnion(fields)))
}

fn parse_type_params<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<(String, Option<Type>)>, E> {
    let (input, _) = tag("<")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut params) = many0(terminated(alt((
        preceded(delimited(whitespace, tag("const"), whitespace), map(pair(parse_symbol, delimited(terminated(whitespace, tag(":")), parse_type, whitespace)), |(name, ty)| (name, Some(ty)))),
        map(parse_symbol, |x| (x, None))
    )), preceded(whitespace, tag(","))))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_param) = opt(alt((
        preceded(delimited(whitespace, tag("const"), whitespace), map(pair(parse_symbol, delimited(terminated(whitespace, tag(":")), parse_type, whitespace)), |(name, ty)| (name, Some(ty)))),
        // map(pair(parse_symbol, delimited(terminated(whitespace, tag(":")), parse_type, whitespace)), |(name, ty)| (name, Some(ty))),
        map(parse_symbol, |x| (x, None))
    )))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(">")(input)?;

    if let Some(last_param) = last_param {
        params.push(last_param);
    }

    Ok((input, params.into_iter().map(|(name, ty)| (name.to_string(), ty)).collect()))
}

fn parse_type_function<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
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
        Some(poly_params) => Ok((
            input,
            Type::Poly(poly_params, Box::new(Type::Proc(args, Box::new(ret)))),
        )),
        None => Ok((input, Type::Proc(args, Box::new(ret)))),
    }
}

fn parse_type_apply<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
    let (input, ty) = parse_type_atom(input)?;
    let (input, _) = whitespace(input)?;

    if let Ok((input, _)) = tag::<&str, &str, E>("<")(input) {
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
    } else {
        return Ok((input, ty))
    }
}

fn parse_type_primitive<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
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
        value(Type::None, pair(tag("("), preceded(whitespace, tag(")")))),
    ))(input)?;

    Ok((input, ty))
}

fn parse_type_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
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
        map(parse_const_atom, |x| Type::ConstParam(x.into()))
    ))(input)?;

    Ok((input, ty))
}

fn parse_type_group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, ty))
}

fn parse_type<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Type, E> {
    let (input, ty) = alt((parse_type_apply, parse_type_atom))(input)?;

    Ok((input, ty))
}

fn parse_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    // let (input, c) = parse_expr_atom(input)?;

    // Optionally parse a decorator
    let (input, _) = whitespace(input)?;
    let (input, decorator) = opt(parse_decorator)(input)?;

    let (input, _) = whitespace(input)?;
    start_source_code_tracking(input);
    let (input, expr) = parse_expr_prec(input, 0)?;
    // println!("Got expr: {expr}");
    let source_code_loc = end_source_code_tracking(input);
    // Ok((input, Expr::ConstExpr(c)))
    let mut env = LISP_ENV.write().unwrap();
    let expr = match decorator {
        // Some(decorator) => sage_lisp::Expr::deserialize::<Expr>(&env.eval(decorator.apply(&[sage_lisp::Expr::serialize(expr)]))).unwrap(),
        Some(decorator) => {
            let input = sage_lisp::Expr::serialize(expr);
            trace!("Input: {input}");
            let result = env.eval(decorator.apply(&[input.quote()]));
            trace!("Result: {result}");
            sage_lisp::Expr::deserialize::<Expr>(&result).unwrap()
        }
        None => expr,
    };

    Ok((input, expr.annotate(source_code_loc)))
}

fn parse_expr_prec<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
    prec: u8,
) -> IResult<&'a str, Expr, E> {
    // if prec <= 0 {
    //     return parse_expr_atom(input);
    // }

    // trace!("Parsing at prec={prec}");
    let (mut input, _) = whitespace(input)?;
    if prec >= get_max_precedence() {
        // trace!("Max prec reached, falling back to atom");
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
            Ok((i, _))
                if is_symbol_char(i.chars().next().unwrap())
                    && is_symbol_char(op.chars().last().unwrap()) =>
            {
                continue
            }
            Ok((i, _)) => {
                input = i;
                has_found_op = true;
                found_op = Some(*op_expr);
                break;
            }
            _ => continue,
        }
    }

    let (mut input, mut lhs) = if has_found_op {
        let (input, lhs) = parse_expr_prec(input, prec)?;
        (input, found_op.unwrap()(lhs))
    } else {
        parse_expr_prec(input, prec + 1)?
    };

    // trace!("Parsed lhs: {lhs}");
    let mut input_updater = input;
    loop {
        input = input_updater;

        // Try to match a binary operator
        let (mut input, _) = whitespace(input)?;
        // Match any of the binary operators
        let mut has_found_op = false;
        let mut found_op = None;
        for (op, (op_prec, _op_expr)) in BIN_OPS.read().unwrap().iter().rev() {
            if *op_prec < prec {
                continue;
            }
            match tag::<&str, &str, E>(op.as_str())(input) {
                Ok((i, op)) if !['=', '&'].contains(&i.chars().next().unwrap()) => {
                    trace!("FOUND OPERATOR: {op}");
                    input = i;
                    has_found_op = true;
                    found_op = Some(op);
                    break;
                }
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
            // trace!("Operator precedence is less than current precedence, returning {lhs}");
            return Ok((input_updater, lhs));
        }

        let (input, rhs) = parse_expr_prec(input, prec + 1)?;
        // trace!("Parsed rhs: {rhs}");
        // lhs = lhs.binop(BIN_OPS.read().unwrap().get(found_op).unwrap().2.clone(), rhs);
        let f = BIN_OPS.read().unwrap().get(found_op).unwrap().1;
        lhs = f(lhs, rhs);

        input_updater = input;
    }
}

fn parse_expr_term<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
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

fn parse_expr_factor<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, expr) = alt((
        parse_expr_variant,
        map(parse_const_monomorph, Expr::ConstExpr),
        parse_expr_atom,
        map(parse_const, Expr::ConstExpr),
    ))(input)?;

    Ok((input, expr))
}

fn parse_expr_variant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = opt(parse_expr_atom)(input)?;
    Ok((
        input,
        Expr::EnumUnion(ty, name.to_string(), Box::new(expr.unwrap_or(Expr::NONE))),
    ))
}

fn parse_expr_index<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    expr: &Expr,
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, index) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = cut(tag("]"))(input)?;

    Ok((input, expr.clone().idx(index)))
}

fn parse_expr_cast<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    expr: &Expr,
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("as")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, ty) = parse_type(input)?;

    Ok((input, expr.clone().as_type(ty)))
}

fn parse_expr_call<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    expr: &Expr,
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    trace!("Parsing call!");
    let (input, _) = whitespace(input)?;
    let (input, mut args) = many0(terminated(parse_expr, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_arg) = opt(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_arg) = last_arg {
        args.push(last_arg);
    }

    if let Expr::ConstExpr(ConstExpr::Symbol(name)) = expr {
        // Ok((input, Expr::var(name).app(args)))
        match name.as_str() {
            "input" => {
                // return Ok((input, args.print()))
                return Ok((
                    input,
                    Expr::Many(args.into_iter().map(|x: Expr| x.unop(Get)).collect()),
                ));
            }
            "print" => {
                // return Ok((input, args.print()))
                return Ok((
                    input,
                    Expr::Many(args.into_iter().map(|x| x.print()).collect()),
                ));
            }
            "println" => {
                // return Ok((input, args.println()))
                return Ok((
                    input,
                    Expr::Many(
                        args.into_iter()
                            .chain(vec![Expr::ConstExpr(ConstExpr::Char('\n'))])
                            .map(|x| x.print())
                            .collect(),
                    ),
                ));
            }
            _ => {}
        }
    }

    Ok((input, expr.clone().app(args)))
}

fn parse_expr_member<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    expr: &Expr,
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let parse_field = |input| {
        let (input, _) = tag(".")(input)?;
        let (input, _) = whitespace(input)?;
        let (input, field) = alt((
            map(parse_symbol, |x| ConstExpr::Symbol(x.to_string())),
            map(parse_int_literal, ConstExpr::Int),
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

fn parse_expr_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, expr) = alt((
        map(parse_const_atom, Expr::ConstExpr),
        parse_expr_tuple,
        parse_expr_group,
        parse_expr_array,
        parse_expr_struct,
        parse_block,
        parse_if_let_expr,
        parse_if_expr,
        parse_match_expr,
        map(parse_type_atom, |t| ConstExpr::Type(t).into()),
    ))(input)?;

    Ok((input, expr))
}

fn parse_expr_group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, expr))
}

fn parse_expr_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many1(terminated(parse_expr, tag(",")))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, Expr::Tuple(exprs)))
}

fn parse_expr_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many0(terminated(parse_expr, preceded(whitespace, tag(","))))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_expr)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("]")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, Expr::Array(exprs)))
}

fn parse_expr_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Expr, E> {
    let (input, _) = whitespace(input)?;
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        alt((
            pair(
                parse_symbol,
                preceded(pair(whitespace, tag("=")), parse_expr),
            ),
            map(parse_symbol, |x| (x, Expr::var(x.to_string()))),
        )),
        preceded(whitespace, tag(",")),
    ))(input)?;

    let (input, _) = whitespace(input)?;
    let (input, last) = opt(alt((
        pair(
            parse_symbol,
            preceded(pair(whitespace, tag("=")), parse_expr),
        ),
        map(parse_symbol, |x| (x, Expr::var(x.to_string()))),
    )))(input)?;

    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((
        input,
        Expr::Struct(fields.into_iter().map(|(k, v)| (k.to_owned(), v)).collect()),
    ))
}

// fn parse_expr_block<'a, E: ParseError<&'a str> + ContextError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
//     let (input, _) = whitespace(input)?;
//     let (input, _) = tag("{")(input)?;
//     trace!("Parsing block");
//     let (input, _) = whitespace(input)?;
//     let (input, mut exprs) = many0(terminated(parse_expr, tag(";")))(input)?;
//     let (input, _) = whitespace(input)?;
//     let (input, last) = opt(parse_expr)(input)?;
//     if let Some(last) = last {
//         exprs.push(last);
//     }

//     let (input, _) = whitespace(input)?;
//     let (input, _) = tag("}")(input)?;

//     Ok((input, Expr::Many(exprs)))
// }

fn parse_const<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    alt((parse_const_monomorph, parse_const_term))(input)
}

fn parse_const_term<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    alt((parse_const_variant, parse_const_member))(input)
}

fn parse_const_monomorph<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, expr) = parse_const_term(input)?;
    let (input, _) = whitespace(input)?;
    if let Ok((input, _)) = tag::<&str, &str, E>("<")(input) {
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
    } else {
        Ok((input, expr))
    }
}

fn parse_const_variant<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, ty) = parse_type(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, name) = parse_symbol(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = opt(parse_const_atom)(input)?;
    Ok((
        input,
        ConstExpr::EnumUnion(
            ty,
            name.to_string(),
            Box::new(expr.unwrap_or(ConstExpr::None)),
        ),
    ))
}

fn parse_const_member<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, mut expr) = parse_const_atom(input)?;
    // Handle multiple dots
    let (input, _) = whitespace(input)?;
    let (input, fields) = many0(terminated(
        preceded(
            tag("."),
            alt((
                map(parse_symbol, |x| ConstExpr::Symbol(x.to_string())),
                map(parse_int_literal, ConstExpr::Int),
            )),
        ),
        whitespace,
    ))(input)?;

    for field in fields {
        expr = expr.field(field);
    }

    Ok((input, expr))
}

fn parse_const_group<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_const(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, expr))
}

fn parse_const_atom<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = whitespace(input)?;
    alt((
        parse_const_sizeof_expr,
        parse_const_sizeof_type,
        parse_const_tuple,
        parse_const_group,
        parse_const_bool,
        parse_const_null,
        parse_const_none,
        map(parse_char_literal, ConstExpr::Char),
        map(parse_float_literal, ConstExpr::Float),
        map(parse_int_literal, ConstExpr::Int),
        map(parse_string_literal, |s| {
            ConstExpr::Array(
                s.to_string()
                    .chars()
                    .map(ConstExpr::Char)
                    .chain(std::iter::once(ConstExpr::Char('\0')))
                    .collect(),
            )
        }),
        parse_const_array,
        parse_const_struct,
        map(parse_symbol, |x| ConstExpr::Symbol(x.to_string())),
    ))(input)
}

fn parse_int_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, i64, E> {
    // First try to parse hex
    // Check if its negative
    let (input, _) = whitespace(input)?;
    let (input, is_negative) = opt(tag("-"))(input)?;
    let is_negative = is_negative.is_some();
    let (input, _) = whitespace(input)?;

    let (input, result) = alt((
        map(preceded(tag("0x"), hex_digit1), |s: &str| {
            i64::from_str_radix(s, 16).unwrap()
        }),
        // Try octal
        map(preceded(tag("0o"), oct_digit1), |s: &str| {
            i64::from_str_radix(s, 8).unwrap()
        }),
        // Try binary
        map(preceded(tag("0b"), bin_digit1), |s: &str| {
            i64::from_str_radix(s, 2).unwrap()
        }),
        map(digit1, |s: &str| s.parse().unwrap()),
    ))(input)?;

    // let (input, result) = map(digit1, |s: &str| s.parse().unwrap())(input)?;
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    let result = if is_negative { -result } else { result };

    Ok((input, result))
}

fn parse_float_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, f64, E> {
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

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str>,
{
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    // `preceded` takes a prefix parser, and if it succeeds, returns the result
    // of the body parser. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
        // It returns the result of the middle parser. In this case, it parses
        // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = map(parse_delimited_hex, move |hex| {
        u32::from_str_radix(hex, 16).unwrap()
    });

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32).parse(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str>,
{
    preceded(
        char('\\'),
        // `alt` tries each parser in sequence, returning the result of
        // the first successful match
        alt((
            parse_unicode,
            // The `value` parser returns a fixed value (the first argument) if its
            // parser (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            value('\0', char('0')),
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
            value('\'', char('\'')),
        )),
    )
    .parse(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    preceded(char('\\'), multispace1).parse(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal_intermediate<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    // `is_not` parses a string of 0 or more characters that aren't one of the
    // given characters.
    let not_quote_slash = is_not("\"\\");

    // `verify` runs a parser, then runs a verification function on the output of
    // the parser. The verification function accepts out output only if it
    // returns true. In this case, we want to ensure that the output of is_not
    // is non-empty.
    verify(not_quote_slash, |s: &str| !s.is_empty()).parse(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal_intermediate_char<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    // `is_not` parses a string of 0 or more characters that aren't one of the
    // given characters.
    let not_quote_slash = is_not("\'\\");

    // `verify` runs a parser, then runs a verification function on the output of
    // the parser. The verification function accepts out output only if it
    // returns true. In this case, we want to ensure that the output of is_not
    // is non-empty.
    verify(not_quote_slash, |s: &str| !s.is_empty()).parse(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment_str<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
where
    E: ParseError<&'a str>,
{
    alt((
        // The `map` combinator runs a parser, then applies a function to the output
        // of that parser.
        map(parse_literal_intermediate, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))
    .parse(input)
}

fn parse_fragment_char<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment, E>
where
    E: ParseError<&'a str>,
{
    alt((
        // The `map` combinator runs a parser, then applies a function to the output
        // of that parser.
        map(parse_literal_intermediate_char, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))
    .parse(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
fn parse_string<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function – parses a single string fragment
        parse_fragment_str,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    delimited(char('"'), build_string, char('"')).parse(input)
}

fn parse_char_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, char, E> {
    let (input, _) = whitespace(input)?;
    // fold is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function – parses a single string fragment
        parse_fragment_char,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold), be sure that the
    // loop won't accidentally match your closing delimiter!
    let (input, result) = delimited(char('\''), cut(build_string), cut(char('\''))).parse(input)?;
    if result.len() != 1 {
        error!("Invalid char literal: {result}");
        return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
    }
    Ok((input, result.chars().next().unwrap()))
}

fn parse_string_literal<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, String, E> {
    // map(context(
    //   "string",
    //   alt((
    //         // preceded(char('\''), cut(terminated(parse_inner_str_single, char('\'')))),
    //         preceded(char('"'), cut(terminated(parse_inner_str_double, char('"')))),
    //   )),
    // ), |s| s.to_string())(input)

    if let Ok((input, s)) = parse_string::<VerboseError<&str>>(input) {
        Ok((input, s))
    } else {
        Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)))
    }
}

fn parse_const_bool<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, result) = alt((value(true, tag("True")), value(false, tag("False"))))(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, ConstExpr::Bool(result)))
}

fn parse_const_null<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("Null")(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, ConstExpr::Null))
}

fn parse_const_none<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    // First, try two parentheses
    let (input, _) = whitespace(input)?;
    if let Ok((input, _)) = delimited(tag("("), whitespace::<E>, tag(")"))(input) {
        return Ok((input, ConstExpr::None));
    }

    let (input, _) = tag("None")(input)?;

    // Peek and make sure the next character is not a symbol character
    if let Some(c) = input.chars().next() {
        if is_symbol_char(c) {
            return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Digit)));
        }
    }

    Ok((input, ConstExpr::None))
}

fn parse_const_sizeof_expr<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("sizeof")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, ConstExpr::SizeOfExpr(Box::new(expr))))
}

fn parse_const_sizeof_type<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
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

fn parse_const_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("(")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many1(terminated(parse_const, preceded(whitespace, tag(","))))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_const)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, ConstExpr::Tuple(exprs)))
}

fn parse_const_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, mut exprs) = many0(terminated(parse_const, preceded(whitespace, tag(","))))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, last_expr) = opt(parse_const)(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("]")(input)?;

    if let Some(last_expr) = last_expr {
        exprs.push(last_expr);
    }

    Ok((input, ConstExpr::Array(exprs)))
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn parse_symbol<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    let (input, _) = whitespace(input)?;
    // recognize(all_consuming(pair(
    //     verify(anychar, |&c| c.is_lowercase() || c == '_'),
    //     many0_count(preceded(opt(char('_')), alphanumeric1)),
    // )))(input)
    let (input, result) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    if KEYWORDS.contains(&result) {
        return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Tag)));
    }
    Ok((input, result))
}

fn parse_const_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstExpr, E> {
    let (input, _) = opt(tag("struct"))(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("{")(input)?;

    let (input, _) = whitespace(input)?;
    let (input, mut fields) = many0(terminated(
        alt((
            pair(
                parse_symbol,
                preceded(pair(whitespace, tag("=")), parse_const),
            ),
            map(parse_symbol, |x| (x, ConstExpr::Symbol(x.to_string()))),
        )),
        tag(","),
    ))(input)?;

    let (input, last) = opt(alt((
        pair(
            parse_symbol,
            preceded(pair(whitespace, tag("=")), parse_const),
        ),
        map(parse_symbol, |x| (x, ConstExpr::Symbol(x.to_string()))),
    )))(input)?;
    if let Some((k, v)) = last {
        fields.push((k, v));
    }

    let (input, _) = whitespace(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((
        input,
        ConstExpr::Struct(fields.into_iter().map(|(k, v)| (k.to_owned(), v)).collect()),
    ))
}

#[cfg(test)]
mod tests {
    fn compile_and_run(code: &str, input: &str) -> Result<String, String> {
        // let has_init = {*INIT.read().unwrap()};
        // if !has_init {
        //     let mut builder = env_logger::Builder::from_default_env();
        //     builder.format_timestamp(None);
        //     builder.filter(
        //         None,
        //         // LogLevel::Error if args.debug.is_none() => log::LevelFilter::Error,
        //         // LogLevel::Warn if args.debug.is_none() => log::LevelFilter::Warn,
        //         // LogLevel::Off if args.debug.is_none() => log::LevelFilter::Error,
        //         // LogLevel::Info if args.debug.is_none() => log::LevelFilter::Info,
        //         log::LevelFilter::Trace,
        //         // log::LevelFilter::Info,
        //     );
        //     builder.init();
        //     *INIT.write().unwrap() = true;
        // }
            
        let _ = rayon::ThreadPoolBuilder::new()
            .num_threads(16)
            .stack_size(512 * 1024 * 1024)
            .build_global();
        // Compiling most examples overflows the tiny stack for tests.
        // So, we spawn a new thread with a larger stack size.
        std::thread::scope(|s| {
            let child = std::thread::Builder::new()
                .stack_size(512 * 1024 * 1024)
                .spawn_scoped(s, move || {
                    use crate::vm::*;
                    use no_comment::{languages, IntoWithoutComments};
                    // Strip comments
                    let code = code
                        .to_string()
                        .chars()
                        .without_comments(languages::rust())
                        .collect::<String>();
                    let parsed = crate::frontend::parse(&code, Some("input"), true, true)?;
                    let asm_code = parsed.compile();
                    // let asm_code = parsed.compile();
                    const CALL_STACK_SIZE: usize = 1024;
                    if let Err(ref e) = asm_code {
                        if let crate::lir::Error::Annotated(ref err, ref metadata) = e {
                            if let Some(loc) = metadata.location().cloned() {
                                // use codespan_reporting::files::SimpleFiles;
                                use codespan_reporting::diagnostic::{Diagnostic, Label};
                                use codespan_reporting::files::SimpleFiles;
                                use codespan_reporting::term::{
                                    emit,
                                    termcolor::{ColorChoice, StandardStream},
                                };

                                let SourceCodeLocation {
                                    line,
                                    column,
                                    filename,
                                    offset,
                                    length,
                                } = loc;

                                let mut files = SimpleFiles::new();

                                let filename = filename.clone().unwrap_or("unknown".to_string());

                                let file_id = files.add(filename.clone(), &code);

                                let loc = format!("{}:{}:{}:{}", filename, line, column, offset);

                                let diagnostic = Diagnostic::error()
                                    .with_message(format!("Error at {}", loc))
                                    .with_labels(vec![Label::primary(
                                        file_id,
                                        offset..(offset + length.unwrap_or(0)),
                                    )
                                    .with_message(format!("{err}"))]);

                                let writer = StandardStream::stderr(ColorChoice::Always);
                                let config = codespan_reporting::term::Config::default();

                                emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();

                                return Err(format!("{e}"));
                            }
                        }
                        return Err(format!("{e}"));
                    }
                    let asm_code = asm_code.unwrap();

                    let vm_code = match asm_code {
                        Ok(core_asm_code) => core_asm_code.assemble(CALL_STACK_SIZE).map(Ok),
                        Err(std_asm_code) => std_asm_code.assemble(CALL_STACK_SIZE).map(Err),
                    }
                    .unwrap();

                    let device = match vm_code {
                        Ok(vm_code) => {
                            CoreInterpreter::new(TestingDevice::new(input)).run(&vm_code)?
                        }
                        // .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                        Err(vm_code) => {
                            StandardInterpreter::new(TestingDevice::new(input)).run(&vm_code)?
                        } // .unwrap_or_else(|_| panic!("Could not interpret code in `{path:?}`")),
                    };

                    let output_text = device.output_str();

                    Ok(output_text)
                })
                .unwrap();
            child.join().unwrap()
        })
    }

    use super::*;

    fn assert_parse_const(input: &str, expected: Option<ConstExpr>) {
        let result = parse_const::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result
            .map_err(|e| match e {
                nom::Err::Error(e) => nom::error::convert_error(input, e),
                nom::Err::Failure(e) => nom::error::convert_error(input, e),
                nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e),
            })
            .unwrap();
        trace!("{:?}", result);

        if let Some(expected) = expected {
            assert_eq!(result.1, expected);
        }
    }

    fn assert_parse_type(input: &str, expected: Option<Type>) {
        let result = parse_type::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result
            .map_err(|e| match e {
                nom::Err::Error(e) => nom::error::convert_error(input, e),
                nom::Err::Failure(e) => nom::error::convert_error(input, e),
                nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e),
            })
            .unwrap();
        trace!("{:?}", result);

        if let Some(expected) = expected {
            assert_eq!(result.1, expected);
        }
    }

    fn unassert_parse_const(input: &str) {
        let result = parse_const::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result.map_err(|e| match e {
            nom::Err::Error(e) => nom::error::convert_error(input, e),
            nom::Err::Failure(e) => nom::error::convert_error(input, e),
            nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e),
        });
        trace!("{:?}", result);
        assert!(result.is_err());
    }

    fn assert_parse_expr(input: &str, expr: Option<Expr>) {
        let result = parse_expr::<nom::error::VerboseError<&str>>(input);
        match result.clone() {
            Err(nom::Err::Error(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Failure(e)) => trace!("Error: {}", nom::error::convert_error(input, e)),
            Err(nom::Err::Incomplete(e)) => unreachable!("Incomplete: {:?}", e),
            _ => {}
        }
        let result = result
            .map_err(|e| match e {
                nom::Err::Error(e) => nom::error::convert_error(input, e),
                nom::Err::Failure(e) => nom::error::convert_error(input, e),
                nom::Err::Incomplete(_) => unreachable!("Incomplete: {:?}", e),
            })
            .unwrap();
        trace!("{:?}", result);

        if let Some(expr) = expr {
            assert_eq!(result.1, expr);
        }
    }

    #[test]
    fn test_parse_const() {
        assert_parse_const("1", Some(ConstExpr::Int(1)));

        assert_parse_const("1.0", Some(ConstExpr::Float(1.0)));
        assert_parse_const("'-'", Some(ConstExpr::Char('-')));

        assert_parse_const(
            "\"hello\"",
            Some(ConstExpr::Array(vec![
                ConstExpr::Char('h'),
                ConstExpr::Char('e'),
                ConstExpr::Char('l'),
                ConstExpr::Char('l'),
                ConstExpr::Char('o'),
                ConstExpr::Char('\0'),
            ])),
        );
        assert_parse_const(
            "\"hello\\n\"",
            Some(ConstExpr::Array(vec![
                ConstExpr::Char('h'),
                ConstExpr::Char('e'),
                ConstExpr::Char('l'),
                ConstExpr::Char('l'),
                ConstExpr::Char('o'),
                ConstExpr::Char('\n'),
                ConstExpr::Char('\0'),
            ])),
        );

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
            ConstExpr::Tuple(vec![
                ConstExpr::Struct(fields.clone()),
                ConstExpr::Struct(fields),
            ])
        };
        assert_parse_const("({a=1, b=2,}, {b=2, a=1})", Some(t.clone()));
        assert_parse_const("(1,)", Some(ConstExpr::Tuple(vec![ConstExpr::Int(1)])));

        let a = ConstExpr::Array(vec![
            ConstExpr::Int(1),
            ConstExpr::Int(2),
            ConstExpr::Int(3),
        ]);
        assert_parse_const("[1, 2, 3]", Some(a.clone()));

        assert_parse_const(
            "std.Option<Int>",
            Some(
                ConstExpr::Symbol("std".to_string())
                    .field(ConstExpr::Symbol("Option".to_string()))
                    .monomorphize(vec![Type::Int]),
            ),
        );
        assert_parse_const(
            "sizeof([1, 2, 3])",
            Some(ConstExpr::SizeOfExpr(Expr::from(a.clone()).into())),
        );
        assert_parse_const("sizeof<Int>()", Some(ConstExpr::SizeOfType(Type::Int)));
        unassert_parse_const("sizeof<Int>(5)");
        assert_parse_const(
            "Result<Int, String> of Ok(5)",
            Some(ConstExpr::EnumUnion(
                Type::Symbol("Result".to_string())
                    .apply(vec![Type::Int, Type::Symbol("String".to_string())]),
                "Ok".to_string(),
                Box::new(ConstExpr::Int(5)),
            )),
        );
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

        assert_parse_type(
            "&Int",
            Some(Type::Pointer(Mutability::Immutable, Box::new(Type::Int))),
        );
        assert_parse_type(
            "&mut Int",
            Some(Type::Pointer(Mutability::Mutable, Box::new(Type::Int))),
        );
        assert_parse_type(
            "& mut Int",
            Some(Type::Pointer(Mutability::Mutable, Box::new(Type::Int))),
        );
        assert_parse_type(
            "&mut (Int, Char)",
            Some(Type::Pointer(
                Mutability::Mutable,
                Box::new(Type::Tuple(vec![Type::Int, Type::Char])),
            )),
        );

        assert_parse_type(
            "[Int * 5]",
            Some(Type::Array(
                Box::new(Type::Int),
                Box::new(ConstExpr::Int(5)),
            )),
        );

        assert_parse_type(
            "enum {Ok Int, Err &Char}",
            Some(Type::EnumUnion(
                vec![
                    ("Ok".to_string(), Type::Int),
                    (
                        "Err".to_string(),
                        Type::Pointer(Mutability::Immutable, Box::new(Type::Char)),
                    ),
                ]
                .into_iter()
                .collect(),
            )),
        );

        assert_parse_type(
            "struct {a: Int, b: Char}",
            Some(Type::Struct(
                vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Char)]
                    .into_iter()
                    .collect(),
            )),
        );

        assert_parse_type(
            "enum {Ok {x: Int, y: Int}, Err &Char}",
            Some(Type::EnumUnion(
                vec![
                    (
                        "Ok".to_string(),
                        Type::Struct(
                            vec![("x".to_string(), Type::Int), ("y".to_string(), Type::Int)]
                                .into_iter()
                                .collect(),
                        ),
                    ),
                    (
                        "Err".to_string(),
                        Type::Pointer(Mutability::Immutable, Box::new(Type::Char)),
                    ),
                ]
                .into_iter()
                .collect(),
            )),
        );

        assert_parse_type(
            "Option<Int>",
            Some(Type::Apply(
                Box::new(Type::Symbol("Option".to_string())),
                vec![Type::Int],
            )),
        );
        assert_parse_type(
            "fun(Option<Int>) -> Bool",
            Some(Type::Proc(
                vec![Type::Apply(
                    Box::new(Type::Symbol("Option".to_string())),
                    vec![Type::Int],
                )],
                Box::new(Type::Bool),
            )),
        );
        assert_parse_type(
            "fun<T>(Option<T>) -> Bool",
            Some(Type::Poly(
                vec![("T".to_string(), None)],
                Type::Proc(
                    vec![Type::Apply(
                        Box::new(Type::Symbol("Option".to_string())),
                        vec![Type::Symbol("T".to_string())],
                    )],
                    Box::new(Type::Bool),
                )
                .into(),
            )),
        );

        assert_parse_type(
            "fun<T>(Option<T>, Int) -> Bool",
            Some(Type::Poly(
                vec![("T".to_string(), None)],
                Type::Proc(
                    vec![
                        Type::Apply(
                            Box::new(Type::Symbol("Option".to_string())),
                            vec![Type::Symbol("T".to_string())],
                        ),
                        Type::Int,
                    ],
                    Box::new(Type::Bool),
                )
                .into(),
            )),
        );
    }

    #[test]
    fn test_parse_operator_precedence() {
        // Basic addition and subtraction
        assert_parse_expr("a - b", Some(Expr::var("a").sub(Expr::var("b"))));
        assert_parse_expr(
            "a + b - c",
            Some(Expr::var("a").add(Expr::var("b")).sub(Expr::var("c"))),
        );

        // Multiplication and division with addition and subtraction
        assert_parse_expr(
            "a * b - c",
            Some(Expr::var("a").mul(Expr::var("b")).sub(Expr::var("c"))),
        );
        assert_parse_expr(
            "a / b + c",
            Some(Expr::var("a").div(Expr::var("b")).add(Expr::var("c"))),
        );
        assert_parse_expr(
            "a * b / c",
            Some(Expr::var("a").mul(Expr::var("b")).div(Expr::var("c"))),
        );

        // Mixed operations with parentheses
        assert_parse_expr(
            "a * (b + c)",
            Some(Expr::var("a").mul(Expr::var("b").add(Expr::var("c")))),
        );
        assert_parse_expr(
            "(a + b) * c",
            Some(Expr::var("a").add(Expr::var("b")).mul(Expr::var("c"))),
        );
        assert_parse_expr(
            "(a + b) * (c - d)",
            Some(
                Expr::var("a")
                    .add(Expr::var("b"))
                    .mul(Expr::var("c").sub(Expr::var("d"))),
            ),
        );

        // Multiple operators with parentheses
        assert_parse_expr(
            "a + (b * c) / d",
            Some(Expr::var("a").add(Expr::var("b").mul(Expr::var("c")).div(Expr::var("d")))),
        );
        assert_parse_expr(
            "a + b * (c - d) / e",
            Some(
                Expr::var("a").add(
                    Expr::var("b")
                        .mul(Expr::var("c").sub(Expr::var("d")))
                        .div(Expr::var("e")),
                ),
            ),
        );

        // Nested parentheses
        assert_parse_expr(
            "a + (b * (c + d))",
            Some(Expr::var("a").add(Expr::var("b").mul(Expr::var("c").add(Expr::var("d"))))),
        );
        assert_parse_expr(
            "((a + b) * c) - d",
            Some(
                Expr::var("a")
                    .add(Expr::var("b"))
                    .mul(Expr::var("c"))
                    .sub(Expr::var("d")),
            ),
        );
        assert_parse_expr(
            "((a + b) * c) - d < 0",
            Some(
                Expr::var("a")
                    .add(Expr::var("b"))
                    .mul(Expr::var("c"))
                    .sub(Expr::var("d"))
                    .lt(ConstExpr::Int(0)),
            ),
        );
        assert_parse_expr(
            "((a + b) * c) - d < 0 && 1 != 0",
            Some(
                Expr::var("a")
                    .add(Expr::var("b"))
                    .mul(Expr::var("c"))
                    .sub(Expr::var("d"))
                    .lt(ConstExpr::Int(0))
                    .and(Expr::from(ConstExpr::Int(1)).neq(ConstExpr::Int(0))),
            ),
        );
    }

    #[test]
    fn test_parse_module() {
        // Set logging level to debug
        // env_logger::builder().filter_level(log::LevelFilter::println).init();

        match compile_and_run(
            r#"
module std {
    module fs {
        fun open(path: &Char, mode: &Char): Int {
            println("Opening file");
            0
        }
    }

    fun println<T>(x: T) {
        println(fs.open(&"stdout", &"w"));
        test();
        print(x);
        print("\n");
    }

    struct Point {
        x: Int,
        y: Int,
    }

    impl Point {
        fun new(x: Int, y: Int): Point {
            return {x = x, y = y};
        }

        fun move(&mut self, dx: Int, dy: Int) {
            self.x += dx;
            self.y += dy;
        }
    }

    extern fun memcpy(dst: &mut Cell, src: &Cell, n: Int);

    fun test() {
        println("Hello, world!");

        let p = Point.new(5, 10);

        println(p.x);

        let mut p2 = p;
        p2.move(5, 5);

        println(p2.x);
    }
}

from std import println as p;
from std import Point;

p<Int>(5);
        "#,
            "hello!",
        ) {
            Ok(expr) => {
                // trace!("{:#?}", expr)
                // Compile and run
                trace!("{}", expr)
            }
            Err(e) => trace!("Error: {}", e),
        }
    }

    #[test]
    fn test_parse_expr() {
        match compile_and_run(
            r#"
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
            "#,
            "hello!",
        ) {
            Ok(expr) => {
                // trace!("{:#?}", expr)
                // Compile and run
                trace!("{}", expr)
            }
            Err(e) => trace!("Error: {}", e),
        }
        assert_parse_expr("1", Some(Expr::ConstExpr(ConstExpr::Int(1))));
        assert_parse_expr("1.0", Some(Expr::ConstExpr(ConstExpr::Float(1.0))));

        assert_parse_expr(
            "(1.0, {x, y})",
            Some(Expr::ConstExpr(ConstExpr::Tuple(vec![
                ConstExpr::Float(1.0),
                ConstExpr::Struct({
                    let mut fields = std::collections::BTreeMap::new();
                    fields.insert("x".to_string(), ConstExpr::Symbol("x".to_string()));
                    fields.insert("y".to_string(), ConstExpr::Symbol("y".to_string()));
                    fields
                }),
            ]))),
        );

        assert_parse_expr(
            "{a;b}",
            Some(Expr::Many(vec![
                Expr::ConstExpr(ConstExpr::Symbol("a".to_string())),
                Expr::ConstExpr(ConstExpr::Symbol("b".to_string())),
            ])),
        );
        assert_parse_expr(
            "{a}",
            Some(Expr::ConstExpr(ConstExpr::Struct({
                let mut fields = std::collections::BTreeMap::new();
                fields.insert("a".to_string(), ConstExpr::var("a"));
                fields
            }))),
        );

        // Now try calls
        assert_parse_expr("a()", Some(Expr::var("a").app(vec![])));
        assert_parse_expr("a(b)", Some(Expr::var("a").app(vec![Expr::var("b")])));
        assert_parse_expr(
            "a.x(b)",
            Some(
                Expr::var("a")
                    .field(ConstExpr::var("x"))
                    .app(vec![Expr::var("b")]),
            ),
        );
        assert_parse_expr(
            "std.println<Int>(5)",
            Some(
                ConstExpr::var("std")
                    .field(ConstExpr::var("println"))
                    .monomorphize(vec![Type::Int])
                    .app(vec![ConstExpr::Int(5).into()]),
            ),
        );
        assert_parse_expr("True", Some(ConstExpr::Bool(true).into()));
        assert_parse_expr(
            "!True || 1 + 2 >= 3",
            Some(
                Expr::from(ConstExpr::Bool(true))
                    .not()
                    .or(Expr::from(ConstExpr::Int(1))
                        .add(Expr::from(ConstExpr::Int(2)))
                        .ge(Expr::from(ConstExpr::Int(3)))),
            ),
        );

        assert_parse_expr(
            "*&a",
            Some(Expr::var("a").refer(Mutability::Immutable).deref()),
        );
        assert_parse_expr(
            "&*a",
            Some(Expr::var("a").deref().refer(Mutability::Immutable)),
        );
        assert_parse_expr(
            "&mut *a",
            Some(Expr::var("a").deref().refer(Mutability::Mutable)),
        );

        trace!("Parsing block");
    }
}
