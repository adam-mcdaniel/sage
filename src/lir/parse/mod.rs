use pest::{Parser, iterators::{Pair, Pairs}, error::Error};
use pest_derive::Parser;
use std::collections::BTreeMap;
use crate::lir::*;

#[derive(Parser)]
#[grammar = "lir/parse/lir.pest"] // relative to src
struct LIRParser;

#[derive(Clone, Debug)]
pub enum Statement {
    Match(Expr, Vec<(Pattern, Self)>),
    Let(String, Option<Type>, Expr),
    Assign(Expr, Option<Box<dyn AssignOp + 'static>>, Expr),
    If(Expr, Box<Self>, Option<Box<Self>>),
    IfLet(Pattern, Expr, Box<Self>, Option<Box<Self>>),
    While(Expr, Box<Self>),
    For(Box<Self>, Expr, Box<Self>, Box<Self>),
    Return(Expr),
    Block(Vec<Self>),
    With(Vec<(String, Option<Type>, Expr)>, Box<Self>),
    Expr(Expr)
}

impl Statement {
    fn to_expr(self, rest: Option<Expr>) -> Expr {
        let rest_expr = Box::new(rest.clone().unwrap_or(Expr::ConstExpr(ConstExpr::None)));
        // Expr::Block(vec![match self {
        //     Self::Match(e, arms) => {                
        //         Expr::Match(Box::new(e), arms.into_iter().map(|(a, b)| (a, b.to_expr(None))).collect())
        //     }
        // }, rest_expr])

        let stmt = match (self, rest.clone()) {
            (Self::Match(e, arms), _) => {
                Expr::Match(Box::new(e), arms.into_iter().map(|(a, b)| (a, b.to_expr(None))).collect())
            }
            (Self::Let(name, ty, val), _) => {
                return Expr::LetVar(name, ty, Box::new(val), rest_expr)
            }
            (Self::Assign(lhs, op, rhs), _) => {
                match op {
                    Some(op) => lhs.refer().assign(op, rhs),
                    // Some(op) => Expr::AssignOp(lhs, op, Box::new(rhs), rest_expr),
                    None => lhs.refer().deref_mut(rhs)
                }
            }
            (Self::If(cond, body, None), _) => {
                Expr::If(Box::new(cond), Box::new(body.to_expr(None)), Box::new(Expr::ConstExpr(ConstExpr::None)))
            }
            (Self::If(cond, body, Some(else_body)), _) => {
                Expr::If(Box::new(cond), Box::new(body.to_expr(None)), Box::new(else_body.to_expr(None)))
            }
            (Self::IfLet(pat, cond, body, None), _) => {
                Expr::IfLet(pat, Box::new(cond), Box::new(body.to_expr(None)), Box::new(Expr::ConstExpr(ConstExpr::None)))
            }
            (Self::IfLet(pat, cond, body, Some(else_body)), _) => {
                Expr::IfLet(pat, Box::new(cond), Box::new(body.to_expr(None)), Box::new(else_body.to_expr(None)))
            }
            (Self::While(cond, body), _) => {
                Expr::While(Box::new(cond), Box::new(body.to_expr(None)))
            }
            (Self::For(init, cond, step, body), _) => {
                init.to_expr(Some(Expr::While(Box::new(cond), Box::new(body.to_expr(Some(step.to_expr(None)))))))
            }
            (Self::Return(val), _) => {
                Expr::Return(Box::new(val))
            }

            (Self::Block(stmts), Some(Expr::Many(mut rest))) => {
                rest.insert(0, Expr::Many(stmts.into_iter().map(|s| s.to_expr(None)).collect()));
                return Expr::Many(rest)
            }
            (Self::Block(stmts), Some(inner)) => {
                let mut result = Some(inner);
                for stmt in stmts.into_iter().rev() {
                    result = Some(stmt.to_expr(result));
                }
                result.unwrap_or(Expr::ConstExpr(ConstExpr::None))
                // Expr::Many(stmts.into_iter().map(|s| s.to_expr(None)).collect())
            }
            (Self::Block(stmts), None) => {
                let mut result = None;
                for stmt in stmts.into_iter().rev() {
                    result = Some(stmt.to_expr(result));
                }
                result.unwrap_or(Expr::ConstExpr(ConstExpr::None))
                // return Expr::Many(stmts.into_iter().map(|s| s.to_expr(None)).collect())
            }
            (Self::With(defs, body), Some(Expr::LetVars(mut vars, mut ret))) => {
                // Expr::LetVars(defs, Box::new(body.to_expr(None)))
                for (name, ty, val) in defs.into_iter().rev() {
                    vars.insert(0, (name, ty, val));
                }
                return Expr::LetVars(vars, Box::new(Expr::Many(vec![body.to_expr(Some(*ret))])))
            }
            (Self::With(defs, body), _) => {
                Expr::LetVars(defs, Box::new(body.to_expr(None)))
            }
            (Self::Expr(e), _) => {
                e
            }
        };
        if let Some(Expr::Many(mut stmts)) = rest {
            stmts.insert(0, stmt);
            Expr::Many(stmts)
        } else if let Some(_) = rest {
            Expr::Many(vec![stmt, *rest_expr])
        } else {
            stmt
        }
    }
}

#[derive(Clone, Debug)]
pub enum Declaration {
    Struct(String, Vec<(String, Type)>),
    Enum(String, Vec<(String, Option<Type>)>),
    Const(Vec<(String, ConstExpr)>),
    Proc(String, Vec<(String, Type)>, Option<Type>, Box<Statement>),
    Type(Vec<(String, Type)>),
    Statement(Statement)
}

impl Declaration {
    fn proc_to_expr(name: String, args: Vec<(String, Type)>, ret: Option<Type>, body: Statement) -> Procedure {
        let mut params = Vec::new();
        for (name, ty) in args.into_iter().rev() {
            params.push((name, ty));
        }
        Procedure::new(params, ret.unwrap_or(Type::None), body.to_expr(None))
    }

    fn to_expr(self, rest: Option<Expr>) -> Expr {
        let rest_expr = Box::new(rest.clone().unwrap_or(Expr::ConstExpr(ConstExpr::None)));
        match (self, rest) {
            (Self::Struct(name, fields), Some(Expr::LetTypes(mut types, ret))) => {
                types.push((name, Type::Struct(fields.into_iter().collect())));
                Expr::LetTypes(types, ret)
            }
            (Self::Struct(name, fields), _) => {
                Expr::LetTypes(vec![(name, Type::Struct(fields.into_iter().collect()))], rest_expr)
            }
            (Self::Enum(name, variants), Some(Expr::LetTypes(mut types, ret))) => {
                let mut simple = true;
                for (_, value) in variants.iter() {
                    if value.is_some() {
                        simple = false;
                        break;
                    }
                }
                if simple {
                    // If we're using a simple enum, then we can just use a simple enum
                    types.push((name, Type::Enum(variants.into_iter().map(|(a, b)| a).collect())));
                } else {
                    // Otherwise, we need to use a tagged union
                    types.push((name, Type::EnumUnion(variants.into_iter().map(|(a, b)| (a, match b {
                        Some(x) => x,
                        None => Type::None
                    })).collect())));
                }
                
                Expr::LetTypes(types, ret)
            }
            (Self::Enum(name, variants), _) => {
                // If none of the variants have a value, then we can just use a simple enum
                // Otherwise, we need to use a tagged union
                let mut simple = true;
                for (_, value) in variants.iter() {
                    if value.is_some() {
                        simple = false;
                        break;
                    }
                }
                if simple {
                    // If we're using a simple enum, then we can just use a simple enum
                    Expr::LetTypes(vec![(name, Type::Enum(variants.into_iter().map(|(a, b)| a).collect()))], rest_expr)
                } else {
                    // Otherwise, we need to use a tagged union
                    Expr::LetTypes(vec![(name, Type::EnumUnion(variants.into_iter().map(|(a, b)| (a, match b {
                        Some(x) => x,
                        None => Type::None
                    })).collect()))], rest_expr)
                }
            }
            (Self::Const(mut consts), Some(Expr::LetConsts(mut defs, ret))) => {
                for (name, value) in consts.drain(..).rev() {
                    defs.insert(name, value);
                }
                Expr::LetConsts(defs, ret)
            }
            (Self::Const(consts), _) => {
                let mut defs = BTreeMap::new();
                for (name, value) in consts.into_iter().rev() {
                    defs.insert(name, value);
                }
                Expr::LetConsts(defs, rest_expr)
            }
            (Self::Proc(name, params, ret, stmt), Some(Expr::LetProcs(mut procs, ret2))) => {
                procs.push((name.clone(), Self::proc_to_expr(name, params, ret, *stmt)));
                Expr::LetProcs(procs, ret2)
            }
            (Self::Proc(name, params, ret, stmt), Some(Expr::LetConsts(mut defs, ret2))) => {
                defs.insert(name.clone(), ConstExpr::Proc(Self::proc_to_expr(name, params, ret, *stmt)));
                Expr::LetConsts(defs, ret2)
            }
            (Self::Proc(name, params, ret, stmt), _) => {
                let mut defs = BTreeMap::new();
                defs.insert(name.clone(), ConstExpr::Proc(Self::proc_to_expr(name, params, ret, *stmt)));
                Expr::LetConsts(defs, rest_expr)
            }
            (Self::Type(mut types), Some(Expr::LetTypes(mut types2, ret))) => {
                types.append(&mut types2);
                Expr::LetTypes(types, ret)
            }
            (Self::Type(types), _) => {
                Expr::LetTypes(types, rest_expr)
            }
            (Self::Statement(stmt), Some(rest)) => stmt.to_expr(Some(rest)),
            (Self::Statement(stmt), None) => stmt.to_expr(None),
            
        }
    }
}


#[derive(Clone, Debug)]
pub struct Program(Vec<Declaration>);

impl Program {
    fn to_expr(self) -> Expr {
        let mut rest = None;
        eprintln!("Program: {:?}", self.0);

        for expr in self.0.into_iter().rev() {
            rest = Some(expr.to_expr(rest))
        }
        if let Some(rest) = rest {
            rest
        } else {
            Expr::ConstExpr(ConstExpr::None)
        }
    }
}

pub fn parse_lir_file(file: &str) -> Result<Expr, Error<Rule>> {
    // ...
    // fn parse_expr(pair: Pair<Rule>) -> Expr {
    //     match pair.as_rule() {
    //         Rule::expr => pair.into_inner().map(parse_expr).next().unwrap(),
    //         Rule::expr_logic_factor => {

    //         }
    //     }
    // }
    let x = LIRParser::parse(Rule::program, file);
    // match x.clone() {
    //     Ok(x) => print(&x),
    //     Err(x) => println!("{}", x)
    // }
    Ok(parse_program(x.unwrap().into_iter().next().unwrap()).to_expr())
}

fn parse_program(pair: Pair<Rule>) -> Program {
    Program(pair.into_inner().map(parse_decl).collect())
}

fn parse_decl(pair: Pair<Rule>) -> Declaration {
    match pair.as_rule() {
        Rule::decl | Rule::decl_proc => pair.into_inner().map(parse_decl).next().unwrap(),
        Rule::decl_proc_block | Rule::decl_proc_expr => {
            let mut inner_rules = pair.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();
            let mut params = Vec::new();
            let mut ret = None;
            let mut stmt = Statement::Block(vec![]);
            for pair in inner_rules {
                match pair.as_rule() {
                    Rule::decl_proc_param => {
                        let mut inner_rules = pair.into_inner();
                        let name = inner_rules.next().unwrap().as_str().to_string();
                        let ty = parse_type(inner_rules.next().unwrap());
                        params.push((name, ty));
                    }
                    Rule::r#type => {
                        ret = Some(parse_type(pair));
                    }
                    Rule::stmt_block => {
                        stmt = parse_stmt(pair);
                    }
                    Rule::expr => {
                        stmt = Statement::Expr(parse_expr(pair));
                    }
                    _ => unreachable!()
                }
            }

            Declaration::Proc(name, params, ret, Box::new(stmt))
        }
        Rule::decl_type => {
            let mut inner_rules = pair.into_inner();
            let mut types = Vec::new();
            while inner_rules.peek().is_some() {
                let name = inner_rules.next().unwrap().as_str().to_string();
                let ty = parse_type(inner_rules.next().unwrap());
                types.push((name, ty));
            }

            Declaration::Type(types)
        }
        Rule::decl_unit => {
            let mut inner_rules = pair.into_inner();
            let mut types = Vec::new();
            while inner_rules.peek().is_some() {
                let name = inner_rules.next().unwrap().as_str().to_string();
                let ty = parse_type(inner_rules.next().unwrap());
                types.push((name.clone(), Type::Unit(name, Box::new(ty))));
            }

            Declaration::Type(types)
        }
        Rule::decl_struct => {
            let mut inner_rules = pair.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();
            let mut fields = Vec::new();
            while inner_rules.peek().is_some() {
                let mut inner_rules = inner_rules.next().unwrap().into_inner();
                let name = inner_rules.next().unwrap().as_str().to_string();
                let ty = parse_type(inner_rules.next().unwrap());
                fields.push((name, ty));
            }
            Declaration::Struct(name, fields)
        }
        Rule::decl_enum => {
            let mut inner_rules = pair.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();
            let mut variants = Vec::new();
            for pair in inner_rules {
                let mut inner_rules = pair.into_inner();
                let variant_name = inner_rules.next().unwrap();
                if variant_name.as_rule() != Rule::symbol { continue }
                let variant_name = variant_name.as_str().to_string();
                
                let ty = inner_rules.next();

                if let Some(ty) = ty {
                    if ty.as_rule() == Rule::r#type {
                        variants.push((variant_name.as_str().to_string(), Some(parse_type(ty))));
                    } else {
                        variants.push((variant_name.as_str().to_string(), None));
                    }
                } else {
                    variants.push((variant_name.as_str().to_string(), None));
                }
            }
            Declaration::Enum(name, variants)
        }
        Rule::decl_const => {
            let mut inner_rules = pair.into_inner();
            let mut defs = Vec::new();
            while inner_rules.peek().is_some() {
                let name = inner_rules.next().unwrap().as_str().to_string();
                let expr = parse_const(inner_rules.next().unwrap());
                defs.push((name, expr));
            }
            Declaration::Const(defs)
        }
        Rule::stmt => Declaration::Statement(parse_stmt(pair)),
        Rule::EOI => Declaration::Statement(Statement::Block(vec![])),
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair)
    }
}

fn parse_stmt(pair: Pair<Rule>) -> Statement {
    match pair.as_rule() {
        Rule::stmt
        | Rule::long_stmt
        | Rule::short_stmt 
        | Rule::stmt_with => pair.into_inner().map(parse_stmt).next().unwrap(),

        Rule::stmt_match => Statement::Expr(parse_match(pair)),
        Rule::stmt_let_typed
        | Rule::stmt_let_untyped
        | Rule::stmt_let => parse_let(pair),

        Rule::stmt_block => {
            let mut inner_rules = pair.into_inner();
            let mut stmts = Vec::new();
            while let Some(stmt) = inner_rules.next() {
                stmts.push(parse_stmt(stmt));
            }
            Statement::Block(stmts)
        }

        Rule::stmt_if => {
            let mut inner_rules = pair.into_inner();
            let cond = parse_expr(inner_rules.next().unwrap());
            let body = parse_stmt(inner_rules.next().unwrap());
            let else_body = inner_rules.next().map(|x| Box::new(parse_stmt(x)));
            Statement::If(cond, Box::new(body), else_body)
        }

        Rule::stmt_if_elif => {
            let mut inner_rules = pair.into_inner();
            let mut elifs = vec![];
            for _ in 0..inner_rules.clone().count() / 2 {
                let cond = inner_rules.next().unwrap();
                let body = inner_rules.next().unwrap();
                elifs.push((parse_expr(cond), parse_stmt(body)));
            }

            let mut else_body = inner_rules.next().map(|x| parse_stmt(x)).unwrap_or(Statement::Block(vec![]));

            for (cond, body) in elifs.into_iter().rev() {
                else_body = Statement::If(cond, Box::new(body), Some(Box::new(else_body)));
            }

            else_body
        }

        Rule::stmt_if_let => {
            let mut inner_rules = pair.into_inner();
            let pat = parse_pattern(inner_rules.next().unwrap());
            let expr = parse_expr(inner_rules.next().unwrap());
            let body = parse_stmt(inner_rules.next().unwrap());
            let else_body = inner_rules.next().map(|x| Box::new(parse_stmt(x)));
            Statement::IfLet(pat, expr, Box::new(body), else_body)
        }
        Rule::stmt_if_elif_let => {
            let mut inner_rules = pair.into_inner();
            let mut elifs = vec![];
            while inner_rules.clone().count() > 3 {
                let pat = inner_rules.next().unwrap();
                let expr = inner_rules.next().unwrap();
                let body = inner_rules.next().unwrap();
                elifs.push((parse_pattern(pat), parse_expr(expr), parse_stmt(body)));
            }

            let mut else_body = inner_rules.next().map(|x| parse_stmt(x)).unwrap_or(Statement::Block(vec![]));

            for (pat, expr, body) in elifs.into_iter().rev() {
                else_body = Statement::IfLet(pat, expr, Box::new(body), Some(Box::new(else_body)));
            }

            else_body
        }

        Rule::stmt_while => {
            let mut inner_rules = pair.into_inner();
            let cond = parse_expr(inner_rules.next().unwrap());
            let body = parse_stmt(inner_rules.next().unwrap());
            Statement::While(cond, Box::new(body))
        }

        Rule::stmt_for => {
            let mut inner_rules = pair.into_inner();
            let pre = parse_stmt(inner_rules.next().unwrap());
            let cond = parse_expr(inner_rules.next().unwrap());
            let post = parse_stmt(inner_rules.next().unwrap());
            let body = parse_stmt(inner_rules.next().unwrap());
            Statement::For(Box::new(pre), cond, Box::new(post), Box::new(body))
        }

        Rule::stmt_with_block | Rule::stmt_with_expr => {
            let mut inner_rules = pair.into_inner();
            let mut defs = vec![];
            for _ in 0..inner_rules.clone().count() / 2 {
                let symbol = inner_rules.next().unwrap().as_str().to_string();
                let ty = inner_rules.next().unwrap();
                if ty.as_rule() == Rule::expr {
                    defs.push((symbol, None, parse_expr(ty)));
                    continue;
                }
                if let Some(expr) = inner_rules.next() {
                    defs.push((symbol, Some(parse_type(ty)), parse_expr(expr)));
                } else {
                    defs.push((symbol, None, parse_expr(ty)));
                }
            }
            let last = inner_rules.next().unwrap();
            match last.as_rule() {
                Rule::stmt_block => Statement::With(defs, Box::new(parse_stmt(last))),
                Rule::expr => Statement::With(defs, Box::new(Statement::Expr(parse_expr(last)))),
                _ => unreachable!()
            }
        }

        Rule::stmt_assign => {
            let mut inner_rules = pair.into_inner();
            let lhs = parse_expr(inner_rules.next().unwrap());
            let op = inner_rules.next().unwrap().as_str();
            let rhs = parse_expr(inner_rules.next().unwrap());
            Statement::Assign(lhs, match op {
                "=" => None,
                "+=" => Some(Box::new(Assign::new(Arithmetic::Add))),
                "-=" => Some(Box::new(Assign::new(Arithmetic::Subtract))),
                "*=" => Some(Box::new(Assign::new(Arithmetic::Multiply))),
                "/=" => Some(Box::new(Assign::new(Arithmetic::Divide))),
                "%=" => Some(Box::new(Assign::new(Arithmetic::Remainder))),
                _ => unreachable!()
            }, rhs)
        }

        Rule::expr => Statement::Expr(parse_expr(pair)),

        other => panic!("Unexpected rule: {:?}: {:?}", other, pair)
    }
}

fn parse_let(pair: Pair<Rule>) -> Statement {
    match pair.as_rule() {
        Rule::stmt_let => {
            let mut inner_rules = pair.into_inner();
            parse_let(inner_rules.next().unwrap())
        }
        Rule::stmt_let_typed => {
            let mut inner_rules = pair.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();
            let ty = inner_rules.next().map(parse_type);
            let expr = parse_expr(inner_rules.next().unwrap());
            Statement::Let(name, ty, expr)
        }
        Rule::stmt_let_untyped => {
            let mut inner_rules = pair.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();
            let expr = parse_expr(inner_rules.next().unwrap());
            Statement::Let(name, None, expr)
        }
        _ => unreachable!()
    }
}

pub fn parse_expr(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::expr
        | Rule::expr_atom
        | Rule::expr_group => pair.into_inner().map(parse_expr).next().unwrap(),
        Rule::stmt_match => parse_match(pair),
        Rule::expr_logic_factor
        | Rule::expr_logic_term
        | Rule::expr_comparison
        | Rule::expr_sum
        | Rule::expr_factor => parse_binop(pair),

        Rule::expr_variant => {
            let mut inner_rules = pair.into_inner();
            let ty = parse_type(inner_rules.next().unwrap());
            let variant = inner_rules.next().unwrap().as_str();
            if let Some(expr) = inner_rules.next() {
                Expr::EnumUnion(ty, variant.to_string(), Box::new(parse_expr(expr)))
            } else {
                Expr::ConstExpr(ConstExpr::Of(ty, variant.to_string()))
            }
        }
        Rule::expr_term => parse_expr_term(pair),
        Rule::expr_tuple => {
            let inner_rules = pair.into_inner();
            let mut result = vec![];
            for x in inner_rules {
                result.push(parse_expr(x));
            }
            Expr::Tuple(result)
        }
        Rule::expr_array => {
            let inner_rules = pair.into_inner();
            let mut result = vec![];
            for x in inner_rules {
                result.push(parse_expr(x));
            }
            Expr::Array(result)
        }
        Rule::expr_struct => {
            let mut inner_rules = pair.into_inner();
            let mut result = vec![];
            while inner_rules.peek().is_some() {
                let field = inner_rules.next().unwrap().as_str().to_string();
                let val = parse_expr(inner_rules.next().unwrap());
                result.push((field, val));
            }
            Expr::Struct(result.into_iter().collect())
        }

        Rule::expr_unary => {
            // eprintln!("Unary: {:?}\n", pair);

            let inner_rules = pair.into_inner();
            let mut result = Expr::ConstExpr(ConstExpr::None);
            for x in inner_rules.rev() {
                result = match x.as_rule() {
                    Rule::expr_unary_op => {
                        match x.as_str() {
                            "!" => result.not(),
                            "-" => result.neg(),
                            "&" => result.refer(),
                            "new" => result.unop(New),
                            "*" => result.deref(),
                            _ => panic!("Unexpected unary op: {}", x.as_str())
                        }
                    },
                    _ => parse_expr(x)
                }
            }
            result
        }

        Rule::const_atom => Expr::ConstExpr(parse_const(pair)),
        Rule::stmt_block => parse_stmt(pair).to_expr(None),
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair)
    }
}

fn parse_expr_term(pair: Pair<Rule>) -> Expr {
    let mut inner_rules = pair.into_inner();
    let mut head = parse_expr(inner_rules.next().unwrap());
    for suffix in inner_rules {
        head = match suffix.as_rule() {
            Rule::expr_int_field => {
                head.field(ConstExpr::Int(suffix.as_str().parse().unwrap()))
            }
            Rule::expr_symbol_field => {
                head.field(ConstExpr::Symbol(suffix.as_str().to_string()))
            }
            Rule::expr_int_deref_field => {
                head.deref().field(ConstExpr::Int(suffix.as_str().parse().unwrap()))
            }
            Rule::expr_symbol_deref_field => {
                head.deref().field(ConstExpr::Symbol(suffix.as_str().to_string()))
            }
            Rule::expr_index => {
                head.idx(parse_expr(suffix))
            }
            Rule::expr_call => {
                let inner_rules = suffix.into_inner();
                let mut args = Vec::new();
                for arg in inner_rules {
                    args.push(parse_expr(arg));
                }
                if head == Expr::ConstExpr(ConstExpr::Symbol("print".to_string())) {
                    let mut exprs: Vec<Expr> = args.into_iter().map(|val| val.unop(Put::Display)).collect();
                    exprs.push(Expr::ConstExpr(ConstExpr::None));
                    Expr::Many(exprs)
                } else {
                    head.app(args)
                }
            }
            Rule::expr_as_type => {
                head.as_type(parse_type(suffix))
            }
            _ => unreachable!()
        }
    }
    head
}

fn parse_binop(pair: Pair<Rule>) -> Expr {
    let mut inner_rules = pair.into_inner().peekable();
    let mut head = parse_expr(inner_rules.next().unwrap());
    // let count = inner_rules.clone().count() / 2;
    for pair in inner_rules {
        let mut inner_rules = pair.clone().into_inner();
        let next_pair = inner_rules.next().unwrap();
        let op = pair.as_str()[..pair.as_str().len() - next_pair.as_str().len()].trim();
        let tail = parse_expr(next_pair);
        head = match op {
            "&&" => head.and(tail),
            "||" => head.or(tail),
            "+" => head.add(tail),
            "-" => head.sub(tail),
            "*" => head.mul(tail),
            "/" => head.div(tail),
            "%" => head.rem(tail),
            "==" => head.eq(tail),
            "!=" => head.neq(tail),
            "<" => head.lt(tail),
            "<=" => head.le(tail),
            ">" => head.gt(tail),
            ">=" => head.ge(tail),
            _ => unreachable!()
        };
    }
    head
}


fn parse_const(pair: Pair<Rule>) -> ConstExpr {
    match pair.as_rule() {
        Rule::r#const
        | Rule::const_term
        | Rule::const_atom
        | Rule::const_group => pair.into_inner().map(parse_const).next().unwrap(),
        Rule::const_tuple => {
            let inner_rules = pair.into_inner();
            let mut exprs = Vec::new();
            for pair in inner_rules {
                exprs.push(parse_const(pair));
            }
            ConstExpr::Tuple(exprs)
        }
        Rule::const_array => {
            let inner_rules = pair.into_inner();
            let mut exprs = Vec::new();
            for pair in inner_rules {
                exprs.push(parse_const(pair));
            }
            ConstExpr::Array(exprs)
        }
        Rule::const_struct => {
            let inner_rules = pair.into_inner();
            let mut fields = Vec::new();
            for pair in inner_rules {
                let mut inner_rules = pair.into_inner();
                if inner_rules.peek().is_none() {
                    break;
                }
                let name = inner_rules.next().unwrap().as_str().to_string();
                let expr = parse_const(inner_rules.next().unwrap());
                fields.push((name, expr));
            }
            ConstExpr::Struct(fields.into_iter().collect())
        }
        Rule::const_variant => {
            let mut inner_rules = pair.into_inner();
            let ty = parse_type(inner_rules.next().unwrap());
            let symbol = inner_rules.next().unwrap().as_str().to_string();
            if let Some(inner_rules) = inner_rules.next() {
                let expr = parse_const(inner_rules);
                // ConstExpr::Variant(ty, symbol, Some(Box::new(expr)))
                ConstExpr::EnumUnion(ty, symbol, Box::new(expr))
            } else {
                ConstExpr::Of(ty, symbol)
            }
        }
        Rule::const_symbol => ConstExpr::Symbol(pair.as_str().to_string()),
        Rule::const_int => ConstExpr::Int(pair.as_str().parse().unwrap()),
        Rule::const_float => ConstExpr::Float(pair.as_str().parse().unwrap()),
        Rule::const_char => {
            let token = pair.into_inner().next().unwrap().as_str();
            let token = &token[1..token.len() - 1];
            let result = snailquote::unescape(&format!("\"{token}\"")).unwrap();
            let ch = result.chars().next().unwrap();
            ConstExpr::Char(ch)
        },
        Rule::const_bool => ConstExpr::Bool(pair.as_str().parse().unwrap()),
        Rule::const_string => ConstExpr::Array(snailquote::unescape(pair.into_inner().next().unwrap().as_str()).unwrap().chars().map(|ch| ConstExpr::Char(ch)).collect()),
        Rule::const_none => ConstExpr::None,
        Rule::const_null => ConstExpr::Null,
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair)
    }
}

fn parse_type(pair: Pair<Rule>) -> Type {
    // todo!()
    match pair.as_rule() {
        Rule::r#type
        | Rule::type_atom => pair.into_inner().map(parse_type).next().unwrap(),

        Rule::type_let => {
            let mut inner_rules = pair.into_inner();
            // Get all but the last rule
            let mut result = vec![];
            for _ in 0..inner_rules.clone().count() - 1 {
                let name = inner_rules.next().unwrap().as_str().to_string();
                let ty = parse_type(inner_rules.next().unwrap());
                result.push((name, ty));
            }
            let mut ty = parse_type(inner_rules.next().unwrap());
            for (name, var) in result.into_iter().rev() {
                ty = Type::Let(name, Box::new(var), Box::new(ty));
            }
            ty
        }

        Rule::type_symbol => Type::Symbol(pair.as_str().to_string()),
        Rule::type_int => Type::Int,
        Rule::type_float => Type::Float,
        Rule::type_bool => Type::Bool,
        Rule::type_char => Type::Char,
        Rule::type_none => Type::None,

        Rule::type_tuple => {
            let inner_rules = pair.into_inner();
            let mut tys = Vec::new();
            for pair in inner_rules {
                tys.push(parse_type(pair));
            }
            Type::Tuple(tys)
        }
        Rule::type_array => {
            let mut inner_rules = pair.into_inner();
            let ty = parse_type(inner_rules.next().unwrap());
            let len = parse_const(inner_rules.next().unwrap());
            Type::Array(Box::new(ty), Box::new(len))
        }
        Rule::type_struct => {
            let mut inner_rules = pair.into_inner();
            let mut fields = Vec::new();
            while inner_rules.peek().is_some() {
                let name = inner_rules.next().unwrap().as_str().to_string();
                let ty = parse_type(inner_rules.next().unwrap());
                fields.push((name, ty));
            }
            Type::Struct(fields.into_iter().collect())
        }
        Rule::type_ptr => {
            let mut inner_rules = pair.into_inner();
            let ty = parse_type(inner_rules.next().unwrap());
            Type::Pointer(Box::new(ty))
        }
        
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair)
    }
}

fn parse_match(pair: Pair<Rule>) -> Expr {
    let mut inner_rules = pair.into_inner();
    let expr = parse_expr(inner_rules.next().unwrap());
    let mut patterns = Vec::new();
    let mut stmts = Vec::new();
    for pair in inner_rules {
        let mut inner_rules = pair.into_inner();
        let pattern = parse_pattern(inner_rules.next().unwrap());
        let stmt = parse_expr(inner_rules.next().unwrap());
        patterns.push(pattern);
        stmts.push(stmt);
    }
    Expr::Match(Box::new(expr), patterns.into_iter().zip(stmts.into_iter()).collect())
}

fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    match pair.as_rule() {
        Rule::pattern
        | Rule::pattern_term
        | Rule::pattern_atom
        | Rule::pattern_group => pair.into_inner().map(parse_pattern).next().unwrap(),
        Rule::pattern_const => Pattern::ConstExpr(parse_const(pair.into_inner().next().unwrap())),
        Rule::pattern_variant => {
            let mut inner_rules = pair.into_inner();
            let symbol = inner_rules.next().unwrap().as_str().to_string();
            let pattern = inner_rules.next().map(parse_pattern);
            Pattern::Variant(symbol, pattern.map(Box::new))
        },
        Rule::pattern_tuple => {
            let mut inner_rules = pair.into_inner();
            let mut patterns = Vec::new();
            for pair in inner_rules {
                let pattern = parse_pattern(pair);
                patterns.push(pattern);
            }
            Pattern::Tuple(patterns)
        },
        Rule::pattern_struct => {
            let mut inner_rules = pair.into_inner();
            let mut fields = Vec::new();
            for pair in inner_rules {
                let mut inner_rules = pair.into_inner();
                let symbol = inner_rules.next().unwrap().as_str().to_string();
                if inner_rules.peek().is_none() {
                    fields.push((symbol.clone(), Pattern::Symbol(symbol)));
                    continue;
                }
                // let pattern = parse_pattern(inner_rules.next().unwrap());
                let pattern = inner_rules.next().map(parse_pattern).unwrap();
                fields.push((symbol, pattern));
            }
            Pattern::Struct(fields.into_iter().collect())
        },
        // Rule::pattern_ptr => {
        //     let mut inner_rules = pair.into_inner();
        //     let pattern = parse_pattern(inner_rules.next().unwrap());
        //     Pattern::Pointer(Box::new(pattern))
        // }
        Rule::pattern_wildcard => Pattern::Wildcard,
        Rule::pattern_symbol => Pattern::Symbol(pair.as_str().to_string()),
        Rule::pattern_alt => {
            let inner_rules = pair.into_inner();
            let mut patterns = Vec::new();
            for pair in inner_rules {
                let pattern = parse_pattern(pair);
                patterns.push(pattern);
            }
            Pattern::Alt(patterns)
        },
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair)
    }
    // pattern = { pattern_alt | pattern_term }
    // pattern_alt = { pattern_term ~ ("|" ~ pattern_term)+ }
    // pattern_term = {
    //     pattern_variant
    //     | pattern_atom
    // }
    // pattern_variant = { "of" ~ symbol ~ pattern? }
    // pattern_atom = {
    //     pattern_group
    //     | pattern_tuple
    //     | pattern_struct
    //     | pattern_ptr
    //     | pattern_wildcard
    //     | pattern_symbol
    //     | pattern_const
    // }
    // pattern_group = { "(" ~ pattern ~ ")" }
    // pattern_tuple = { "(" ~ (pattern ~ ",")+ ~ pattern? ~ ")" }
    // pattern_struct = { "struct"? ~ "{" ~ (pattern_field ~ ",")* ~ pattern_field? ~ "}" }
    // pattern_field = { symbol ~ "=" ~ pattern }
    // pattern_symbol = { symbol }
    // pattern_wildcard = { "_" }
    // pattern_ptr = { "&" ~ pattern }
    // pattern_const = { const_atom }   
}

pub fn print(pairs: &Pairs<Rule>) {
    // ...
    let pairs = pairs.clone();
    for pair in pairs.into_iter() {
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        // println!("Tokens:    {:?}", pair.clone.tokens());
        print(&pair.into_inner());
    }
}

// fn parse_lir_file(file: &str) -> Result<JSONValue, Error<Rule>> {
//     // ...
//     use pest::iterators::Pair;
//     fn parse_value(pair: Pair<Rule>) -> JSONValue {
//         match pair.as_rule() {
//             Rule::object => JSONValue::Object(
//                 pair.into_inner()
//                     .map(|pair| {
//                         let mut inner_rules = pair.into_inner();
//                         let name = inner_rules
//                             .next()
//                             .unwrap()
//                             .into_inner()
//                             .next()
//                             .unwrap()
//                             .as_str();
//                         let value = parse_value(inner_rules.next().unwrap());
//                         (name, value)
//                     })
//                     .collect(),
//             ),
//             Rule::array => JSONValue::Array(pair.into_inner().map(parse_value).collect()),
//             Rule::string => JSONValue::String(pair.into_inner().next().unwrap().as_str()),
//             Rule::number => JSONValue::Number(pair.as_str().parse().unwrap()),
//             Rule::boolean => JSONValue::Boolean(pair.as_str().parse().unwrap()),
//             Rule::null => JSONValue::Null,
//             Rule::json
//             | Rule::EOI
//             | Rule::pair
//             | Rule::value
//             | Rule::inner
//             | Rule::char
//             | Rule::WHITESPACE => unreachable!(),
//         }
//     }
//     // ...
// }