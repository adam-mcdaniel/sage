use crate::lir::*;
use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;
use std::collections::BTreeMap;

#[derive(Parser)]
#[grammar = "frontend/parse.pest"] // relative to src
struct FrontendParser;

#[derive(Clone, Debug)]
pub enum Statement {
    Match(Expr, Vec<(Pattern, Self)>),
    Let(Vec<(String, Option<Type>, Expr)>),
    Assign(Expr, Option<Box<dyn AssignOp + 'static>>, Expr),
    If(Expr, Box<Self>, Option<Box<Self>>),
    IfLet(Pattern, Expr, Box<Self>, Option<Box<Self>>),
    While(Expr, Box<Self>),
    For(Box<Self>, Expr, Box<Self>, Box<Self>),
    Return(Expr),
    Block(Vec<Declaration>),
    LetIn(Vec<(String, Option<Type>, Expr)>, Box<Self>),
    Expr(Expr),
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
            (Self::Match(e, arms), _) => Expr::Match(
                Box::new(e),
                arms.into_iter()
                    .map(|(a, b)| (a, b.to_expr(None)))
                    .collect(),
            ),
            (Self::Assign(lhs, op, rhs), _) => {
                match op {
                    Some(op) => lhs.refer().assign(op, rhs),
                    // Some(op) => Expr::AssignOp(lhs, op, Box::new(rhs), rest_expr),
                    None => lhs.refer().deref_mut(rhs),
                }
            }
            (Self::If(cond, body, None), _) => Expr::If(
                Box::new(cond),
                Box::new(body.to_expr(None)),
                Box::new(Expr::ConstExpr(ConstExpr::None)),
            ),
            (Self::If(cond, body, Some(else_body)), _) => Expr::If(
                Box::new(cond),
                Box::new(body.to_expr(None)),
                Box::new(else_body.to_expr(None)),
            ),
            (Self::IfLet(pat, cond, body, None), _) => Expr::IfLet(
                pat,
                Box::new(cond),
                Box::new(body.to_expr(None)),
                Box::new(Expr::ConstExpr(ConstExpr::None)),
            ),
            (Self::IfLet(pat, cond, body, Some(else_body)), _) => Expr::IfLet(
                pat,
                Box::new(cond),
                Box::new(body.to_expr(None)),
                Box::new(else_body.to_expr(None)),
            ),
            (Self::While(cond, body), _) => {
                Expr::While(Box::new(cond), Box::new(body.to_expr(None)))
            }
            (Self::For(init, cond, step, body), _) => {
                // init.to_expr(Some(Expr::While(Box::new(cond), Box::new(Expr::Many(vec![body.to_expr(None), step.to_expr(None)])))))
                init.to_expr(Some(Expr::While(
                    Box::new(cond),
                    Box::new(body.to_expr(Some(step.to_expr(None)))),
                )))
            }
            (Self::Return(val), _) => Expr::Return(Box::new(val)),

            (Self::Block(stmts), Some(Expr::Many(mut rest))) => {
                rest.insert(
                    0,
                    Expr::Many(stmts.into_iter().map(|s| s.to_expr(None)).collect()),
                );
                return Expr::Many(rest);
            }
            (Self::Block(stmts), Some(inner)) => {
                let mut result = Some(inner);
                for stmt in stmts.into_iter().rev() {
                    result = Some(stmt.to_expr(result));
                }
                return result.unwrap_or(Expr::ConstExpr(ConstExpr::None));
            }
            (Self::Block(stmts), None) => {
                let mut result = None;
                for stmt in stmts.into_iter().rev() {
                    result = Some(stmt.to_expr(result));
                }
                return result.unwrap_or(Expr::ConstExpr(ConstExpr::None));
            }
            // (Self::LetIn(defs, body), Some(Expr::LetVars(mut vars, mut ret))) => {
            //     // Expr::LetVars(defs, Box::new(body.to_expr(None)))
            //     for (name, ty, val) in defs.into_iter().rev() {
            //         vars.insert(0, (name, ty, val));
            //     }
            //     return Expr::LetVars(vars, Box::new(Expr::Many(vec![body.to_expr(Some(*ret))])))
            // }
            (Self::LetIn(defs, body), _) => Expr::LetVars(defs, Box::new(body.to_expr(None))),
            (Self::Let(defs), Some(Expr::LetVars(mut vars, ret))) => {
                for (name, ty, val) in defs.into_iter().rev() {
                    vars.insert(0, (name, ty, val));
                }
                return Expr::LetVars(vars, ret);
            }
            (Self::Let(defs), _) => return Expr::LetVars(defs, rest_expr),
            (Self::Expr(e), _) => e,
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
    PolyProc(
        String,
        Vec<String>,
        Vec<(String, Type)>,
        Option<Type>,
        Box<Statement>,
    ),
    Type(Vec<(String, Type)>),
    Statement(Statement),
}

impl Declaration {
    fn proc_to_expr(
        name: String,
        args: Vec<(String, Type)>,
        ret: Option<Type>,
        body: Statement,
    ) -> Procedure {
        Procedure::new(
            Some(name),
            args,
            ret.unwrap_or(Type::None),
            body.to_expr(None),
        )
    }

    fn to_expr(self, rest: Option<Expr>) -> Expr {
        let rest_expr = Box::new(rest.clone().unwrap_or(Expr::ConstExpr(ConstExpr::None)));
        match (self, rest) {
            (Self::Struct(name, fields), Some(Expr::LetTypes(mut types, ret))) => {
                types.push((name, Type::Struct(fields.into_iter().collect())));
                Expr::LetTypes(types, ret)
            }
            (Self::Struct(name, fields), _) => Expr::LetTypes(
                vec![(name, Type::Struct(fields.into_iter().collect()))],
                rest_expr,
            ),
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
                    types.push((
                        name,
                        Type::Enum(variants.into_iter().map(|(a, _)| a).collect()),
                    ));
                } else {
                    // Otherwise, we need to use a tagged union
                    types.push((
                        name,
                        Type::EnumUnion(
                            variants
                                .into_iter()
                                .map(|(a, b)| {
                                    (
                                        a,
                                        match b {
                                            Some(x) => x,
                                            None => Type::None,
                                        },
                                    )
                                })
                                .collect(),
                        ),
                    ));
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
                    Expr::LetTypes(
                        vec![(
                            name,
                            Type::Enum(variants.into_iter().map(|(a, _)| a).collect()),
                        )],
                        rest_expr,
                    )
                } else {
                    // Otherwise, we need to use a tagged union
                    Expr::LetTypes(
                        vec![(
                            name,
                            Type::EnumUnion(
                                variants
                                    .into_iter()
                                    .map(|(a, b)| {
                                        (
                                            a,
                                            match b {
                                                Some(x) => x,
                                                None => Type::None,
                                            },
                                        )
                                    })
                                    .collect(),
                            ),
                        )],
                        rest_expr,
                    )
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
                defs.insert(
                    name.clone(),
                    ConstExpr::Proc(Self::proc_to_expr(name, params, ret, *stmt)),
                );
                Expr::LetConsts(defs, ret2)
            }
            (Self::Proc(name, params, ret, stmt), _) => {
                let mut defs = BTreeMap::new();
                defs.insert(
                    name.clone(),
                    ConstExpr::Proc(Self::proc_to_expr(name, params, ret, *stmt)),
                );
                Expr::LetConsts(defs, rest_expr)
            }
            (
                Self::PolyProc(name, ty_params, params, ret, stmt),
                Some(Expr::LetConsts(mut consts, ret2)),
            ) => {
                consts.insert(
                    name.clone(),
                    ConstExpr::PolyProc(PolyProcedure::new(
                        name,
                        ty_params,
                        params,
                        ret.unwrap_or(Type::None),
                        stmt.to_expr(None),
                    )),
                );
                Expr::LetConsts(consts, ret2)
            }
            (Self::PolyProc(name, ty_params, params, ret, stmt), _) => {
                let mut consts = BTreeMap::new();
                consts.insert(
                    name.clone(),
                    ConstExpr::PolyProc(PolyProcedure::new(
                        name,
                        ty_params,
                        params,
                        ret.unwrap_or(Type::None),
                        stmt.to_expr(None),
                    )),
                );
                Expr::LetConsts(consts, rest_expr)
            }
            (Self::Type(mut types), Some(Expr::LetTypes(mut types2, ret))) => {
                types.append(&mut types2);
                Expr::LetTypes(types, ret)
            }
            (Self::Type(types), _) => Expr::LetTypes(types, rest_expr),
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

pub fn parse_frontend(code: &str) -> Result<Expr, Error<Rule>> {
    let x = FrontendParser::parse(Rule::program, code)?;
    Ok(parse_program(x.into_iter().next().unwrap()).to_expr())
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

            let mut ty_params = vec![];
            if let Some(ty_params_pair) = inner_rules.peek() {
                if ty_params_pair.as_rule() == Rule::type_parameters
                    && ty_params_pair.into_inner().count() > 0
                {
                    let ty_params_pair = inner_rules.next().unwrap();
                    for ty_param_pair in ty_params_pair.into_inner() {
                        ty_params.push(ty_param_pair.as_str().to_string());
                    }
                }
            }

            let mut params = vec![];
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
                    other => panic!("unexpected rule {:?}", other),
                }
            }
            if ty_params.is_empty() {
                Declaration::Proc(name, params, ret, Box::new(stmt))
            } else {
                Declaration::PolyProc(name, ty_params, params, ret, Box::new(stmt))
            }
        }
        Rule::decl_type => {
            let mut inner_rules = pair.into_inner();
            let mut types = Vec::new();
            while inner_rules.peek().is_some() {
                let name = inner_rules.next().unwrap().as_str().to_string();

                let mut ty_params = vec![];
                if let Some(ty_params_pair) = inner_rules.peek() {
                    if ty_params_pair.as_rule() == Rule::type_parameters
                        && ty_params_pair.into_inner().count() > 0
                    {
                        let ty_params_pair = inner_rules.next().unwrap();
                        for ty_param_pair in ty_params_pair.into_inner() {
                            ty_params.push(ty_param_pair.as_str().to_string());
                        }
                    }
                }

                let ty = parse_type(inner_rules.next().unwrap());
                if ty_params.is_empty() {
                    types.push((name, ty));
                } else {
                    types.push((name, Type::Poly(ty_params, Box::new(ty))));
                }
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

            let mut ty_params = vec![];
            if let Some(ty_params_pair) = inner_rules.peek() {
                if ty_params_pair.as_rule() == Rule::type_parameters
                    && ty_params_pair.into_inner().count() > 0
                {
                    let ty_params_pair = inner_rules.next().unwrap();
                    for ty_param_pair in ty_params_pair.into_inner() {
                        ty_params.push(ty_param_pair.as_str().to_string());
                    }
                }
            }

            let mut fields = Vec::new();
            while inner_rules.peek().is_some() {
                let mut inner_rules = inner_rules.next().unwrap().into_inner();
                let name = inner_rules.next().unwrap().as_str().to_string();
                let ty = parse_type(inner_rules.next().unwrap());
                fields.push((name, ty));
            }
            if ty_params.is_empty() {
                Declaration::Struct(name, fields)
            } else {
                Declaration::Type(vec![(
                    name.to_string(),
                    Type::Poly(
                        ty_params,
                        Box::new(Type::Struct(fields.into_iter().collect())),
                    ),
                )])
            }
        }
        Rule::decl_enum => {
            let mut inner_rules = pair.into_inner();
            let name = inner_rules.next().unwrap().as_str().to_string();

            let mut ty_params = vec![];
            if let Some(ty_params_pair) = inner_rules.peek() {
                if ty_params_pair.as_rule() == Rule::type_parameters
                    && ty_params_pair.into_inner().count() > 0
                {
                    let ty_params_pair = inner_rules.next().unwrap();
                    for ty_param_pair in ty_params_pair.into_inner() {
                        ty_params.push(ty_param_pair.as_str().to_string());
                    }
                }
            }

            let mut variants = Vec::new();
            for pair in inner_rules {
                let mut inner_rules = pair.into_inner();
                let variant_name = inner_rules.next().unwrap();
                if variant_name.as_rule() != Rule::symbol {
                    continue;
                }
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

            if ty_params.is_empty() {
                Declaration::Enum(name, variants)
            } else {
                let is_simple = variants.iter().all(|(_, ty)| ty.is_none());
                if is_simple {
                    Declaration::Type(vec![(
                        name.to_string(),
                        Type::Poly(
                            ty_params,
                            Box::new(Type::Enum(
                                variants.into_iter().map(|(name, _)| name).collect(),
                            )),
                        ),
                    )])
                } else {
                    Declaration::Type(vec![(
                        name.to_string(),
                        Type::Poly(
                            ty_params,
                            Box::new(Type::EnumUnion(
                                variants
                                    .into_iter()
                                    .map(|(a, b)| (a, b.unwrap_or(Type::None)))
                                    .collect(),
                            )),
                        ),
                    )])
                }
            }
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
        Rule::stmt | Rule::stmt_block => Declaration::Statement(parse_stmt(pair)),
        Rule::EOI => Declaration::Statement(Statement::Block(vec![])),
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair),
    }
}

fn parse_stmt(pair: Pair<Rule>) -> Statement {
    match pair.as_rule() {
        Rule::stmt | Rule::long_stmt | Rule::short_stmt | Rule::stmt_let_in => {
            pair.into_inner().map(parse_stmt).next().unwrap()
        }

        Rule::stmt_match => Statement::Expr(parse_match(pair)),

        Rule::stmt_block => {
            let mut inner_rules = pair.into_inner();
            let mut stmts = Vec::new();
            while let Some(stmt) = inner_rules.next() {
                stmts.push(parse_decl(stmt));
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

            let mut else_body = inner_rules
                .next()
                .map(|x| parse_stmt(x))
                .unwrap_or(Statement::Block(vec![]));

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

            let mut else_body = inner_rules
                .next()
                .map(|x| parse_stmt(x))
                .unwrap_or(Statement::Block(vec![]));

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

        Rule::stmt_let => {
            let mut inner_rules = pair.into_inner();
            let mut defs = vec![];
            while inner_rules.peek().is_some() {
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
            Statement::Let(defs)
        }

        Rule::stmt_let_in_expr | Rule::stmt_let_in_block => {
            let mut inner_rules = pair.into_inner();
            let mut defs = vec![];
            while inner_rules.clone().count() > 1 {
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
                Rule::stmt_block => Statement::LetIn(defs, Box::new(parse_stmt(last))),
                Rule::expr => Statement::LetIn(defs, Box::new(Statement::Expr(parse_expr(last)))),
                other => unreachable!("Unexpected rule {:?}", other),
            }
        }

        Rule::stmt_assign => {
            let mut inner_rules = pair.into_inner();
            let lhs = parse_expr(inner_rules.next().unwrap());
            let op = inner_rules.next().unwrap().as_str();
            let rhs = parse_expr(inner_rules.next().unwrap());
            Statement::Assign(
                lhs,
                match op {
                    "=" => None,
                    "+=" => Some(Box::new(Assign::new(Add))),
                    "-=" => Some(Box::new(Assign::new(Arithmetic::Subtract))),
                    "*=" => Some(Box::new(Assign::new(Arithmetic::Multiply))),
                    "/=" => Some(Box::new(Assign::new(Arithmetic::Divide))),
                    "%=" => Some(Box::new(Assign::new(Arithmetic::Remainder))),
                    "&=" => Some(Box::new(Assign::new(BitwiseAnd))),
                    "^=" => Some(Box::new(Assign::new(BitwiseXor))),
                    "|=" => Some(Box::new(Assign::new(BitwiseOr))),
                    _ => unreachable!(),
                },
                rhs,
            )
        }

        Rule::stmt_return => {
            let mut inner_rules = pair.into_inner();
            let expr = inner_rules.next().map(parse_expr);
            Statement::Return(expr.unwrap_or(Expr::ConstExpr(ConstExpr::None)))
        }

        Rule::expr => Statement::Expr(parse_expr(pair)),

        other => panic!("Unexpected rule: {:?}: {:?}", other, pair),
    }
}

// fn parse_let(pair: Pair<Rule>) -> Statement {
//     match pair.as_rule() {

//         // Rule::stmt_let => {
//         //     let mut inner_rules = pair.into_inner();
//         //     parse_let(inner_rules.next().unwrap())
//         // }
//         // Rule::stmt_let_typed => {
//         //     let mut inner_rules = pair.into_inner();
//         //     let name = inner_rules.next().unwrap().as_str().to_string();
//         //     let ty = inner_rules.next().map(parse_type);
//         //     let expr = parse_expr(inner_rules.next().unwrap());
//         //     Statement::Let(name, ty, expr)
//         // }
//         // Rule::stmt_let_untyped => {
//         //     let mut inner_rules = pair.into_inner();
//         //     let name = inner_rules.next().unwrap().as_str().to_string();
//         //     let expr = parse_expr(inner_rules.next().unwrap());
//         //     Statement::Let(name, None, expr)
//         // }
//         _ => unreachable!()
//     }
// }

pub fn parse_expr(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::expr | Rule::expr_atom | Rule::expr_group => {
            pair.into_inner().map(parse_expr).next().unwrap()
        }
        Rule::stmt_match => parse_match(pair),
        Rule::expr_logic_factor
        | Rule::expr_logic_term
        | Rule::expr_comparison
        | Rule::expr_sum
        | Rule::expr_index
        | Rule::expr_factor
        | Rule::expr_bitwise_factor
        | Rule::expr_bitwise_term
        | Rule::expr_bitwise_atom => parse_binop(pair),
        Rule::expr_ternary => {
            let mut inner_rules = pair.into_inner();
            let cond = parse_expr(inner_rules.next().unwrap());
            let if_true = parse_expr(inner_rules.next().unwrap());
            let if_false = parse_expr(inner_rules.next().unwrap());
            Expr::If(Box::new(cond), Box::new(if_true), Box::new(if_false))
        }
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
        Rule::expr_term_non_keyword => parse_expr_term(pair),
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

        Rule::expr_unary | Rule::expr_term => {
            let inner_rules = pair.into_inner();
            let mut result = Expr::ConstExpr(ConstExpr::None);
            for x in inner_rules.rev() {
                result = match x.as_rule() {
                    Rule::expr_unary_op | Rule::expr_keyword_unary_op => match x.as_str() {
                        "!" => result.not(),
                        "-" => result.neg(),
                        "~" => result.bitnot(),
                        "&" => result.refer(),
                        "new" => result.unop(New),
                        "del" => result.unop(Delete),
                        "*" => result.deref(),
                        _ => panic!("Unexpected unary op: {}", x.as_str()),
                    },
                    _ => parse_expr(x),
                }
            }
            result
        }

        Rule::r#const | Rule::const_term | Rule::const_monomorph | Rule::const_atom => {
            Expr::ConstExpr(parse_const(pair))
        }
        Rule::stmt_block => parse_stmt(pair).to_expr(None),
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair),
    }
}

fn parse_expr_term(pair: Pair<Rule>) -> Expr {
    let mut inner_rules = pair.into_inner();
    let mut head = parse_expr(inner_rules.next().unwrap());
    for suffix in inner_rules {
        head = match suffix.as_rule() {
            Rule::expr_int_field => head.field(ConstExpr::Int(
                suffix
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
            )),
            Rule::expr_symbol_field => head.field(ConstExpr::Symbol(
                suffix.into_inner().next().unwrap().as_str().to_string(),
            )),
            Rule::expr_int_deref_field => head.deref().field(ConstExpr::Int(
                suffix
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap(),
            )),
            Rule::expr_symbol_deref_field => head.deref().field(ConstExpr::Symbol(
                suffix.into_inner().next().unwrap().as_str().to_string(),
            )),
            Rule::expr_index => head.idx(parse_expr(suffix)),
            Rule::expr_call => {
                let inner_rules = suffix.into_inner();
                let mut args = Vec::new();
                for arg in inner_rules {
                    args.push(parse_expr(arg));
                }
                if head == Expr::ConstExpr(ConstExpr::Symbol("print".to_string())) {
                    let mut exprs: Vec<Expr> =
                        args.into_iter().map(|val| val.unop(Put::Display)).collect();
                    exprs.push(Expr::ConstExpr(ConstExpr::None));
                    Expr::Many(exprs)
                } else if head == Expr::ConstExpr(ConstExpr::Symbol("input".to_string())) {
                    let mut exprs: Vec<Expr> = args.into_iter().map(|val| val.unop(Get)).collect();
                    exprs.push(Expr::ConstExpr(ConstExpr::None));
                    Expr::Many(exprs)
                } else {
                    head.app(args)
                }
            }
            Rule::expr_as_type => head.as_type(parse_type(suffix.into_inner().next().unwrap())),
            _ => unreachable!(),
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
            "&" => head.bitand(tail),
            "|" => head.bitor(tail),
            "^" => head.bitxor(tail),
            "~&" => head.bitnand(tail),
            "~|" => head.bitnor(tail),
            _ => unreachable!(),
        };
    }
    head
}

fn parse_const(pair: Pair<Rule>) -> ConstExpr {
    match pair.as_rule() {
        Rule::r#const | Rule::const_term | Rule::const_atom | Rule::const_group => {
            pair.into_inner().map(parse_const).next().unwrap()
        }
        Rule::const_monomorph => {
            let mut inner_rules = pair.into_inner();
            let c = parse_const(inner_rules.next().unwrap());
            let mut args = Vec::new();
            for arg in inner_rules.next().unwrap().into_inner() {
                args.push(parse_type(arg));
            }
            ConstExpr::Monomorphize(Box::new(c), args)
        }
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
        Rule::const_int => {
            let s = pair.as_str();
            ConstExpr::Int(if s.len() > 2 && &s[..2] == "0b" {
                i64::from_str_radix(&s[2..], 2).unwrap() as i64
            } else if s.len() > 2 && &s[..2] == "0o" {
                i64::from_str_radix(&s[2..], 8).unwrap() as i64
            } else if s.len() > 2 && &s[..2] == "0x" {
                i64::from_str_radix(&s[2..], 16).unwrap() as i64
            } else if s.len() > 0 {
                i64::from_str_radix(s, 10).unwrap() as i64
            } else {
                0
            })
        }
        Rule::const_float => ConstExpr::Float(pair.as_str().parse().unwrap()),
        Rule::const_char => {
            let token = pair.into_inner().next().unwrap().as_str();
            let token = &token[1..token.len() - 1];
            let result = snailquote::unescape(&format!("\"{token}\"").replace("\\0", "\\\\0"))
                .unwrap()
                .replace("\\0", "\0");
            let ch = result.chars().next().unwrap();
            ConstExpr::Char(ch)
        }
        Rule::const_bool => ConstExpr::Bool(pair.as_str().to_lowercase().parse().unwrap()),
        Rule::const_string => ConstExpr::Array(
            snailquote::unescape(
                &pair
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .replace("\\0", "\\\\0"),
            )
            .unwrap()
            .replace("\\0", "\0")
            .chars()
            .map(|ch| ConstExpr::Char(ch))
            .collect(),
        ),
        Rule::const_none => ConstExpr::None,
        Rule::const_null => ConstExpr::Null,
        Rule::const_size_of => ConstExpr::SizeOfType(parse_type(pair.into_inner().next().unwrap())),
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair),
    }
}

fn parse_type(pair: Pair<Rule>) -> Type {
    // todo!()
    match pair.as_rule() {
        Rule::r#type | Rule::type_atom | Rule::type_term => {
            pair.into_inner().map(parse_type).next().unwrap()
        }

        Rule::type_apply => {
            let mut inner_rules = pair.into_inner();
            let mut head = parse_type(inner_rules.next().unwrap());

            while inner_rules.peek().is_some() {
                while let Some(parsed_args) = inner_rules.next() {
                    let mut ty_args = vec![];
                    // type_application_suffix
                    // args.push(parse_type(arg));
                    for parsed_arg in parsed_args.into_inner() {
                        ty_args.push(parse_type(parsed_arg));
                    }
                    head = Type::Apply(Box::new(head), ty_args);
                }
            }
            head
        }

        Rule::type_template => {
            let inner_rules = pair.into_inner();
            let mut head = Type::None;
            let mut params = vec![];
            for pair in inner_rules {
                match pair.as_rule() {
                    Rule::symbol => {
                        params.push(pair.as_str().to_string());
                    }
                    Rule::r#type => {
                        let ty = parse_type(pair);
                        head = Type::Poly(params.clone(), Box::new(ty));
                    }
                    _ => unreachable!(),
                }
            }
            head
        }

        Rule::type_let => {
            let mut inner_rules = pair.into_inner();
            // Get all but the last rule
            let mut result = vec![];
            while inner_rules.clone().count() > 2 {
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

        Rule::type_enum => {
            let inner_rules = pair.into_inner();
            let mut variants = Vec::new();
            for pair in inner_rules {
                let mut inner_rules = pair.into_inner();
                let variant_name = inner_rules.next().unwrap();
                if variant_name.as_rule() != Rule::symbol {
                    continue;
                }
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
            let mut is_simple = true;
            for (_, ty) in &variants {
                if ty.is_some() {
                    is_simple = false;
                    break;
                }
            }
            if is_simple {
                Type::Enum(variants.into_iter().map(|(name, _)| name).collect())
            } else {
                Type::EnumUnion(
                    variants
                        .into_iter()
                        .map(|(name, ty)| (name, ty.unwrap_or(Type::None)))
                        .collect(),
                )
            }
        }
        Rule::type_ptr => {
            let mut inner_rules = pair.into_inner();
            let ty = parse_type(inner_rules.next().unwrap());
            Type::Pointer(Box::new(ty))
        }
        Rule::type_proc => {
            let mut inner_rules = pair.into_inner();
            let mut args_rules = inner_rules.next().unwrap().into_inner();
            let mut args = Vec::new();
            while args_rules.peek().is_some() {
                let ty = parse_type(args_rules.next().unwrap());
                args.push(ty);
            }
            let ret = parse_type(inner_rules.next().unwrap());
            Type::Proc(args, Box::new(ret))
        }

        other => panic!("Unexpected rule: {:?}: {:?}", other, pair),
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
    Expr::Match(
        Box::new(expr),
        patterns.into_iter().zip(stmts.into_iter()).collect(),
    )
}

fn parse_pattern(pair: Pair<Rule>) -> Pattern {
    match pair.as_rule() {
        Rule::pattern | Rule::pattern_term | Rule::pattern_atom | Rule::pattern_group => {
            pair.into_inner().map(parse_pattern).next().unwrap()
        }
        Rule::pattern_const => Pattern::ConstExpr(parse_const(pair.into_inner().next().unwrap())),
        Rule::pattern_variant => {
            let mut inner_rules = pair.into_inner();
            let symbol = inner_rules.next().unwrap().as_str().to_string();
            let pattern = inner_rules.next().map(parse_pattern);
            Pattern::Variant(symbol, pattern.map(Box::new))
        }
        Rule::pattern_tuple => {
            let inner_rules = pair.into_inner();
            let mut patterns = Vec::new();
            for pair in inner_rules {
                let pattern = parse_pattern(pair);
                patterns.push(pattern);
            }
            Pattern::Tuple(patterns)
        }
        Rule::pattern_struct => {
            let inner_rules = pair.into_inner();
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
        }
        Rule::pattern_ptr => {
            let mut inner_rules = pair.into_inner();
            let pattern = parse_pattern(inner_rules.next().unwrap());
            Pattern::Pointer(Box::new(pattern))
        }
        Rule::pattern_wildcard => Pattern::Wildcard,
        Rule::pattern_symbol => {
            let symbol = pair.as_str().to_string();
            if symbol == "_" {
                Pattern::Wildcard
            } else {
                Pattern::Symbol(symbol)
            }
        }
        Rule::pattern_alt => {
            let inner_rules = pair.into_inner();
            let mut patterns = Vec::new();
            for pair in inner_rules {
                let pattern = parse_pattern(pair);
                patterns.push(pattern);
            }
            Pattern::Alt(patterns)
        }
        other => panic!("Unexpected rule: {:?}: {:?}", other, pair),
    }
}
