use std::collections::HashMap;
use core::fmt::{Display, Formatter, Result as FmtResult};
use crate::lir::*;

/// A pattern which can be matched against an expression.
/// 
/// Patterns are used in the `match` expression, and in the `let` expression.
/// Patterns can either `match` or `bind` to an expression.
/// When `match`ing, the pattern just checks if the expression matches the pattern.
/// When `bind`ing, the pattern binds the expression to corresponding variables in the
/// pattern, and evaluates an expression with those variables.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Tuple(Vec<Pattern>),
    Struct(HashMap<String, Pattern>),
    Symbol(String),
    ConstExpr(ConstExpr),
    Alt(Vec<Pattern>),
    Wildcard,
}

impl Pattern {
    pub fn tup(patterns: Vec<Pattern>) -> Self {
        Self::Tuple(patterns)
    }
    pub fn struct_(patterns: HashMap<String, Pattern>) -> Self {
        Self::Struct(patterns)
    }
    pub fn sym(name: impl ToString) -> Self {
        Self::Symbol(name.to_string())
    }
    pub fn int(n: i32) -> Self {
        Self::ConstExpr(ConstExpr::Int(n))
    }
    pub fn float(n: f64) -> Self {
        Self::ConstExpr(ConstExpr::Float(n))
    }
    pub fn bool(b: bool) -> Self {
        Self::ConstExpr(ConstExpr::Bool(b))
    }
    pub fn alt(patterns: Vec<Pattern>) -> Self {
        Self::Alt(patterns)
    }
    pub fn wildcard() -> Self {
        Self::Wildcard
    }

    pub fn get_branch_result_type(&self, expr: &Expr, branch: &Expr, env: &Env) -> Result<Type, Error> {
        let ty = expr.get_type(env)?;
        let bindings = self.get_bindings(expr, &ty, env)?;
        let mut new_env = env.clone();
        for (var, t) in bindings {
            new_env.define_var(var, t)?;
        }

        branch.get_type(&new_env)
    }

    pub fn type_check(&self, expr: &Expr, branch: &Expr, env: &Env) -> Result<(), Error> {
        let ty = expr.get_type(env)?;
        let expected = branch.get_type(env)?;
        self.matches(&expr, &ty, env)?.type_check(env)?;
        let result_expr = self.bind(&expr, &ty, &branch, env)?;
        result_expr.type_check(env)?;
        let found = result_expr.get_type(env)?;
        if !expected.equals(&found, env)? {
            return Err(Error::MismatchedTypes {
                expected,
                found,
                expr: expr.clone(),
            })
        }
        Ok(())
    }

    pub fn let_pattern(&self, expr: &Expr, ret: &Expr, env: &Env) -> Result<Expr, Error> {
        use rand::Rng;
        let ty = expr.get_type(env)?;
        let var: String = rand::thread_rng()
            .sample_iter(&rand::distributions::Alphanumeric)
            .take(7)
            .map(char::from)
            .collect();
        Ok(Expr::let_var(var.clone(), None, expr.clone(), self.bind(&Expr::var(var), &ty, ret, env)?))
    }

    pub fn match_pattern(expr: &Expr, branches: &[(Self, Expr)], env: &Env) -> Result<Expr, Error> {
        let ty = expr.get_type(env)?;
        let mut result = Expr::ConstExpr(ConstExpr::None);
        for (pattern, ret) in branches {
            let cond = pattern.matches(&expr, &ty, env)?;
            result = Expr::If(
                Box::new(cond),
                Box::new(pattern.bind(&expr, &ty, ret, env)?),
                Box::new(result)
            );
        }
        Ok(result)
    }

    fn get_bindings(&self, expr: &Expr, ty: &Type, env: &Env) -> Result<HashMap<String, Type>, Error> {
        Ok(match (self, ty) {
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                let mut result = HashMap::new();
                for (i, (pattern, item_type)) in patterns.iter().zip(item_types.iter()).enumerate() {
                    result.extend(pattern.get_bindings(&expr.clone().field(ConstExpr::Int(i as i32)), item_type, env)?.into_iter())
                }
                result
            }

            (Self::Struct(patterns), Type::Struct(item_types)) => {
                let mut result = HashMap::new();
                for (name, pattern) in patterns.iter() {
                    let item_type = item_types.get(name);
                    if let Some(item_type) = item_type {
                        result.extend(pattern.get_bindings(&expr.clone().field(ConstExpr::Symbol(name.clone())), item_type, env)?.into_iter())
                    } else {
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()))
                    }
                }
                result
            }

            (Self::Symbol(name), ty) => {
                let mut result = HashMap::new();
                result.insert(name.clone(), ty.clone());
                result
            }

            (Self::Wildcard, _)
            | (Self::ConstExpr(_), _) => {
                HashMap::new()
            }

            (Self::Alt(patterns), _) => {
                let mut result = HashMap::new();
                for (i, pattern) in patterns.iter().enumerate() {
                    let bindings = pattern.get_bindings(expr, ty, env)?;
                    if i == 0 {
                        result = bindings;
                    } else if result != bindings {
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()))
                    }
                }
                result
            }

            _ => todo!(),
        })
    }

    fn matches(&self, expr: &Expr, ty: &Type, env: &Env) -> Result<Expr, Error> {
        Ok(match (self, ty) {
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                let mut result = Expr::ConstExpr(ConstExpr::Bool(true));

                for (i, (pattern, item_type)) in patterns.iter().zip(item_types.iter()).enumerate() {
                    result = result.and(
                        pattern.matches(&expr.clone().field(ConstExpr::Int(i as i32)), 
                        item_type, env)?
                    );
                }

                result
            }

            (Self::Struct(patterns), Type::Struct(item_types)) => {
                let mut result = Expr::ConstExpr(ConstExpr::Bool(true));

                for (name, pattern) in patterns.iter() {
                    let item_type = item_types.get(name);
                    if let Some(item_type) = item_type {
                        result = result.and(
                            pattern.matches(&expr.clone().field(ConstExpr::Symbol(name.clone())), 
                            item_type, env)?
                        );
                    } else {
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()))
                    }
                }

                result
            }

            (Self::Symbol(_name), _) => {
                Expr::ConstExpr(ConstExpr::Bool(true))
            }

            (Self::ConstExpr(const_expr), _) => {
                Expr::ConstExpr(const_expr.clone())
                    .eq(expr.clone())
            }

            (Self::Alt(patterns), _) => {
                let mut result = Expr::ConstExpr(ConstExpr::Bool(false));

                for pattern in patterns.iter() {
                    result = result.or(
                        pattern.matches(expr, ty, env)?
                    );
                }

                result
            }

            (Self::Wildcard, _) => Expr::ConstExpr(ConstExpr::Bool(true)),
            
            _ => todo!(),
        })
    }

    fn bind(&self, expr: &Expr, ty: &Type, ret: &Expr, env: &Env) -> Result<Expr, Error> {
        Ok(match (self, ty) {
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                let mut result = ret.clone();
                for (i, (pattern, item_type)) in patterns.iter().zip(item_types.iter()).enumerate() {
                    result = pattern.bind(&expr.clone().field(ConstExpr::Int(i as i32)), item_type, &result, env)?;
                }
                result
            }

            (Self::Struct(patterns), Type::Struct(item_types)) => {
                let mut result = ret.clone();
                for (name, pattern) in patterns.iter() {
                    let item_type = item_types.get(name);
                    if let Some(item_type) = item_type {
                        result = pattern.bind(&expr.clone().field(ConstExpr::Symbol(name.clone())), item_type, &result, env)?;
                    } else {
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()))
                    }
                }

                result
            }

            (Self::Symbol(name), ty) => {
                Expr::let_var(name.clone(), Some(ty.clone()), expr.clone(), ret.clone())
            }

            (Self::Wildcard, _)
            | (Self::ConstExpr(_), _) => {
                ret.clone()
            }

            (Self::Alt(patterns), _) => {
                let mut result = ret.clone();
                for pattern in patterns.iter() {
                    result = pattern.bind(expr, ty, &result, env)?;
                    break;
                }
                result
            }

            _ => todo!(),
        })
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Tuple(patterns) => {
                write!(f, "(")?;
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pattern)?;
                }
                write!(f, ")")
            }

            Self::Struct(patterns) => {
                write!(f, "{{")?;
                for (i, (name, pattern)) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, pattern)?;
                }
                write!(f, "}}")
            }

            Self::Symbol(name) => write!(f, "{}", name),

            Self::ConstExpr(const_expr) => write!(f, "{}", const_expr),

            Self::Alt(patterns) => {
                write!(f, "(")?;
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", pattern)?;
                }
                write!(f, ")")
            }

            Self::Wildcard => write!(f, "_"),
        }
    }
}