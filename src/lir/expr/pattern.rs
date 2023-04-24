use crate::lir::*;
use core::fmt::{Display, Formatter, Result as FmtResult};
use std::collections::HashMap;

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
    /// Construct a new pattern which matches a tuple of patterns.
    pub fn tup(patterns: Vec<Pattern>) -> Self {
        Self::Tuple(patterns)
    }
    /// Construct a new pattern which matches a struct with a given set of fields.
    pub fn struct_(patterns: HashMap<String, Pattern>) -> Self {
        Self::Struct(patterns)
    }
    /// Construct a new pattern which matches a symbol with a given name.
    pub fn sym(name: impl ToString) -> Self {
        Self::Symbol(name.to_string())
    }
    /// Construct a new pattern which matches a constant integer.
    pub fn int(n: i32) -> Self {
        Self::ConstExpr(ConstExpr::Int(n))
    }
    /// Construct a new pattern which matches a constant float.
    pub fn float(n: f64) -> Self {
        Self::ConstExpr(ConstExpr::Float(n))
    }
    /// Construct a new pattern which matches a constant boolean.
    pub fn bool(b: bool) -> Self {
        Self::ConstExpr(ConstExpr::Bool(b))
    }
    /// Construct a new pattern which binds to several alternate patterns.
    pub fn alt(patterns: Vec<Pattern>) -> Self {
        Self::Alt(patterns)
    }
    /// Construct a new pattern which matches any expression.
    pub fn wildcard() -> Self {
        Self::Wildcard
    }

    /// Get the type of a branch with a given expression matched to this pattern.
    pub fn get_branch_result_type(
        &self,
        expr: &Expr,
        branch: &Expr,
        env: &Env,
    ) -> Result<Type, Error> {
        // Get the type of the expression being matched.
        let ty = expr.get_type(env)?;
        // Get the bindings for the pattern.
        let bindings = self.get_bindings(expr, &ty, env)?;
        // Create a new environment with the bindings.
        let mut new_env = env.clone();
        for (var, t) in bindings {
            // Define the variables in the new environment.
            new_env.define_var(var, t)?;
        }

        // Get the type of the branch.
        branch.get_type(&new_env)
    }

    /// Type-check a pattern match of an expression against this pattern,
    /// and type-check the branch where the expression is bound to the pattern.
    pub fn type_check(&self, matching_expr: &Expr, branch: &Expr, env: &Env) -> Result<(), Error> {
        // Get the type of the expression being matched.
        let matching_ty = matching_expr.get_type(env)?;
        // Get the type of the branch as a result of the match.
        let expected = self.get_branch_result_type(matching_expr, branch, env)?;
        // Type-check the expression generated to match the pattern.
        self.matches(&matching_expr, &matching_ty, env)?
            .type_check(env)?;
        // Generate an expression with the bindings defined for the branch.
        let result_expr = self.bind(&matching_expr, &matching_ty, &branch, env)?;
        // Type-check the expression generated to bind the pattern.
        result_expr.type_check(env)?;
        // Get the type of the expression generated to bind the pattern.
        let found = result_expr.get_type(env)?;
        // It should be the same as the type of the branch.
        if !expected.equals(&found, env)? {
            // If not, return an error.
            return Err(Error::MismatchedTypes {
                expected,
                found,
                expr: matching_expr.clone(),
            });
        }
        // If no error was returned, the type-checking succeeded.
        Ok(())
    }

    /// Generate an `if let`expression, which matches a given `expr`, and executes
    /// `then` if the expression matches the pattern, and `else_` otherwise.
    pub fn if_let_pattern(
        &self,
        expr: &Expr,
        then: &Expr,
        else_: &Expr,
        env: &Env,
    ) -> Result<Expr, Error> {
        // Create a var which is used to do pattern matching instead of the original expression.
        // This is to avoid evaluating the expression multiple times.
        let var_name = expr.to_string() + "__PATTERN_MATCH";
        let var = Expr::var(&var_name);
        // Get the type of the expression being matched.
        let ty = expr.get_type(env)?;
        // An expression which evaluates to true if the expression matches the pattern.
        let cond = self.matches(&var, &ty, env)?;
        // The `if let` expression.
        let if_let = Expr::If(
            Box::new(cond),
            // If the expression matches the pattern, bind the pattern to the expression,
            // and evaluate the `then` expression.
            Box::new(self.bind(&var, &ty, &then, env)?),
            // Otherwise, evaluate the `else_` expression.
            Box::new(else_.clone()),
        );
        // Create a new environment with the bindings and evaluate the `if let` expression.
        Ok(Expr::let_var(
            var_name,
            None,
            expr.clone(),
            self.bind(&var, &ty, &if_let, env)?,
        ))
    }

    /// Generate an expression which evaluates a `match` expression, which matches
    /// a given `expr` against a set of patterns and branches.
    pub fn match_pattern(expr: &Expr, branches: &[(Self, Expr)], env: &Env) -> Result<Expr, Error> {
        // Create a var name which is used to do pattern matching instead of the original expression.
        // This is to avoid evaluating the expression multiple times.
        let var_name = expr.to_string() + "__PATTERN_MATCH";
        // Create a new environment with the bindings
        let mut new_env = env.clone();
        // Define the variable in the new environment.
        new_env.define_var(var_name.clone(), expr.get_type(env)?)?;
        // Generate the expression which evaluates the `match` expression.
        let match_expr = Pattern::match_pattern_helper(&Expr::var(&var_name), &branches, &new_env)?;
        // Define the variable in the generated expression.
        Ok(Expr::let_var(var_name, None, expr.clone(), match_expr))
    }

    /// A helper function for generating a `match` expression.
    /// This function simply generates code which matches a given `expr` against a set of patterns and branches.
    /// It does not prevent multiple evaluations of the `expr`.
    fn match_pattern_helper(
        expr: &Expr,
        branches: &[(Self, Expr)],
        env: &Env,
    ) -> Result<Expr, Error> {
        // Get the type of the expression being matched.
        let ty = expr.get_type(env)?;
        // The result of the `match` expression.
        let mut result = Expr::ConstExpr(ConstExpr::None);
        // Iterate over the patterns and branches in reverse order.
        // This is because the first pattern should be checked with
        // the first if statement, and so it should be added to the
        // chain of if statements last.
        for (pattern, ret) in branches.into_iter().rev() {
            // An expression which evaluates to true if the expression matches the pattern.
            let cond = pattern.matches(&expr, &ty, env)?;
            // A new if statement which checks if the expression matches the pattern.
            result = Expr::If(
                Box::new(cond),
                Box::new(pattern.bind(&expr, &ty, ret, env)?),
                Box::new(result),
            );
        }
        Ok(result)
    }

    /// Get the map of new variables and their types which are bound by this pattern.
    ///
    /// For example `(a, b)` binds the variables `a` and `b` to the first and second
    /// elements of the tuple respectively. This function would return a map with
    /// the keys `a` and `b` and the values being their types.
    pub fn get_bindings(
        &self,
        expr: &Expr,
        ty: &Type,
        env: &Env,
    ) -> Result<HashMap<String, Type>, Error> {
        Ok(match (self, ty) {
            // If the pattern is a tuple, and the type is a tuple, then
            // get the bindings for each element of the tuple.
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                let mut result = HashMap::new();
                // Iterate over the patterns and types of the tuple.
                for (i, (pattern, item_type)) in patterns.iter().zip(item_types.iter()).enumerate()
                {
                    // Get the bindings for the pattern and add them to the result.
                    result.extend(
                        pattern
                            .get_bindings(
                                &expr.clone().field(ConstExpr::Int(i as i32)),
                                item_type,
                                env,
                            )?
                            .into_iter(),
                    )
                }
                // Return the result.
                result
            }

            // If the pattern is a struct, and the type is a struct, then
            // get the bindings for each field of the struct.
            (Self::Struct(patterns), Type::Struct(item_types)) => {
                let mut result = HashMap::new();
                // Iterate over the field names and patterns of the struct.
                for (name, pattern) in patterns.iter() {
                    // If the struct has a field with the given name, then
                    // get the bindings for the pattern and add them to the result.
                    if let Some(item_type) = item_types.get(name) {
                        // Get the bindings for the pattern and add them to the result.
                        result.extend(
                            pattern
                                .get_bindings(
                                    &expr.clone().field(ConstExpr::Symbol(name.clone())),
                                    item_type,
                                    env,
                                )?
                                .into_iter(),
                        )
                    } else {
                        // If the struct does not have a field with the given name, then
                        // return an error.
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
                    }
                }
                result
            }

            // If the pattern is a symbol, then bind the symbol to the expression's type
            (Self::Symbol(name), ty) => {
                let mut result = HashMap::new();
                // Add the symbol to the result with the type of the expression.
                result.insert(name.clone(), ty.clone());
                result
            }

            // If the pattern is a wildcard, then return an empty map (no bindings).
            (Self::Wildcard, _) | (Self::ConstExpr(_), _) => HashMap::new(),

            // If the pattern is an alternative, then get the bindings for each pattern
            (Self::Alt(patterns), _) => {
                let mut result = HashMap::new();
                // Iterate over the patterns.
                for (i, pattern) in patterns.iter().enumerate() {
                    // Get the bindings for the pattern and add them to the result.
                    let bindings = pattern.get_bindings(expr, ty, env)?;
                    // If this is the first pattern, then set the result to the bindings.
                    if i == 0 {
                        result = bindings;
                    } else if result != bindings {
                        // If the bindings for this pattern are different to the previous patterns,
                        // then return an error.
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
                    }
                }
                // Return the result.
                result
            }

            _ => return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone())),
        })
    }

    /// Does this pattern match the given expression?
    /// This function returns an expression which evaluates to true if the expression matches the pattern.
    fn matches(&self, expr: &Expr, ty: &Type, env: &Env) -> Result<Expr, Error> {
        Ok(match (self, ty) {
            // If the pattern is a tuple, and the type is a tuple, then
            // check if each element of the tuple matches the pattern.
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                // The result of the match expression.
                let mut result = Expr::ConstExpr(ConstExpr::Bool(true));

                // If the number of patterns does not match the number of members,
                // then return an error.
                if patterns.len() != item_types.len() {
                    return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
                }

                // Iterate over the patterns and types of the tuple.
                for (i, (pattern, item_type)) in patterns.iter().zip(item_types.iter()).enumerate()
                {
                    // Check if the pattern matches the element of the tuple.
                    result = result.and(pattern.matches(
                        &expr.clone().field(ConstExpr::Int(i as i32)),
                        item_type,
                        env,
                    )?);
                }

                // Return the result.
                result
            }

            // If the pattern is a struct, and the type is a struct, then
            // check if each field of the struct matches the pattern.
            (Self::Struct(patterns), Type::Struct(item_types)) => {
                // The result of the match expression.
                let mut result = Expr::ConstExpr(ConstExpr::Bool(true));

                // If the number of patterns does not match the number of members,
                // then return an error.
                if patterns.len() != item_types.len() {
                    return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
                }

                // Iterate over the field names and patterns of the struct.
                for (name, pattern) in patterns.iter() {
                    // If the struct has a field with the given name, then
                    // check if the pattern matches the field.
                    if let Some(item_type) = item_types.get(name) {
                        // Check if the pattern matches the field.
                        result = result.and(pattern.matches(
                            &expr.clone().field(ConstExpr::Symbol(name.clone())),
                            item_type,
                            env,
                        )?);
                    } else {
                        // If the struct does not have a field with the given name, then
                        // return an error.
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
                    }
                }

                // Return the result.
                result
            }

            // If the pattern is a symbol, then it will match any expression.
            (Self::Wildcard, _) | (Self::Symbol(_), _) => {
                // Return true (the symbol will match any expression).
                Expr::ConstExpr(ConstExpr::Bool(true))
            }

            // If the pattern is a constant expression, it will match any expression
            // which is equal to the constant expression.
            (Self::ConstExpr(const_expr), _) => {
                expr.clone().eq(Expr::ConstExpr(const_expr.clone()))
            }

            // If the pattern is an alternative, then check if any of the patterns match.
            (Self::Alt(patterns), _) => {
                // The result of the match expression.
                let mut result = Expr::ConstExpr(ConstExpr::Bool(false));

                // Iterate over the patterns.
                for pattern in patterns.iter() {
                    // Check if the pattern matches the expression.
                    // If any of the patterns match, then the alternative matches.
                    result = result.or(pattern.matches(expr, ty, env)?);
                }

                // Return the result.
                result
            }

            _ => return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone())),
        })
    }

    /// Bind the pattern to the given expression.
    ///
    /// This will take the given `ret` expression, and return a new expression
    /// which will evaluate to the same value as `ret`, but with the pattern bound
    /// in the environment.
    ///
    /// Binding a pattern simply means that each `Symbol` in the pattern will be
    /// added to the environment, and will be bound to the corresponding value in
    /// the expression which is being matched.
    fn bind(&self, expr: &Expr, ty: &Type, ret: &Expr, env: &Env) -> Result<Expr, Error> {
        Ok(match (self, ty) {
            // If the pattern is a tuple and the type is a tuple, then
            // bind each pattern to the corresponding element of the tuple.
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                // The result of the binding.
                let mut result = ret.clone();
                // Iterate over the patterns and types of the tuple.
                for (i, (pattern, item_type)) in patterns.iter().zip(item_types.iter()).enumerate()
                {
                    // Bind the sub-pattern to the element of the tuple.
                    result = pattern.bind(
                        &expr.clone().field(ConstExpr::Int(i as i32)),
                        item_type,
                        &result,
                        env,
                    )?;
                }
                // Return the result.
                result
            }

            // If the pattern is a struct and the type is a struct, then
            // bind each pattern to the corresponding field of the struct.
            (Self::Struct(patterns), Type::Struct(item_types)) => {
                // The result of the binding.
                let mut result = ret.clone();
                // Iterate over the field names and patterns of the struct.
                for (name, pattern) in patterns.iter() {
                    // If the struct has a field with the given name, then
                    // bind the sub-pattern to the field.
                    if let Some(item_type) = item_types.get(name) {
                        // Bind the sub-pattern to the field.
                        result = pattern.bind(
                            &expr.clone().field(ConstExpr::Symbol(name.clone())),
                            item_type,
                            &result,
                            env,
                        )?;
                    } else {
                        // If the struct does not have a field with the given name, then
                        // return an error.
                        return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
                    }
                }

                // Return the result.
                result
            }

            // If the pattern is a symbol, then bind the symbol to the expression.
            (Self::Symbol(name), ty) => {
                Expr::let_var(name.clone(), Some(ty.clone()), expr.clone(), ret.clone())
            }

            // If the pattern is a wildcard, then it will not add any bindings.
            (Self::Wildcard, _) | (Self::ConstExpr(_), _) => ret.clone(),

            // If the pattern is an alternative, then bind the first pattern.
            // All their bindings will be the same, so it doesn't matter which one
            // we choose.
            (Self::Alt(patterns), _) => {
                let mut result = ret.clone();
                // Bind the first pattern.
                for pattern in patterns.iter() {
                    result = pattern.bind(expr, ty, &result, env)?;
                    break;
                }
                result
            }

            _ => return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone())),
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
