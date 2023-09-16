use crate::lir::*;
use core::fmt::{Display, Formatter, Result as FmtResult};
use std::collections::HashMap;

use log::trace;

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
    Variant(String, Option<Box<Pattern>>),
    Symbol(Mutability, String),
    ConstExpr(ConstExpr),
    Alt(Vec<Pattern>),
    Pointer(Box<Pattern>),
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
    pub fn sym(mutable: impl Into<Mutability>, name: impl ToString) -> Self {
        Self::Symbol(mutable.into(), name.to_string())
    }
    /// Construct a new pattern which matches a constant integer.
    pub fn int(n: i64) -> Self {
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
    /// Construct a new pattern which matches a pointer.
    pub fn pointer(pattern: Pattern) -> Self {
        Self::Pointer(Box::new(pattern))
    }

    /// Get the type of a branch with a given expression matched to this pattern.
    pub fn get_branch_result_type(
        &self,
        expr: &Expr,
        branch: &Expr,
        env: &Env,
    ) -> Result<Type, Error> {
        // Get the type of the expression being matched.
        let ty = expr.get_type(env)?.simplify_until_concrete(env)?;
        // Get the bindings for the pattern.
        let bindings = self.get_bindings(expr, &ty, env)?;
        // Create a new environment with the bindings.
        let mut new_env = env.clone();
        for (var, (mutabilty, ty)) in bindings {
            // Define the variables in the new environment.
            new_env.define_var(var, mutabilty, ty)?;
        }

        // Get the type of the branch.
        branch.get_type(&new_env)
    }

    /// This associated function returns whether or not a set of patterns is exhaustive,
    /// that is, whether or not it matches all possible values of a given type.
    /// This is used to check if a `match` expression is exhaustive.
    pub fn are_patterns_exhaustive(
        expr: &Expr,
        patterns: &[Pattern],
        matching_expr_ty: &Type,
        env: &Env,
    ) -> Result<bool, Error> {
        let matching_expr_ty = &matching_expr_ty.simplify_until_concrete(env)?;
        match matching_expr_ty {
            Type::Bool => {
                // If the type is a boolean, the patterns are exhaustive if they match both `true` and `false`.
                let mut true_found = false;
                let mut false_found = false;

                // Check if the patterns match `true` and `false`.
                for pattern in patterns {
                    match pattern {
                        // If there's a pattern which matches `true`, set `true_found` to true.
                        Pattern::ConstExpr(ConstExpr::Bool(true)) => true_found = true,
                        // If there's a pattern which matches `false`, set `false_found` to true.
                        Pattern::ConstExpr(ConstExpr::Bool(false)) => false_found = true,
                        // If there's a wildcard or a symbol, set both `true_found` and `false_found` to true.
                        Pattern::Wildcard | Pattern::Symbol(_, _) => {
                            true_found = true;
                            false_found = true;
                        }
                        // Check if the alternate pattern branches are exhaustive.
                        Pattern::Alt(branches) => {
                            // If there's an alternate pattern, check if it's exhaustive.
                            if Self::are_patterns_exhaustive(expr, branches, matching_expr_ty, env)?
                            {
                                // If it is exhaustive, set both `true_found` and `false_found` to true.
                                true_found = true;
                                false_found = true;
                            }
                        }
                        _ => {}
                    }
                }

                // Return whether or not both `true` and `false` were matched.
                Ok(true_found && false_found)
            }

            // Confirm all the variants of a tagged enum are matched.
            Type::EnumUnion(variants) => {
                let mut found = vec![false; variants.len()];
                for pattern in patterns {
                    match pattern {
                        Pattern::Variant(name, None) => {
                            // Find the index of the variant.
                            if let Some(index) = variants.keys().position(|item| *item == *name) {
                                // Set the corresponding boolean to true.
                                found[index] = true;
                            }
                        }
                        Pattern::Variant(name, Some(p)) => {
                            // Find the index of the variant.
                            if let Some(index) = variants.keys().position(|item| *item == *name) {
                                // Set the corresponding boolean to true.
                                found[index] = found[index]
                                    || Self::are_patterns_exhaustive(
                                        expr,
                                        &[*p.clone()],
                                        &variants[name],
                                        env,
                                    )?;
                            }
                        }

                        // If this pattern matches a wildcard or a symbol, set all the booleans to true.
                        Pattern::Symbol(_, _) | Pattern::Wildcard => {
                            for found_item in found.iter_mut().take(variants.len()) {
                                *found_item = true;
                            }
                        }
                        // Check if the alternate pattern branches are exhaustive.
                        Pattern::Alt(branches) => {
                            // If there's an alternate pattern, check if it's exhaustive.
                            if Self::are_patterns_exhaustive(expr, branches, matching_expr_ty, env)?
                            {
                                // If it is exhaustive, set all the booleans to true.
                                for found_item in found.iter_mut().take(variants.len()) {
                                    *found_item = true;
                                }
                            }
                        }

                        _ => {}
                    }
                }
                Ok(found.iter().all(|b| *b))
            }

            // Confirm all the variants of an enum are matched.
            Type::Enum(items) => {
                // Create a vector of booleans, one for each variant.
                let mut found = vec![false; items.len()];
                // Iterate over the patterns.
                for pattern in patterns {
                    match pattern {
                        Pattern::Variant(name, _) => {
                            // Find the index of the variant.
                            if let Some(index) = items.iter().position(|item| *item == *name) {
                                // Set the corresponding boolean to true.
                                found[index] = true;
                            }
                        }

                        // If this pattern matches a variant, set the corresponding boolean to true.
                        Pattern::ConstExpr(ConstExpr::Of(ty, name)) => {
                            // Confirm the type of the expression matches the type of the enum.
                            if ty.can_decay_to(matching_expr_ty, env)? {
                                // Find the index of the variant.
                                if let Some(index) = items.iter().position(|item| *item == *name) {
                                    // Set the corresponding boolean to true.
                                    found[index] = true;
                                }
                            } else {
                                // If the types don't match (the pattern doesn't match the matched expression's type), return an error.
                                return Err(Error::MismatchedTypes {
                                    expected: matching_expr_ty.clone(),
                                    found: ty.clone(),
                                    expr: expr.clone(),
                                });
                            }
                        }
                        // If this pattern matches a wildcard or a symbol, set all the booleans to true.
                        Pattern::Symbol(_, _) | Pattern::Wildcard => {
                            for found_item in found.iter_mut().take(items.len()) {
                                *found_item = true;
                            }
                        }
                        // Check if the alternate pattern branches are exhaustive.
                        Pattern::Alt(branches) => {
                            // If there's an alternate pattern, check if it's exhaustive.
                            if Self::are_patterns_exhaustive(expr, branches, matching_expr_ty, env)?
                            {
                                // If it is exhaustive, set all the booleans to true.
                                for found_item in found.iter_mut().take(items.len()) {
                                    *found_item = true;
                                }
                            }
                        }
                        _ => {}
                    }
                }

                // Return whether or not all the variants are exhaustively matched.
                Ok(found.iter().all(|b| *b))
            }

            // Confirm all the fields of a tuple are matched.
            Type::Tuple(items) => {
                // Create a vector of booleans, one for each field.
                let mut found = vec![false; items.len()];

                // Iterate over the patterns.
                for pattern in patterns {
                    match pattern {
                        // If this pattern matches a tuple, recursively check if the patterns are exhaustive.
                        Pattern::Tuple(patterns) => {
                            // Iterate over the patterns.
                            let mut all_found = true;
                            for (i, pattern) in patterns.iter().enumerate() {
                                // If the subpattern is non-exhaustive, set `all_found` to false.
                                if !Self::are_patterns_exhaustive(
                                    expr,
                                    &[pattern.clone()],
                                    &items[i],
                                    env,
                                )? {
                                    all_found = false;
                                }
                            }

                            // If all the subpatterns are exhaustive, set all the booleans to true.
                            if all_found {
                                for i in 0..items.len() {
                                    if let Some(found) = found.get_mut(i) {
                                        *found = true;
                                    }
                                }
                            }
                        }
                        // If this pattern matches a wildcard or a symbol, set all the booleans to true.
                        Pattern::Wildcard | Pattern::Symbol(_, _) => {
                            for i in 0..items.len() {
                                if let Some(found) = found.get_mut(i) {
                                    *found = true;
                                }
                            }
                        }
                        // Check if the alternate pattern branches are exhaustive.
                        Pattern::Alt(branches) => {
                            // If there's an alternate pattern, check if it's exhaustive.
                            if Self::are_patterns_exhaustive(expr, branches, matching_expr_ty, env)?
                            {
                                // If it is exhaustive, set all the booleans to true.
                                for i in 0..items.len() {
                                    if let Some(found) = found.get_mut(i) {
                                        *found = true;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }

                // Return whether or not all the fields are exhaustively matched.
                Ok(found.iter().all(|b| *b))
            }

            // Confirm all the members of a struct are matched.
            Type::Struct(members) => {
                // Create a vector of booleans, one for each member.
                let mut found = vec![false; members.len()];

                // Iterate over the patterns.
                for pattern in patterns {
                    match pattern {
                        // If this pattern matches a struct, recursively check if the patterns are exhaustive.
                        Pattern::Struct(patterns) => {
                            // Iterate over the patterns.
                            for (name, pattern) in patterns.iter() {
                                if let Some(index) =
                                    members.iter().position(|member| *member.0 == *name)
                                {
                                    if let Some(found) = found.get_mut(index) {
                                        // If the pattern is exhaustive, set the corresponding boolean to true.
                                        if Self::are_patterns_exhaustive(
                                            expr,
                                            &[pattern.clone()],
                                            &members[name],
                                            env,
                                        )? {
                                            *found = true;
                                        }
                                    }
                                }
                            }
                        }
                        // If this pattern matches a wildcard or a symbol, set all the booleans to true.
                        Pattern::Wildcard | Pattern::Symbol(_, _) => {
                            for i in 0..members.len() {
                                if let Some(found) = found.get_mut(i) {
                                    *found = true;
                                }
                            }
                        }
                        // Check if the alternate pattern branches are exhaustive.
                        Pattern::Alt(branches) => {
                            // If there's an alternate pattern, check if it's exhaustive.
                            if Self::are_patterns_exhaustive(expr, branches, matching_expr_ty, env)?
                            {
                                // If it is exhaustive, set all the booleans to true.
                                for i in 0..members.len() {
                                    if let Some(found) = found.get_mut(i) {
                                        *found = true;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }

                // Return whether or not all the members are exhaustively matched.
                Ok(found.iter().all(|b| *b))
            }

            // For any other type, only a default pattern is exhaustive.
            _ => {
                for pattern in patterns {
                    match pattern {
                        Pattern::Wildcard | Pattern::Symbol(_, _) => return Ok(true),
                        Pattern::Alt(branches) => {
                            // If there's an alternate pattern, check if it's exhaustive.
                            if Self::are_patterns_exhaustive(expr, branches, matching_expr_ty, env)?
                            {
                                // If it is exhaustive, return `true`.
                                return Ok(true);
                            }
                        }
                        _ => {}
                    }
                }
                Ok(false)
            }
        }
    }

    /// Type-check a pattern match of an expression against this pattern,
    /// and type-check the branch where the expression is bound to the pattern.
    pub fn type_check(&self, matching_expr: &Expr, branch: &Expr, env: &Env) -> Result<(), Error> {
        trace!("Type checking pattern match: {} => {}", self, branch);
        // Get the type of the expression being matched.
        let matching_ty = matching_expr.get_type(env)?.simplify_until_concrete(env)?;
        // Get the type of the branch as a result of the match.
        let expected = self.get_branch_result_type(matching_expr, branch, env)?;
        // Type-check the expression generated to match the pattern.
        match self
            .matches(matching_expr, &matching_ty, env)
            .and_then(|x| x.type_check(env))
        {
            Ok(()) => {}
            Err(Error::InvalidBinaryOp(_, a, b)) => {
                let expected = a.get_type(env)?;
                let found = b.get_type(env)?;
                return Err(Error::MismatchedTypes {
                    expected,
                    found,
                    expr: Expr::Match(Box::new(matching_expr.clone()), vec![(self.clone(), branch.clone())]),
                });
            },
            Err(Error::InvalidBinaryOpTypes(_, expected, found)) => {
                return Err(Error::MismatchedTypes {
                    expected,
                    found,
                    expr: Expr::Match(Box::new(matching_expr.clone()), vec![(self.clone(), branch.clone())]),
                })
            },
            _ => return Err(Error::MismatchedTypes {
                expected,
                found: matching_ty,
                expr: Expr::Match(Box::new(matching_expr.clone()), vec![(self.clone(), branch.clone())])
            })
            // Err(e) => return Err(Error::InvalidPatternForExpr(matching_expr.clone(), self.clone())),
        }
        // Generate an expression with the bindings defined for the branch.
        let result_expr = self.bind(matching_expr, &matching_ty, branch, env).map_err(|_| Error::InvalidPatternForExpr(matching_expr.clone(), self.clone()))?;
        // Type-check the expression generated to bind the pattern.
        result_expr.type_check(env)?;
        // Get the type of the expression generated to bind the pattern.
        let found = result_expr.get_type(env)?;
        // It should be the same as the type of the branch.
        if !expected.can_decay_to(&found, env)? {
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
            Box::new(self.bind(&var, &ty, then, env)?),
            // Otherwise, evaluate the `else_` expression.
            Box::new(else_.clone()),
        );
        // Create a new environment with the bindings and evaluate the `if let` expression.
        Ok(Expr::let_var(
            var_name,
            Mutability::Immutable,
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
        // Get the type of the expression being matched
        let match_type = expr.get_type(env)?.simplify_until_concrete(env)?;
        // Define the variable in the new environment.
        // for _ in 0..Type::SIMPLIFY_RECURSION_LIMIT {
        //     match match_type {
        //         Type::Let(_, _, _) | Type::Symbol(_) => {
        //             match_type = match_type.simplify(env)?;
        //         }
        //         _ => break,
        //     }
        // }
        new_env.define_var(var_name.clone(), Mutability::Immutable, match_type.clone())?;
        // Generate the expression which evaluates the `match` expression.
        let match_expr = Pattern::match_pattern_helper(&Expr::var(&var_name), branches, &new_env)?;

        // Define the variable in the generated expression.
        let result = Expr::let_var(var_name, Mutability::Immutable, Some(match_type), expr.clone(), match_expr);
        Ok(result)
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
        for (pattern, ret) in branches.iter().rev() {
            // An expression which evaluates to true if the expression matches the pattern.
            let cond = pattern.matches(expr, &ty, env)?;
            // A new if statement which checks if the expression matches the pattern.
            result = Expr::If(
                Box::new(cond),
                Box::new(pattern.bind(expr, &ty, ret, env)?),
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
    ) -> Result<HashMap<String, (Mutability, Type)>, Error> {
        let ty = &ty.simplify_until_concrete(env)?;
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
                                &expr.clone().field(ConstExpr::Int(i as i64)),
                                item_type,
                                env,
                            )?
                            .into_iter(),
                    )
                }
                // Return the result.
                result
            }

            // If the pattern is a variant, and the type is a EnumUnion,
            // get the bindings for the variant.
            (Self::Variant(name, Some(pattern)), Type::EnumUnion(variants)) => {
                // If the variant is not found in the type, throw an error
                if !variants.contains_key(name) {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
                // If no error was thrown, the variant is an option which can be matched.
                // Now, check if the tag matches the variant.
                pattern.get_bindings(
                    &expr
                        .clone()
                        .unop(super::ops::Data)
                        .field(ConstExpr::Symbol(name.clone())),
                    &variants[name],
                    env,
                )?
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
            (Self::Symbol(mutability, name), ty) => {
                let mut result = HashMap::new();
                // Add the symbol to the result with the type of the expression.
                result.insert(name.clone(), (*mutability, ty.clone()));
                result
            }

            // If the pattern is a wildcard, then return an empty map (no bindings).
            (Self::Variant(_, None), Type::Enum(_))
            | (Self::Variant(_, None), Type::EnumUnion(_))
            | (Self::Wildcard, _)
            | (Self::ConstExpr(_), _) => HashMap::new(),

            (Self::Pointer(pattern), Type::Pointer(_, item_type)) => {
                pattern.get_bindings(&expr.clone().deref(), item_type, env)?
            }

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

            (_pat, _ty) => {
                // If the pattern does not match the type, then return an error.
                return Err(Error::InvalidPatternForExpr(expr.clone(), self.clone()));
            }
        })
    }

    /// Does this pattern match the given expression?
    /// This function returns an expression which evaluates to true if the expression matches the pattern.
    fn matches(&self, expr: &Expr, ty: &Type, env: &Env) -> Result<Expr, Error> {
        let ty = &ty.simplify_until_concrete(env)?;
        Ok(match (self, ty) {
            // If the pattern is a variant, and the type is a EnumUnion,
            // check if the variant matches the pattern.
            (Self::Variant(name, Some(pattern)), Type::EnumUnion(variants)) => {
                // If the variant is not found in the type, throw an error
                if !variants.contains_key(name) {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
                // If no error was thrown, the variant is an option which can be matched.
                // Now, check if the tag matches the variant.
                expr.clone()
                    .unop(super::ops::Tag)
                    .eq(ConstExpr::Of(
                        Type::Enum(variants.clone().into_keys().collect()),
                        name.clone(),
                    ))
                    .and(
                        pattern.matches(
                            &expr
                                .clone()
                                .unop(super::ops::Data)
                                .field(ConstExpr::Symbol(name.clone())),
                            &variants[name].clone().simplify(env)?,
                            env,
                        )?,
                    )
            }

            (Self::Variant(name, None), Type::EnumUnion(variants)) => {
                // If the variant is not found in the type, throw an error
                if !variants.contains_key(name) {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
                // If no error was thrown, the variant is an option which can be matched.
                // Now, check if the tag matches the variant.
                expr.clone().unop(super::ops::Tag).eq(ConstExpr::Of(
                    Type::Enum(variants.clone().into_keys().collect()),
                    name.clone(),
                ))
            }

            (Self::Variant(name, None), Type::Enum(items)) => {
                // If the variant is not found in the type, throw an error
                if !items.contains(name) {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
                // If no error was thrown, the variant is an option which can be matched.
                // Now, check if the tag matches the variant.
                expr.clone().eq(ConstExpr::Of(ty.clone(), name.clone()))
            }

            // If the pattern is a tuple, and the type is a tuple, then
            // check if each element of the tuple matches the pattern.
            (Self::Tuple(patterns), Type::Tuple(item_types)) => {
                // The result of whether or not this expression matches
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
                        &expr.clone().field(ConstExpr::Int(i as i64)),
                        item_type,
                        env,
                    )?);
                }

                // Return the result.
                result
            }

            (Self::Pointer(pattern), Type::Pointer(_, item_type)) => {
                pattern.matches(&expr.clone().deref(), item_type, env)?
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
            (Self::Wildcard, _)
            | (Self::Symbol(_, _), _)
            | (Self::ConstExpr(ConstExpr::None), Type::None) => {
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
        let ty = &ty.simplify_until_concrete(env)?;
        Ok(match (self, ty) {
            // If the pattern is a variant, and the type is a tagged union,
            // bind the pattern to the corresponding variant type in the tagged union.
            (Self::Variant(name, Some(pattern)), Type::EnumUnion(variants)) => {
                // Get the inner variant type from the tagged union
                if let Some(variant_ty) = variants.get(name) {
                    // Bind the inner expression of the tagged union
                    pattern.bind(
                        &expr
                            .clone()
                            .unop(super::ops::Data)
                            .field(ConstExpr::Symbol(name.clone())),
                        variant_ty,
                        ret,
                        env,
                    )?
                } else {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
            }

            // If the pattern is a variant, and the type is a tagged union,
            // but there is no pattern to bind, simply error check.
            (Self::Variant(name, None), Type::EnumUnion(variants)) => {
                // Get the inner variant type from the tagged union
                if variants.get(name).is_none() {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
                ret.clone()
            }

            // If the pattern is a variant, and the type is an enum,
            // simply error check (there is no pattern to bind).
            (Self::Variant(name, None), Type::Enum(items)) => {
                // Get the inner variant type from the tagged union
                if !items.contains(name) {
                    return Err(Error::VariantNotFound(ty.clone(), name.clone()));
                }
                ret.clone()
            }

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
                        &expr.clone().field(ConstExpr::Int(i as i64)),
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
            (Self::Symbol(mutability, name), ty) => {
                Expr::let_var(name.clone(), *mutability, Some(ty.clone()), expr.clone(), ret.clone())
            }

            (Self::Pointer(pattern), Type::Pointer(_, item_type)) => {
                pattern.bind(&expr.clone().deref(), item_type, ret, env)?
            }

            // If the pattern is a wildcard, then it will not add any bindings.
            (Self::Wildcard, _) | (Self::ConstExpr(_), _) => ret.clone(),

            // If the pattern is an alternative, then bind the first pattern.
            // All their bindings will be the same type, so it doesn't matter which one
            // we choose.
            (Self::Alt(patterns), _) => {
                // Bind the first pattern.
                patterns.first().map(|x| x.bind(expr, ty, ret, env)).unwrap_or(Err(Error::InvalidPatternForExpr(expr.clone(), self.clone())))?
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

            Self::Variant(name, pattern) => match pattern {
                Some(p) => write!(f, "{name} {p}"),
                None => write!(f, "{name}"),
            },

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

            Self::Pointer(ptr) => write!(f, "&{}", ptr),
            Self::Symbol(mutability, name) => {
                if mutability.is_mutable() {
                    write!(f, "mut ")?;
                }
                write!(f, "{name}")
            },

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
