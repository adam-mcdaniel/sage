//! # Expression
//!
//! An expression is a runtime value.
//!
//! Expressions are compiled down into equivalent assembly code
//! which are then executed by the runtime.

use super::ops::*;
use crate::lir::{
    Annotation, ConstExpr, Declaration, Env, Error, GetType, Mutability, Pattern, Procedure, Type,
};
use core::fmt;
use std::{
    collections::BTreeMap,
    hash::{Hash, Hasher},
};

use log::*;
use serde_derive::{Deserialize, Serialize};

/// TODO: Add variants for `LetProc`, `LetVar`, etc. to support multiple definitions.
///       This way, we don't overflow the stack with several clones of the environment.
/// A runtime expression.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expr {
    /// An expression along with data about its source code location.
    /// This is used for error reporting.
    Annotated(Box<Self>, Annotation),

    /// A constant expression.
    ConstExpr(ConstExpr),
    /// A block of expressions. The last expression in the block is the value of the block.
    Many(Vec<Self>),

    /// Declare any number of variables, procedures, types, or constants.
    /// Evaluate a sub-expression in that scope.
    Declare(Box<Declaration>, Box<Self>),

    /// Create a while loop: while the first expression evaluates to true, evaluate the second expression.
    While(Box<Self>, Box<Self>),
    /// An if-then-else expression.
    ///
    /// Evaluate a condition.
    /// If the condition is true, evaluate the first expression.
    /// Otherwise, evaluate the second expression.
    If(Box<Self>, Box<Self>, Box<Self>),
    /// A constant, compile time if-then-else expression.
    ///
    /// Evaluate the condition as a constant expression.
    /// If the condition is true, then this `when` expression is replaced with the first expression.
    /// Otherwise, this `when` expression is replaced with the second expression.
    When(ConstExpr, Box<Self>, Box<Self>),

    /// A match expression.
    Match(Box<Self>, Vec<(Pattern, Self)>),
    /// An if-let expression.
    ///
    /// This attempts to match an expression against a pattern,
    /// and if it succeeds, evaluates the body expression with
    /// the matched variables in scope.
    /// If the match fails, the else expression is evaluated.
    IfLet(Pattern, Box<Self>, Box<Self>, Box<Self>),

    /// Perform a unary operation on two expressions.
    UnaryOp(String, Box<Self>),
    /// Perform a binary operation on two expressions.
    BinaryOp(String, Box<Self>, Box<Self>),
    /// Perform a ternary operation on three expressions.
    TernaryOp(String, Box<Self>, Box<Self>, Box<Self>),
    /// Perform an assignment operation on two expressions.
    AssignOp(String, Box<Self>, Box<Self>),

    /// Reference this expression (i.e. get a pointer to it).
    Refer(Mutability, Box<Self>),
    /// Dereference this expression (i.e. get the value it points to).
    Deref(Box<Self>),
    /// Store an expression to an address (a pointer).
    DerefMut(Box<Self>, Box<Self>),

    /// Apply a function with some arguments.
    Apply(Box<Self>, Vec<Self>),

    /// Return a value from a function.
    Return(Box<Self>),

    /// An array of expressions.
    Array(Vec<Self>),
    /// A tuple of expressions.
    Tuple(Vec<Self>),
    /// A union: a collection of named fields.
    /// The `Type` value is the type of the union.
    /// The `String` field is the field the union is being initialized with.
    /// The `Box<Self>` field is the value of the field we want to initialize.
    Union(Type, String, Box<Self>),
    /// A tagged union: a typechecked union of different variants.
    /// The `Type` value is the type of the tagged union.
    /// The `String` field is the variant the tagged union is being initialized with.
    /// The `Box<Self>` field is the value of the union's data we want to initialize.
    EnumUnion(Type, String, Box<Self>),

    /// A structure of fields to expressions.
    Struct(BTreeMap<String, Self>),

    /// Cast an expression to another type.
    As(Box<Self>, Type),

    /// Get a field or member from a structure, union, or tuple.
    /// For tuples, use an `Int` constant expression to access the nth field (zero indexed).
    /// For unions or structures, use a `Symbol` constant expression to access the field.
    ///
    /// Do NOT instantiate this directly: use the `.field` method on the container expression.
    Member(Box<Self>, ConstExpr),
    /// Index an array or pointer with an expression that evaluates to an `Int` at runtime.
    Index(Box<Self>, Box<Self>),
}

impl From<ConstExpr> for Expr {
    fn from(c: ConstExpr) -> Self {
        Expr::ConstExpr(c)
    }
}

impl Expr {
    /// A constant expression that evaluates to `None`.
    /// This constant is defined so that we don't have to write `Expr::ConstExpr`
    /// every time we want to use `None`.
    pub const NONE: Self = Self::ConstExpr(ConstExpr::None);

    pub fn print(self) -> Self {
        self.unop(Put::Display)
    }

    pub fn println(self) -> Self {
        Self::Many(vec![
            self.clone().print(),
            Self::ConstExpr(ConstExpr::Char('\n')).print(),
        ])
    }

    pub fn is_method_call(&self, env: &Env) -> Result<bool, Error> {
        let result = match self {
            Self::Annotated(inner, annotation) => {
                return inner
                    .is_method_call(env)
                    .map_err(|e| e.annotate(annotation.clone()))
            }

            Self::Apply(fun, args) => {
                match *fun.clone() {
                    Self::Annotated(inner, annotation) => {
                        return Self::Apply(inner, args.clone())
                            .is_method_call(env)
                            .map_err(|e| e.annotate(annotation.clone()))
                    }

                    Self::Member(val, name) => {
                        let val_type = val.get_type(env)?;
                        // val_type.add_monomorphized_associated_consts(env)?;
                        // Is this an associated function?
                        match val_type {
                            Type::Type(_) => false,
                            _ => {
                                if let Ok(name) = name.as_symbol(env) {
                                    env.has_associated_const(&val_type, &name)
                                } else {
                                    false
                                }
                            }
                        }
                    }
                    Self::ConstExpr(ConstExpr::Monomorphize(template, _ty_args)) => {
                        match *template {
                            ConstExpr::Member(val, name) => {
                                let val_type = val.get_type(env)?;
                                // val_type.add_monomorphized_associated_consts(env)?;
                                // Is this an associated function?
                                match val_type {
                                    Type::Type(_) => false,
                                    _ => {
                                        if let Ok(name) = name.as_symbol(env) {
                                            env.has_associated_const(&val_type, &name)
                                        } else {
                                            false
                                        }
                                    }
                                }
                            }
                            _ => false,
                        }
                    }

                    Self::ConstExpr(ConstExpr::Member(val, name)) => {
                        let val_type = val.get_type(env)?;
                        // val_type.add_monomorphized_associated_consts(env)?;
                        // Is this an associated function?
                        match val_type {
                            Type::Type(_) => false,
                            _ => {
                                if let Ok(name) = name.as_symbol(env) {
                                    env.has_associated_const(&val_type, &name)
                                } else {
                                    false
                                }
                            }
                        }
                    }
                    _ => false,
                }
            }

            _ => false,
        };
        debug!("is_method_call: {self} = {result}");
        Ok(result)
    }

    pub fn transform_method_call(&self, env: &Env) -> Result<Self, Error> {
        debug!("transform_method_call: {self}");

        let result = match self {
            Self::Annotated(inner, metadata) => inner
                .transform_method_call(env)
                .map_err(|e| e.annotate(metadata.clone())),
            Self::Apply(f, args) => {
                match *f.clone() {
                    Self::Annotated(inner, metadata) => Self::Apply(inner, args.clone())
                        .transform_method_call(env)
                        .map_err(|e| e.annotate(metadata.clone())),

                    Self::ConstExpr(ConstExpr::Monomorphize(template, ty_args)) => {
                        debug!("transform_method_call: template={template} -- ty_args={ty_args:?}");
                        match *template {
                            ConstExpr::Member(val, member) => {
                                // Apply the type arguments to the method.
                                let name = member.as_symbol(env)?;

                                // Check if the value actually has a member with this name.
                                let val_type = val.get_type(env)?;
                                // val_type.add_monomorphized_associated_consts(env)?;
                                trace!(target: "member", "got value type: {:#?}: {val}.{name}", val_type);
                                // If the value has a member with this name,
                                trace!(target: "member", "WHOOP WHOOP");
                                let (mut associated_function, mut associated_function_type) = env
                                    .get_associated_const(&val_type, &name)
                                    .ok_or_else(|| {
                                        error!(target: "member", "Symbol not defined: {name} while getting member");
                                        Error::SymbolNotDefined(name.clone())
                                })?;
                                // .monomorphize(ty_args.clone());
                                associated_function =
                                    associated_function.monomorphize(ty_args.clone());
                                associated_function_type =
                                    associated_function_type.apply(ty_args.clone());
                                // let associated_function = ConstExpr::Type(val_type.clone())
                                //     .field(*member)
                                //     .monomorphize(ty_args.clone());
                                trace!(target: "member", "function value: {associated_function} in {env}");
                                // Get the type of the function
                                // let associated_function_type = env
                                //     .get_type_of_associated_const(&val_type, &name)
                                //     .ok_or_else(|| Error::SymbolNotDefined(name))?
                                //     .apply(ty_args);
                                // trace!(target: "member", "got function type: {}", associated_function_type);

                                let mut new_args = vec![];
                                if let Some(expected_mutability) =
                                    associated_function_type.get_self_param_mutability(env)
                                {
                                    if val_type.can_decay_to(
                                        &Type::Pointer(expected_mutability, Type::Any.into()),
                                        env,
                                    )? {
                                        trace!(target: "member", "decaying {val} to {expected_mutability} pointer");
                                        new_args.push(Expr::ConstExpr(*val.clone()));
                                    } else {
                                        trace!(target: "member", "referencing {val} as {expected_mutability} pointer");
                                        new_args.push(
                                            Expr::ConstExpr(*val.clone())
                                                .refer(expected_mutability),
                                        );
                                    }
                                } else {
                                    trace!(target: "member", "passing by value {val}");
                                    if val_type.can_decay_to(
                                        &Type::Pointer(Mutability::Any, Type::Any.into()),
                                        env,
                                    )? {
                                        new_args.push(Expr::ConstExpr(*val.clone()).deref());
                                    } else {
                                        new_args.push(Expr::ConstExpr(*val.clone()));
                                    }
                                }
                                new_args.extend(args.clone());
                                Ok(Self::Apply(
                                    Expr::ConstExpr(associated_function).into(),
                                    new_args,
                                ))
                            }
                            _ => Ok(Self::Apply(
                                Expr::ConstExpr(ConstExpr::Monomorphize(
                                    template.clone(),
                                    ty_args.clone(),
                                ))
                                .into(),
                                args.clone(),
                            )),
                        }
                    }

                    Self::ConstExpr(ConstExpr::Member(val, member)) => {
                        let name = member.as_symbol(env);
                        if self.is_method_call(env)? {
                            let name = name?;
                            // Check if the value actually has a member with this name.
                            let val_type = val.get_type(env)?;
                            // val_type.add_monomorphized_associated_consts(env)?;
                            trace!(target: "member", "got value type: {:#?}: {val}.{name}", val_type);
                            // If the value has a member with this name,
                            trace!(target: "member", "WHOOP WHOOP");

                            let (associated_function, associated_function_type) = env
                                .get_associated_const(&val_type, &name)
                                .ok_or_else(|| {
                                    error!(target: "member", "Symbol not defined: {name} while getting member");
                                    Error::SymbolNotDefined(name.clone())
                                })?;

                            // let associated_function = env
                            //     .get_associated_const(&val_type, &name)
                            //     .ok_or_else(|| Error::SymbolNotDefined(name.clone()))?;
                            // let associated_function = ConstExpr::Type(val_type.clone())
                            //     .field(*member)
                            //     .monomorphize(ty_args.clone());
                            trace!(target: "member", "function value: {associated_function} in {env}");

                            // Get the type of the function
                            // let associated_function_type = env
                            //     .get_type_of_associated_const(&val_type, &name)
                            //     .ok_or_else(|| Error::SymbolNotDefined(name))?;
                            trace!(target: "member", "got function type: {}", associated_function_type);
                            // Get the first argument's type
                            let mut new_args = vec![];
                            if let Some(expected_mutability) =
                                associated_function_type.get_self_param_mutability(env)
                            {
                                if val_type.can_decay_to(
                                    &Type::Pointer(expected_mutability, Type::Any.into()),
                                    env,
                                )? {
                                    trace!(target: "member", "decaying {val} to {expected_mutability} pointer");
                                    new_args.push(Expr::ConstExpr(*val.clone()));
                                } else {
                                    trace!(target: "member", "referencing {val} as {expected_mutability} pointer");
                                    new_args.push(
                                        Expr::ConstExpr(*val.clone()).refer(expected_mutability),
                                    );
                                }
                            } else {
                                trace!(target: "member", "passing by value {val}");
                                if val_type.can_decay_to(
                                    &Type::Pointer(Mutability::Any, Type::Any.into()),
                                    env,
                                )? {
                                    new_args.push(Expr::ConstExpr(*val.clone()).deref());
                                } else {
                                    new_args.push(Expr::ConstExpr(*val.clone()));
                                }
                            }
                            new_args.extend(args.clone());
                            Ok(Self::Apply(
                                Expr::ConstExpr(associated_function).into(),
                                new_args,
                            ))
                        } else {
                            // Compile it normally:
                            // Push the procedure on the stack.
                            Ok(self.clone())
                        }
                    }

                    Self::Member(val, member) => {
                        // Try to get the member of the underlying type.
                        let name = member.as_symbol(env);
                        if self.is_method_call(env)? {
                            let name = name?;
                            // Check if the value actually has a member with this name.
                            let val_type = val.get_type(env)?;
                            // val_type.add_monomorphized_associated_consts(env)?;
                            trace!(target: "member", "got value type: {:#?}: {val}.{name}", val_type);
                            // If the value has a member with this name,
                            trace!(target: "member", "WHOOP WHOOP");
                            let (associated_function, associated_function_type) = env
                                .get_associated_const(&val_type, &name)
                                .ok_or_else(|| {
                                    error!(target: "member", "Symbol not defined: {name} while getting member");
                                    Error::SymbolNotDefined(name.clone())
                                })?;
                            trace!(target: "member", "function value: {associated_function} in {env}");

                            // Get the type of the function
                            // let associated_function_type = env
                            //     .get_type_of_associated_const(&val_type, &name)
                            //     .ok_or_else(|| Error::SymbolNotDefined(name))?
                            //     .clone();
                            trace!(target: "member", "got function type: {}", associated_function_type);
                            // Get the first argument's type
                            let mut new_args = vec![];
                            if let Some(expected_mutability) =
                                associated_function_type.get_self_param_mutability(env)
                            {
                                if val_type.can_decay_to(
                                    &Type::Pointer(expected_mutability, Type::Any.into()),
                                    env,
                                )? {
                                    trace!(target: "member", "decaying {val} to {expected_mutability} pointer");
                                    new_args.push(*val.clone());
                                } else {
                                    trace!(target: "member", "referencing {val} as {expected_mutability} pointer");
                                    new_args.push(val.refer(expected_mutability));
                                }
                            } else {
                                trace!(target: "member", "passing by value {val}");
                                if val_type.can_decay_to(
                                    &Type::Pointer(Mutability::Any, Type::Any.into()),
                                    env,
                                )? {
                                    new_args.push(val.deref());
                                } else {
                                    new_args.push(*val.clone());
                                }
                            }
                            new_args.extend(args.clone());
                            Ok(Self::Apply(
                                Expr::ConstExpr(associated_function).into(),
                                new_args,
                            ))
                        } else {
                            // Compile it normally:
                            // Push the procedure on the stack.
                            Ok(self.clone())
                        }
                    }
                    _ => Err(Error::MemberNotFound(self.clone(), ConstExpr::None)),
                }
            }
            _ => Ok(self.clone()),
        }?;

        debug!("transform_method_call result: {}", result);
        Ok(result)
    }

    pub fn get_method_call_mutability(&self, env: &Env) -> Result<Option<Mutability>, Error> {
        match self {
            Self::Annotated(inner, annotation) => inner
                .get_method_call_mutability(env)
                .map_err(|e| e.annotate(annotation.clone())),

            Self::Apply(fun, args) => {
                let fun_type = fun.get_type(env)?;
                match *fun.clone() {
                    Self::Annotated(inner, annotation) => Self::Apply(inner, args.clone())
                        .get_method_call_mutability(env)
                        .map_err(|e| e.annotate(annotation.clone())),
                    Self::Member(_val, _name) => {
                        // Check if the value actually has a member with this name.
                        // let val_type = val.get_type(env)?;
                        Ok(fun_type.get_self_param_mutability(env))
                    }
                    Self::ConstExpr(ConstExpr::Member(_val, _name)) => {
                        // let val_type = val.get_type(env)?;
                        Ok(fun_type.get_self_param_mutability(env))
                    }
                    _ => Err(Error::MemberNotFound(self.clone(), ConstExpr::None)),
                }
            }

            _ => Err(Error::MemberNotFound(self.clone(), ConstExpr::None)),
        }
    }

    /// An annotated expression with some metadata.
    pub fn annotate(&self, annotation: impl Into<Annotation>) -> Self {
        match self {
            Self::Annotated(expr, metadata) => {
                let mut result = annotation.into();
                result |= metadata.clone();
                Self::Annotated(expr.clone(), result)
            }
            _ => Self::Annotated(Box::new(self.clone()), annotation.into()),
        }
    }

    /// Return this expression, but with a given declaration in scope.
    pub fn with(&self, older_decls: impl Into<Declaration>) -> Self {
        match self {
            // If the expression is an annotated expression, we need to unwrap it.
            Self::Annotated(expr, annotation) => {
                // Just unwrap the expression and recurse.
                expr.with(older_decls).annotate(annotation.clone())
            }

            // If the expression is a declaration, we need to merge the declarations.
            Self::Declare(younger_decls, expr) => {
                // Start with the older declarations.
                let mut result = older_decls.into();

                if let Declaration::Module(..) = result {
                    self.hard_with(result)
                } else {
                    // Add the younder declarations to the older declarations.
                    result.append(*younger_decls.clone());
                    // Return the merged declaration.
                    Self::Declare(Box::new(result), expr.clone())
                }
            }

            // Return the expression with the declaration in scope.
            _ => Self::Declare(Box::new(older_decls.into()), Box::new(self.clone())),
        }
    }

    pub fn hard_with(&self, older_decls: impl Into<Declaration>) -> Self {
        Self::Declare(Box::new(older_decls.into()), self.clone().into())
    }

    /// Get the size of an expression.
    pub fn size_of(self) -> Self {
        Self::ConstExpr(ConstExpr::SizeOfExpr(Box::new(self)))
    }

    /// Cast an expression as another type.
    pub fn as_type(self, t: Type) -> Self {
        Self::As(Box::new(self), t)
    }

    /// Apply a unary operation to this expression.
    pub fn unop(self, op: impl ToString) -> Self {
        Self::UnaryOp(op.to_string(), Box::new(self))
    }

    /// Logical not this expression.
    #[allow(clippy::should_implement_trait)]
    pub fn not(self) -> Self {
        self.unop(Not)
    }

    /// Bitwise this expression with another.
    pub fn bitxor(self, other: impl Into<Self>) -> Self {
        self.binop(BitwiseXor, other)
    }

    /// BitwiseOr this expression with another.
    pub fn bitor(self, other: impl Into<Self>) -> Self {
        self.binop(BitwiseOr, other)
    }

    /// BitwiseOr this expression with another.
    pub fn bitnand(self, other: impl Into<Self>) -> Self {
        self.binop(BitwiseNand, other)
    }

    /// BitwiseAnd this expression with another.
    pub fn bitand(self, other: impl Into<Self>) -> Self {
        self.binop(BitwiseAnd, other)
    }

    /// BitwiseAnd this expression with another.
    pub fn bitnor(self, other: impl Into<Self>) -> Self {
        self.binop(BitwiseNor, other)
    }

    /// BitwiseAnd this expression with another.
    pub fn bitnot(self) -> Self {
        self.unop(BitwiseNot)
    }

    /// Is this expression less than another?
    pub fn lt(self, other: impl Into<Self>) -> Self {
        self.binop(Comparison::LessThan, other)
    }

    /// Is this expression less than or equal to another?
    pub fn le(self, other: impl Into<Self>) -> Self {
        self.binop(Comparison::LessThanOrEqual, other)
    }

    /// Is this expression greater than another?
    pub fn gt(self, other: impl Into<Self>) -> Self {
        self.binop(Comparison::GreaterThan, other)
    }

    /// Is this expression greater than or equal to another?
    pub fn ge(self, other: impl Into<Self>) -> Self {
        self.binop(Comparison::GreaterThanOrEqual, other)
    }

    /// Is this expression greater than another?
    pub fn eq(self, other: impl Into<Self>) -> Self {
        self.binop(Comparison::Equal, other)
    }

    /// Is this expression greater than or equal to another?
    pub fn neq(self, other: impl Into<Self>) -> Self {
        self.binop(Comparison::NotEqual, other)
    }

    /// Logical and this expression with another.
    pub fn and(self, other: impl Into<Self>) -> Self {
        self.binop(And, other)
    }

    pub(crate) fn binop(self, op: impl ToString, other: impl Into<Self>) -> Self {
        Expr::BinaryOp(op.to_string(), Box::new(self), Box::new(other.into()))
    }

    /// Logical or this expression with another.
    pub fn or(self, other: impl Into<Self>) -> Self {
        self.binop(Or, other)
    }

    /// Get the power of this expression to another.
    #[allow(clippy::should_implement_trait)]
    pub fn pow(self, exp: impl Into<Self>) -> Self {
        self.binop(Arithmetic::Power, exp)
    }

    /// Add this expression to another.
    #[allow(clippy::should_implement_trait)]
    pub fn add(self, other: impl Into<Self>) -> Self {
        self.binop(Add, other)
    }

    /// Subtract an expression from this expression.
    #[allow(clippy::should_implement_trait)]
    pub fn sub(self, other: impl Into<Self>) -> Self {
        self.binop(Arithmetic::Subtract, other)
    }

    /// Multiply this expression by another.
    #[allow(clippy::should_implement_trait)]
    pub fn mul(self, other: impl Into<Self>) -> Self {
        self.binop(Arithmetic::Multiply, other)
    }

    /// Divide this expression by another.
    #[allow(clippy::should_implement_trait)]
    pub fn div(self, other: impl Into<Self>) -> Self {
        self.binop(Arithmetic::Divide, other)
    }

    /// Get the remainder of this expression divided by another.
    #[allow(clippy::should_implement_trait)]
    pub fn rem(self, other: impl Into<Self>) -> Self {
        self.binop(Arithmetic::Remainder, other)
    }

    /// Get the remainder of this expression divided by another.
    #[allow(clippy::should_implement_trait)]
    pub fn neg(self) -> Self {
        self.unop(Negate)
    }

    /// Get a field from a structure, union, or tuple.
    ///
    /// For tuples, use an `Int` constant expression to access the nth field (zero indexed).
    /// For unions or structures, use a `Symbol` constant expression to access the field.
    pub fn field(self, field: ConstExpr) -> Self {
        match self {
            Self::ConstExpr(cexpr) => Self::ConstExpr(cexpr.field(field)),
            _ => Self::Member(Box::new(self), field),
        }
    }

    /// Index an array or pointer with an expression that evaluates to an `Int` at runtime.
    pub fn idx(self, idx: impl Into<Self>) -> Self {
        Expr::Index(Box::new(self), Box::new(idx.into()))
    }

    /// Create a `let` binding for an expression.
    ///
    /// This will create a new scope with the variable `var` defined.
    /// `var` will be declared with the type `t`, and the expression `e` will be assigned to it.
    ///
    /// The result of this expression is the `ret` value, which is evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `var` will be removed from the scope.
    pub fn let_var(
        var: impl ToString,
        mutability: impl Into<Mutability>,
        t: Option<Type>,
        expr: impl Into<Self>,
        ret: impl Into<Self>,
    ) -> Self {
        ret.into()
            .with((var.to_string(), mutability.into(), t, expr.into()))
    }

    /// Create a `let` binding for an expression, and define multiple variables.
    pub fn let_vars(
        vars: Vec<(&str, Mutability, Option<Type>, Self)>,
        ret: impl Into<Self>,
    ) -> Self {
        ret.into().with(vars)
    }

    /// Create a `let` binding for an type.
    ///
    /// This will create a new scope with the type `typename` defined.
    /// `typename` will be declared with the type `t`, and the expression
    /// `ret` will be evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `typename` will be removed from the scope.
    pub fn let_type(typename: impl ToString, t: Type, ret: impl Into<Self>) -> Self {
        ret.into().with((typename.to_string(), t))
    }
    /// Create several `type` bindings at onces.
    pub fn let_types(vars: Vec<(&str, Type)>, ret: impl Into<Self>) -> Self {
        ret.into().with(vars)
    }

    /// Create a `let` binding for a constant expression.
    ///
    /// This will create a new scope with the constant `constname` defined.
    /// `ret` will be evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `constname` will be removed from the scope.
    pub fn let_const(constname: impl ToString, e: ConstExpr, ret: impl Into<Self>) -> Self {
        ret.into().with((constname.to_string(), e))
    }

    /// Create several `const` bindings at onces.
    pub fn let_consts(constants: Vec<(&str, ConstExpr)>, ret: impl Into<Self>) -> Self {
        ret.into().with(constants)
    }

    /// Create a `proc` binding for a procedure.
    ///
    /// This will create a new scope with the procedure `proc` defined.
    /// `ret` will be evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `proc` will be removed from the scope.
    pub fn let_proc(procname: impl ToString, proc: Procedure, ret: impl Into<Self>) -> Self {
        ret.into().with((procname.to_string(), proc))
    }

    /// Create several `proc` bindings at onces.
    pub fn let_procs(procs: BTreeMap<&str, Procedure>, ret: impl Into<Self>) -> Self {
        ret.into().with(procs)
    }

    /// Create a structure of fields to expressions.
    pub fn structure(vars: BTreeMap<&str, Self>) -> Self {
        let mut result = BTreeMap::new();
        for (var, val) in vars {
            result.insert(var.to_string(), val);
        }
        Self::Struct(result)
    }

    /// Evaluate a variable in the current scope.
    pub fn var(var: impl ToString) -> Self {
        Expr::ConstExpr(ConstExpr::Symbol(var.to_string()))
    }

    /// Apply this expression as a procedure to some arguments.
    pub fn app(self, args: Vec<Self>) -> Self {
        Expr::Apply(Box::new(self), args)
    }

    /// Create an if-then-else statement with this expression as the condition.
    pub fn if_then(self, t: impl Into<Self>, e: impl Into<Self>) -> Self {
        Expr::If(Box::new(self), Box::new(t.into()), Box::new(e.into()))
    }

    /// Create a while statement with this expression as the condition.
    pub fn while_loop(self, body: impl Into<Self>) -> Self {
        Expr::While(Box::new(self), Box::new(body.into()))
    }

    /// Reference this expression (i.e. get a pointer to it).
    pub fn refer(self, mutability: impl Into<Mutability>) -> Self {
        Expr::Refer(mutability.into(), Box::new(self))
    }

    /// Dereference this expression (i.e. get the value it points to).
    pub fn deref(self) -> Self {
        Expr::Deref(Box::new(self))
    }

    /// Dereference this expression (i.e. get the value it points to),
    /// and write another expression to its position in memory.
    pub fn deref_mut(self, e: impl Into<Self>) -> Self {
        Expr::DerefMut(Box::new(self), Box::new(e.into()))
    }

    /// Perform an AssignOp on this expression.
    pub fn assign_op(self, op: impl ToString, e: impl Into<Self>) -> Self {
        Expr::AssignOp(op.to_string(), Box::new(self), Box::new(e.into()))
    }

    /// Perform an AssignOp on this expression.
    pub fn assign(self, op: impl ToString, e: impl Into<Self>) -> Self {
        Expr::AssignOp(op.to_string(), Box::new(self), Box::new(e.into()))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Declare(declaration, result) => {
                write!(f, "let {declaration} in {result}")
            }
            Self::Annotated(expr, _) => write!(f, "{expr}"),
            Self::ConstExpr(expr) => write!(f, "{expr}"),
            Self::Many(exprs) => {
                write!(f, "{{ ")?;
                for (i, item) in exprs.iter().enumerate() {
                    write!(f, "{item}")?;
                    if i < exprs.len() - 1 {
                        write!(f, "; ")?
                    }
                }
                write!(f, " }}")
            }
            Self::Array(exprs) => {
                write!(f, "[")?;
                for (i, item) in exprs.iter().enumerate() {
                    write!(f, "{item}")?;
                    if i < exprs.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "]")
            }
            Self::Tuple(exprs) => {
                write!(f, "(")?;
                for (i, item) in exprs.iter().enumerate() {
                    write!(f, "{item}")?;
                    if i < exprs.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }

            Self::While(cond, body) => {
                write!(f, "while ({cond}) {body}")
            }
            Self::If(cond, t, e) => {
                write!(f, "if ({cond}) {t} else {e}")
            }
            Self::Match(expr, branches) => {
                write!(f, "match {expr} {{")?;
                for (i, (pat, val)) in branches.iter().enumerate() {
                    write!(f, "{pat} => {val}")?;
                    if i < branches.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::IfLet(pat, expr, t, e) => {
                write!(f, "if let {pat} = {expr} {t} else {e}")
            }
            Self::When(cond, t, e) => {
                write!(f, "when ({cond}) {t} else {e}")
            }
            Self::As(val, ty) => write!(f, "{val} as {ty}"),

            Self::Struct(items) => {
                write!(f, "struct {{")?;
                for (i, (name, val)) in items.iter().enumerate() {
                    write!(f, "{name} = {val}")?;
                    if i < items.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            Self::Union(ty, variant, val) => {
                write!(f, "union {{ {variant} = {val}, {ty}.. }}")
            }
            Self::EnumUnion(ty, variant, val) => {
                write!(f, "{ty} of {variant} {val}")
            }

            Self::UnaryOp(op, x) => write!(f, "{op}{x}"),
            Self::BinaryOp(op, x, y) => write!(f, "{x}{op}{y}"),
            Self::TernaryOp(op, x, y, z) => write!(f, "{x}{op}{y}, {z}"),
            Self::AssignOp(op, x, y) => write!(f, "{x} {op}= {y}"),

            Self::Member(val, field) => write!(f, "({val}).{field}"),
            Self::Index(val, idx) => write!(f, "{val}[{idx}]"),

            Self::Return(val) => write!(f, "return {val}"),
            Self::Refer(mutability, val) => {
                write!(f, "&")?;
                if mutability.is_mutable() {
                    write!(f, "mut ")?;
                }
                write!(f, "{val}")
            }
            Self::Deref(ptr) => write!(f, "*{ptr}"),
            Self::DerefMut(ptr, val) => write!(f, "(*{ptr}) = {val}"),
            Self::Apply(fun, args) => {
                write!(f, "{fun}(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, ")")
            }
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        use Expr::*;
        match (self, other) {
            (Self::Annotated(expr, _), other) | (other, Self::Annotated(expr, _)) => {
                &**expr == other
            }
            // A constant expression.
            (ConstExpr(a), ConstExpr(b)) => a == b,
            // A block of expressions. The last expression in the block is the value of the block.
            (Many(a), Many(b)) => a == b,

            // Create a while loop: while the first expression evaluates to true, evaluate the second expression.
            (While(cond1, body1), While(cond2, body2)) => cond1 == cond2 && body1 == body2,

            // An if-then-else expression.
            //
            // Evaluate a condition.
            // If the condition is true, evaluate the first expression.
            // Otherwise, evaluate the second expression.
            (If(cond1, then1, else1), If(cond2, then2, else2)) => {
                cond1 == cond2 && then1 == then2 && else1 == else2
            }
            // A constant, compile time if-then-else expression.
            //
            // Evaluate the condition as a constant expression.
            // If the condition is true, then this `when` expression is replaced with the first expression.
            // Otherwise, this `when` expression is replaced with the second expression.
            (When(cond1, then1, else1), When(cond2, then2, else2)) => {
                cond1 == cond2 && then1 == then2 && else1 == else2
            }

            // Perform a unary operation on two expressions.
            (UnaryOp(op1, val1), UnaryOp(op2, val2)) => op1 == op2 && val1 == val2,
            // Perform a binary operation on two expressions.
            (BinaryOp(op1, lhs1, rhs1), BinaryOp(op2, lhs2, rhs2)) => {
                op1 == op2 && lhs1 == lhs2 && rhs1 == rhs2
            }
            // Perform a ternary operation on three expressions.
            (TernaryOp(op1, a1, b1, c1), TernaryOp(op2, a2, b2, c2)) => {
                op1 == op2 && a1 == a2 && b1 == b2 && c1 == c2
            }
            // Perform an assignment operation on two expressions.
            (AssignOp(op1, lhs1, rhs1), AssignOp(op2, lhs2, rhs2)) => {
                op1 == op2 && lhs1 == lhs2 && rhs1 == rhs2
            }

            // Reference this expression (i.e. get a pointer to it).
            (Refer(m1, val1), Refer(m2, val2)) => m1 == m2 && val1 == val2,
            // Dereference this expression (i.e. get the value it points to).
            (Deref(val1), Deref(val2)) => val1 == val2,
            // Store an expression to an address (a pointer).
            (DerefMut(dst1, src1), DerefMut(dst2, src2)) => dst1 == dst2 && src1 == src2,

            // Apply a function with some arguments.
            (Apply(func1, args1), Apply(func2, args2)) => func1 == func2 && args1 == args2,
            // Return a value from a function.
            (Return(val1), Return(val2)) => val1 == val2,

            // An array of expressions.
            (Array(vals1), Array(vals2)) => vals1 == vals2,
            // A tuple of expressions.
            (Tuple(vals1), Tuple(vals2)) => vals1 == vals2,
            // A union: a collection of named fields.
            // The `Type` value is the type of the union.
            // The `String` field is the field the union is being initialized with.
            // The `Box<Self>` field is the value of the field we want to initialize.
            (Union(ty1, field1, val1), Union(ty2, field2, val2)) => {
                ty1 == ty2 && field1 == field2 && val1 == val2
            }
            // A tagged union: a typechecked union of different variants.
            // The `Type` value is the type of the tagged union.
            // The `String` field is the variant the tagged union is being initialized with.
            // The `Box<Self>` field is the value of the union's data we want to initialize.
            (EnumUnion(ty1, field1, val1), EnumUnion(ty2, field2, val2)) => {
                ty1 == ty2 && field1 == field2 && val1 == val2
            }
            // A structure of fields to expressions.
            (Struct(fields1), Struct(fields2)) => fields1 == fields2,

            // Cast an expression to another type.
            (As(val1, ty1), As(val2, ty2)) => val1 == val2 && ty1 == ty2,

            // Get a field or member from a structure, union, or tuple.
            // For tuples, use an `Int` constant expression to access the nth field (zero indexed).
            // For unions or structures, use a `Symbol` constant expression to access the field.
            (Member(val1, field1), Member(val2, field2)) => val1 == val2 && field1 == field2,

            // Index an array or pointer with an expression that evaluates to an `Int` at runtime.
            (Index(val1, idx1), Index(val2, idx2)) => val1 == val2 && idx1 == idx2,

            (Declare(decl1, expr1), Declare(decl2, expr2)) => expr1 == expr2 && decl1 == decl2,

            _ => false,
        }
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Expr::*;
        match self {
            ConstExpr(expr) => {
                state.write_u8(0);
                expr.hash(state)
            }
            Many(exprs) => {
                state.write_u8(1);
                exprs.hash(state)
            }
            Match(expr, branches) => {
                state.write_u8(2);
                expr.hash(state);
                branches.hash(state);
            }
            IfLet(pat, expr, t, e) => {
                state.write_u8(3);
                pat.hash(state);
                expr.hash(state);
                t.hash(state);
                e.hash(state);
            }

            While(cond, body) => {
                state.write_u8(2);
                cond.hash(state);
                body.hash(state);
            }

            If(cond, then, else_) => {
                state.write_u8(3);
                cond.hash(state);
                then.hash(state);
                else_.hash(state);
            }

            When(cond, then, else_) => {
                state.write_u8(4);
                cond.hash(state);
                then.hash(state);
                else_.hash(state);
            }

            UnaryOp(op, val) => {
                state.write_u8(5);
                // op.display(val).hash(state);
                op.hash(state);
                val.hash(state);
            }

            BinaryOp(op, lhs, rhs) => {
                state.write_u8(6);
                op.hash(state);
                lhs.hash(state);
                rhs.hash(state);
            }

            TernaryOp(op, a, b, c) => {
                state.write_u8(7);
                op.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
            }

            AssignOp(op, lhs, rhs) => {
                state.write_u8(8);
                op.hash(state);
                lhs.hash(state);
                rhs.hash(state);
            }

            Refer(m, val) => {
                state.write_u8(9);
                m.hash(state);
                val.hash(state);
            }

            Deref(val) => {
                state.write_u8(10);
                val.hash(state);
            }

            DerefMut(dst, src) => {
                state.write_u8(11);
                dst.hash(state);
                src.hash(state);
            }

            Apply(func, args) => {
                state.write_u8(12);
                func.hash(state);
                args.hash(state);
            }

            Return(val) => {
                state.write_u8(13);
                val.hash(state);
            }

            Array(vals) => {
                state.write_u8(14);
                vals.hash(state);
            }

            Tuple(vals) => {
                state.write_u8(15);
                vals.hash(state);
            }

            Union(ty, field, val) => {
                state.write_u8(16);
                ty.hash(state);
                field.hash(state);
                val.hash(state);
            }

            EnumUnion(ty, field, val) => {
                state.write_u8(17);
                ty.hash(state);
                field.hash(state);
                val.hash(state);
            }

            Struct(fields) => {
                state.write_u8(18);
                fields.hash(state);
            }

            As(val, ty) => {
                state.write_u8(19);
                val.hash(state);
                ty.hash(state);
            }

            Member(val, field) => {
                state.write_u8(20);
                val.hash(state);
                field.hash(state);
            }

            Index(val, idx) => {
                state.write_u8(21);
                val.hash(state);
                idx.hash(state);
            }

            Annotated(expr, _) => {
                expr.hash(state);
            }

            Declare(decl, expr) => {
                state.write_u8(22);
                decl.hash(state);
                expr.hash(state);
            }
        }
    }
}
