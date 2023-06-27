//! # Expression
//!
//! An expression is a runtime value.
//!
//! Expressions are compiled down into equivalent assembly code
//! which are then executed by the runtime.

use super::ops::*;
use crate::lir::{ConstExpr, Pattern, Procedure, Type};
use crate::parse::SourceCodeLocation;
use core::fmt;
use std::collections::BTreeMap;

/// TODO: Add variants for `LetProc`, `LetVar`, etc. to support multiple definitions.
///       This way, we don't overflow the stack with several clones of the environment.
/// A runtime expression.
#[derive(Clone, Debug)]
pub enum Expr {
    /// An expression along with data about its source code location.
    /// This is used for error reporting.
    AnnotatedWithSource {
        // The expression itself.
        expr: Box<Self>,
        /// The source code location of the expression.
        loc: SourceCodeLocation
    },

    /// A constant expression.
    ConstExpr(ConstExpr),
    /// A block of expressions. The last expression in the block is the value of the block.
    Many(Vec<Self>),

    /// A `const` binding expression.
    /// Declare a constant under a new scope, and evaluate a subexpression in that scope.
    LetConst(String, ConstExpr, Box<Self>),
    /// A `const` binding expression.
    /// Declare multiple constants under a new scope, and evaluate a subexpression in that scope.
    LetConsts(BTreeMap<String, ConstExpr>, Box<Self>),
    /// A `proc` binding expression.
    /// Declare a procedure under a new scope, and evaluate a subexpression in that scope.
    LetProc(String, Procedure, Box<Self>),
    /// A `proc` binding expression.
    /// Declare multiple procedures under a new scope, and evaluate a subexpression in that scope.
    LetProcs(Vec<(String, Procedure)>, Box<Self>),
    /// A `type` binding expression.
    /// Declare a type under a new scope, and evaluate a subexpression in that scope.
    LetType(String, Type, Box<Self>),
    /// A `type` binding expression.
    /// Declare multiple types under a new scope, and evaluate a subexpression in that scope.
    LetTypes(Vec<(String, Type)>, Box<Self>),
    /// A `let` binding expression.
    /// Declare a variable under a new scope, and evaluate a subexpression in that scope.
    LetVar(String, Option<Type>, Box<Self>, Box<Self>),
    /// A `let` binding expression.
    /// Declare multiple variables under a new scope, and evaluate a subexpression in that scope.
    LetVars(Vec<(String, Option<Type>, Self)>, Box<Self>),

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
    UnaryOp(Box<dyn UnaryOp>, Box<Self>),
    /// Perform a binary operation on two expressions.
    BinaryOp(Box<dyn BinaryOp>, Box<Self>, Box<Self>),
    /// Perform a ternary operation on three expressions.
    TernaryOp(Box<dyn TernaryOp>, Box<Self>, Box<Self>, Box<Self>),
    /// Perform an assignment operation on two expressions.
    AssignOp(Box<dyn AssignOp>, Box<Self>, Box<Self>),

    /// Reference this expression (i.e. get a pointer to it).
    Refer(Box<Self>),
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
    /// Get the size of an expression.
    pub fn size_of(self) -> Self {
        Self::ConstExpr(ConstExpr::SizeOfExpr(Box::new(self)))
    }

    /// Cast an expression as another type.
    pub fn as_type(self, t: Type) -> Self {
        Self::As(Box::new(self), t)
    }

    pub fn unop(self, op: impl UnaryOp + 'static) -> Self {
        Self::UnaryOp(Box::new(op), Box::new(self))
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

    fn binop(self, op: impl BinaryOp + 'static, other: impl Into<Self>) -> Self {
        Expr::BinaryOp(Box::new(op), Box::new(self), Box::new(other.into()))
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
        Expr::Member(Box::new(self), field)
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
        t: Option<Type>,
        e: impl Into<Self>,
        ret: impl Into<Self>,
    ) -> Self {
        Expr::LetVar(var.to_string(), t, Box::new(e.into()), Box::new(ret.into()))
    }

    /// Create a `let` binding for an expression, and define multiple variables.
    pub fn let_vars(vars: Vec<(&str, Option<Type>, Self)>, ret: impl Into<Self>) -> Self {
        Self::LetVars(
            vars.into_iter()
                .map(|(name, t, e)| (name.to_string(), t, e))
                .collect(),
            Box::new(ret.into()),
        )
    }

    /// Create a `let` binding for an type.
    ///
    /// This will create a new scope with the type `typename` defined.
    /// `typename` will be declared with the type `t`, and the expression
    /// `ret` will be evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `typename` will be removed from the scope.
    pub fn let_type(typename: impl ToString, t: Type, ret: impl Into<Self>) -> Self {
        Expr::LetType(typename.to_string(), t, Box::new(ret.into()))
    }
    /// Create several `type` bindings at onces.
    pub fn let_types(vars: Vec<(&str, Type)>, ret: impl Into<Self>) -> Self {
        Self::LetTypes(
            vars.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            Box::new(ret.into()),
        )
    }

    /// Create a `let` binding for a constant expression.
    ///
    /// This will create a new scope with the constant `constname` defined.
    /// `ret` will be evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `constname` will be removed from the scope.
    pub fn let_const(constname: impl ToString, e: ConstExpr, ret: impl Into<Self>) -> Self {
        Expr::LetConst(constname.to_string(), e, Box::new(ret.into()))
    }

    /// Create several `const` bindings at onces.
    pub fn let_consts(constants: Vec<(&str, ConstExpr)>, ret: impl Into<Self>) -> Self {
        Self::LetConsts(
            constants
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
            Box::new(ret.into()),
        )
    }

    /// Create a `proc` binding for a procedure.
    ///
    /// This will create a new scope with the procedure `proc` defined.
    /// `ret` will be evaluated under this new scope.
    ///
    /// When this expression is finished evaluating, `proc` will be removed from the scope.
    pub fn let_proc(procname: impl ToString, proc: Procedure, ret: impl Into<Self>) -> Self {
        Expr::LetProc(procname.to_string(), proc, Box::new(ret.into()))
    }

    /// Create several `proc` bindings at onces.
    pub fn let_procs(procs: BTreeMap<&str, Procedure>, ret: impl Into<Self>) -> Self {
        Self::LetProcs(
            procs.into_iter().map(|(k, v)| (k.to_string(), v)).collect(),
            Box::new(ret.into()),
        )
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
    pub fn refer(self) -> Self {
        Expr::Refer(Box::new(self))
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
    pub fn assign_op(self, op: impl AssignOp + 'static, e: impl Into<Self>) -> Self {
        Expr::AssignOp(Box::new(op), Box::new(self.clone()), Box::new(e.into()))
    }

    /// Perform an AssignOp on this expression.
    pub fn assign(self, op: Box<dyn AssignOp>, e: impl Into<Self>) -> Self {
        Expr::AssignOp(op, Box::new(self.clone()), Box::new(e.into()))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::AnnotatedWithSource { expr, .. } => write!(f, "{expr}"),
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
            Self::LetVar(name, ty, val, ret) => {
                write!(f, "let {name}")?;
                if let Some(ty) = ty {
                    write!(f, ": {ty}")?
                }
                write!(f, " = {val} in {ret}")
            }
            Self::LetVars(vars, ret) => {
                write!(f, "let ")?;
                for (i, (name, ty, val)) in vars.iter().enumerate() {
                    write!(f, "{name}")?;
                    if let Some(ty) = ty {
                        write!(f, ": {ty}")?
                    }
                    write!(f, " = {val}")?;
                    if i < vars.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, " in {ret}")
            }
            Self::LetConst(name, val, ret) => {
                write!(f, "const {name} = {val} in {ret}")
            }
            Self::LetConsts(consts, ret) => {
                write!(f, "const ")?;
                for (i, (name, val)) in consts.iter().enumerate() {
                    write!(f, "{name} = {val}")?;
                    if i < consts.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, " in {ret}")
            }
            Self::LetProc(name, val, ret) => {
                write!(f, "const {name} = {val} in {ret}")
            }
            Self::LetProcs(consts, ret) => {
                write!(f, "const ")?;
                for (i, (name, val)) in consts.iter().enumerate() {
                    write!(f, "{name} = {val}")?;
                    if i < consts.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, " in {ret}")
            }
            Self::LetType(name, ty, ret) => {
                write!(f, "type {name} = {ty} in {ret}")
            }
            Self::LetTypes(types, ret) => {
                write!(f, "type ")?;
                for (i, (name, ty)) in types.iter().enumerate() {
                    write!(f, "{name} = {ty}")?;
                    if i < types.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, " in {ret}")
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

            Self::UnaryOp(op, x) => write!(f, "{}", op.display(x)),
            Self::BinaryOp(op, x, y) => write!(f, "{}", op.display(x, y)),
            Self::TernaryOp(op, x, y, z) => write!(f, "{}", op.display(x, y, z)),
            Self::AssignOp(op, x, y) => write!(f, "{}", op.display(x, y)),

            Self::Member(val, field) => write!(f, "({val}).{field}"),
            Self::Index(val, idx) => write!(f, "{val}[{idx}]"),

            Self::Return(val) => write!(f, "return {val}"),
            Self::Refer(val) => write!(f, "&{val}"),
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
            (Self::AnnotatedWithSource { expr, .. }, other) | (other, Self::AnnotatedWithSource { expr, .. })  => &**expr == other,
            // A constant expression.
            (ConstExpr(a), ConstExpr(b)) => a == b,
            // A block of expressions. The last expression in the block is the value of the block.
            (Many(a), Many(b)) => a == b,

            // A `const` binding expression.
            // Declare a constant under a new scope, and evaluate a subexpression in that scope.
            (LetConst(name1, const_expr1, ret1), LetConst(name2, const_expr2, ret2)) => {
                name1 == name2 && const_expr1 == const_expr2 && ret1 == ret2
            }

            // A `const` binding expression.
            // Declare multiple constants under a new scope, and evaluate a subexpression in that scope.
            (LetConsts(consts1, ret1), LetConsts(consts2, ret2)) => {
                consts1 == consts2 && ret1 == ret2
            }

            // A `proc` binding expression.
            // Declare a procedure under a new scope, and evaluate a subexpression in that scope.
            (LetProc(name1, proc1, ret1), LetProc(name2, proc2, ret2)) => {
                name1 == name2 && proc1 == proc2 && ret1 == ret2
            }
            // A `proc` binding expression.
            // Declare multiple procedures under a new scope, and evaluate a subexpression in that scope.
            (LetProcs(procs1, ret1), LetProcs(procs2, ret2)) => procs1 == procs2 && ret1 == ret2,
            // A `type` binding expression.
            // Declare a type under a new scope, and evaluate a subexpression in that scope.
            (LetType(name1, ty1, ret1), LetType(name2, ty2, ret2)) => {
                name1 == name2 && ty1 == ty2 && ret1 == ret2
            }
            // A `type` binding expression.
            // Declare multiple types under a new scope, and evaluate a subexpression in that scope.
            (LetTypes(types1, ret1), LetTypes(types2, ret2)) => types1 == types2 && ret1 == ret2,
            // A `let` binding expression.
            // Declare a variable under a new scope, and evaluate a subexpression in that scope.
            (LetVar(name1, ty1, val1, ret1), LetVar(name2, ty2, val2, ret2)) => {
                name1 == name2 && ty1 == ty2 && val1 == val2 && ret1 == ret2
            }
            // A `let` binding expression.
            // Declare multiple variables under a new scope, and evaluate a subexpression in that scope.
            (LetVars(vars1, ret1), LetVars(vars2, ret2)) => vars1 == vars2 && ret1 == ret2,

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
            (Refer(val1), Refer(val2)) => val1 == val2,
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
            _ => false,
        }
    }
}
