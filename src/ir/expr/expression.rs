use crate::asm::{AssemblyProgram, CoreOp, StandardOp, A, B, C, FP, SP};
use crate::ir::{Compile, ConstExpr, Env, Error, GetSize, GetType, Type, TypeCheck};
use std::collections::BTreeMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// A constant expression.
    ConstExpr(ConstExpr),
    /// A block of expressions. The last expression in the block is the value of the block.
    Many(Vec<Self>),

    /// A `const` binding expression.
    /// Declare a constant under a new scope, and evaluate a subexpression in that scope.
    Const(String, ConstExpr, Box<Self>),
    /// A `let` binding expression.
    /// Declare a variable under a new scope, and evaluate a subexpression in that scope.
    Let(String, Option<Type>, Box<Self>, Box<Self>),

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

    /// Add two expressions.
    Add(Box<Self>, Box<Self>),
    /// Subtract an expression from another.
    Sub(Box<Self>, Box<Self>),
    /// Multiply an expression by another.
    Mul(Box<Self>, Box<Self>),
    /// Divide an expression by another.
    Div(Box<Self>, Box<Self>),
    /// Get the remainder of this expression divided by another.
    Rem(Box<Self>, Box<Self>),

    /// Take the logical and of two expressions.
    And(Box<Self>, Box<Self>),
    /// Take the logical or of two expressions.
    Or(Box<Self>, Box<Self>),
    /// Take the logical not of an expression.
    Not(Box<Self>),

    /// TODO: implement comparison operators.
    // Lt(Box<Self>, Box<Self>),
    // Le(Box<Self>, Box<Self>),
    // Gt(Box<Self>, Box<Self>),
    // Ge(Box<Self>, Box<Self>),
    // Eq(Box<Self>, Box<Self>),
    // Neq(Box<Self>, Box<Self>),

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
    /// Cast an expression as another type.
    pub fn as_type(self, t: Type) -> Self {
        Expr::As(Box::new(self), t)
    }

    /// Logical not this expression.
    pub fn not(self) -> Self {
        Expr::Not(Box::new(self))
    }

    /// Logical and this expression with another.
    pub fn and(self, other: impl Into<Self>) -> Self {
        Expr::And(Box::new(self), Box::new(other.into()))
    }

    /// Logical or this expression with another.
    pub fn or(self, other: impl Into<Self>) -> Self {
        Expr::Or(Box::new(self), Box::new(other.into()))
    }

    /// Add this expression to another.
    pub fn add(self, other: impl Into<Self>) -> Self {
        Expr::Add(Box::new(self), Box::new(other.into()))
    }

    /// Subtract an expression from this expression.
    pub fn sub(self, other: impl Into<Self>) -> Self {
        Expr::Sub(Box::new(self), Box::new(other.into()))
    }

    /// Multiply this expression by another.
    pub fn mul(self, other: impl Into<Self>) -> Self {
        Expr::Mul(Box::new(self), Box::new(other.into()))
    }

    /// Divide this expression by another.
    pub fn div(self, other: impl Into<Self>) -> Self {
        Expr::Div(Box::new(self), Box::new(other.into()))
    }

    /// Get the remainder of this expression divided by another.
    pub fn rem(self, other: impl Into<Self>) -> Self {
        Expr::Rem(Box::new(self), Box::new(other.into()))
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
        Expr::Let(var.to_string(), t, Box::new(e.into()), Box::new(ret.into()))
    }

    /// Create a `let` binding for an expression, and define multiple variables.
    pub fn let_vars(vars: BTreeMap<&str, (Option<Type>, Self)>, ret: impl Into<Self>) -> Self {
        let mut result = ret.into();
        for (var, (t, val)) in vars {
            result = Expr::Let(var.to_string(), t, Box::new(val), Box::new(result));
        }
        result
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
}

impl TypeCheck for Expr {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        match self {
            Self::ConstExpr(c) => c.type_check(env),

            Self::Many(exprs) => {
                for expr in exprs {
                    expr.type_check(env)?;
                }
                Ok(())
            }

            Self::Const(var, e, ret) => {
                // Typecheck the expression we're assigning to the variable.
                e.type_check(env)?;
                let mut new_env = env.clone();
                new_env.consts.insert(var.clone(), e.clone());
                ret.type_check(&new_env)
            }

            Self::Let(var, t, e, ret) => {
                // Typecheck the expression we're assigning to the variable.
                e.type_check(env)?;
                // Get the inferred type of the expression.
                let inferred_t = e.get_type(env)?;
                // If there's a type specification for the variable, check it.
                if let Some(t) = t {
                    // Check that the inferred type is compatible with the type specified.
                    if !inferred_t.equals(t, env)? {
                        return Err(Error::MismatchedTypes {
                            expected: t.clone(),
                            found: inferred_t,
                            expr: self.clone(),
                        });
                    }
                }
                let mut new_env = env.clone();
                new_env.def_var(var.clone(), inferred_t)?;
                ret.type_check(&new_env)
            }

            Self::While(cond, body) => {
                // Typecheck the subexpressions.
                cond.type_check(env)?;
                body.type_check(env)?;
                Ok(())
            }

            Self::If(cond, t, e) => {
                // Typecheck the subexpressions.
                cond.type_check(env)?;
                t.type_check(env)?;
                e.type_check(env)?;

                // Get the types of the then and else branches.
                let t_type = t.get_type(env)?;
                let e_type = e.get_type(env)?;
                // Check that the types of the then and else branches are compatible.
                if !t_type.equals(&e_type, env)? {
                    // If they're not, return an error.
                    return Err(Error::MismatchedTypes {
                        expected: t_type,
                        found: e_type,
                        expr: self.clone(),
                    });
                }
                Ok(())
            }

            Self::When(cond, t, e) => {
                // Typecheck the subexpressions.
                cond.type_check(env)?;
                t.type_check(env)?;
                e.type_check(env)
                // Since `when` expressions are computed at compile time,
                // we don't have to care about matching the types of the then and else branches.
            }

            Self::Add(a, b)
            | Self::Sub(a, b)
            | Self::Mul(a, b)
            | Self::Div(a, b)
            | Self::Rem(a, b) => {
                a.type_check(env)?;
                b.type_check(env)?;
                let a_type = a.get_type(env)?;
                let b_type = b.get_type(env)?;

                // Now, perform the correct assembly expressions based on the types of the two expressions.
                match (a_type, b_type) {
                    // If a `Float` and a `Cell` are used, we just interpret the `Cell` as a `Float`.
                    (Type::Cell, Type::Float) | (Type::Float, Type::Cell)
                    // Two floats are used as floats.
                    | (Type::Float, Type::Float)
                    // An integer used with a float is promoted, and returns a float.
                    | (Type::Int, Type::Float)
                    | (Type::Float, Type::Int)

                    // If cells and/or ints are used, we just use them as integers.
                    | (Type::Int, Type::Int)
                    | (Type::Cell, Type::Cell)
                    | (Type::Cell, Type::Int)
                    | (Type::Int, Type::Cell) => Ok(()),

                    // Cannot do arithmetic on other pairs of types.
                    _ => Err(Error::InvalidBinop(self.clone())),
                }
            }

            Self::And(a, b) | Self::Or(a, b) => {
                a.type_check(env)?;
                b.type_check(env)?;
                let a_type = a.get_type(env)?;
                let b_type = b.get_type(env)?;

                if let (Type::Bool, Type::Bool) = (a_type, b_type) {
                    Ok(())
                } else {
                    Err(Error::InvalidBinop(self.clone()))
                }
            }

            Self::Not(x) => {
                x.type_check(env)?;
                match x.get_type(env)? {
                    Type::Bool => Ok(()),
                    other => Err(Error::MismatchedTypes {
                        expected: Type::Bool,
                        found: other,
                        expr: self.clone(),
                    }),
                }
            }

            Self::Refer(e) => e.type_check(env),
            Self::Deref(e) => {
                e.type_check(env)?;
                let t = e.get_type(env)?;
                if let Type::Pointer(_) = t {
                    Ok(())
                } else {
                    Err(Error::MismatchedTypes {
                        expected: Type::Pointer(Box::new(Type::Any)),
                        found: t,
                        expr: self.clone(),
                    })
                }
            }

            Self::DerefMut(ptr, val) => {
                ptr.type_check(env)?;
                val.type_check(env)?;
                let ptr_type = ptr.get_type(env)?;
                let val_type = val.get_type(env)?;
                if let Type::Pointer(t) = ptr_type {
                    if t.equals(&val_type, env)? {
                        Ok(())
                    } else {
                        Err(Error::MismatchedTypes {
                            expected: val_type,
                            found: *t,
                            expr: self.clone(),
                        })
                    }
                } else {
                    Err(Error::MismatchedTypes {
                        expected: Type::Pointer(Box::new(Type::Any)),
                        found: ptr_type,
                        expr: self.clone(),
                    })
                }
            }

            Self::Apply(f, args) => {
                // Typecheck the expression we want to call as a procedure.
                f.type_check(env)?;
                // Typecheck the supplied arguments.
                for arg in args {
                    arg.type_check(env)?;
                }
                // Get the type of the function.
                let f_type = f.get_type(env)?;
                // Infer the types of the supplied arguments.
                let mut args_inferred = vec![];
                for arg in args {
                    args_inferred.push(arg.get_type(env)?);
                }
                if let Type::Proc(args_t, ret_t) = f_type {
                    // If the number of arguments is incorrect, then return an error.
                    if args_t.len() != args_inferred.len() {
                        return Err(Error::MismatchedTypes {
                            expected: Type::Proc(args_t, ret_t.clone()),
                            found: Type::Proc(args_inferred, ret_t),
                            expr: self.clone(),
                        });
                    }
                    // If the function is a procedure, confirm that the type of each
                    // argument matches the the type of the supplied value.
                    for (arg_t, arg) in args_t.into_iter().zip(args_inferred.into_iter()) {
                        // If the types don't match, return an error.
                        if !arg_t.equals(&arg, env)? {
                            return Err(Error::MismatchedTypes {
                                expected: arg_t,
                                found: arg,
                                expr: self.clone(),
                            });
                        }
                    }
                    Ok(())
                } else {
                    // If the function is not a procedure, return an error.
                    Err(Error::MismatchedTypes {
                        expected: Type::Proc(args_inferred, Box::new(Type::Any)),
                        found: f_type,
                        expr: self.clone(),
                    })
                }
            }

            Self::Return(e) => e.type_check(env),

            Self::Array(elems) | Self::Tuple(elems) => {
                for elem in elems {
                    elem.type_check(env)?;
                }
                Ok(())
            }

            Self::Struct(fields) => {
                for field_expr in fields.values() {
                    field_expr.type_check(env)?;
                }
                Ok(())
            }

            Self::Union(t, variant, val) => {
                if let Type::Union(fields) = t {
                    if fields.get(variant).is_some() {
                        val.type_check(env)?;
                        Ok(())
                    } else {
                        Err(Error::VariantNotFound(t.clone(), variant.clone()))
                    }
                } else {
                    Err(Error::VariantNotFound(t.clone(), variant.clone()))
                }
            }

            Self::As(e, t) => {
                e.type_check(env)?;
                let original_t = e.get_type(env)?;

                if original_t.can_cast_to(t, env)? {
                    Ok(())
                } else {
                    Err(Error::InvalidAs(self.clone()))
                }
            }

            Self::Member(e, field) => {
                e.type_check(env)?;
                let e_type = e.get_type(env)?;
                e_type.get_member_offset(field, e, env)?;
                Ok(())
            }

            Self::Index(val, idx) => {
                val.type_check(env)?;
                idx.type_check(env)?;
                let val_type = val.get_type(env)?;
                let idx_type = idx.get_type(env)?;
                match val_type {
                    Type::Array(_, _) | Type::Pointer(_) => {}
                    _ => return Err(Error::InvalidIndex(self.clone())),
                }

                if let Type::Int = idx_type {
                    Ok(())
                } else {
                    Err(Error::InvalidIndex(self.clone()))
                }
            }
        }
    }
}

impl Compile for Expr {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        match self {
            Self::ConstExpr(expr) => expr.compile_expr(env, output)?,
            Self::Many(exprs) => {
                for expr in exprs {
                    expr.compile_expr(env, output)?;
                }
            }

            Self::As(ref expr, ref t) => {
                expr.clone().compile_expr(env, output)?;
                match (expr.get_type(env)?, t) {
                    (Type::Int, Type::Float) => {
                        output.std_op(StandardOp::ToFloat(SP.deref()))?;
                    }
                    (Type::Float, Type::Int) => {
                        output.std_op(StandardOp::ToInt(SP.deref()))?;
                    }
                    (a, b) if a.get_size(env)? == b.get_size(env)? => {}
                    _ => {
                        return Err(Error::InvalidAs(self));
                    }
                }
            }

            Self::And(a, b) => {
                // Push the two expressions on the stack.
                a.compile_expr(env, output)?;
                b.compile_expr(env, output)?;
                // Perform logical and on them.
                output.op(CoreOp::And {
                    src: SP.deref(),
                    dst: SP.deref().offset(-1),
                });
                output.op(CoreOp::Pop(None, 1));
            }
            Self::Or(a, b) => {
                // Push the two expressions on the stack.
                a.compile_expr(env, output)?;
                b.compile_expr(env, output)?;
                // Perform logical or on them.
                output.op(CoreOp::Or {
                    src: SP.deref(),
                    dst: SP.deref().offset(-1),
                });
                output.op(CoreOp::Pop(None, 1));
            }
            Self::Not(x) => {
                x.compile_expr(env, output)?;
                output.op(CoreOp::Not(SP.deref()));
            }

            Self::Add(ref a, ref b)
            | Self::Sub(ref a, ref b)
            | Self::Mul(ref a, ref b)
            | Self::Div(ref a, ref b)
            | Self::Rem(ref a, ref b) => {
                let src = SP.deref();
                let dst = SP.deref().offset(-1);
                // Get the respective core operation for the current expression.
                let core_op = match self {
                    Self::Add(_, _) => CoreOp::Add { src, dst },
                    Self::Sub(_, _) => CoreOp::Sub { src, dst },
                    Self::Mul(_, _) => CoreOp::Mul { src, dst },
                    Self::Div(_, _) => CoreOp::Div { src, dst },
                    Self::Rem(_, _) => CoreOp::Rem { src, dst },
                    _ => unreachable!(),
                };
                let src = SP.deref();
                let dst = SP.deref().offset(-1);
                // Get the respective standard operation for the current expression.
                let std_op = match self {
                    Self::Add(_, _) => StandardOp::Add { src, dst },
                    Self::Sub(_, _) => StandardOp::Sub { src, dst },
                    Self::Mul(_, _) => StandardOp::Mul { src, dst },
                    Self::Div(_, _) => StandardOp::Div { src, dst },
                    Self::Rem(_, _) => StandardOp::Rem { src, dst },
                    _ => unreachable!(),
                };
                // Evaluate the two expression on the stack.
                a.clone().compile_expr(env, output)?;
                b.clone().compile_expr(env, output)?;
                // Now, perform the correct assembly expressions based on the types of the two expressions.
                match (a.get_type(env)?, b.get_type(env)?) {
                    // If a `Float` and a `Cell` are used, we just interpret the `Cell` as a `Float`.
                    (Type::Cell, Type::Float) | (Type::Float, Type::Cell) => {
                        output.std_op(std_op)?;
                    }
                    // Two floats are used as floats.
                    (Type::Float, Type::Float) => {
                        output.std_op(std_op)?;
                    }
                    // An integer used with a float is promoted, and returns a float.
                    (Type::Int, Type::Float) => {
                        output.std_op(StandardOp::ToFloat(SP.deref().offset(-1)))?;
                        output.std_op(std_op)?;
                    }
                    (Type::Float, Type::Int) => {
                        output.std_op(StandardOp::ToFloat(SP.deref()))?;
                        output.std_op(std_op)?;
                    }

                    // If cells and/or ints are used, we just use them as integers.
                    (Type::Int, Type::Int)
                    | (Type::Cell, Type::Cell)
                    | (Type::Cell, Type::Int)
                    | (Type::Int, Type::Cell) => {
                        output.op(core_op);
                    }

                    // Cannot do arithmetic on other pairs of types.
                    _ => return Err(Error::InvalidBinop(self.clone())),
                }
                // Pop `b` off of the stack: we only needed it to evaluate
                // the arithmetic and store the result to `a` on the stack.
                output.op(CoreOp::Pop(None, 1));
            }

            Self::Const(name, expr, body) => {
                // TODO: don't use a temporary variable for the new scope, just use the existing scope and remove
                // the constant from the environment when we're done.

                // Create a new scope to bind the constant under.
                let mut new_env = env.clone();
                // Bind the constant in the new scope.
                new_env.consts.insert(name, expr);
                // Compile the body under this new scope.
                body.compile_expr(&mut new_env, output)?;
            }
            Self::Apply(f, args) => {
                // Push the arguments to the procedure on the stack.
                for arg in args {
                    arg.compile_expr(env, output)?;
                }
                match *f {
                    Expr::ConstExpr(ConstExpr::CoreBuiltin(builtin)) => {
                        // Apply the core builtin to the arguments on the stack.
                        builtin.compile_expr(env, output)?;
                    }
                    Expr::ConstExpr(ConstExpr::StandardBuiltin(builtin)) => {
                        // Apply the standard builtin to the arguments on the stack.
                        builtin.compile_expr(env, output)?;
                    }
                    proc => {
                        // Push the procedure on the stack.
                        proc.compile_expr(env, output)?;
                        // Pop the "function pointer" from the stack.
                        output.op(CoreOp::Pop(Some(A), 1));
                        // Call the procedure on the arguments.
                        output.op(CoreOp::Call(A));
                    }
                }
            }
            Self::Return(e) => {
                let args_size = env.get_args_size();
                let ret_size = e.get_size(env)?;
                // Execute the body to leave the return value
                e.compile_expr(env, output)?;

                // Overwrite the arguments with the return value
                output.op(CoreOp::Copy {
                    dst: FP.deref().offset(1 - args_size as isize),
                    src: SP.deref().offset(1 - ret_size as isize),
                    size: ret_size,
                });
                // Decrement the stack pointer by the difference between the size of the
                // arguments and return value, to leave the return value on the stack.
                output.op(CoreOp::Prev(
                    SP,
                    Some(args_size as isize - ret_size as isize),
                ));
                output.op(CoreOp::Return);
            }
            Self::Let(name, specifier, e, body) => {
                // Get the type of the variable using its specifier,
                // or by deducing the type ourselves.
                let t = if let Some(t) = specifier {
                    t
                } else {
                    e.get_type(env)?
                };
                output.op(CoreOp::Comment(format!("begin let '{name}' val")));
                // Compile the expression to leave the value on the stack.
                e.compile_expr(env, output)?;
                output.op(CoreOp::Comment(format!("begin let '{name}' body")));

                // Create a new scope
                let mut new_env = env.clone();
                // Get the size of the variable we are writing to.
                let var_size = t.get_size(env)?;
                new_env.def_var(name.clone(), t)?;

                let result_type = body.get_type(&new_env)?;
                let result_size = result_type.get_size(&new_env)?;
                // Compile the body under the new scope
                body.compile_expr(&mut new_env, output)?;

                output.op(CoreOp::Comment(format!("destruct '{name}'")));
                // Copy the return value over where the arguments were stored,
                // so that when we pop the stack, it's as if we popped the variables
                // and arguments, and pushed our return value.
                output.op(CoreOp::Copy {
                    src: SP.deref().offset(1 - result_size as isize),
                    dst: SP
                        .deref()
                        .offset(1 - var_size as isize - result_size as isize),
                    size: result_size,
                });
                output.op(CoreOp::Pop(None, var_size));
                output.op(CoreOp::Comment(format!("end let '{name}'")));
            }
            Self::While(cond, body) => {
                // Eval the condition
                cond.clone().compile_expr(env, output)?;
                output.op(CoreOp::Pop(Some(A), 1));
                // While the condition
                output.op(CoreOp::While(A));
                // Compile the body
                body.compile_expr(env, output)?;
                // Eval the condition again
                cond.compile_expr(env, output)?;
                output.op(CoreOp::Pop(Some(A), 1));
                // Label the end of the loop
                output.op(CoreOp::End);
            }
            Self::If(c, t, e) => {
                // Compile the condition
                c.compile_expr(env, output)?;
                output.op(CoreOp::Pop(Some(A), 1));
                // If the condition is true
                output.op(CoreOp::If(A));
                // Compile the true branch
                t.compile_expr(env, output)?;
                // If the condition is false
                output.op(CoreOp::Else);
                // Compile the false branch
                e.compile_expr(env, output)?;
                // Label the end of the if statement
                output.op(CoreOp::End);
            }

            Self::When(c, t, e) => if c.as_bool(env)? { t } else { e }.compile_expr(env, output)?,

            Self::Deref(ptr) => {
                // Compile the pointer
                let ptr_type = ptr.get_type(env)?;
                ptr.clone().compile_expr(env, output)?;
                // If the pointer is a pointer, dereference it.
                if let Type::Pointer(inner) = ptr_type {
                    // Pop the address into A
                    output.op(CoreOp::Pop(Some(A), 1));
                    // Push all of the data at the address onto the stack.
                    output.op(CoreOp::Push(A.deref(), inner.get_size(env)?));
                } else {
                    return Err(Error::DerefNonPointer(*ptr));
                }
            }
            Self::DerefMut(ptr, val) => {
                // Push the value to the stack
                let val_type = val.get_type(env)?;
                val.compile_expr(env, output)?;

                // Compile the pointer
                ptr.compile_expr(env, output)?;

                // Get the size of the value to store
                let size = val_type.get_size(env)?;
                // Pop the pointer into A
                output.op(CoreOp::Pop(Some(A), 1));
                // Copy the result of the compiled value into the pointer
                output.op(CoreOp::Copy {
                    src: SP.deref().offset(1 - size as isize),
                    dst: A.deref(),
                    size,
                });
                // Pop the value off the stack
                output.op(CoreOp::Pop(None, size));
            }

            Self::Array(elems) => {
                // Compile the elements
                for elem in elems {
                    elem.compile_expr(env, output)?;
                }
            }

            Self::Tuple(items) => {
                // Compile the items
                for item in items {
                    item.compile_expr(env, output)?;
                }
            }

            Self::Struct(items) => {
                // Compile the items
                for (_, val) in items {
                    val.compile_expr(env, output)?;
                }
            }

            Self::Union(t, _, val) => {
                // Get the size of the union.
                let result_size = t.get_size(env)?;
                // Get the size of the value we are storing in the union.
                let val_size = val.get_size(env)?;

                // Evaluate the value and push it onto the stack
                val.compile_expr(env, output)?;
                // Increment the stack pointer to pad out the union.
                output.op(CoreOp::Next(
                    SP,
                    Some(result_size as isize - val_size as isize),
                ));
            }

            Self::Index(val, idx) => {
                // TODO: optimize this by using `Refer` when possible
                // (not loading the entire array onto the stack to index it).

                // Get the type of this expression.
                let t = Self::Index(val.clone(), idx.clone()).get_type(env)?;
                // Calculate the size of this expression.
                let size = t.get_size(env)?;
                // Get the type of the value being indexed
                let val_type = val.get_type(env)?;
                // Get the size of the value being indexed.
                let val_size = val_type.get_size(env)?;
                // Figure out what to do based on the value's type.
                match val_type {
                    // If the value being indexed is an array:
                    Type::Array(ref elem, _) => {
                        // Get the size of the element we will return.
                        let elem_size = elem.get_size(env)?;
                        // Push the array onto the stack.
                        val.compile_expr(env, output)?;
                        // Push the index onto the stack.
                        idx.compile_expr(env, output)?;

                        // Calculate the offset of the element we want to return
                        // (the index times the size of the element), and store it in `B`.
                        output.op(CoreOp::Pop(Some(B), 1));
                        output.op(CoreOp::Set(A, elem_size as isize));
                        output.op(CoreOp::Mul { dst: B, src: A });

                        // Get the address of the array's first element, and store it in `A`.
                        output.op(CoreOp::GetAddress {
                            addr: SP.deref().offset(1 - val_size as isize),
                            dst: A,
                        });
                        // Index the address stored in `A` with the offset stored in `B`,
                        // and store the address of that index in `C`.
                        output.op(CoreOp::Index {
                            src: A,
                            offset: B,
                            dst: C,
                        });

                        // Copy the contents of the element at `C` overtop of the
                        // array's first element on the stack.
                        output.op(CoreOp::Copy {
                            src: C.deref(),
                            dst: SP.deref().offset(1 - val_size as isize),
                            size,
                        });
                        // Pop the remaining elements off the stack, so the element we indexed remains.
                        output.op(CoreOp::Pop(None, val_size - size));
                    }
                    Type::Pointer(elem) => {
                        // Push the index onto the stack.
                        idx.compile_expr(env, output)?;
                        // Push the pointer being indexed onto the stack.
                        val.compile_expr(env, output)?;

                        // Get the size of the element we are indexing.
                        let elem_size = elem.get_size(env)?;
                        // Store the pointer in `A`.
                        output.op(CoreOp::Pop(Some(A), 1));
                        // Store the index in `B`.
                        output.op(CoreOp::Pop(Some(B), 1));
                        // Store the size of the element in `C`.
                        output.op(CoreOp::Set(C, elem_size as isize));
                        // Store the offset of the element from the pointer in `B`
                        // (the index times the size of the element).
                        output.op(CoreOp::Mul { dst: B, src: C });
                        // Get the address of the element and store it in `C`.
                        output.op(CoreOp::Index {
                            src: A.deref(),
                            offset: B,
                            dst: C,
                        });
                        // Push the contents of the element onto the stack.
                        output.op(CoreOp::Push(C.deref(), elem_size));
                    }
                    _ => unreachable!(),
                }
            }

            Self::Member(ref val, ref member) => {
                // Get the size of the field we want to retrieve.
                let size = self.get_size(env)?;
                // Get the type of the value we want to get a field from.
                let val_type = val.get_type(env)?;
                // Get the size of the value we want to get a field from.
                let val_size = val_type.get_size(env)?;
                // Get the offset of the field from the address of the value.
                let offset = val_type.get_member_offset(member, &self, env)?;
                // Evaluate the value and push it onto the stack.
                val.clone().compile_expr(env, output)?;
                // Copy the contents of the field over top of the value on the stack.
                output.op(CoreOp::Copy {
                    src: SP.deref().offset(1 - val_size as isize + offset as isize),
                    dst: SP.deref().offset(1 - val_size as isize),
                    size,
                });
                // Pop the remaining elements off the stack, so the field remains.
                output.op(CoreOp::Pop(None, val_size - size));
            }

            Self::Refer(val) => match *val {
                // Get the reference of a variable.
                Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                    // Get the variable's offset from the frame pointer.
                    if let Some((_, offset)) = env.get_var(&name) {
                        // Calulate the address of the variable from the offset
                        output.op(CoreOp::Many(vec![
                            CoreOp::Move { src: FP, dst: A },
                            CoreOp::Set(B, *offset),
                            // Index the frame pointer with the offset of the variable.
                            // This is the address of the variable.
                            CoreOp::Index {
                                src: A,
                                offset: B,
                                dst: C,
                            },
                            // Push the address of the variable onto the stack.
                            CoreOp::Push(C, 1),
                        ]))
                    } else {
                        // Return an error if the symbol isn't defined.
                        return Err(Error::SymbolNotDefined(name.clone()));
                    }
                }
                Expr::Deref(ptr) => {
                    // The address of a dereferenced value is just the inner, dereferenced value.
                    ptr.compile_expr(env, output)?;
                }
                Expr::Member(val, name) => {
                    // Get the type of the value we want to get a field from.
                    let val_type = val.get_type(env)?;
                    // Push the address of the struct, tuple, or union onto the stack.
                    Self::Refer(val.clone()).compile_expr(env, output)?;
                    // Calculate the offset of the field from the address of the value.
                    let offset = val_type.get_member_offset(&name, &*val, env)?;

                    output.op(CoreOp::Pop(Some(A), 1));
                    output.op(CoreOp::Set(B, offset as isize));
                    // Index the address of the struct, tuple, or union with the offset of the field.
                    // This is the address of the field.
                    output.op(CoreOp::Index {
                        src: A,
                        offset: B,
                        dst: C,
                    });
                    // Push this address to the stack.
                    output.op(CoreOp::Push(C, 1));
                }

                Expr::Index(val, idx) => {
                    // Get the type of the value we want to index.
                    let val_type = val.get_type(env)?;
                    match val_type {
                        // If the value is an array:
                        Type::Array(ref elem, _) => {
                            // Push the address of the array onto the stack.
                            Self::Refer(val.clone()).compile_expr(env, output)?;
                            // Push the index onto the stack.
                            idx.compile_expr(env, output)?;

                            // Get the size of the element we are indexing.
                            let elem_size = elem.get_size(env)?;
                            // Store the index in `B`.
                            output.op(CoreOp::Pop(Some(B), 1));
                            // Store the address of the array in `A`.
                            output.op(CoreOp::Pop(Some(A), 1));
                            // Store the size of the element in `C`.
                            output.op(CoreOp::Set(C, elem_size as isize));

                            // Calculate the offset of the element from the address of the array.
                            // (the index times the size of the element).
                            output.op(CoreOp::Mul { dst: B, src: C });

                            // Index the address of the array with the offset of the element.
                            // This is the address of the element.
                            output.op(CoreOp::Index {
                                src: A,
                                offset: B,
                                dst: C,
                            });
                            // Push the address of the element onto the stack.
                            output.op(CoreOp::Push(C, 1));
                        }
                        // If the value is a pointer:
                        Type::Pointer(elem) => {
                            // Push the index onto the stack.
                            idx.compile_expr(env, output)?;
                            // Push the pointer onto the stack.
                            val.compile_expr(env, output)?;

                            // Get the size the element we are indexing.
                            let elem_size = elem.get_size(env)?;
                            // Store the pointer in `A`.
                            output.op(CoreOp::Pop(Some(A), 1));
                            // Store the index in `B`.
                            output.op(CoreOp::Pop(Some(B), 1));
                            // Store the size of the element in `C`.
                            output.op(CoreOp::Set(C, elem_size as isize));

                            // Calculate the offset of the element from the pointer.
                            // (the index times the size of the element).
                            output.op(CoreOp::Mul { dst: B, src: C });
                            output.op(CoreOp::Index {
                                src: A.deref(),
                                offset: B,
                                dst: C,
                            });
                            // Push the address of the element onto the stack.
                            output.op(CoreOp::Push(C, 1));
                        }
                        _ => return Err(Error::InvalidIndex(Expr::Index(val, idx))),
                    }
                }

                other => return Err(Error::InvalidRefer(other)),
            },
        }
        Ok(())
    }
}

impl GetType for Expr {
    fn get_type_checked(&self, env: &Env, i: usize) -> Result<Type, Error> {
        let i = i + 1;

        Ok(match self {
            Self::ConstExpr(c) => c.get_type_checked(env, i)?,
            Self::Many(exprs) => {
                if let Some(expr) = exprs.last() {
                    expr.get_type_checked(env, i)?
                } else {
                    Type::None
                }
            }

            Self::As(_, t) => t.clone(),

            Self::Add(a, b)
            | Self::Sub(a, b)
            | Self::Mul(a, b)
            | Self::Div(a, b)
            | Self::Rem(a, b) => match (a.get_type(env)?, b.get_type(env)?) {
                (Type::Float, Type::Float)
                | (Type::Int, Type::Float)
                | (Type::Float, Type::Int) => Type::Float,
                (Type::Cell, Type::Int) | (Type::Int, Type::Cell) | (Type::Int, Type::Int) => {
                    Type::Int
                }
                (Type::Cell, Type::Cell) => Type::Cell,
                _ => return Err(Error::InvalidBinop(self.clone())),
            },

            Self::And(_, _) | Self::Or(_, _) | Self::Not(_) => Type::Bool,

            Self::Const(var, const_val, ret) => {
                let mut new_env = env.clone();
                new_env
                    .consts
                    .insert(var.clone(), const_val.clone().eval(env)?);

                ret.get_type_checked(&new_env, i)?
            }

            Self::Let(var, t, val, ret) => {
                let mut new_env = env.clone();
                new_env.def_var(
                    var.clone(),
                    match t {
                        Some(t) => t.clone(),
                        None => val.get_type_checked(env, i)?,
                    },
                )?;

                ret.get_type_checked(&new_env, i)?
            }

            Self::While(_, _) => Type::Never,

            Self::If(_, t, _) => t.get_type_checked(env, i)?,
            Self::When(c, t, e) => {
                if c.clone().as_bool(env)? { t } else { e }.get_type_checked(env, i)?
            }

            Self::Refer(expr) => Type::Pointer(Box::new(expr.get_type_checked(env, i)?)),
            Self::Deref(expr) => {
                if let Type::Pointer(inner) = expr.get_type_checked(env, i)? {
                    *inner
                } else {
                    return Err(Error::DerefNonPointer(self.clone()));
                }
            }
            Self::DerefMut(_, _) => Type::None,

            Self::Return(_) => Type::Never,
            Self::Apply(func, _) => {
                if let Type::Proc(_, ret) = func.get_type_checked(env, i)? {
                    *ret
                } else {
                    return Err(Error::ApplyNonProc(self.clone()));
                }
            }

            Self::Tuple(items) => Type::Tuple(
                items
                    .clone()
                    .into_iter()
                    .map(|c| c.get_type_checked(env, i))
                    .collect::<Result<Vec<Type>, Error>>()?,
            ),
            Self::Array(items) => Type::Array(
                Box::new(if !items.is_empty() {
                    items[0].get_type_checked(env, i)?
                } else {
                    Type::Any
                }),
                Box::new(ConstExpr::Int(items.len() as i32)),
            ),
            Self::Struct(fields) => Type::Struct(
                fields
                    .clone()
                    .into_iter()
                    .map(|(k, c)| Ok((k, c.get_type_checked(env, i)?)))
                    .collect::<Result<BTreeMap<String, Type>, Error>>()?,
            ),

            Self::Union(t, _, _) => t.clone(),

            Self::Member(val, field) => {
                let as_symbol = field.clone().as_symbol(env);
                let as_int = field.clone().as_int(env);
                match val.get_type_checked(env, i)? {
                    Type::Tuple(items) => {
                        let n = as_int? as usize;
                        if n < items.len() {
                            items[n].clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    Type::Struct(fields) => {
                        if let Some(t) = fields.get(&as_symbol?) {
                            t.clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }
                    Type::Union(types) => {
                        if let Some(t) = types.get(&as_symbol?) {
                            t.clone()
                        } else {
                            return Err(Error::MemberNotFound(*val.clone(), field.clone()));
                        }
                    }

                    _ => return Err(Error::MemberNotFound(*val.clone(), field.clone())),
                }
            }

            Self::Index(val, _) => match val.get_type_checked(env, i)? {
                Type::Array(item, _) => *item,
                Type::Pointer(item) => *item,

                _ => return Err(Error::InvalidIndex(self.clone())),
            },
        })
    }
}
