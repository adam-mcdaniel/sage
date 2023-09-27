//! # Compile
//!
//! This module contains the `Compile` trait, which allows an LIR expression to be compiled to one of the two variants of the assembly language.
//!
//! ## Compilation Process
//!
//! How does the compiler compile an LIR expression into an assembly program?
//!
//! 1. First, type check the expression.
//! 2. Then, attempt to compile the expression into a core assembly program.
//! 3. If the expression cannot be compiled into a core assembly program, then compile it into a standard assembly program.
use super::*;
use crate::asm::{
    AssemblyProgram, CoreOp, CoreProgram, StandardOp, StandardProgram, A, B, C, FP, SP,
};
use crate::NULL;

use log::{trace, error, info, warn};

/// A trait which allows an LIR expression to be compiled to one of the
/// two variants of the assembly language.
pub trait Compile: TypeCheck {
    /// Compile the expression into an assembly program.
    ///
    /// On success, this will return an Ok value containing either a successfully
    /// compiled core assembly program, or a fallback standard assembly program.
    ///
    /// On an error, this will return an Err value containing the error.
    fn compile(self) -> Result<Result<CoreProgram, StandardProgram>, Error>
    where
        Self: Sized + Clone,
    {
        info!("Type checking...");
        // First, type check the expression.
        self.type_check(&Env::default())?;
        info!("Type checked successfully.");
        // Then, attempt to compile the expression into a core assembly program.
        let mut core_asm = CoreProgram::default();

        info!("Compiling...");
        // If the expression cannot be compiled into a core assembly program,
        // then compile it into a standard assembly program.
        if let Err(err) = self
            .clone()
            // Compile the expression into the core assembly program.
            .compile_expr(&mut Env::default(), &mut core_asm)
        {
            warn!("Failed to compile into core assembly program: {err}, falling back on standard assembly");
            let mut std_asm = StandardProgram::default();
            // Compile the expression into the standard assembly program.
            self.compile_expr(&mut Env::default(), &mut std_asm)?;
            info!("Compiled to standard assembly successfully.");
            // Return the fallback standard assembly program.
            Ok(Err(std_asm))
        } else {
            info!("Compiled to core assembly successfully.");
            // Return the successfully compiled core assembly program.
            Ok(Ok(core_asm))
        }
    }
    // Compile a specific expression into an assembly program.
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error>;
}

/// Compile an LIR expression into several core assembly instructions.
impl Compile for Expr {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        trace!("Compiling expression {self} in environment {env}");
        let mut debug_str = format!("{self:50}");
        debug_str.truncate(50);

        // Write a little comment about what we're compiling.
        if !matches!(self, Self::ConstExpr(_)) {
            let mut comment = format!("{self}");
            comment.truncate(70);
        }

        // Compile the expression.
        match self {
            Self::Annotated(expr, metdata) => {
                // Compile the expression.
                expr.compile_expr(env, output)
                    .map_err(|e| e.annotate(metdata))?;
            }

            Self::Match(expr, branches) => {
                // Generate the pattern matching code.
                Pattern::match_pattern(&expr, &branches, env)?
                    // Compile the pattern matching code.
                    .compile_expr(env, output)?;
            }

            Self::IfLet(pat, expr, t, e) => {
                // Generate the pattern matching code.
                pat.if_let_pattern(&expr, &t, &e, env)?
                    // Compile the pattern matching code.
                    .compile_expr(env, output)?;
            }

            Self::UnaryOp(unop, expr) => {
                // Compile the unary operation on the expression.
                unop.compile(&expr, env, output)?;
            }
            Self::BinaryOp(binop, lhs, rhs) => {
                // Compile the binary operation on the two expressions.
                binop.compile(&lhs, &rhs, env, output)?;
            }
            Self::TernaryOp(ternop, a, b, c) => {
                // Compile the ternary operation on the three expressions.
                ternop.compile(&a, &b, &c, env, output)?;
            }
            Self::AssignOp(op, dst, src) => {
                // Compile the assignment operation on the two expressions.
                op.compile(&dst, &src, env, output)?;
            }

            // Compile a constant expression.
            Self::ConstExpr(expr) => expr.compile_expr(env, output)?,
            // Compile a block of expressions.
            Self::Many(exprs) => {
                for expr in exprs {
                    // Compile the expression in the block.
                    expr.compile_expr(env, output)?;
                }
            }

            // Compile a type cast.
            Self::As(ref expr, ref t) => {
                // Compile the expression.
                expr.clone().compile_expr(env, output)?;
                // Cast the expression to the specified type.
                match (expr.get_type(env)?, t.clone()) {
                    // If the cast is an integer to a float,
                    // then convert the integer to a float.
                    (Type::Int, Type::Float) => {
                        output.std_op(StandardOp::ToFloat(SP.deref()))?;
                    }
                    // If the cast is a float to an integer,
                    // then convert the float to an integer.
                    (Type::Float, Type::Int) => {
                        output.std_op(StandardOp::ToInt(SP.deref()))?;
                    }
                    // If the cast is to a type of the same size,
                    // we will trust the user.
                    (a, b) if a.get_size(env)? == b.get_size(env)? => {}
                    // Otherwise, the cast is invalid.
                    (a, b) => {
                        return Err(Error::InvalidAs(self, a, b));
                    }
                }
            }
            
            Self::Apply(f, args) => {
                let self_clone = Self::Apply(f.clone(), args.clone());
                if let Self::Annotated(expr, metadata) = *f {
                    // Compile the inner expression.
                    return Self::Apply(expr, args)
                        .compile_expr(env, output)
                        .map_err(|e| {
                            // If the inner expression fails to compile,
                            // then add the source location to the error.
                            e.annotate(metadata)
                        });
                }

                if !matches!(*f, Expr::Member(_, _)) {
                    // Push the arguments to the procedure on the stack.
                    for arg in &args {
                        // Compile the argument (push it on the stack)
                        arg.clone().compile_expr(env, output)?;
                    }
                }

                // Apply the procedure to the arguments on the stack.
                match *f.clone() {
                    // If the procedure is a core builtin,
                    Expr::ConstExpr(ConstExpr::CoreBuiltin(builtin)) => {
                        // Apply the core builtin to the arguments on the stack.
                        builtin.compile_expr(env, output)?;
                    }
                    // If the procedure is a standard builtin,
                    Expr::ConstExpr(ConstExpr::StandardBuiltin(builtin)) => {
                        // Apply the standard builtin to the arguments on the stack.
                        builtin.compile_expr(env, output)?;
                    }
                    // If the procedure is a foreign function,
                    Expr::ConstExpr(ConstExpr::FFIProcedure(ffi)) => {
                        // Apply the foreign function to the arguments on the stack.
                        ffi.compile_expr(env, output)?;
                    }
                    // If the procedure is a symbol, get the procedure from the environment.
                    Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                        match env.get_const(&name) {
                            // If the procedure is a core builtin,
                            Some(ConstExpr::CoreBuiltin(builtin)) => {
                                builtin.clone().compile_expr(env, output)?;
                            }
                            // If the procedure is a standard builtin,
                            Some(ConstExpr::StandardBuiltin(builtin)) => {
                                builtin.clone().compile_expr(env, output)?;
                            }
                            // If the procedure is a foreign function,
                            Some(ConstExpr::FFIProcedure(ffi)) => {
                                ffi.clone().compile_expr(env, output)?;
                            }
                            // Otherwise, it must be a procedure.
                            _ => {
                                // Push the procedure on the stack.
                                ConstExpr::Symbol(name).compile_expr(env, output)?;
                                // Pop the "function pointer" from the stack.
                                output.op(CoreOp::Pop(Some(A), 1));
                                // Call the procedure on the arguments.
                                output.op(CoreOp::Call(A));
                            }
                        }
                    }
                    Expr::Member(val, name) => {
                        // Try to get the member of the underlying type.
                        if self_clone.is_method_call(env)? {
                            self_clone.transform_method_call(env)?.compile_expr(env, output)?;
                        } else {
                            // Push the arguments to the procedure on the stack.
                            for arg in &args {
                                // Compile the argument (push it on the stack)
                                arg.clone().compile_expr(env, output)?;
                            }

                            // Compile it normally:
                            // Push the procedure on the stack.
                            val.field(name).compile_expr(env, output)?;
                            // Pop the "function pointer" from the stack.
                            output.op(CoreOp::Pop(Some(A), 1));
                            // Call the procedure on the arguments.
                            output.op(CoreOp::Call(A));
                        }
                    }
                    // Otherwise, it must be a procedure.
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
            // Compile a return statement.
            Self::Return(e) => {
                // Get the size of the arguments and return value.
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

                // Because we could be terminating the function at an
                // arbitrary point on the stack, we have to make the stack
                // pointer point to the beginning of the stack frame.
                output.op(CoreOp::GetAddress {
                    addr: FP.deref(),
                    dst: SP,
                });

                // Decrement the stack pointer by the difference between the size of the
                // arguments and return value, to leave the return value on the stack.
                output.op(CoreOp::Prev(
                    SP,
                    Some(args_size as isize - ret_size as isize),
                ));
                output.op(CoreOp::Return);
            }

            // Compile a declaration statement.
            Self::Declare(declaration, body) => {
                // Create a new scope
                declaration.compile(*body, env, output)?;
            }

            // Compile a while loop.
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

            // Compile an if statement.
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

            // Compile a compile time if statement.
            Self::When(c, t, e) => if c.as_bool(env)? { t } else { e }.compile_expr(env, output)?,

            // Compile a dereference operation.
            Self::Deref(ptr) => {
                // Compile the pointer
                let ptr_type = ptr.get_type(env)?;
                ptr.clone().compile_expr(env, output)?;
                // If the pointer is a pointer, dereference it.
                if let Type::Pointer(_, inner) = ptr_type {
                    // Pop the address into A
                    output.op(CoreOp::Pop(Some(A), 1));
                    // Push all of the data at the address onto the stack.
                    output.op(CoreOp::Push(A.deref(), inner.get_size(env)?));
                } else {
                    return Err(Error::DerefNonPointer(*ptr));
                }
            }

            // Compile an assignment operation to a pointer.
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

            // Compile an array literal.
            Self::Array(elems) => {
                // Compile the elements
                for elem in elems {
                    elem.compile_expr(env, output)?;
                }
            }

            // Compile a tuple literal.
            Self::Tuple(items) => {
                // Compile the items
                for item in items {
                    item.compile_expr(env, output)?;
                }
            }

            // Compile a struct literal.
            Self::Struct(items) => {
                // Compile the items
                for (_, val) in items {
                    val.compile_expr(env, output)?;
                }
            }

            // Compile a union literal.
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

            // Compile a tagged union literal.
            Self::EnumUnion(t, variant, val) => {
                // Get the size of the tagged union.
                let result_size = t.get_size(env)?;
                let t = t.simplify_until_concrete(env)?;
                if let Type::EnumUnion(fields) = t {
                    // Get the list of possible variant names.
                    let variants = fields.clone().into_keys().collect::<Vec<_>>();
                    // Get the value of the tag associated with this variant.
                    if let Some(tag_value) = Type::variant_index(&variants, &variant) {
                        // Get the size of the value we are storing in the union.
                        let val_size = val.get_size(env)?;

                        // Evaluate the value and push it onto the stack
                        val.compile_expr(env, output)?;

                        // Increment the stack pointer to pad out the union.
                        output.op(CoreOp::Next(
                            SP,
                            // This size *includes* the tag: it allocates space for it so we
                            // can immediately set the value under the stack poiner as the tag.
                            Some(result_size as isize - val_size as isize),
                        ));

                        output.op(CoreOp::Set(SP.deref(), tag_value as i64));
                        return Ok(());
                    } else {
                        // If we could not find the variant return an error.
                        return Err(Error::VariantNotFound(Type::EnumUnion(fields), variant));
                    }
                } else {
                    return Err(Error::VariantNotFound(t.clone(), variant));
                }
            }

            // Compile an indexing operation.
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
                        // First, lets try to compile the same index expression using pointer
                        // arithmetic. This will be faster than pushing the entire array
                        // onto the stack and indexing it.
                        let optimized_idx = val
                            .clone()
                            // Reference the current
                            .refer(Mutability::Immutable)
                            // Make the type a pointer to the inner element type
                            .as_type(Type::Pointer(Mutability::Immutable, elem.clone()))
                            // Index the new pointer
                            .idx(*idx.clone());
                        // The optimized index *may not be possible* if the array is
                        // not able to be referenced (like an array literal). Type checking
                        // a reference operation will confirm that the array is able to be
                        // referenced. If the array is not a:
                        // 1. Dereference
                        // 2. Index access
                        // 3. Member access
                        // 4. Variable
                        // If the array is a literal, then we will have to push the array
                        // on the stack and essentially do the same address calculations,
                        // but the difference is that the operation is not in-place.
                        if optimized_idx.type_check(env).is_ok() {
                            return optimized_idx.compile_expr(env, output);
                        }

                        // Get the size of the element we will return.
                        let elem_size = elem.get_size(env)?;
                        // Push the array onto the stack.
                        val.compile_expr(env, output)?;
                        // Push the index onto the stack.
                        idx.compile_expr(env, output)?;

                        // Calculate the offset of the element we want to return
                        // (the index times the size of the element), and store it in `B`.
                        output.op(CoreOp::Pop(Some(B), 1));
                        output.op(CoreOp::Set(A, elem_size as i64));
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
                    // If the value being indexed is a pointer:
                    Type::Pointer(_, elem) => {
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
                        output.op(CoreOp::Set(C, elem_size as i64));
                        // Store the offset of the element from the pointer in `B`
                        // (the index times the size of the element).
                        output.op(CoreOp::Mul { dst: B, src: C });
                        // Get the address of the element and store it in `C`.
                        output.op(CoreOp::Index {
                            src: A,
                            offset: B,
                            dst: C,
                        });
                        // Push the contents of the element onto the stack.
                        output.op(CoreOp::Push(C.deref(), elem_size));
                    }
                    // Otherwise, we can't index this value.
                    _ => unreachable!(),
                }
            }

            // Compile a member access operation.
            Self::Member(ref val, ref member) => {
                // If the value we're getting a field from is a pointer,
                // then dereference it and get the field from the value.
                match val.get_type(env)? {
                    Type::Pointer(_, _) => {
                        val.clone()
                            .deref()
                            .field(member.clone())
                            .compile_expr(env, output)?;
                    }
                    Type::Type(ty) => {
                        let member_as_symbol = member.clone().as_symbol(env)?;

                        if let Some(constant) = env.get_associated_const(&ty, &member_as_symbol) {
                            return constant.clone().compile_expr(env, output);
                        } else {
                            return Err(Error::SymbolNotDefined(member_as_symbol));
                        }
                    }
                    val_type => {
                        // Get the size of the field we want to retrieve.
                        let size = self.get_size(env)?;
                        // Get the size of the value we want to get a field from.
                        let val_size = val_type.get_size(env)?;
                        // Get the offset of the field from the address of the value.
                        if let Ok((_, offset)) = val_type.get_member_offset(member, &self, env) {
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
                        } else {
                            // Try to get the member of the underlying type.
                            let name = member.clone().as_symbol(env)?;
                            return env.get_associated_const(&val_type, &name).ok_or_else(|| {
                                // If we could not find the member return an error.
                                Error::MemberNotFound(self.clone(), member.clone())
                            }).and_then(|constant| {
                                constant.clone().compile_expr(env, output)
                            });
                        }
                    }
                }
            }

            // Compile a reference operation (on a symbol or a field of a value).
            Self::Refer(expected_mutability, val) => match *val.clone() {
                // Get the value being referenced
                Expr::Annotated(expr, metdata) => Self::Refer(expected_mutability, expr)
                    .compile_expr(env, output)
                    .map_err(|e| e.annotate(metdata))?,

                Expr::ConstExpr(ConstExpr::Annotated(expr, metdata)) => {
                    Self::Refer(expected_mutability, Box::new(Expr::ConstExpr(*expr)))
                        .compile_expr(env, output)
                        .map_err(|e| e.annotate(metdata))?
                }

                // Get the reference of a variable.
                Expr::ConstExpr(ConstExpr::Symbol(name)) => {
                    // Get the variable's offset from the frame pointer.
                    if let Some((found_mutability, _ty, offset)) = env.get_var(&name) {
                        if !found_mutability.can_decay_to(&expected_mutability) {
                            return Err(Error::MismatchedMutability {
                                found: *found_mutability,
                                expected: expected_mutability,
                                expr: Expr::ConstExpr(ConstExpr::Symbol(name)),
                            });
                        }

                        // Calculate the address of the variable from the offset
                        output.op(CoreOp::Many(vec![
                            CoreOp::Move { src: FP, dst: A },
                            CoreOp::Set(B, *offset as i64),
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
                    } else if let Some((found_mutability, _ty, location)) =
                        env.get_static_var(&name)
                    {
                        if !found_mutability.can_decay_to(&expected_mutability) {
                            return Err(Error::MismatchedMutability {
                                found: *found_mutability,
                                expected: expected_mutability,
                                expr: Expr::ConstExpr(ConstExpr::Symbol(name)),
                            });
                        }

                        // Calculate the address of the variable from the offset
                        output.op(CoreOp::Many(vec![
                            // Push the address of the variable onto the stack.
                            CoreOp::Next(SP, None),
                            CoreOp::GetAddress {
                                addr: location.clone(),
                                dst: SP.deref(),
                            },
                        ]))
                    } else {
                        error!("Tried to get the reference of a symbol that isn't a variable: {name} in environment {env}");
                        // Return an error if the symbol isn't defined.
                        return Err(Error::SymbolNotDefined(name.clone()));
                    }
                }
                // Get the reference of a dereferenced value.
                Expr::Deref(ptr) => {
                    // The address of a dereferenced value is just the inner, dereferenced value.
                    ptr.compile_expr(env, output)?;
                }
                // Get the reference of a field of a value.
                Expr::Member(val, name) => {
                    // Get the type of the value we want to get a field from.
                    let val_type = val.get_type(env)?;
                    // Push the address of the struct, tuple, or union onto the stack.
                    match val_type.simplify_until_has_members(env)? {
                        // If the value is a struct, tuple, or union:
                        Type::Struct(_) | Type::Tuple(_) | Type::Union(_) => {
                            // Compile a reference to the inner value with the expected mutability.
                            Self::Refer(expected_mutability, val.clone())
                                .compile_expr(env, output)?;
                        }
                        // If the value is a pointer:
                        Type::Pointer(found_mutability, _) => {
                            // Confirm that the pointer can decay to the expected mutability.
                            if !found_mutability.can_decay_to(&expected_mutability) {
                                // If the pointer cannot decay to the expected mutability,
                                // then return an error.
                                return Err(Error::MismatchedMutability {
                                    found: found_mutability,
                                    expected: expected_mutability,
                                    expr: Expr::Member(val, name),
                                });
                            }
                            // Compile the pointer to get the address of the value.
                            val.clone().compile_expr(env, output)?;
                        }
                        other => {
                            error!("Tried to get a member {name} of a non-struct, non-tuple, non-union, non-pointer type: {other} of value {val} in environment {env}");
                            return Err(Error::InvalidRefer(Expr::Member(val, name)));
                        }
                    }

                    // Calculate the offset of the field from the address of the value.
                    let (_, offset) = val_type.get_member_offset(&name, &val, env)?;

                    output.op(CoreOp::Pop(Some(A), 1));
                    output.op(CoreOp::Set(B, offset as i64));
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
                // Get the reference of an indexed value.
                Expr::Index(val, idx) => {
                    // Get the type of the value we want to index.
                    let val_type = val.get_type(env)?;
                    match val_type {
                        // If the value is an array:
                        Type::Array(ref elem, _) => {
                            // Push the address of the array onto the stack.
                            Self::Refer(expected_mutability, val.clone())
                                .compile_expr(env, output)?;
                            // Push the index onto the stack.
                            idx.compile_expr(env, output)?;

                            // Get the size of the element we are indexing.
                            let elem_size = elem.get_size(env)?;
                            // Store the index in `B`.
                            output.op(CoreOp::Pop(Some(B), 1));
                            // Store the address of the array in `A`.
                            output.op(CoreOp::Pop(Some(A), 1));
                            // Store the size of the element in `C`.
                            output.op(CoreOp::Set(C, elem_size as i64));

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
                        Type::Pointer(found_mutability, elem) => {
                            if !found_mutability.can_decay_to(&expected_mutability) {
                                return Err(Error::MismatchedMutability {
                                    found: found_mutability,
                                    expected: expected_mutability,
                                    expr: Expr::Index(val, idx),
                                });
                            }

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
                            output.op(CoreOp::Set(C, elem_size as i64));

                            // Calculate the offset of the element from the pointer.
                            // (the index times the size of the element).
                            output.op(CoreOp::Mul { dst: B, src: C });
                            output.op(CoreOp::Index {
                                src: A,
                                offset: B,
                                dst: C,
                            });
                            // Push the address of the element onto the stack.
                            output.op(CoreOp::Push(C, 1));
                        }
                        // Otherwise, return an error.
                        _ => return Err(Error::InvalidIndex(Expr::Index(val, idx))),
                    }
                }
                // Otherwise, return an error.
                other => return Err(Error::InvalidRefer(other)),
            },
        }

        // Return success.
        Ok(())
    }
}

/// Compile a constant expression.
impl Compile for ConstExpr {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        trace!("Compiling constant expression {self} in environment {env}");
        let mut debug_str = format!("{self}");
        debug_str.truncate(50);

        let current_instruction = output.current_instruction();
        // Compile the constant expression.
        match self {
            Self::Type(_) => {
                // Do nothing.
            }
            Self::Member(container, member) => {
                match (container.clone().eval(env)?, member.clone().eval(env)?) {
                    (Self::Tuple(tuple), Self::Int(n)) => {
                        // If the index is out of bounds, return an error.
                        if n >= tuple.len() as i64 || n < 0 {
                            return Err(Error::MemberNotFound(
                                (*container).into(),
                                (*member).into(),
                            ));
                        }
                        tuple[n as usize].clone().compile_expr(env, output)?
                    }
                    (Self::Struct(fields), Self::Symbol(name)) => {
                        // If the field is not in the struct, return an error.
                        if !fields.contains_key(&name) {
                            return Err(Error::MemberNotFound(
                                (*container).into(),
                                (*member).into(),
                            ));
                        }
                        fields[&name].clone().compile_expr(env, output)?
                    }
                    (Self::Type(ty), Self::Symbol(name)) => {
                        if let Some(constant) = env.get_associated_const(&ty, &name) {
                            return constant.clone().compile_expr(env, output);
                        } else {
                            return Err(Error::SymbolNotDefined(name));
                        }
                    }
                    _ => {
                        return Err(Error::MemberNotFound(
                            (*container).into(),
                            (*member).into(),
                        ));
                    }
                }
            }
            Self::Annotated(expr, metadata) => {
                expr.compile_expr(env, output)
                    .map_err(|err| err.annotate(metadata))?;
            }
            Self::Declare(bindings, expr) => {
                bindings.compile(Expr::ConstExpr(*expr), env, output)?;
            }
            Self::Monomorphize(expr, ty_args) => match expr.eval(env)? {
                Self::PolyProc(poly_proc) => {
                    // Simplify the type arguments.
                    let ty_args = ty_args
                        .into_iter()
                        .map(|ty| ty.simplify(env))
                        .collect::<Result<Vec<_>, _>>()?;

                    let common_name = poly_proc.get_name();
                    let message = format!(
                        "Monomorphized {common_name} with type arguments {}",
                        ty_args
                            .iter()
                            .map(|ty| format!("{}", ty))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    let current_instruction = output.current_instruction();
                    // Monomorphize the function
                    let proc = poly_proc.monomorphize(ty_args, env)?;
                    // Typecheck the monomorphized function.
                    proc.type_check(env)?;
                    // Compile the monomorphized function.
                    proc.compile_expr(env, output)?;

                    output.log_instructions_after(&common_name, &message, current_instruction);
                }
                val => {
                    return Err(Error::InvalidMonomorphize(val));
                }
            },

            Self::As(expr, _ty) => {
                // Compile a compile time type cast expression.
                expr.compile_expr(env, output)?;
            }
            // Compile a None constant.
            Self::None => {}
            // Compile a null constant.
            Self::Null => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), NULL));
            }
            // Compile a char constant.
            Self::Char(ch) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), ch as usize as i64));
            }
            // Compile a bool constant.
            Self::Bool(x) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), x as i64));
            }
            // Compile a cell value.
            Self::Cell(n) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), n));
            }
            // Compile an integer constant.
            Self::Int(n) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), n));
            }
            // Compile a float constant.
            Self::Float(f) => {
                output.op(CoreOp::Next(SP, None));
                output.std_op(StandardOp::Set(SP.deref(), f))?;
            }
            // Calculate the size of a type.
            Self::SizeOfType(t) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), t.get_size(env)? as i64));
            }
            // Calculate the size of an expression.
            Self::SizeOfExpr(e) => {
                output.op(CoreOp::Next(SP, None));
                output.op(CoreOp::Set(SP.deref(), e.get_size(env)? as i64));
            }
            // Compile a tuple constant.
            Self::Tuple(items) => {
                for item in items {
                    // Compile the item.
                    item.compile_expr(env, output)?;
                }
            }
            // Compile an array constant.
            Self::Array(items) => {
                for item in items {
                    // Compile the item.
                    item.compile_expr(env, output)?;
                }
            }
            // Compile a struct constant.
            Self::Struct(items) => {
                for (_, expr) in items {
                    // Compile the item.
                    expr.compile_expr(env, output)?;
                }
            }
            // Compile a union constant.
            Self::Union(t, _, val) => {
                // Get the size of the padded union.
                let result_size = t.get_size(env)?;
                // Get the size of the value.
                let val_size = val.get_size(env)?;
                // Compile the value.
                val.compile_expr(env, output)?;
                // Pad the value to the size of the union.
                output.op(CoreOp::Next(
                    SP,
                    Some(result_size as isize - val_size as isize),
                ));
            }
            // Compile a tagged union constant.
            Self::EnumUnion(t, variant, val) => {
                // Get the size of the tagged union.
                let result_size = t.get_size(env)?;
                let t = t.simplify_until_has_variants(env)?;

                // Get the inner list of variants and compile the expression using this information.
                if let Type::EnumUnion(variants) = t.clone().simplify(env)? {
                    // Get the list of possible variant names.
                    let variants = variants.into_keys().collect::<Vec<_>>();
                    // Get the value of the tag associated with this variant.
                    if let Some(tag_value) = Type::variant_index(&variants, &variant) {
                        // Get the size of the value we are storing in the union.
                        let val_size = val.get_size(env)?;

                        // Evaluate the value and push it onto the stack
                        val.compile_expr(env, output)?;

                        // Increment the stack pointer to pad out the union.
                        output.op(CoreOp::Next(
                            SP,
                            // This size *includes* the tag: it allocates space for it so we
                            // can immediately set the value under the stack pointer as the tag.
                            Some(result_size as isize - val_size as isize),
                        ));

                        output.op(CoreOp::Set(SP.deref(), tag_value as i64));
                    } else {
                        // If we could not find the variant return an error.
                        return Err(Error::VariantNotFound(t, variant));
                    }
                } else {
                    // If we could not find the variant return an error.
                    return Err(Error::VariantNotFound(t, variant));
                }
            }
            // Compile a core builtin.
            Self::CoreBuiltin(builtin) => {
                builtin.compile_expr(env, output)?;
            }
            // Compile a standard builtin.
            Self::StandardBuiltin(builtin) => {
                builtin.compile_expr(env, output)?;
            }
            // Compile a foreign call.
            Self::FFIProcedure(ffi_proc) => {
                ffi_proc.compile_expr(env, output)?;
            }
            // Compile a procedure.
            Self::Proc(proc) => {
                // Get the mangled name of the procedure.
                let name = proc.get_mangled_name().to_string();

                if !env.has_proc(&name) {
                    // If the procedure is not yet defined, define it.
                    env.define_proc(&name, proc);
                }

                // Push the procedure onto the stack.
                env.push_proc(&name, output)?;
            }

            Self::PolyProc(poly_proc) => {
                return Err(Error::CompilePolyProc(poly_proc));
            }

            Self::TypeOf(expr) => {
                // Get the type of the expression.
                let ty = expr.get_type(env)?;
                // Return the type as a string.
                ConstExpr::Array(ty.to_string().chars().map(ConstExpr::Char).collect())
                    .compile_expr(env, output)?
            }

            // Compile a variant of an enum.
            Self::Of(enum_type, variant) => {
                // Only try to simplify the type 50 times at most.
                // This is to prevent infinite loops and to keep recursion under control.
                match enum_type.simplify_until_has_variants(env)? {
                    // If the type is an enum, we can continue.
                    Type::Enum(variants) => {
                        // Get the index of the variant.
                        if let Some(index) = Type::variant_index(&variants, &variant) {
                            // Push the index of the variant onto the stack.
                            output.op(CoreOp::Set(A, index as i64));
                            output.op(CoreOp::Push(A, 1));
                            return Ok(());
                        } else {
                            // If the variant is not found, return an error.
                            return Err(Error::VariantNotFound(enum_type, variant));
                        }
                    }
                    // If the type is an enum union, we can continue.
                    Type::EnumUnion(variants) if variants.get(&variant) == Some(&Type::None) => {
                        // Get the index of the variant.
                        if let Some(index) = Type::variant_index(
                            variants.into_keys().collect::<Vec<_>>().as_slice(),
                            &variant,
                        ) {
                            // Push the index of the variant onto the stack.
                            // Allocate the size of the structure on the stack by
                            // incrementing the stack pointer by the size of the structure.
                            // Then, set the value under the stack pointer to the index of the variant.
                            output.op(CoreOp::Next(SP, Some(enum_type.get_size(env)? as isize)));
                            output.op(CoreOp::Set(SP.deref(), index as i64));
                            return Ok(());
                        } else {
                            // If the variant is not found, return an error.
                            return Err(Error::VariantNotFound(enum_type, variant));
                        }
                    }
                    _ => {
                        // If the type is not an enum, return an error.
                        return Err(Error::VariantNotFound(enum_type, variant));
                    }
                }
            }

            // Compile a symbol.
            Self::Symbol(name) => {
                // Compile a symbol.
                if let Some((_, t, offset)) = env.get_var(&name) {
                    // If the symbol is a variable, push it onto the stack.
                    output.op(CoreOp::Push(FP.deref().offset(*offset), t.get_size(env)?))
                } else if let Some((_, t, location)) = env.get_static_var(&name) {
                    // If the symbol is a static variable, push it onto the stack.
                    output.op(CoreOp::Push(location.clone(), t.get_size(env)?))
                } else {
                    // If the symbol is not a variable, evaluate it like a constant.
                    match Self::Symbol(name).eval(env)? {
                        // If the symbol isn't a constant, try to get the procedure
                        // with the same name.
                        Self::Symbol(name) => env.push_proc(&name, output)?,
                        // If the symbol is a constant, push it onto the stack.
                        x => x.compile_expr(env, output)?,
                    }
                }
            }
        }
        output.log_instructions_after(&"expr", &debug_str, current_instruction);
        Ok(())
    }
}
