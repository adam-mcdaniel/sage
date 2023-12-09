//! # Mono-Procedure
//!
//! A procedure of LIR code which can be applied to a list of arguments.
//!
//! A procedure is compiled down to a label in the assembly code.
//! The label is called with the `Call` instruction. Procedures are also
//! only compiled once, and referenced by their label every other time
//! they are used.
//!
//! Procedures are created by the `proc` keyword.
use crate::asm::{AssemblyProgram, CoreOp, A, FP, SP};
use crate::lir::{
    Compile, ConstExpr, Env, Error, Expr, GetSize, GetType, Mutability, Type, TypeCheck,
};
use std::hash::{Hash, Hasher};
use core::fmt;
use std::{
    rc::Rc,
    sync::{Mutex, RwLock},
};

use log::trace;

// TODO: Do this without lazy_static. This is used to create unique mangled IDs for each compiled function.
use lazy_static::lazy_static;
lazy_static! {
    // The number of procedures compiled so far.
    static ref LAMBDA_COUNT: Mutex<usize> = Mutex::new(0);
}

/// A monomorphic procedure of LIR code which can be applied to a list of arguments.
/// A procedure is compiled down to a label in the assembly code.
/// The label is called with the `Call` instruction.
#[derive(Clone, Debug)]
pub struct Procedure {
    /// The name of the procedure, if it was given one.
    common_name: Option<String>,
    /// The generated name of the procedure created by the compiler to be unique.
    mangled_name: String,
    /// The arguments of the procedure, and their types.
    args: Vec<(String, Mutability, Type)>,
    /// The return type of the procedure
    ret: Type,
    /// The procedure's body expression
    body: Box<Expr>,
    has_type_checked: Rc<RwLock<bool>>,
}

impl PartialEq for Procedure {
    fn eq(&self, other: &Self) -> bool {
        self.common_name == other.common_name
            && self.mangled_name == other.mangled_name
            && self.args == other.args
            && self.ret == other.ret
            && self.body == other.body
    }
}

impl Procedure {
    /// Construct a new procedure with a given list of arguments and their types,
    /// a return type, and the body of the procedure.
    pub fn new(
        common_name: Option<String>,
        args: Vec<(String, Mutability, Type)>,
        ret: Type,
        body: impl Into<Expr>,
    ) -> Self {
        let mut lambda_count = LAMBDA_COUNT.lock().unwrap();
        *lambda_count += 1;
        Self {
            common_name,
            mangled_name: format!("__LAMBDA_{lambda_count}"),
            args,
            ret,
            body: Box::new(body.into()),
            has_type_checked: Rc::new(RwLock::new(false)),
        }
    }

    /// Get the arguments of the procedure.
    pub fn get_args(&self) -> &[(String, Mutability, Type)] {
        &self.args
    }

    /// Get the return type of the procedure.
    pub fn get_ret(&self) -> &Type {
        &self.ret
    }

    /// Get the body of the procedure.
    pub fn get_body(&self) -> &Expr {
        &self.body
    }

    /// Get the mangled name of the procedure.
    /// The procedure's mangled name is used to store the procedure in the environment.
    pub fn get_mangled_name(&self) -> &str {
        &self.mangled_name
    }

    /// Get the name of the procedure known to the LIR front-end.
    pub fn get_common_name(&self) -> Option<&str> {
        self.common_name.as_deref()
    }

    /// This is just for debugging purposes.
    /// This sets the common name of the procedure known to the user,
    /// or the front-end of the compiler.
    pub fn set_common_name(&mut self, name: impl ToString) {
        self.common_name = Some(name.to_string());
    }

    /// Push this procedure's label to the stack.
    pub fn push_label(&self, output: &mut dyn AssemblyProgram) {
        // Set a register to the address of the procedure's label.
        output.op(CoreOp::SetLabel(A, self.mangled_name.clone()));
        // Push the register to the stack.
        output.op(CoreOp::Push(A, 1));
    }
}

impl TypeCheck for Procedure {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        trace!("type checking procedure: {}", self);

        if *self.has_type_checked.read().unwrap() {
            return Ok(());
        }

        // Mark this procedure as having been type checked.
        *self.has_type_checked.write().unwrap() = true;
        
        // Typecheck the types of the arguments and return value
        for (_, _, t) in &self.args {
            // t.simplify_until_simple(env)?.add_monomorphized_associated_consts(env)?;
            t.type_check(env)?;
        }
        // self.ret.simplify_until_simple(env)?.add_monomorphized_associated_consts(env)?;
        self.ret.type_check(env)?;

        // Create a new scope for the procedure's body, and define the arguments for the scope.
        let mut new_env = env.new_scope();
        new_env.define_args(self.args.clone())?;
        new_env.set_expected_return_type(self.ret.clone());

        // Get the type of the procedure's body, and confirm that it matches the return type.
        let body_type = self.body.get_type(&new_env)?;
        if !body_type.can_decay_to(&self.ret, env)? {
            Err(Error::MismatchedTypes {
                expected: self.ret.clone(),
                found: body_type,
                expr: ConstExpr::Proc(self.clone()).into(),
            })
        } else {
            // Typecheck the procedure's body.
            self.body.type_check(&new_env)
        }
    }
}

impl GetType for Procedure {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Proc(
            self.args.iter().map(|(_, _, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
    }

    fn substitute(&mut self, name: &str, ty: &Type) {
        for (_, _, t) in &mut self.args {
            *t = t.substitute(name, ty);
        }
        self.ret = self.ret.substitute(name, ty);

        self.body.substitute(name, ty);
    }
}

impl Compile for Procedure {
    fn compile_expr(self, env: &mut Env, output: &mut dyn AssemblyProgram) -> Result<(), Error> {
        // Compile the contents of the procedure under a new environment
        let mut new_env = env.new_scope();

        // Declare the arguments and get their size
        let args_size = new_env.define_args(self.args)?;
        // Get the size of the return value to leave on the stack
        let ret_size = self.ret.get_size(env)?;

        // Check to see if the procedure has already been defined
        if output.is_defined(&self.mangled_name) {
            // If the procedure has already been defined, then we don't need to
            // compile it again.
            // Push the procedure label address onto the stack
            output.op(CoreOp::Next(SP, None));
            output.op(CoreOp::SetLabel(SP.deref(), self.mangled_name));
            return Ok(());
        }

        // Declare the function body
        output.op(CoreOp::Fn(self.mangled_name.clone()));
        if let Some(common_name) = &self.common_name {
            output.comment(format!("{}({})", common_name, args_size));
        }
        let current_instruction = output.current_instruction();

        // Execute the body to leave the return value
        self.body.compile_expr(&mut new_env, output)?;

        // Overwrite the arguments with the return value
        output.op(CoreOp::Copy {
            dst: FP.deref().offset(1 - args_size as isize),
            src: SP.deref().offset(1 - ret_size as isize),
            size: ret_size,
        });
        // Decrement the stack pointer by the difference between the size of the
        // arguments and return value, to leave the return value on the stack.
        output.op(CoreOp::Pop(None, args_size));
        // End the function body
        output.op(CoreOp::End);

        output.comment(format!("push {} onto the stack", self.mangled_name));
        // Push the procedure label address onto the stack
        output.op(CoreOp::SetLabel(A, self.mangled_name.clone()));
        output.op(CoreOp::Push(A, 1));

        let name = self
            .common_name
            .as_ref()
            .map(|x| x.as_str())
            .unwrap_or_else(|| "<anonymous>");
        // Log the compiled procedure
        let message = format!("Compiled procedure {common_name} to {mangled_name} with args of size {args_size} and return value of size {ret_size}",
            common_name = name,
            mangled_name = self.mangled_name,
            args_size = args_size,
            ret_size = ret_size,
        );
        output.log_instructions_after(name, &message, current_instruction);

        Ok(())
    }
}

impl fmt::Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "proc(")?;
        for (i, (name, mutability, ty)) in self.args.iter().enumerate() {
            if mutability.is_mutable() {
                write!(f, "mut ")?;
            }
            write!(f, "{name}: {ty}")?;
            if i < self.args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, ") -> {} = {}", self.ret, self.body)
    }
}

impl Eq for Procedure {}

impl Hash for Procedure {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.mangled_name.hash(state);
        // self.args.hash(state);
        // self.ret.hash(state);
    }
}