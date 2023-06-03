//! # Procedure
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
use crate::lir::{Compile, ConstExpr, Env, Error, Expr, GetSize, GetType, Type, TypeCheck, Simplify};
use core::fmt;
use std::{rc::Rc, sync::Mutex, collections::{BTreeMap, HashMap}};

// TODO: Do this without lazy_static.
use lazy_static::lazy_static;
lazy_static! {
    static ref LAMBDA_COUNT: Mutex<usize> = Mutex::new(0);
}

/// A polymorphic procedure of LIR code which can be applied to a list of arguments with type arguments.
#[derive(Clone, Debug)]
pub struct PolyProcedure {
    /// The name of the procedure.
    name: String,
    /// The type parameters of the procedure.
    ty_params: Vec<String>,
    /// The arguments of the procedure.
    args: Vec<(String, Type)>,
    /// The return type of the procedure.
    ret: Type,
    /// The body of the procedure.
    body: Box<Expr>,
    /// The monomorphs of the procedure.
    monomorphs: Rc<Mutex<HashMap<String, Procedure>>>
}

impl PartialEq for PolyProcedure {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
        && self.ty_params == other.ty_params
        && self.args == other.args
        && self.ret == other.ret
        && self.body == other.body
    }
}

impl PolyProcedure {
    pub fn new(
        name: String,
        ty_params: Vec<String>,
        args: Vec<(String, Type)>,
        ret: Type,
        body: impl Into<Expr>,
    ) -> Self {
        Self {
            name,
            ty_params,
            args,
            ret,
            body: Box::new(body.into()),
            monomorphs: Rc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn mark_monomorph_compiled(&self, name: &String) {
        self.monomorphs.lock().unwrap().get_mut(name).unwrap().compiled = true;
    }

    pub fn monomorphize(&self, ty_args: Vec<Type>, env: &Env) -> Result<(Procedure, ConstExpr), Error> {
        // eprintln!("Monomorphizing {} with {:?}", self.name, ty_args);
        // ConstExpr::LetTypes(
        //     self.ty_params.clone().into_iter().zip(ty_args).collect(),
        //     Box::new(ConstExpr::proc(
        //         Some(self.name.clone()),
        //         self.args.clone(),
        //         self.ret.clone(),
        //         *self.body.clone(),
        //     )),
        // )
        
        // let mut lambda_count = LAMBDA_COUNT.lock().unwrap();
        // *lambda_count += 1;
        // let uuid = *lambda_count;
        // drop(lambda_count);
        
        let bind_type_args = |mut ty| {
            for (name, arg) in self.ty_params.iter().zip(ty_args.iter()) {
                if arg == &Type::Symbol(name.clone()) {
                    continue;
                }
                ty = Type::let_bind(name, arg.clone(), ty)
            }
            // Ok::<Type, Error>(ty)
            ty.simplify(env)
        };
        
        // Distribute the type parameters over the body and arguments of the function.
        let args = self.args.clone().into_iter().map(|(name, t)| Ok((name, bind_type_args(t)?))).collect::<Result<Vec<_>, Error>>()?;
        let ret = bind_type_args(self.ret.clone())?;
        let body = Expr::LetTypes(self.ty_params.iter().zip(ty_args.iter()).map(|(a, b)| (a.clone(), b.clone())).collect(), self.body.clone());

        let mangled_name = format!("__MONOMORPHIZED_({ty_args:?}){}{args:?}{ret:?}", self.name);

        let mut monomorphs = self.monomorphs.lock().unwrap();
        let monomorph = monomorphs.entry(mangled_name.clone()).or_insert_with(|| Procedure::new(
            Some(mangled_name.clone()),
            args,
            ret,
            body,
        )).clone();
        drop(monomorphs);

        Ok((monomorph, ConstExpr::Symbol(mangled_name)))
    }
}

impl GetType for PolyProcedure {
    fn get_type_checked(&self, _env: &Env, _i: usize) -> Result<Type, Error> {
        Ok(Type::Poly(
            self.ty_params.clone(),
            Box::new(Type::Proc(
                self.args.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(self.ret.clone()),
            )),
        ))
    }
}

impl TypeCheck for PolyProcedure {
    fn type_check(&self, env: &Env) -> Result<(), Error> {
        // Create a new scope for the procedure's body, and define the arguments for the scope.
        let mut new_env = env.new_scope();
        for ty_param in &self.ty_params {
            new_env.define_type(ty_param, Type::Unit(ty_param.clone(), Box::new(Type::None)));
        }

        new_env.define_args(self.args.clone())?;
        new_env.set_expected_return_type(self.ret.clone());

        // Typecheck the types of the arguments and return value
        for (_, t) in &self.args {
            t.type_check(&new_env)?;
        }
        self.ret.type_check(&new_env)?;

        // Get the type of the procedure's body, and confirm that it matches the return type.
        let body_type = self.body.get_type(&new_env)?;
        // eprintln!("body_type: {}", body_type);
        if !body_type.equals(&self.ret, &new_env)? {
            Err(Error::MismatchedTypes {
                expected: self.ret.clone(),
                found: body_type,
                expr: ConstExpr::PolyProc(self.clone()).into(),
            })
        } else {
            // Typecheck the procedure's body.
            self.body.type_check(&new_env)
        }
    }
}

impl fmt::Display for PolyProcedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "proc[")?;
        for (i, ty_param) in self.ty_params.iter().enumerate() {
            write!(f, "{}", ty_param)?;
            if i < self.ty_params.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "](")?;
        for (i, (name, ty)) in self.args.iter().enumerate() {
            write!(f, "{name}: {ty}")?;
            if i < self.args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, ") -> {} = {}", self.ret, self.body)
    }
}

/// A procedure of LIR code which can be applied to a list of arguments.
#[derive(Clone, Debug, PartialEq)]
pub struct Procedure {
    /// The name of the procedure, if it was given one.
    common_name: Option<String>,
    /// The generated name of the procedure created by the compiler to be unique.
    mangled_name: String,
    /// The arguments of the procedure, and their types.
    args: Vec<(String, Type)>,
    /// The return type of the procedure
    ret: Type,
    /// The procedure's body expression
    body: Box<Expr>,
    /// Has this procedure been compiled yet?
    pub compiled: bool,
}

impl Procedure {
    /// Construct a new procedure with a given list of arguments and their types,
    /// a return type, and the body of the procedure.
    pub fn new(
        common_name: Option<String>,
        args: Vec<(String, Type)>,
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
            compiled: false,
        }
    }

    /// Get the mangled name of the procedure.
    /// The procedure's mangled name is used to store the procedure in the environment.
    pub fn get_mangled_name(&self) -> &str {
        &self.mangled_name
    }

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
        // Typecheck the types of the arguments and return value
        for (_, t) in &self.args {
            t.type_check(env)?;
        }
        self.ret.type_check(env)?;

        // Create a new scope for the procedure's body, and define the arguments for the scope.
        let mut new_env = env.new_scope();
        new_env.define_args(self.args.clone())?;
        new_env.set_expected_return_type(self.ret.clone());

        // Get the type of the procedure's body, and confirm that it matches the return type.
        let body_type = self.body.get_type(&new_env)?;
        if !body_type.equals(&self.ret, &env)? {
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
            self.args.iter().map(|(_, t)| t.clone()).collect(),
            Box::new(self.ret.clone()),
        ))
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
        // Declare the function body
        output.op(CoreOp::Fn(self.mangled_name.clone()));
        if let Some(common_name) = self.common_name {
            output.comment(format!("{}({})", common_name, args_size));
        }
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
        output.op(CoreOp::SetLabel(A, self.mangled_name));
        output.op(CoreOp::Push(A, 1));
        output.comment("done".to_string());
        Ok(())
    }
}

impl fmt::Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "proc(")?;
        for (i, (name, ty)) in self.args.iter().enumerate() {
            write!(f, "{name}: {ty}")?;
            if i < self.args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, ") -> {} = {}", self.ret, self.body)
    }
}
