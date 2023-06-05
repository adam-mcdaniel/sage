//! # Poly-Procedure
//!
//! A polymorphic procedure of LIR code which can be applied to a list of arguments with type arguments.
//! This is mono-morphed into a `Procedure` when it is called with a list of type arguments.
//! A procedure is compiled down to a label in the assembly code.
use crate::lir::{ConstExpr, Env, Error, Expr, GetSize, GetType, Simplify, Type, TypeCheck};
use core::fmt;
use std::{collections::HashMap, rc::Rc, sync::Mutex};

use super::Procedure;

/// A polymorphic procedure of LIR code which can be applied to a list of arguments with type arguments.
/// This is mono-morphed into a `Procedure` when it is called with a list of type arguments.
/// A procedure is compiled down to a label in the assembly code.
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
    monomorphs: Rc<Mutex<HashMap<String, Procedure>>>,
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
    /// Construct a new polymorphic procedure with type parameters, a list of arguments + their types,
    /// a return type, and the body of the procedure.
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

    /// Take some type arguments and produce a monomorphized version of the procedure.
    /// This monomorphized version can then be compiled directly. Additionally, the
    /// mono version of the procedure is memoized, so that it is only compiled once.
    pub fn monomorphize(&self, ty_args: Vec<Type>, env: &Env) -> Result<Procedure, Error> {
        // This is a helper function to distribute the defined type
        // arguments over the body and arguments of the function.

        let simplified_ty_args = ty_args
            .clone()
            .into_iter()
            .map(|ty| {
                ty.simplify_until_matches(env, Type::Any, |t, env| {
                    t.get_size(env).map(|_| t.is_simple())
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let bind_type_args = |mut ty: Type| -> Result<Type, Error> {
            // for (name, arg) in self.ty_params.iter().zip(ty_args.iter()) {
            //     // ty = Type::let_bind(name, if arg == &Type::Symbol(name.clone()) {
            //     //     Type::Unit(name.clone(), Box::new(Type::Any))
            //     // } else {
            //     //     arg.clone()
            //     // }, ty)
            //     ty = ty.clone().substitute(name, arg);
            //     // ty = Type::let_bind(&name, if arg == &Type::Symbol(name.clone()) {
            //     //     Type::Unit(name.clone(), Box::new(Type::Any))
            //     // } else {
            //     //     arg.clone()
            //     // }, ty);
            // }

            // Type::Apply(Box::new(Type::Poly(self.ty_params.clone(), Box::new(ty))), ty_args.clone()).simplify(env)?.perform_template_applications(env, &mut HashMap::new(), 0)

            let ty = Type::Apply(
                Box::new(Type::Poly(self.ty_params.clone(), Box::new(ty))),
                simplified_ty_args.clone(),
            );
            ty.simplify_until_matches(env, Type::Any, |t, env| {
                t.get_size(env).map(|_| t.is_simple())
            })
        };

        // Distribute the type parameters over the body and arguments of the function.
        let args = self
            .args
            .clone()
            .into_iter()
            .map(|(name, t)| Ok((name, bind_type_args(t)?)))
            .collect::<Result<Vec<_>, Error>>()?;
        let ret = bind_type_args(self.ret.clone())?;
        let mut body = *self.body.clone();
        // eprintln!("Before substitution: {}", body);
        body.substitute_types(&self.ty_params, &simplified_ty_args);
        // eprintln!("After substitution: {}", body);
        body = Expr::LetTypes(
            self.ty_params
                .iter()
                .zip(simplified_ty_args.iter())
                .map(|(a, b)| (a.clone(), b.clone()))
                .collect(),
            Box::new(body),
        );

        let mangled_name = format!("__MONOMORPHIZED_({ty_args:?}){}{args:?}{ret:?}", self.name);

        let mut monomorphs = self.monomorphs.lock().unwrap();
        let monomorph = monomorphs
            .entry(mangled_name.clone())
            .or_insert_with(|| Procedure::new(Some(mangled_name.clone()), args, ret, body))
            .clone();
        drop(monomorphs);
        // eprintln!("monomorphized: {}", monomorph);

        Ok(monomorph)
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

    fn substitute(&mut self, name: &str, ty: &Type) {
        if self.ty_params.contains(&name.to_string()) {
            return;
        }
        self.args
            .iter_mut()
            .for_each(|(_, t)| *t = t.substitute(name, ty));
        self.ret = self.ret.substitute(name, ty);
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
        // Ok(())
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
