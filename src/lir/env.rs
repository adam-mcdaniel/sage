//! # Environment
//!
//! This module defines the `Env` type, which is used to store the types, constants, and procedures
//! defined in a given scope. It also stores the variables defined in the scope, and the their offsets
//! with respect to the frame pointer.

use super::{
    AssignOp, BinaryOp, Compile, ConstExpr, Declaration, Error, Expr, FFIProcedure, GetSize,
    GetType, Mutability, PolyProcedure, Procedure, TernaryOp, Type, UnaryOp,
};
use crate::asm::{AssemblyProgram, Globals, Location};
use core::fmt::{Debug, Display, Formatter, Result as FmtResult};

use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, RwLock},
};

use log::*;

/// An environment under which expressions and types are compiled and typechecked.
/// This is essentially the scope of an expression.
#[derive(Clone, Debug)]
pub struct Env {
    /// Unary Operators
    unops: Arc<HashMap<String, Box<dyn UnaryOp>>>,
    /// Binary Operators
    binops: Arc<HashMap<String, Box<dyn BinaryOp>>>,
    /// Ternary Operators
    ternops: Arc<HashMap<String, Box<dyn TernaryOp>>>,
    /// Assignment Operators
    assignops: Arc<HashMap<String, Box<dyn AssignOp>>>,

    /// The types (and also their sizes) defined under the environment.
    types: Arc<HashMap<String, Type>>,
    /// The constants defined under the environment.
    consts: Arc<HashMap<String, ConstExpr>>,
    /// The procedures defined under the environment.
    procs: Arc<HashMap<String, Procedure>>,
    /// The variables defined under the environment.
    vars: Arc<HashMap<String, (Mutability, Type, isize)>>,
    modules: Arc<HashMap<String, Arc<Vec<Declaration>>>>,
    /// The static variables defined under the environment.
    static_vars: Arc<HashMap<String, (Mutability, Type, Location)>>,
    /// A lookup for the offsets of global variables.
    globals: Arc<RwLock<Globals>>,

    processed_monomorphizations: Arc<RwLock<HashMap<Type, Vec<Type>>>>,
    /// Associated constants for types.
    associated_constants: Arc<RwLock<HashMap<Type, HashMap<String, (ConstExpr, Type)>>>>,
    type_checked_consts: Arc<RwLock<HashSet<ConstExpr>>>,

    /// The current offset of the frame pointer to assign to the next variable.
    /// This is incremented by the size of each variable as it is defined.
    fp_offset: isize,
    /// The size of the arguments supplied to the function, in cells.
    /// This is incremented by the size of each argument defined (for a procedure).
    /// This is unaffected by defining *variables* in the scope of the function.
    args_size: usize,
    /// Expected return type of the current function.
    /// This is `None` if we are not currently compiling a function.
    expected_ret: Option<Type>,

    /// Memoized type sizes.
    type_sizes: Arc<HashMap<Type, usize>>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            unops: Arc::new({
                let mut map: HashMap<String, Box<dyn UnaryOp>> = HashMap::new();
                map.insert("!".to_owned(), Box::new(crate::lir::Not));
                map.insert("-".to_owned(), Box::new(crate::lir::Negate));
                map.insert("~".to_owned(), Box::new(crate::lir::BitwiseNot));
                map.insert("get".to_owned(), Box::new(crate::lir::Get));
                map.insert("put".to_owned(), Box::new(crate::lir::Put::Display));
                map.insert("debug".to_owned(), Box::new(crate::lir::Put::Debug));
                map.insert("new".to_owned(), Box::new(crate::lir::New));
                map.insert("del".to_owned(), Box::new(crate::lir::Delete));
                map.insert("tag".to_owned(), Box::new(crate::lir::Tag));
                map.insert("data".to_owned(), Box::new(crate::lir::Data));

                map
            }),
            binops: Arc::new({
                let mut map: HashMap<String, Box<dyn BinaryOp>> = HashMap::new();
                map.insert("+".to_owned(), Box::new(crate::lir::Add));
                map.insert("-".to_owned(), Box::new(crate::lir::Arithmetic::Subtract));
                map.insert("*".to_owned(), Box::new(crate::lir::Arithmetic::Multiply));
                map.insert("/".to_owned(), Box::new(crate::lir::Arithmetic::Divide));
                map.insert("%".to_owned(), Box::new(crate::lir::Arithmetic::Remainder));
                map.insert("==".to_owned(), Box::new(crate::lir::Comparison::Equal));
                map.insert("!=".to_owned(), Box::new(crate::lir::Comparison::NotEqual));
                map.insert("<".to_owned(), Box::new(crate::lir::Comparison::LessThan));
                map.insert(
                    "<=".to_owned(),
                    Box::new(crate::lir::Comparison::LessThanOrEqual),
                );
                map.insert(
                    ">".to_owned(),
                    Box::new(crate::lir::Comparison::GreaterThan),
                );
                map.insert(
                    ">=".to_owned(),
                    Box::new(crate::lir::Comparison::GreaterThanOrEqual),
                );

                map.insert("&&".to_owned(), Box::new(crate::lir::And));
                map.insert("||".to_owned(), Box::new(crate::lir::Or));
                map.insert("&".to_owned(), Box::new(crate::lir::BitwiseAnd));
                map.insert("|".to_owned(), Box::new(crate::lir::BitwiseOr));
                map.insert("^".to_owned(), Box::new(crate::lir::BitwiseXor));
                map
            }),

            ternops: Arc::new(HashMap::new()),

            assignops: Arc::new({
                let mut map: HashMap<String, Box<dyn AssignOp>> = HashMap::new();
                map.insert(
                    "+=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::Arithmetic::Add)),
                );
                map.insert(
                    "-=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::Arithmetic::Subtract)),
                );
                map.insert(
                    "*=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::Arithmetic::Multiply)),
                );
                map.insert(
                    "/=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::Arithmetic::Divide)),
                );
                map.insert(
                    "%=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::Arithmetic::Remainder)),
                );
                map.insert(
                    "&=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::BitwiseAnd)),
                );
                map.insert(
                    "|=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::BitwiseOr)),
                );
                map.insert(
                    "^=".to_owned(),
                    Box::new(crate::lir::Assign::new(crate::lir::BitwiseXor)),
                );
                map
            }),

            // It is important that we use reference counting for the tables because the environment
            // will be copied many times during the compilation process to create new scopes.
            types: Arc::new(HashMap::new()),
            type_sizes: Arc::new(HashMap::new()),
            consts: Arc::new(HashMap::new()),
            procs: Arc::new(HashMap::new()),
            vars: Arc::new(HashMap::new()),
            modules: Arc::new(HashMap::new()),
            static_vars: Arc::new(HashMap::new()),
            globals: Arc::new(RwLock::new(Globals::new())),
            associated_constants: Arc::new(RwLock::new(HashMap::new())),
            processed_monomorphizations: Arc::new(RwLock::new(HashMap::new())),
            type_checked_consts: Arc::new(RwLock::new(HashSet::new())),

            // The last argument is stored at `[FP]`, so our first variable must be at `[FP + 1]`.
            fp_offset: 1,
            args_size: 0,
            expected_ret: None,
        }
    }
}

impl Env {
    pub(super) fn get_unop(&self, op: &str) -> Option<&Box<dyn UnaryOp>> {
        self.unops.get(op)
    }

    pub(super) fn get_binop(&self, op: &str) -> Option<&Box<dyn BinaryOp>> {
        self.binops.get(op)
    }

    pub(super) fn get_ternop(&self, op: &str) -> Option<&Box<dyn TernaryOp>> {
        self.ternops.get(op)
    }

    pub(super) fn get_assignop(&self, op: &str) -> Option<&Box<dyn AssignOp>> {
        self.assignops.get(op)
    }

    /// Create a copy of the current environment but without any variables or arguments defined.
    pub(super) fn new_scope(&self) -> Self {
        Self {
            // Only keep the types, constants, and procedures defined.
            types: self.types.clone(),
            consts: self.consts.clone(),
            procs: self.procs.clone(),
            static_vars: self.static_vars.clone(),
            modules: self.modules.clone(),
            type_sizes: {
                // Copy the data but not the lock.
                // let type_sizes = (*self.type_sizes).clone();
                // Arc::new(type_sizes)
                self.type_sizes.clone()
            },
            globals: self.globals.clone(),
            processed_monomorphizations: self.processed_monomorphizations.clone(),
            // associated_constants: self.associated_constants.clone(),
            associated_constants: {
                // Copy the data but not the lock.
                let associated_constants = self.associated_constants.read().unwrap().clone();
                Arc::new(RwLock::new(associated_constants))
            },
            type_checked_consts: {
                // let type_checked_consts = self.type_checked_consts.read().unwrap().clone();
                // Arc::new(RwLock::new(type_checked_consts))
                self.type_checked_consts.clone()
            },

            // The rest are the same as a new environment.
            ..Env::default()
        }
    }

    pub(crate) fn has_type_checked_const(&self, const_expr: &ConstExpr) -> bool {
        self.type_checked_consts
            .read()
            .unwrap()
            .contains(const_expr)
    }

    pub(crate) fn save_type_checked_const(&self, const_expr: ConstExpr) {
        self.type_checked_consts.write().unwrap().insert(const_expr);
    }

    /// Get the type of an associated constant of a type.
    pub fn get_type_of_associated_const(&self, ty: &Type, name: &str) -> Option<Type> {
        trace!(
            "Getting type of associated const {name} of type {ty} in {self} with types {:?}",
            self.types
        );
        let associated_constants = self.associated_constants.read().unwrap();

        if let Some((_, expr_ty)) = associated_constants
            .get(ty)
            .and_then(|consts| consts.get(name))
        {
            trace!("Found memoized type of associated const {name} of type {ty} in {self}");
            return Some(expr_ty.clone());
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.iter() {
            if matches!(ty.is_monomorph_of(other_ty, self), Ok(true)) {
                debug!("Type {ty} is monomorph of {other_ty}");
                for (name, (constant, ty)) in consts {
                    debug!("   {name}: {ty} = {constant}")
                }
                let template = other_ty.clone();

                let ty_params = template.get_template_params(self);
                let ty_param_set = ty_params.clone().into_iter().map(|x| x.0).collect::<HashSet<_>>();
                let monomorph = ty.clone();
                debug!("Monomorph of {template} is {monomorph}");
                let mut symbols = HashMap::new();
                if monomorph
                    .get_monomorph_template_args(
                        &template.strip_template(self),
                        &mut symbols,
                        &ty_param_set,
                        self,
                    )
                    .is_err()
                {
                    debug!("Failed to get monomorph template args for {monomorph} of {template}");
                    continue;
                }
                for (symbol, (ty, specifier)) in &symbols {
                    debug!("----> {symbol} == {ty}: {specifier:?}");
                }
                let template_associated_consts = consts.clone();
                let mut ty_args = Vec::new();
                for (ty_param, _) in &ty_params {
                    if let Some(arg) = symbols.get(ty_param) {
                        ty_args.push(arg.clone());
                    } else {
                        continue;
                    }
                }

                if ty_args.len() != ty_params.len() {
                    error!("Mismatched number of template arguments for {monomorph} of {template}");
                    error!("Expected {ty_params:?}, found {ty_args:?}");
                    continue;
                }

                debug!(
                    "Associated constants for {monomorph} are {:?}",
                    template_associated_consts.keys().to_owned()
                );

                if let Some((_, const_ty)) = template_associated_consts.get(name) {
                    debug!("Found cached associated const (type) {name} of type {const_ty}");

                    let result = const_ty.apply(ty_args.clone().into_iter().map(|x|x.0).collect());
                    match result.simplify_until_simple(self, false) {
                        Ok(result) => {
                            debug!("Found associated const (type) {name} of type {ty} = {result}");
                            return Some(result);
                        }
                        Err(_err) => {
                            debug!("Found associated const (type) {name} of type {ty} = {result} (failed to simplify)");
                            return Some(result);
                        }
                    }
                }

                debug!("Could not find associated const {name} of type {ty} in {template}");
                for template_const_name in template_associated_consts.keys() {
                    debug!("   {template_const_name} != {name}");
                }
            } else {
                debug!("Type {ty} is not monomorph of {other_ty}");
            }

            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                debug!("Type {other_ty} does not equal {ty}");
                continue;
            } else {
                debug!("Type {other_ty} equals {ty}");
            }
            if let Some((constant, expr_ty)) = consts.get(name) {
                let constant = constant.clone();
                let expr_ty = expr_ty.clone();
                drop(associated_constants);
                self.memoize_associated_const(ty, name, constant, expr_ty.clone())
                    .ok()?;
                return Some(expr_ty);
            }
        }
        drop(associated_constants);

        if let Type::Type(inner_ty) = ty {
            if let Some(ty) = self.get_type_of_associated_const(inner_ty, name) {
                return Some(ty);
            }
        }
        if let Type::Pointer(_, inner_ty) = ty {
            if let Some(ty) = self.get_type_of_associated_const(inner_ty, name) {
                return Some(ty);
            }
        }
        if let Type::Unit(_, inner_ty) = ty {
            if let Some(ty) = self.get_type_of_associated_const(inner_ty, name) {
                return Some(ty);
            }
        }
        error!("Could not find associated const {name} of type {ty} in {self}");
        None
    }

    pub fn get_associated_const(&self, ty: &Type, name: &str) -> Option<(ConstExpr, Type)> {
        debug!(
            "Getting associated const {name} of type {ty} in {self} with types {:?}",
            self.types
        );
        let associated_constants = self.associated_constants.read().unwrap();

        if let Some((constant, const_ty)) = associated_constants
            .get(ty)
            .and_then(|consts| consts.get(name))
        {
            trace!("Found associated const {name} of type {ty} in {self}");
            return Some((constant.clone(), const_ty.clone()));
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.clone().iter() {
            if matches!(ty.is_monomorph_of(other_ty, self), Ok(true)) {
                debug!("Type {ty} is monomorph of {other_ty}");
                let template = other_ty.clone();

                let monomorph = ty.clone();
                let mut symbols = HashMap::new();
                let ty_params = template.get_template_params(self);
                let ty_param_set = ty_params.clone().into_iter().map(|x| x.0).collect::<HashSet<_>>();
                if monomorph
                    .get_monomorph_template_args(
                        &template.strip_template(self),
                        &mut symbols,
                        &ty_param_set,
                        self,
                    )
                    .is_err()
                {
                    debug!("Failed to get monomorph template args for {monomorph} of {template}");
                    continue;
                }
                for (symbol, (ty, expected)) in &symbols {
                    debug!("----> {symbol} == {ty}, {expected:?}");
                }
                let template_associated_consts = consts.clone();
                let mut ty_args = Vec::new();
                for (ty_param, _) in &ty_params {
                    if let Some((arg, _)) = symbols.get(ty_param) {
                        ty_args.push(arg.clone());
                    } else {
                        debug!("Could not find symbol {ty_param}");
                    }
                }

                if ty_args.len() != ty_params.len() {
                    debug!("Mismatched number of template arguments for {monomorph} of {template}");
                    debug!("Expected {ty_params:?}, found {ty_args:?}");
                    continue;
                }

                if let Some((const_expr, const_ty)) = template_associated_consts.get(name) {
                    debug!("Found cached associated const pair: {const_expr} with type {const_ty}");
                    let mut result_ty = const_ty.apply(ty_args.clone());
                    result_ty = match result_ty.simplify_until_simple(self, true) {
                        Ok(result) => {
                            debug!("Found associated const {name} of type {ty} = {result}");
                            result
                        }
                        Err(_err) => {
                            debug!("Found associated const {name} of type {ty} = {result_ty} (failed to simplify)");
                            result_ty
                            // debug!("Failed to simplify associated const (type) {name} of type {ty} = {result}");
                            // debug!("Error: {err}");
                            // continue;
                        }
                    };

                    return Some((const_expr.clone().monomorphize(ty_args.clone()), result_ty));
                }

                debug!("Could not find associated const {name} of type {ty} in {template}");
                // return self.get_associated_const(&monomorph, name);
            } else {
                debug!("Type {ty} is not monomorph of {other_ty}");
            }

            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                trace!("Type {other_ty} does not equal {ty}");
                continue;
            }
            trace!("Found eligible type {other_ty} for {ty}");
            if let Some((const_expr, expr_ty)) = consts.get(name) {
                let expr_ty = expr_ty.clone();
                let const_expr = const_expr.clone();
                drop(associated_constants);
                self.memoize_associated_const(ty, name, const_expr.clone(), expr_ty.clone())
                    .ok()?;
                return Some((const_expr, expr_ty));
            }
        }
        trace!("Could not find associated const {name} of type {ty} in {self}");
        drop(associated_constants);

        if let Type::Type(inner_ty) = ty {
            if let Some((constant, const_ty)) = self.get_associated_const(inner_ty, name) {
                let expr_ty = constant.get_type(self).ok()?;
                self.memoize_associated_const(ty, name, constant.clone(), expr_ty)
                    .ok()?;
                return Some((constant, const_ty));
            }
        }
        if let Type::Pointer(_mutability, inner_ty) = ty {
            if let Some((constant, const_ty)) = self.get_associated_const(inner_ty, name) {
                // Memoize the associated constant.
                let expr_ty = constant.get_type(self).ok()?;
                self.memoize_associated_const(ty, name, constant.clone(), expr_ty)
                    .ok()?;
                return Some((constant, const_ty));
            }
        }
        if let Type::Unit(_unit_name, inner_ty) = ty {
            if let Some((constant, const_ty)) = self.get_associated_const(inner_ty, name) {
                // Memoize the associated constant.
                let expr_ty = constant.get_type(self).ok()?;
                self.memoize_associated_const(ty, name, constant.clone(), expr_ty)
                    .ok()?;
                return Some((constant, const_ty));
            }
        }
        None
    }

    fn memoize_associated_const(
        &self,
        ty: &Type,
        name: &str,
        constant: ConstExpr,
        expr_ty: Type,
    ) -> Result<(), Error> {
        trace!("Memoizing associated const {name} of type {ty} in {self}");
        let mut associated_constants = self.associated_constants.write().unwrap();
        // Does the type already have the associated constant?
        if let Some(consts) = associated_constants.get(ty) {
            if let Some((_, _)) = consts.get(name) {
                // If so, we don't need to memoize it.
                return Ok(());
            }
        }

        let consts = associated_constants.entry(ty.clone()).or_default();
        if consts.contains_key(name) {
            return Ok(());
        }
        consts.insert(name.to_owned(), (constant, expr_ty));
        Ok(())
    }

    pub fn has_associated_const(&self, ty: &Type, name: &str) -> bool {
        self.get_associated_const(ty, name).is_some()
    }

    pub fn get_all_associated_consts(&self, ty: &Type) -> Vec<(String, ConstExpr)> {
        trace!("Getting all associated constants of type {ty}");
        let associated_constants = self.associated_constants.read().unwrap();
        let mut result = Vec::new();
        if let Some(consts) = associated_constants.get(ty) {
            for (name, (const_expr, _)) in consts.iter() {
                result.push((name.to_owned(), const_expr.clone()));
            }
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.iter() {
            if ty == other_ty {
                continue;
            }
            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                trace!("Type {other_ty} does not equal {ty}");
                continue;
            }
            trace!("Found eligible type {other_ty} for {ty}");
            for (name, (const_expr, _)) in consts.iter() {
                result.push((name.to_owned(), const_expr.clone()));
            }
        }
        result
    }

    pub(super) fn has_any_associated_const(&self, ty: &Type) -> bool {
        trace!("Checking if type {ty} has any associated constants");
        let associated_constants = self.associated_constants.read().unwrap();
        if let Some(consts) = associated_constants.get(ty) {
            if !consts.is_empty() {
                return true;
            }
        }
        // Go through all the types and see if any equals the given type.
        for (other_ty, consts) in associated_constants.iter() {
            if ty == other_ty {
                continue;
            }
            if !ty.can_decay_to(other_ty, self).unwrap_or(false) {
                trace!("Type {other_ty} does not equal {ty}");
                continue;
            }
            trace!("Found eligible type {other_ty} for {ty}");
            if !consts.is_empty() {
                return true;
            }
        }
        false
    }

    pub(super) fn add_monomorphized_associated_consts(
        &self,
        template: Type,
        monomorph: Type,
        ty_args: Vec<Type>,
    ) -> Result<(), Error> {
        debug!("Adding monomorphized associated constants of type {template} to {monomorph} with type arguments {ty_args:?} to environment");

        let monomorph = if let Ok(simplified) = monomorph.simplify_until_simple(self, false) {
            debug!("Simplified {monomorph} to {simplified}");
            simplified
        } else {
            debug!("Failed to simplify type {monomorph}");
            return Ok(());
        };

        if monomorph.get_size(self).is_err() {
            debug!("Type {monomorph} is not atomic");
            return Ok(());
        } else {
            debug!("Type {monomorph} is atomic");
        }

        let is_processed = {
            self.processed_monomorphizations
                .read()
                .unwrap()
                .get(&template)
                .map(|monomorphs| monomorphs.iter().any(|mono| mono == &monomorph))
                .unwrap_or(false)
        };
        if is_processed {
            debug!("Type {template} has already been monomorphized to {monomorph}");
            return Ok(());
        }

        {
            self.processed_monomorphizations
                .write()
                .unwrap()
                .entry(template.clone())
                .or_default()
                .push(template.clone());
            self.processed_monomorphizations
                .write()
                .unwrap()
                .entry(template.clone())
                .or_default()
                .push(monomorph.clone());
        };

        if !self.has_any_associated_const(&template) {
            debug!("Type {template} has no associated constants");
            return Ok(());
        }

        debug!("Adding monomorphized associated constants of type {template} with type arguments {ty_args:?} to environment");
        // Get the template type's associated constants.
        let template_associated_consts = self.get_all_associated_consts(&template);
        // Check if monomorph already has the associated constants.
        let mono_associated_consts = self.get_all_associated_consts(&monomorph);
        debug!("Template associated consts: {template_associated_consts:?}");
        debug!("Mono associated consts: {mono_associated_consts:?}");
        for (name, const_expr) in template_associated_consts {
            if mono_associated_consts
                .iter()
                .any(|(mono_name, _)| mono_name == &name)
            {
                debug!("Monomorphized type {monomorph} already has associated constant {name}");
                continue;
            }
            // Monomorphize the associated constant.
            // Strip off the template parameters from the type arguments.
            let mono_const = if let ConstExpr::Template(ty_params, cexpr) = const_expr {
                let mut tmp = *cexpr.clone();
                for ((param, _), arg) in ty_params.iter().zip(ty_args.iter()) {
                    tmp.substitute(param, arg);
                }
                tmp
            } else {
                debug!("Monomorphizing {const_expr} the old fashioned way");
                const_expr.monomorphize(ty_args.clone())
                // continue;
                // debug!("Monomorphizing the old fashioned way");
            };
            debug!("Adding monomorphized associated constant {name} = {mono_const} to type {monomorph}");
            // Add the monomorphized associated constant to the environment.
            self.add_associated_const(monomorph.clone(), name, mono_const)?;
        }
        debug!("Done adding monomorphized associated constants of type {template} with type arguments {ty_args:?}");
        Ok(())
    }

    pub fn add_associated_const(
        &self,
        ty: Type,
        associated_const_name: impl ToString,
        expr: ConstExpr,
    ) -> Result<(), Error> {
        let associated_const_name = associated_const_name.to_string();
        debug!("Defining associated const {associated_const_name} as {expr} to type {ty}");
        let expr_ty = expr.get_type(self)?;
        let mut associated_constants = self.associated_constants.write().unwrap();
        associated_constants
            .entry(ty)
            .or_default()
            .insert(associated_const_name, (expr, expr_ty));

        Ok(())
    }

    /// Add all the declarations to this environment.
    pub(super) fn add_declaration(&mut self, declaration: &Declaration, compiling: bool) -> Result<(), Error> {
        self.add_compile_time_declaration(declaration, compiling)?;
        self.add_local_variable_declaration(declaration, compiling)?;
        Ok(())
    }

    /// Add all the compile-time declarations to this environment. These are declarations
    /// for types, constants, and procedures that are defined at compile-time. Variables
    /// are not included because they are defined at runtime.
    pub(super) fn add_compile_time_declaration(
        &mut self,
        declaration: &Declaration,
        compiling: bool,
    ) -> Result<(), Error> {
        debug!("Adding compile-time declaration {declaration}");
        match declaration {
            Declaration::Module(module_name, decls, checked) => {
                if !checked {
                    self.save_type_checked_const(ConstExpr::Symbol(module_name.clone()));
                }

                // Get all the declaration names
                if self.consts.contains_key(module_name) {
                    // If the module is already defined, we don't need to recompile it
                    return Ok(());
                }
                
                if let Some(module) = self.modules.get(module_name) {
                    // Check if the declarations are the same
                    if module == decls {
                        // If they are the same, we don't need to recompile the module
                        return Ok(());
                    } else {
                        // If they are different, we need to recompile the module
                        Arc::make_mut(&mut self.modules).insert(module_name.clone(), decls.clone());
                    }
                }

                let mut exports = vec![];
                for decl in Declaration::Many(decls.clone()).flatten().iter() {
                    match decl {
                        Declaration::Type(name, _) => {
                            exports.push(name.clone());
                        }
                        Declaration::Module(name, ..) => {
                            exports.push(name.clone());
                        }
                        Declaration::Const(name, _) => {
                            exports.push(name.clone());
                        }
                        Declaration::Proc(name, _) => {
                            exports.push(name.clone());
                        }
                        Declaration::PolyProc(name, _) => {
                            exports.push(name.clone());
                        }
                        Declaration::ExternProc(name, _) => {
                            exports.push(name.clone());
                        }
                        Declaration::FromImport { names, .. } => {
                            for (name, alias) in names {
                                match alias {
                                    Some(alias) => exports.push(alias.clone()),
                                    None => exports.push(name.clone()),
                                }
                            }
                        }
                        Declaration::FromImportAll(module) => {
                            let module_ty = module.get_type(self)?;
                            if let Type::Struct(fields) = module_ty {
                                for name in fields.keys() {
                                    exports.push(name.clone());
                                }
                            } else {
                                error!("Could not find type of module {module_ty}");
                            }
                        }
                        Declaration::Impl(_ty, _attrs) => {
                            self.add_compile_time_declaration(decl, compiling)?;
                        }
                        Declaration::Many(_decls) => {
                            unreachable!()
                        }
                        _ => {}
                    }
                }

                // Create a const struct with all the exported names.
                let exports = ConstExpr::Struct(
                    exports
                        .into_iter()
                        .map(|name| (name.clone(), ConstExpr::Symbol(name)))
                        .collect(),
                );
                
                let result = exports.with(Declaration::Many(decls.clone())).eval(self)?;
                self.define_const(module_name, result)
            }
            Declaration::Type(name, ty) => {
                self.define_type(name, ty.clone());
            }
            Declaration::Const(name, e) => {
                self.define_const(name, e.clone());
            }
            Declaration::FromImport { module, names } => {
                let module = module.clone().eval(self)?;
                for (name, alias) in names {
                    let access = module.clone().field(ConstExpr::Symbol(name.clone())).eval(self)?;
                    let name = alias.clone().unwrap_or(name.clone());

                    if let Ok(Type::Type(ty)) = access.get_type(self) {
                        self.define_type(&name, *ty);
                    }
                    self.define_const(&name, access.clone());
                }
            }
            Declaration::FromImportAll(module) => {
                let module = module.clone().eval(self)?;
                let module_ty = module.get_type(self)?;
                if let Type::Struct(fields) = module_ty {
                    for name in fields.keys() {
                        let access = module.clone().field(ConstExpr::Symbol(name.clone())).eval(self)?;

                        if let Ok(Type::Type(ty)) = access.get_type(self) {
                            self.define_type(name, *ty);
                        }
                        self.define_const(name, access.clone());
                    }
                } else {
                    error!("Invalid module type: {module_ty}");
                }
            }
            Declaration::Proc(name, proc) => {
                self.define_proc(name, proc.clone());
            }
            Declaration::PolyProc(name, proc) => {
                self.define_poly_proc(name, proc.clone());
            }
            Declaration::ExternProc(name, proc) => {
                self.define_ffi_proc(name, proc.clone());
            }
            Declaration::StaticVar(name, mutability, ty, _expr) => {
                self.define_static_var(name, *mutability, ty.clone())?;
            }
            Declaration::Impl(ty, impls) => {
                // Hash the impls
                if let Type::Apply(template, supplied_params) = ty {
                    // If this is an implementation for a template type, we need to
                    // get the template parameters and add them to each associated constant.
                    let template_params = template.get_template_params(self);

                    if template_params.len() != supplied_params.len() && !template_params.is_empty()
                    {
                        // The number of template parameters must match the number of supplied parameters.
                        error!("Mismatched types in template {template}");
                        return Err(Error::MismatchedTypes {
                            expected: *template.clone(),
                            found: Type::Apply(template.clone(), supplied_params.clone()),
                            expr: Expr::NONE.with(declaration.clone()),
                        });
                    } else if supplied_params.is_empty() {
                        // Not sure why this case happens,
                        // when I don't add it then imported types from other modules
                        // have type errors whenever they are used.
                        // I think this is how the types in modules are distributed
                        // amongst the impl-methods. If the type is redefined this way
                        // by redistributing the type declarations, then maybe it could
                        // interfere with the coherence between the actual types in methods
                        // vs. the type they're defined for.
                        return Ok(());
                    }

                    let supplied_param_symbols = supplied_params
                        .iter()
                        .map(|ty| {
                            if let Type::Symbol(sym) = ty {
                                Ok(sym.clone())
                            } else {
                                Err(Error::MismatchedTypes {
                                    expected: Type::Symbol(
                                        "A symbol, not a concrete type".to_owned(),
                                    ),
                                    found: ty.clone(),
                                    expr: Expr::NONE.with(declaration.clone()),
                                })
                            }
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    for (name, associated_const) in impls {
                        let templated_const =
                            associated_const.template(template_params.clone().into_iter().zip(supplied_param_symbols.clone().into_iter()).map(|((_, ty), param)| (param, ty)).collect());
                        self.add_associated_const(*template.clone(), name, templated_const)?;
                    }
                } else {
                    for (name, associated_const) in impls {
                        self.add_associated_const(ty.clone(), name, associated_const.clone())?;
                    }

                    // impls.par_iter().try_for_each(|(name, associated_const)| {
                    //     self.add_associated_const(ty.clone(), name, associated_const.clone())
                    // })?;
                }
            }
            Declaration::Var(_, _, _, _) => {}
            Declaration::VarPat(_, _) => {}
            Declaration::Many(decls) => {
                for decl in decls.iter() {
                    self.add_compile_time_declaration(decl, compiling)?;
                }

                for decl in decls.iter() {
                    if let Declaration::Type(name, ty) = decl {
                        if let Ok(size) = ty.get_size(self) {
                            self.set_precalculated_size(ty.clone(), size);
                        } else {
                            debug!("Failed to memoize type size for {name} = {ty}");
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Add a single variable declaration to this environment. These are declarations
    /// for variables that are defined at runtime. Types, constants, and procedures
    /// are not included because they are defined at compile-time.
    pub(super) fn add_local_variable_declaration(
        &mut self,
        declaration: &Declaration,
        compiling: bool
    ) -> Result<(), Error> {
        trace!("Adding local declaration {declaration}");
        match declaration {
            Declaration::Type(_, _) => {
                // Types are not defined at runtime.
            }
            Declaration::Const(_, _) => {
                // Constants are not defined at runtime.
            }
            Declaration::Proc(_, _) => {
                // Procedures are not defined at runtime.
            }
            Declaration::PolyProc(_, _) => {
                // Polymorphic procedures are not defined at runtime.
            }
            Declaration::ExternProc(_, _) => {
                // FFI procedures are not defined at runtime.
            }
            Declaration::StaticVar(name, mutability, ty, _expr) => {
                self.define_static_var(name, *mutability, ty.clone())?;
            }
            Declaration::Impl(_, _) => {
                // Implementations are not defined at runtime.
            }
            Declaration::Module(..) => {
                // Modules are not defined at runtime.
            }
            Declaration::FromImport { .. } => {
                // From imports are not defined at runtime.
            }
            Declaration::FromImportAll(..) => {
                // From imports are not defined at runtime.
            }
            Declaration::Var(name, mutability, ty, expr) => {
                let ty = match ty {
                    Some(ty) => ty.clone(),
                    None => expr.get_type(self)?,
                };
                // ty.add_monomorphized_associated_consts(self)?;
                self.define_var(name, *mutability, ty, compiling)?;
            }
            Declaration::VarPat(pat, expr) => {
                let ty = expr.get_type(self)?;
                // ty.add_monomorphized_associated_consts(self)?;
                pat.declare_let_bind(expr, &ty, self)?;
            }
            Declaration::Many(decls) => {
                for decl in decls.iter() {
                    self.add_local_variable_declaration(decl, compiling)?;
                }
            }
        }
        Ok(())
    }

    /// Define a static variable with a given name under this environment.
    pub(super) fn define_static_var(
        &mut self,
        name: impl ToString,
        mutability: Mutability,
        ty: Type,
    ) -> Result<Location, Error> {
        let name = name.to_string();
        let size = ty.get_size(self)?;
        let mut globals = self.globals.write().unwrap();
        let location = globals.add_global(name.clone(), size);

        trace!("Defining static variable {name} of type {ty} at {location}");
        Arc::make_mut(&mut self.static_vars)
            .insert(name.clone(), (mutability, ty, Location::Global(name)));
        Ok(location)
    }

    /// Get a static variable definition from this environment.
    /// This contains:
    /// 1. The mutability of the variable.
    /// 2. The type of the variable.
    /// 3. The location of the variable in memory.
    pub(super) fn get_static_var(&self, name: &str) -> Option<&(Mutability, Type, Location)> {
        self.static_vars.get(name)
    }

    /// Define a type with a given name under this environment.
    pub(super) fn define_type(&mut self, name: impl ToString, ty: Type) {
        let name = name.to_string();
        trace!("Defining type {name} as {ty}");
        if ty.is_const_param() {
            if let Ok(ty) = ty.clone().simplify_until_const_param(self, false) {
                self.define_const(&name, ty);
                return;
            }
        }
        match &ty {
            Type::Symbol(sym) if sym == &name => {
                trace!("Defining type {ty} to itself as {name}");
            }
            _ => {
                Arc::make_mut(&mut self.consts).insert(name.clone(), ConstExpr::Type(ty.clone()));
                Arc::make_mut(&mut self.types).insert(name.clone(), ty.clone());

                if let Ok(simplified) = ty.simplify_until_concrete(self, false) {
                    if let Ok(size) = simplified.get_size(self) {
                        self.set_precalculated_size(simplified.clone(), size);
                    }
                    if let Type::ConstParam(cexpr) = simplified {
                        trace!("Found const param \"{name}\": {cexpr}");
                        self.define_const(&name, *cexpr);
                    }
                }
            }
        }
    }

    /// Define multiple types with the given names under this environment.
    ///
    /// This must be used in situations where the different types depend on each other.
    /// This is because the sizes of types are memoized, and this will interfere with
    /// the memoization process if the types are defined separately. It will lead to
    /// typechecking errors if the environment does not already have a memoized size
    /// for the type of a subexpression.
    pub fn define_types(&mut self, types: Vec<(String, Type)>) {
        for (name, ty) in types {
            self.define_type(name, ty)
        }
    }

    /// Get a type definition from this environment.
    pub(super) fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    /// Define a constant with a given name under this environment.
    pub(super) fn define_const(&mut self, name: impl ToString, e: ConstExpr) {
        let name = name.to_string();
        trace!("Defining constant {name} as {e}");

        /*
        Removed this code in favor of using Declaration::FromImport
        to add types from constant expressions

        trace!("Defining constant {name} as {e}");
        match e.get_type(self) {
            Ok(Type::Type(t)) => {
                trace!("{name} is a type declaration for {t}");
                self.define_type(name.clone(), *t);
            }
            _ => {
                trace!("{name} is a constant declaration for {e}");
            }
        }
        */

        Arc::make_mut(&mut self.consts).insert(name, e);
    }

    /// Get a constant definition from this environment.
    pub(super) fn get_const(&self, name: &str) -> Option<&ConstExpr> {
        self.consts.get(name)
    }

    /// Define a procedure with a given name under this environment.
    pub(super) fn define_proc(&mut self, name: impl ToString, proc: Procedure) {
        let name = name.to_string();
        trace!("Defining procedure {name} as {proc}");
        Arc::make_mut(&mut self.procs).insert(name.clone(), proc.clone());
        Arc::make_mut(&mut self.consts).insert(name, ConstExpr::Proc(proc));
    }

    /// Define a polymorphic procedure with a given name under this environment.
    pub(super) fn define_poly_proc(&mut self, name: impl ToString, proc: PolyProcedure) {
        let name = name.to_string();
        trace!("Defining polymorphic procedure {name} as {proc}");
        Arc::make_mut(&mut self.consts).insert(name, ConstExpr::PolyProc(proc));
    }

    /// Define an FFI procedure with a given name under this environment.
    pub(super) fn define_ffi_proc(&mut self, name: impl ToString, proc: FFIProcedure) {
        let name = name.to_string();
        trace!("Defining FFI procedure {name} as {proc}");
        Arc::make_mut(&mut self.consts).insert(name, ConstExpr::FFIProcedure(proc));
    }

    /// Get a procedure definition from this environment.
    pub(super) fn get_proc(&self, name: &str) -> Option<&Procedure> {
        self.procs.get(name).or_else(|| {
            let result = self.consts.get(name).and_then(|x| match x {
                ConstExpr::Proc(proc) => Some(proc),
                _ => None,
            });
            if result.is_none() {
                debug!("Procedure {name} not found in {self}");
            }
            result
        })
    }

    /// Does this environment have a procedure with the given name?
    pub(super) fn has_proc(&self, name: &str) -> bool {
        self.procs.contains_key(name)
    }

    /// Push a procedure defined in the environment onto the stack.
    pub(super) fn push_proc(
        &mut self,
        name: &str,
        output: &mut dyn AssemblyProgram,
    ) -> Result<(), Error> {
        // Check if the procedure is defined.
        if let Some(proc) = Arc::make_mut(&mut self.procs).get_mut(name) {
            debug!("Pushing procedure {} onto the stack", name);
            // Compile the procedure.
            proc.clone().compile_expr(self, output)
        } else {
            error!("Undefined procedure {}", name);
            // If not, the symbol isn't defined.
            Err(Error::SymbolNotDefined(name.to_string()))
        }
    }

    /// Get a variable's size, in cells.
    pub(super) fn get_args_size(&self) -> usize {
        self.args_size
    }

    /// Get a variable's type and its offset from the frame pointer in the current scope.
    pub(super) fn get_var(&self, var: &str) -> Option<&(Mutability, Type, isize)> {
        self.vars.get(var)
    }

    /// Is the variable defined in scope as mutable?
    pub(super) fn is_defined_as_mutable(&self, var: &str) -> bool {
        if let Some((mutability, _, _)) = self.vars.get(var) {
            mutability.is_mutable()
        } else if let Some((mutability, _, _)) = self.static_vars.get(var) {
            mutability.is_mutable()
        } else {
            false
        }
    }

    /// Define the arguments for the current scope (if this is a procedure).
    pub(super) fn define_args(
        &mut self,
        args: Vec<(String, Mutability, Type)>,
        compiling: bool,
    ) -> Result<usize, Error> {
        debug!("Defining arguments {args:?} in\n{self}");
        self.fp_offset = 1;
        self.args_size = 0;

        // For each argument in reverse order (starting from the last argument)
        for (name, mutability, ty) in args.into_iter().rev() {
            // Get the size of the argument we're defining.
            let size = if compiling {
                ty.get_size(self)?
            } else {
                0
            };
            
            // Add the size of the argument to the total number of cells taken up by the arguments.
            self.args_size += size;
            // Decrement the frame pointer offset by the size of the argument
            // so that the FP + the offset is the address of the argument.
            self.fp_offset -= size as isize;
            // Store the argument's type and offset in the environment.
            debug!(
                "Defined argument {name} of type {ty} at offset {} in\n{self}",
                self.fp_offset
            );
            Arc::make_mut(&mut self.vars).insert(name, (mutability, ty, self.fp_offset));
        }
        // Set the frame pointer offset to `1` so that the first variable defined under the scope is at `[FP + 1]`.
        self.fp_offset = 1;

        // Return the size of the arguments for the procedure in cells,
        // so that the compiler can deallocate the arguments after compiling the procedure.
        Ok(self.args_size)
    }

    /// Define a variable in the current scope.
    /// This will increment the scope's frame pointer offset by the size of the variable.
    /// This method returns the offset of the variable from the frame pointer under this scope.
    pub fn define_var(
        &mut self,
        var: impl ToString,
        mutability: Mutability,
        ty: Type,
        compiling: bool
    ) -> Result<isize, Error> {
        let var = var.to_string();
        // Get the size of the variable we're defining.
        let size = if compiling {
            ty.get_size(self)?
        } else {
            0
        } as isize;
        // Remember the offset of the variable under the current scope.
        let offset = self.fp_offset;
        // Increment the frame pointer offset by the size of the variable
        // so that the next variable is allocated directly after this variable.
        debug!("Defining variable {var} of type {ty} at {offset} in\n{self}");
        self.fp_offset += size;

        let ty = match ty {
            Type::Type(ty) => *ty,
            Type::ConstParam(ty) => ty.get_type(self)?,
            other => other
        };
        
        // Store the variable's type and offset in the environment.
        Arc::make_mut(&mut self.vars).insert(var, (mutability, ty, offset));
        // Return the offset of the variable from the frame pointer.
        Ok(offset)
    }

    /// Get the expected return type of the current function.
    /// This is used to check if the returned value of a function matches the expected return type.
    /// This method returns `None` if the current scope is not a function.
    pub(super) fn get_expected_return_type(&self) -> Option<&Type> {
        self.expected_ret.as_ref()
    }

    /// Set the expected return type of the current function.
    /// If we're in a function, this will be the type of the function.
    /// If we're not in a function, this will be `None`.
    pub(super) fn set_expected_return_type(&mut self, t: Type) {
        self.expected_ret = Some(t);
    }

    /// Does the environment have some precalculated size for the given type?
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn has_precalculated_size(&self, ty: &Type) -> bool {
        self.type_sizes.contains_key(ty)
    }

    /// Get the precalculated size of the given type.
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn get_precalculated_size(&self, ty: &Type) -> Option<usize> {
        // Get the precalculated size of the given type.
        // let size = self.type_sizes.get(ty).copied()?;
        let size = self.type_sizes.get(ty).copied()?;
        // Log the size of the type.
        debug!(target: "size", "Getting memoized type size for {ty} => {size}");
        // Return the size of the type.
        Some(size)
    }

    /// Set the precalculated size of the given type.
    /// This helps the compiler memoize the size of types so that it doesn't have to
    /// recalculate the size of the same type multiple times.
    pub(super) fn set_precalculated_size(&mut self, ty: Type, size: usize) {
        debug!(target: "size", "Memoizing type size {ty} with size {size}");
        if let Some(old_size) = self.get_precalculated_size(&ty) {
            if old_size == size {
                debug!(target: "size", "Type size {ty} was already memoized with size {size}");
                return;
            } else {
                debug!(target: "size", "Type size {ty} was already memoized with size {old_size}, but we memoized it with size {size}");
            }
        }
        Arc::make_mut(&mut self.type_sizes).insert(ty, size);
    }
}

impl Display for Env {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        writeln!(f, "Env")?;
        // return Ok(());
        /*
        writeln!(f, "   Types:")?;
        for (name, ty) in self.types.iter() {
            writeln!(f, "      {}: {}", name, ty)?;
            let constants = self.get_all_associated_consts(ty);
            if constants.is_empty() {
                continue;
            }
            writeln!(f, "         Associated constants:")?;
            for (name, cexpr) in constants {
                writeln!(f, "            {}: {}", name, cexpr)?;
            }
        }
        writeln!(f, "   Constants:")?;
        for (i, (name, e)) in self.consts.iter().enumerate() {
            writeln!(f, "      {i}. {}: {}", name, e)?;
        }
        writeln!(f, "   Procedures:")?;
        for (name, proc) in self.procs.iter() {
            writeln!(f, "      {}: {}", name, proc)?;
        }
        writeln!(f, "   Globals:")?;
        for (name, (mutability, ty, location)) in self.static_vars.iter() {
            writeln!(f, "      {mutability} {name}: {ty} (location {location})")?;
        }
        writeln!(f, "   Variables:")?;
        for (name, (mutability, ty, offset)) in self.vars.iter() {
            writeln!(f, "      {mutability} {name}: {ty} (frame-offset {offset})")?;
        }
         */
        Ok(())
    }
}
