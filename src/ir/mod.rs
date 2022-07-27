mod expr;
pub use expr::*;
mod types;
pub use types::*;
mod env;
pub use env::*;

pub trait GetSize {
    fn get_size(&self, env: &Env) -> Result<usize, Error> {
        self.get_size_checked(env, 0)
    }

    fn get_size_checked(&self, env: &Env, i: usize) -> Result<usize, Error>;
}

pub trait Simplify: Sized {
    fn simplify(self, env: &Env) -> Result<Self, Error> {
        self.simplify_checked(env, 0)
    }

    fn simplify_checked(self, env: &Env, i: usize) -> Result<Self, Error>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    AssemblyError(crate::asm::Error),
    VariantNotFound(Type, String),
    MemberNotFound(Expr, ConstExpr),
    RecursionDepthConst(ConstExpr),
    NonIntegralConst(ConstExpr),
    ConstNotDefined(String),
    UnsizedType(Type),
    DerefNonPointer(Expr),
    ApplyNonProc(Expr),
    NonSymbol(ConstExpr),
    InvalidIndex(Expr),
    InvalidRefer(Expr),
    InvalidBinop(Expr),

    SymbolNotDefined(String),
    TypeNotDefined(String),

    InvalidAs(Expr),
}

impl From<crate::asm::Error> for Error {
    fn from(e: crate::asm::Error) -> Self {
        Self::AssemblyError(e)
    }
}
