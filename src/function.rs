use somok::Somok;

use crate::{
    eval::Atom,
    jit::Jit,
    parser::{Arglist, DefunExpr, Expr},
    CranelispError, EvalError, Result,
};
use std::{fmt::Debug, str::FromStr};

#[derive(Clone, Copy)]
pub struct Func {
    pub body: unsafe extern "C" fn(usize, *mut *mut Atom) -> *mut Atom,
}

impl Func {
    pub fn from_fn(body: unsafe extern "C" fn(usize, *mut *mut Atom) -> *mut Atom) -> Self {
        Self { body }
    }
    pub fn jit(jit: &mut Jit, defun: DefunExpr) -> Result<Self> {
        let body = unsafe { std::mem::transmute::<_, _>(jit.compile(defun)?) };
        Self { body }.okay()
    }

    pub fn call(&self, args: Vec<Atom>) -> Atom {
        let (count, args) = (args.len(), &mut args.leak().as_mut_ptr());
        unsafe { *(self.body)(count, args) }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ body: {:?} }}", self.body as *const u8)
    }
}

#[derive(Clone)]
pub enum FnBody {
    Virtual(Expr),
    Native(*const u8),
}

#[derive(Debug, Clone)]
pub enum FnArgs {
    Arglist(Arglist),
    Foldable(Type),
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub args: Vec<String>,
    pub name: String,
    pub foldable: bool,
}

pub struct SignatureBuilder {
    args: Vec<String>,
    name: Option<String>,
    foldable: bool,
}
impl SignatureBuilder {
    pub fn set_foldable(mut self, foldable: bool) -> Self {
        self.foldable = foldable;
        self
    }
    pub fn set_name(mut self, n: String) -> Self {
        self.name = Some(n);
        self
    }
    pub fn push_arg(mut self, arg: String) -> Self {
        self.args.push(arg);
        self
    }
    pub fn finish(self) -> Result<Signature> {
        let name = self.name.ok_or_else(|| {
            CranelispError::Eval(EvalError::InvalidSignature("Missing name".into()))
        })?;
        let args = if self.foldable {
            vec!["a".into(), "b".into()]
        } else {
            self.args
        };
        Signature {
            args,
            name,
            foldable: self.foldable,
        }
        .okay()
    }
}

impl Signature {
    pub fn build() -> SignatureBuilder {
        SignatureBuilder {
            args: vec![],
            name: None,
            foldable: false,
        }
    }
    pub fn build_from_arglist(args: Vec<String>) -> SignatureBuilder {
        SignatureBuilder {
            args,
            name: None,
            foldable: false,
        }
    }

    pub fn arity(&self) -> usize {
        if self.foldable {
            usize::MAX
        } else {
            self.args.len()
        }
    }

    pub fn args(&self) -> Vec<String> {
        self.args.clone()
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Type {
    Float,
    Integer,
}

impl FromStr for Type {
    type Err = CranelispError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Float" => Type::Float.okay(),
            "Int" => Type::Integer.okay(),
            _ => syntax!(InvalidLiteral).error(),
        }
    }
}
