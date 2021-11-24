use somok::Somok;

use crate::{
    errors,
    eval::Atom,
    jit::Jit,
    parser::{Arglist, Args, DefunExpr, Expr},
    CranelispError, EvalError, Result,
};
use std::{fmt::Debug, str::FromStr};

#[derive(Clone, Copy)]
pub struct Func {
    pub arity: u8,
    pub stack_return: bool,
    pub foldable: bool,
    body: *const u8,
}
// extern "C" fn does not implement FnMut
macro_rules! transmute {
    ($ptr:expr; $($typ:ty),+) => {
        std::mem::transmute::<*const u8, extern "C" fn(*mut Atom, $(&$typ),+)>($ptr)
    };
    ($ptr:expr) => {
        std::mem::transmute::<*const u8, extern "C" fn(*mut Atom)>($ptr)
    };
}

macro_rules! call {
    ($args:expr; $fn:expr; $($n:literal),+) => {
        {
            let mut ret = Atom::NULL;
            $fn(&mut ret, $(&$args[$n]),+);
            ret
        }
    };
    ($fn:expr) => {
        {
            let mut ret = Atom::NULL;
            $fn(&mut ret);
            ret
        }
    };
}

impl Func {
    pub fn from_fn(body: *const u8, arity: u8, stack_return: bool, foldable: bool) -> Self {
        Self {
            arity,
            stack_return,
            body,
            foldable,
        }
    }
    pub fn jit(jit: &mut Jit, defun: DefunExpr) -> Result<Self> {
        let mut foldable = false;
        let arity = match &defun.args {
            Args::Foldable => {
                foldable = true;
                2
            }
            Args::Arglist(a) => a.len() as u8,
        };
        let body = jit.compile(defun)?;
        Self {
            arity,
            stack_return: false,
            body,
            foldable,
        }
        .okay()
    }

    pub fn call(&self, args: Vec<Atom>) -> Result<Atom> {
        if self.arity > args.len() as u8 && !self.foldable {
            return errors::eval(EvalError::ArityMismatch);
        }
        unsafe {
            match self.arity {
                0 => {
                    let func = transmute!(self.body);
                    call!(func).okay()
                }
                1 => {
                    let func = transmute!(self.body; Atom);
                    call!(args; func; 0).okay()
                }
                2 if self.foldable => {
                    let func = transmute!(self.body; Atom, Atom);
                    #[allow(clippy::redundant_closure)]
                    args.into_iter()
                        .reduce(|acc, n| call!([acc, n]; func; 0, 1))
                        .unwrap_or(Atom::NULL)
                        .okay()
                }
                2 => {
                    let func = transmute!(self.body; Atom, Atom);
                    call!(args; func; 0, 1).okay()
                }
                3 => {
                    let func = transmute!(self.body; Atom, Atom, Atom);
                    call!(args; func; 0, 1, 2).okay()
                }

                arity => todo!("arity: {}", arity),
            }
        }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{{ arity: {:?}, stack_return: {:?}, body: {:?} }}",
            self.arity, self.stack_return, self.body
        )
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
