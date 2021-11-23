use somok::Somok;

use crate::{
    errors,
    eval::Value,
    jit::Jit,
    parser::{Arglist, Args, DefunExpr, Expr},
    CranelispError, EvalError, Result,
};
use std::{fmt::Debug, str::FromStr};

#[derive(Clone)]
pub struct Func {
    pub arity: usize,
    body: *const u8,
}
// extern "C" fn does not implement FnMut
macro_rules! transmute {
    ($ptr:expr; $($typ:ty),+) => {
        std::mem::transmute::<*const u8, extern "C" fn(*mut Value, $(&$typ),+)>($ptr)
    };
    ($ptr:expr) => {
        std::mem::transmute::<*const u8, extern "C" fn(*mut Value)>($ptr)
    };
}

macro_rules! call {
    ($args:expr; $fn:expr; $($n:literal),+) => {
        {
            let mut ret = Value::NULL;
            $fn(&mut ret, $(&$args[$n]),+);
            ret
        }
    };
    ($fn:expr) => {
        {
            let mut ret = Value::NULL;
            $fn(&mut ret);
            ret
        }
    };
}

impl Func {
    pub fn from_fn(body: *const u8, arity: usize) -> Self {
        Self { arity, body }
    }
    pub fn jit(jit: &mut Jit, defun: DefunExpr) -> Result<Self> {
        let arity = match &defun.args {
            Args::Foldable => usize::MAX,
            Args::Arglist(a) => a.len(),
        };
        let body = jit.compile(defun)?;
        Self { arity, body }.okay()
    }
    pub fn foldable(&self) -> bool {
        self.arity == usize::MAX
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value> {
        if self.arity > args.len() && self.arity != usize::MAX {
            return errors::eval(EvalError::ArityMismatch);
        }
        unsafe {
            match self.arity {
                0 => {
                    let func = transmute!(self.body);
                    call!(func).okay()
                }
                1 => {
                    let func = transmute!(self.body; Value);
                    call!(args; func; 0).okay()
                }
                2 => {
                    let func = transmute!(self.body; Value, Value);
                    call!(args; func; 0, 1).okay()
                }
                3 => {
                    let func = transmute!(self.body; Value, Value, Value);
                    call!(args; func; 0, 1, 2).okay()
                }
                usize::MAX => {
                    let func = transmute!(self.body; Value, Value);
                    #[allow(clippy::redundant_closure)]
                    args.into_iter()
                        .reduce(|acc, n| call!([acc, n]; func; 0, 1))
                        .unwrap_or(Value::NULL)
                        .okay()
                }
                arity => todo!("arity: {}", arity),
            }
        }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Native function")
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
