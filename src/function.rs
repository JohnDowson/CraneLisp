use somok::Somok;

use crate::{
    errors,
    eval::Value,
    jit::Jit,
    parser::{Arglist, Expr},
    CranelispError, EvalError, Result,
};
use std::{fmt::Debug, str::FromStr};

#[derive(Clone)]
pub struct Function {
    sig: Signature,
    body: FnBody,
}

macro_rules! transmute {
    ($ptr:expr; $($typ:tt),+ => $ret:tt) => {
        std::mem::transmute::<*const u8, extern "C" fn($($typ),+) -> $ret>($ptr)
    };
    ($ptr:expr; => $ret:tt) => {
        std::mem::transmute::<*const u8, extern "C" fn() -> $ret>($ptr)
    };
}

macro_rules! call {
    ($args:expr; $fn:expr; $($n:literal),+) => {
        {
            $fn($($args[$n]),+)
        }
    };
}

impl Function {
    pub fn new(sig: Signature, body: FnBody) -> Self {
        Self { sig, body }
    }
    pub fn jit(self, jit: &mut Jit) -> Result<Self> {
        let sig = self.sig.clone();
        let body = jit.compile(self)?;
        let body = FnBody::Native(body);
        Function { sig, body }.okay()
    }
    pub fn args(&self) -> Vec<(String, Type)> {
        self.sig.args()
    }

    pub fn signature(&self) -> Signature {
        self.sig.clone()
    }

    pub fn name(&self) -> String {
        self.sig.name.clone()
    }
    pub fn body(&self) -> Expr {
        match &self.body {
            FnBody::Native(_) => todo!(),
            FnBody::Virtual(e) => e.clone(),
        }
    }

    pub fn foldable(&self) -> bool {
        self.sig.foldable()
    }

    pub fn arity(&self) -> usize {
        self.sig.arity()
    }
    pub fn call(&self, args: Vec<Value>) -> Result<Value> {
        if self.arity() > args.len() && self.arity() != usize::MAX {
            return errors::eval(EvalError::ArityMismatch);
        }
        match &self.body {
            FnBody::Virtual(expr) => {
                errors::eval(EvalError::UnexpectedVirtualFunction(expr.span()))
            }
            FnBody::Native(ptr) => unsafe {
                let args: Vec<_> = args.into_iter().map(|v| v.unwrap_float()).collect();
                match self.arity() {
                    0 => {
                        let func = transmute!(*ptr; => f64);
                        let val = func();
                        Value::Float(val).okay()
                    }
                    1 => {
                        let func = transmute!(*ptr; f64 => f64);
                        let val = call!(args; func; 0);
                        Value::Float(val).okay()
                    }
                    2 => {
                        let func = transmute!(*ptr; f64,f64 => f64);
                        let val = call!(args; func; 0, 1);
                        Value::Float(val).okay()
                    }
                    3 => {
                        let func = transmute!(*ptr; f64,f64,f64 => f64);
                        let val = call!(args; func; 0, 1, 2);
                        Value::Float(val).okay()
                    }
                    usize::MAX => {
                        let func = transmute!(*ptr; f64,f64 => f64);
                        #[allow(clippy::redundant_closure)]
                        // extern "C" fn does not implement FnMut
                        let result = args
                            .into_iter()
                            .reduce(|acc, n| func(acc, n))
                            .unwrap_or(0.0);
                        Value::Float(result).okay()
                    }
                    arity => todo!("arity: {}", arity),
                }
            },
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.body {
            FnBody::Native(_) => writeln!(f, "Native function")?,
            FnBody::Virtual(e) => writeln!(f, "Virtual function \n{:#?}", e.clone().unwrap_list())?,
        };
        match &self.sig.args {
            FnArgs::Arglist(args) => writeln!(
                f,
                "{:?} => {:?}",
                args.iter().map(|(_, t)| t).collect::<Vec<_>>(),
                self.sig.ret
            ),
            FnArgs::Foldable(ty) => writeln!(f, "*{:?} => {:?}", ty, self.sig.ret),
        }
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
    pub args: FnArgs,
    pub ret: Type,
    pub name: String,
}

pub struct SignatureBuilder {
    args: Vec<(String, Type)>,
    ret: Option<Type>,
    name: Option<String>,
    foldable_type: Option<Type>,
}
impl SignatureBuilder {
    pub fn set_ret(mut self, t: Type) -> Self {
        self.ret = Some(t);
        self
    }
    pub fn set_foldable(mut self, ty: Type) -> Self {
        self.foldable_type = ty.some();
        self
    }
    pub fn set_name(mut self, n: String) -> Self {
        self.name = Some(n);
        self
    }
    pub fn push_arg(mut self, arg: (String, Type)) -> Self {
        self.args.push(arg);
        self
    }
    pub fn finish(self) -> Result<Signature> {
        let ret = self.ret.ok_or_else(|| {
            CranelispError::Eval(EvalError::InvalidSignature("Missing return type".into()))
        })?;
        let name = self.name.ok_or_else(|| {
            CranelispError::Eval(EvalError::InvalidSignature("Missing name".into()))
        })?;
        if self.foldable_type.is_some() {
            Signature {
                args: FnArgs::Foldable(self.foldable_type.unwrap()),
                ret,
                name,
            }
        } else {
            Signature {
                args: FnArgs::Arglist(self.args),
                ret,
                name,
            }
        }
        .okay()
    }
}

impl Signature {
    pub fn build() -> SignatureBuilder {
        SignatureBuilder {
            args: vec![],
            ret: None,
            name: None,
            foldable_type: None,
        }
    }

    pub fn build_from_arglist(args: Arglist) -> SignatureBuilder {
        SignatureBuilder {
            args,
            ret: None,
            name: None,
            foldable_type: None,
        }
    }

    pub fn arity(&self) -> usize {
        match &self.args {
            FnArgs::Arglist(args) => args.len(),
            FnArgs::Foldable(_) => usize::MAX,
        }
    }

    pub fn args(&self) -> Arglist {
        match &self.args {
            FnArgs::Arglist(args) => args.clone(),
            FnArgs::Foldable(t) => vec![("a".into(), *t), ("b".into(), *t)],
        }
    }

    pub fn foldable(&self) -> bool {
        match &self.args {
            FnArgs::Arglist(_) => false,
            FnArgs::Foldable(_) => true,
        }
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
