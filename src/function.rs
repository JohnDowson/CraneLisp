use somok::Somok;

use crate::{
    errors,
    eval::Value,
    jit::Jit,
    parser::{Arglist, Expr},
    CranelispError, EvalError, Result,
};
use std::fmt::Debug;

#[derive(Clone)]
pub struct Function {
    sig: Signature,
    body: FnBody,
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
        self.sig.args.clone()
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
        self.sig.foldable
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
                let args: Vec<_> = args.into_iter().map(|v| v.unwrap_number()).collect();
                match self.arity() {
                    1 => {
                        let func =
                            std::mem::transmute::<*const u8, extern "C" fn(f64) -> f64>(*ptr);
                        let arg = args[0];
                        let val = func(arg);
                        Value::Number(val).okay()
                    }
                    2 => {
                        let func =
                            std::mem::transmute::<*const u8, extern "C" fn(f64, f64) -> f64>(*ptr);
                        Value::Number(func(args[0], args[1])).okay()
                    }
                    usize::MAX => {
                        let func =
                            std::mem::transmute::<*const u8, extern "C" fn(f64, f64) -> f64>(*ptr);
                        #[allow(clippy::redundant_closure)]
                        // extern "C" fn does not implement FnMut
                        let result = args
                            .into_iter()
                            .reduce(|acc, n| func(acc, n))
                            .unwrap_or(0.0);
                        Value::Number(result).okay()
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
        if self.sig.foldable {
            writeln!(f, "*Num => {:?}", self.sig.ret)
        } else {
            writeln!(
                f,
                "{:?} => {:?}",
                self.sig.args.iter().map(|(_, t)| t).collect::<Vec<_>>(),
                self.sig.ret
            )
        }
    }
}

#[derive(Clone)]
pub enum FnBody {
    Virtual(Expr),
    Native(*const u8),
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub args: Vec<(String, Type)>,
    pub ret: Type,
    pub name: String,
    pub foldable: bool,
}

pub struct SignatureBuilder {
    args: Vec<(String, Type)>,
    ret: Option<Type>,
    name: Option<String>,
    foldable: bool,
}
impl SignatureBuilder {
    pub fn set_ret(mut self, t: Type) -> Self {
        self.ret = Some(t);
        self
    }
    pub fn set_foldable(mut self, t: bool) -> Self {
        self.foldable = t;
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
        Signature {
            args: self.args,
            ret,
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
            ret: None,
            name: None,
            foldable: false,
        }
    }

    pub fn build_from_arglist(args: Arglist) -> SignatureBuilder {
        SignatureBuilder {
            args,
            ret: None,
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
}

#[derive(Clone, Debug)]
pub enum Type {
    Number,
}

impl Type {
    pub fn number_from_str(str: &str) -> Option<Self> {
        match str {
            "Num" => Self::Number.some(),
            _ => None,
        }
    }
}
