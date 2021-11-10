use std::{
    fmt::Debug,
    ops::{Add, Sub},
};

use log::trace;

use crate::{parser::Expr, Env};

#[derive(Clone)]
pub struct Function {
    sig: Signature,
    body: FnBody,
}

impl Function {
    pub fn arity(&self) -> usize {
        self.sig.arity()
    }
    pub fn call(&self, args: &[Value], mut env: Env) -> Value {
        for (i, (name, ty)) in self.sig.args.iter().enumerate() {
            env.insert(name.clone(), args[i].clone());
        }

        self.body.call(env)
    }
}

#[derive(Clone)]
pub enum FnBody {
    Native(fn(Env) -> Value),
    Virtual(Expr),
}
impl FnBody {
    fn call(&self, env: Env) -> Value {
        match self {
            FnBody::Native(f) => f(env),
            FnBody::Virtual(expr) => eval(expr.clone(), env),
        }
    }
}

#[derive(Clone)]
pub struct Signature {
    args: Vec<(String, Type)>,
    ret: Type,
}

pub struct SignatureBuilder {
    args: Vec<(String, Type)>,
    ret: Option<Type>,
}
impl SignatureBuilder {
    pub fn set_ret(mut self, t: Type) -> Self {
        self.ret = Some(t);
        self
    }

    pub fn push_arg(mut self, arg: (String, Type)) -> Self {
        self.args.push(arg);
        self
    }
    pub fn finish(self) -> Signature {
        Signature {
            args: self.args,
            ret: self.ret.unwrap(),
        }
    }
}

impl Signature {
    pub fn build() -> SignatureBuilder {
        SignatureBuilder {
            args: vec![],
            ret: None,
        }
    }

    pub fn arity(&self) -> usize {
        self.args.len()
    }
}

#[derive(Clone)]
pub enum Type {
    Number,
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Func(Function),
    None,
}

impl Value {
    pub fn number(n: f64) -> Self {
        Self::Number(n)
    }
    pub fn func(sig: Signature, body: FnBody) -> Self {
        Value::Func(Function { sig, body })
    }

    fn unwrap_fn(self) -> Function {
        match self {
            Value::Number(_) => panic!("Called 'unwrap_fn' on a value of type Number"),
            Value::None => panic!("Called 'unwrap_fn' on a value of type None"),
            Value::Func(f) => f,
        }
    }
}

impl Add for &Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl Sub for &Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(inner) => write!(f, "{:?}", inner),
            Value::Func(_) => write!(f, "Function"),
            Value::None => write!(f, "None"),
        }
    }
}

pub fn eval(expr: Expr, mut env: Env) -> Value {
    match expr {
        Expr::Symbol(s, _) => env.get(&s).unwrap().clone(),
        Expr::Number(val, _) => Value::Number(val),
        Expr::List(mut list, _) => {
            let defun;
            let fun = match list.remove(0) {
                Expr::Symbol(s, _) => env.get(&s).unwrap(),
                Expr::Defun(args, body, _) => {
                    let sig = {
                        let mut builder = Signature::build().set_ret(Type::Number);
                        for arg in args.as_list() {
                            builder = builder.push_arg((arg.as_symbol(), Type::Number));
                        }
                        builder.finish()
                    };
                    defun = Value::func(sig, FnBody::Virtual(*body));
                    &defun
                }
                _ => panic!(),
            };
            list.into_iter()
                .map(|e| eval(e, env.clone()))
                .reduce(|acc, v| fun.clone().unwrap_fn().call(&[acc, v], env.clone()))
                .unwrap()
        }
        Expr::Quoted(_, _) => todo!(),
        Expr::Defun(args, body, _) => {
            let sig = {
                let mut builder = Signature::build().set_ret(Type::Number);
                for arg in args.as_list() {
                    builder = builder.push_arg((arg.as_symbol(), Type::Number));
                }
                builder.finish()
            };
            Value::func(sig, FnBody::Virtual(*body))
        }
    }
}
