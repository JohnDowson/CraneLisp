use std::{
    fmt::Debug,
    ops::{Add, Sub},
};

use somok::Somok;

use crate::{
    parser::{Arglist, Expr},
    Env,
};

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
        for (i, (name, _ty)) in self.sig.args.iter().enumerate() {
            // TODO: check types
            env.insert(name.clone(), args[i].clone());
        }

        self.body.call(env)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.body {
            FnBody::Native(_) => writeln!(f, "Native function")?,
            FnBody::Virtual(e) => writeln!(f, "Virtual function \n{:?}", e.clone().unwrap_list())?,
        };
        writeln!(
            f,
            "{:?} => {:?}",
            self.sig.args.iter().map(|(_, t)| t).collect::<Vec<_>>(),
            self.sig.ret
        )
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

    pub fn build_from_arglist(args: Arglist) -> SignatureBuilder {
        SignatureBuilder { args, ret: None }
    }

    pub fn arity(&self) -> usize {
        self.args.len()
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Number,
}

impl Type {
    pub fn from_str(str: &str) -> Option<Self> {
        match str {
            "Num" => Self::Number.some(),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Func(Function),
    Return(Box<Value>),
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
            Value::Return(..) => panic!("Called 'unwrap_fn' on a value of type Return"),
            Value::Func(f) => f,
        }
    }

    /// Returns `true` if the value is [`Func`].
    ///
    /// [`Func`]: Value::Func
    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func(..))
    }

    /// Returns `true` if the value is [`Return`].
    ///
    /// [`Return`]: Value::Return
    pub fn is_return(&self) -> bool {
        matches!(self, Self::Return(..))
    }

    pub fn is_valued_return(&self) -> bool {
        matches!(self, Value::Return(v) if !v.is_none())
    }

    pub fn return_inner(self) -> Value {
        match self {
            Value::Return(v) => *v,
            _ => panic!("Tried to unwrap return on non-return value"),
        }
    }

    /// Returns `true` if the value is [`None`].
    ///
    /// [`None`]: Value::None
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0.partial_cmp(r0),
            _ => None,
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
            Value::Func(fun) => write!(f, "{:?}", fun),
            Value::None => write!(f, "None"),
            Value::Return(v) => write!(f, "Return({:?})", v),
        }
    }
}

pub fn eval(expr: Expr, env: Env) -> Value {
    match expr {
        Expr::Symbol(s, _) => env.get(&s).unwrap().clone(),
        Expr::Number(val, _) => Value::Number(val),
        Expr::List(mut list, _) => {
            let value_store;
            let fun = match list.remove(0) {
                Expr::Symbol(s, _) => env.get(&s).unwrap(),
                Expr::Defun(args, body, ret, _) => {
                    let sig = { Signature::build_from_arglist(args).set_ret(ret).finish() };
                    value_store = Value::func(sig, FnBody::Virtual(*body));
                    &value_store
                }
                Expr::If(cond, truth, lie, _) => {
                    let cond = eval(*cond, env.clone());
                    value_store = if cond > Value::Number(0.0) {
                        eval(*truth, env.clone())
                    } else {
                        eval(*lie, env.clone())
                    };
                    return value_store;
                }
                expr @ Expr::Return(..) => return eval(expr, env),
                _ => todo!("Error: unquoted list that isn't application"),
            };
            list.into_iter()
                .map(|e| eval(e, env.clone()))
                .reduce(|acc, v| fun.clone().unwrap_fn().call(&[acc, v], env.clone()))
                .unwrap()
        }
        Expr::Quoted(_, _) => todo!("Deal vith evaluating things that shouldn't be evaluated"),
        Expr::Defun(args, body, ret, _) => {
            let sig = Signature::build_from_arglist(args).set_ret(ret).finish();
            Value::func(sig, FnBody::Virtual(*body))
        }
        Expr::If(..) => todo!("Evaluate if"),
        Expr::Return(expr, ..) => {
            if let Some(expr) = expr {
                Value::Return(Box::new(eval(*expr, env)))
            } else {
                Value::Return(Box::new(Value::None))
            }
        }
        Expr::Loop(body, ..) => {
            let mut previous_value = Value::None;
            loop {
                let value = eval(*body.clone(), env.clone());
                if value.is_return() {
                    if value.is_valued_return() {
                        return value.return_inner();
                    }
                    return previous_value;
                }
                previous_value = value;
            }
        }
    }
}
