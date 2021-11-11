use std::{
    fmt::Debug,
    ops::{Add, Div, Mul, Sub},
};

use fnv::{FnvBuildHasher, FnvHashMap};
use somok::Somok;

use crate::{
    jit::Jit,
    parser::{Arglist, Expr},
    CranelispError, Env, EvalError, Result,
};

#[derive(Clone)]
pub struct Function {
    sig: Signature,
    body: FnBody,
}

impl Function {
    pub fn jit(self, jit: &mut Jit) -> Self {
        let sig = self.sig.clone();
        let body = jit.compile(self).unwrap();
        let body = FnBody::Jit(body);
        Function { sig, body }
    }
    pub fn args(&self) -> Vec<(String, Type)> {
        self.sig.args.clone()
    }
    pub fn body(&self) -> Expr {
        match &self.body {
            FnBody::Native(_) => todo!(),
            FnBody::Virtual(e) => e.clone(),
            FnBody::Jit(_) => todo!(),
        }
    }

    pub fn arity(&self) -> usize {
        self.sig.arity()
    }
    pub fn call(&self, mut args: Vec<Value>, env: &mut Env, jit: &mut Jit) -> Result<Value> {
        if self.arity() != args.len() && self.arity() != usize::MAX {
            return CranelispError::Eval(EvalError::ArityMismatch).error();
        }
        let mut predefined =
            FnvHashMap::with_capacity_and_hasher(args.len(), FnvBuildHasher::default());
        for (i, (name, _ty)) in self.sig.args.iter().enumerate().rev() {
            // TODO: check types, but probably earlier
            if let Some(v) = env.insert(name.clone(), args.remove(i)) {
                predefined.insert(name, v);
            };
        }

        let ret = self.body.call(env, jit);
        for (name, _ty) in self.sig.args.iter() {
            if let Some(val) = predefined.remove(name) {
                env.entry(name.clone()).and_modify(|v| *v = val);
            } else {
                env.remove(name);
            }
        }
        ret
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.body {
            FnBody::Native(_) => writeln!(f, "Native function")?,
            FnBody::Virtual(e) => writeln!(f, "Virtual function \n{:#?}", e.clone().unwrap_list())?,
            FnBody::Jit(_) => writeln!(f, "Jit function")?,
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
    Native(fn(&Env) -> Value),
    Virtual(Expr),
    Jit(*const u8),
}
impl FnBody {
    fn call(&self, env: &mut Env, jit: &mut Jit) -> Result<Value> {
        match self {
            FnBody::Native(f) => f(env).okay(),
            FnBody::Virtual(expr) => eval(expr.clone(), env, jit),
            FnBody::Jit(ptr) => unsafe {
                let func = std::mem::transmute::<_, fn(f64) -> f64>(ptr);
                Value::Number(func(1.0)).okay()
            },
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
    pub const TRUE: Value = Value::Number(1.0);
    pub const FALSE: Value = Value::Number(0.0);

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

impl Mul for &Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl Div for &Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
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

pub fn eval(expr: Expr, env: &mut Env, jit: &mut Jit) -> Result<Value> {
    match expr {
        ref expr @ Expr::Symbol(ref s, _) => env
            .get(s)
            .cloned()
            .ok_or_else(|| CranelispError::Eval(EvalError::Undefined(s.clone(), expr.span()))),
        Expr::Number(val, _) => Value::Number(val).okay(),
        Expr::List(mut list, _) => {
            let value_store;
            let fun = match list.remove(0) {
                Expr::Symbol(s, _) => env.get(&s).unwrap(),
                expr @ Expr::Defun(..) => {
                    value_store = eval(expr, env, jit)?;
                    &value_store
                }
                Expr::If(cond, truth, lie, _) => {
                    let cond = eval(*cond, env, jit)?;
                    return if cond > Value::Number(0.0) {
                        eval(*truth, env, jit)
                    } else {
                        eval(*lie, env, jit)
                    };
                }
                Expr::Number(val, _) => return Value::Number(val).okay(),
                expr @ Expr::List(..) => {
                    let mut last = eval(expr, env, jit);
                    for expr in list {
                        last = eval(expr, env, jit);
                    }
                    return last;
                }
                Expr::Let(sym, expr, _) => {
                    let v = eval(*expr, env, jit)?;
                    env.insert(sym, v);
                    return Value::None.okay();
                }
                expr @ Expr::Return(..) => return eval(expr, env, jit),
                expr @ Expr::Loop(..) => return eval(expr, env, jit),
                e => todo!("Error: unquoted list that isn't application\n{:#?}", e),
            }
            .clone();
            let values = list
                .into_iter()
                .map(|e| eval(e, env, jit))
                .collect::<Result<Vec<_>>>()?;
            fun.unwrap_fn().call(values, env, jit)
        }
        Expr::Quoted(_, _) => todo!("Deal vith evaluating things that shouldn't be evaluated"),
        Expr::Defun(args, body, ret, _) => {
            let sig = Signature::build_from_arglist(args).set_ret(ret).finish();
            let func = Function {
                sig,
                body: FnBody::Virtual(*body),
            }
            .jit(jit);
            Value::Func(func).okay()
        }
        Expr::Let(_sym, expr, _) => eval(*expr, env, jit),
        Expr::If(cond, truth, lie, _) => {
            let cond = eval(*cond, env, jit)?;
            if cond > Value::Number(0.0) {
                eval(*truth, env, jit)
            } else {
                eval(*lie, env, jit)
            }
        }
        Expr::Return(expr, ..) => {
            if let Some(expr) = expr {
                Value::Return(Box::new(eval(*expr, env, jit)?)).okay()
            } else {
                Value::Return(Box::new(Value::None)).okay()
            }
        }
        Expr::Loop(body, ..) => {
            let mut previous_value = Value::None;
            loop {
                let value = eval(*body.clone(), env, jit)?;
                if value.is_return() {
                    if value.is_valued_return() {
                        return value.return_inner().okay();
                    }
                    return previous_value.okay();
                }
                previous_value = value;
            }
        }
    }
}
