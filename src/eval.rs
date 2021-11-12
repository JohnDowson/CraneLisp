use std::{
    fmt::Debug,
    ops::{Add, Div, Mul, Sub},
};

use somok::Somok;

use crate::{
    function::{FnBody, Function, Signature},
    jit::Jit,
    parser::{Expr, FnArgs},
    CranelispError, Env, EvalError, Result,
};

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
        Value::Func(Function::new(sig, body))
    }

    pub fn unwrap_fn(self) -> Function {
        match self {
            Value::Number(_) => panic!("Called 'unwrap_fn' on a value of type Number"),
            Value::None => panic!("Called 'unwrap_fn' on a value of type None"),
            Value::Return(..) => panic!("Called 'unwrap_fn' on a value of type Return"),
            Value::Func(f) => f,
        }
    }

    pub fn unwrap_number(self) -> f64 {
        match self {
            Value::Number(n) => n,
            _ => panic!("Called 'unwrap_number' on a non number value"),
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
            fun.unwrap_fn().call(values)
        }
        Expr::Quoted(_, _) => todo!("Deal vith evaluating things that shouldn't be evaluated"),
        Expr::Defun(name, args, body, ret, _) => {
            let sig = match args {
                FnArgs::Arglist(args) => Signature::build_from_arglist(args)
                    .set_name(name.clone())
                    .set_ret(ret)
                    .finish()?,
                FnArgs::Foldable => Signature::build()
                    .set_foldable(true)
                    .set_name(name.clone())
                    .set_ret(ret)
                    .finish()?,
            };
            let func = Function::new(sig, FnBody::Virtual(*body)).jit(jit)?;
            env.insert(name, Value::Func(func.clone()));
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
