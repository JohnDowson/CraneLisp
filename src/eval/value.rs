use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};

use crate::function::{FnBody, Function, Signature};

#[derive(Clone)]
pub enum Value {
    Float(f64),
    Integer(i64),
    String(String),
    Func(Function),
    Return(Box<Value>),
    None,
}

impl Value {
    pub const TRUE: Value = Value::Integer(1);
    pub const FALSE: Value = Value::Integer(0);

    pub fn float(n: f64) -> Self {
        Self::Float(n)
    }
    pub fn func(sig: Signature, body: FnBody) -> Self {
        Value::Func(Function::new(sig, body))
    }

    pub fn unwrap_fn(self) -> Function {
        match self {
            Value::Float(_) => panic!("Called 'unwrap_fn' on a value of type Number"),
            Value::Integer(_) => panic!("Called 'unwrap_fn' on a value of type Integer"),
            Value::String(_) => panic!("Called 'unwrap_fn' on a value of type String"),
            Value::None => panic!("Called 'unwrap_fn' on a value of type None"),
            Value::Return(..) => panic!("Called 'unwrap_fn' on a value of type Return"),
            Value::Func(f) => f,
        }
    }

    pub fn unwrap_float(self) -> f64 {
        match self {
            Value::Float(n) => n,
            Value::Integer(_) => panic!("Called 'unwrap_float' on a value of type Integer"),
            Value::String(_) => panic!("Called 'unwrap_float' on a value of type String"),
            Value::None => panic!("Called 'unwrap_float' on a value of type None"),
            Value::Return(..) => panic!("Called 'unwrap_float' on a value of type Return"),
            Value::Func(_) => panic!("Called 'unwrap_float' on a value of type Func"),
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
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl Mul for &Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl Div for &Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Float(l0), Self::Float(r0)) => l0.partial_cmp(r0),
            _ => None,
        }
    }
}

impl Sub for &Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            _ => panic!("Type mismatch"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(inner) => write!(f, "{:?}", inner),
            Value::Integer(inner) => write!(f, "{:?}", inner),
            Value::String(inner) => write!(f, "{:?}", inner),
            Value::Func(fun) => write!(f, "{:?}", fun),
            Value::None => write!(f, "None"),
            Value::Return(v) => write!(f, "Return({:?})", v),
        }
    }
}
