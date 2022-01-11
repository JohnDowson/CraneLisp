use somok::Somok;

use crate::{
    env::{get_env, Env, EnvId},
    mem,
    value::{collect, Atom, SymId},
    CranelispError, Result, Span,
};
use std::{fmt::Debug, str::FromStr};

pub enum Func {
    Native(fn(mem::Ref, &mut Env) -> Atom),
    Interp { params: Vec<SymId>, body: Atom },
}

impl Func {
    pub fn from_fn(body: fn(mem::Ref, &mut Env) -> Atom) -> Self {
        Self::Native(body)
    }

    pub fn interp(params: Vec<SymId>, body: Atom) -> Self {
        Self::Interp { params, body }
    }

    // pub fn jit(jit: &mut Jit, defun: Atom) -> Result<Self> {
    //     let body = unsafe { std::mem::transmute::<_, _>(jit.compile("Fubar".into(), defun)?) };
    //     Self::Native(body).okay()
    // }

    pub fn call(&self, args: mem::Ref, env: EnvId) -> Atom {
        match self {
            Func::Native(body) => (body)(args, get_env(env)),
            Func::Interp { params, body } => {
                let args = collect(args);
                for (arg, sym) in args.into_iter().zip(params.iter()) {
                    get_env(env).insert(*sym, arg)
                }
                crate::eval::eval((body.clone(), Span::new(0, 0)), env).unwrap()
            }
        }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Func::Native(body) => write!(f, "{{ body: {:?} }}", *body as *const u8),
            Func::Interp { params, body } => {
                write!(f, "{{ params: {:?} body: {:?} }}", params, body)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum FnArgs {
    Foldable(Type),
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub args: Vec<String>,
    pub name: String,
}

impl Signature {
    pub fn new(name: String, args: Vec<String>) -> Self {
        Self { args, name }
    }

    pub fn arity(&self) -> usize {
        self.args.len()
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
