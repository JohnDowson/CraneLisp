use somok::{Leaksome, Somok};

use crate::{value::Atom, CranelispError, Result};
use std::{fmt::Debug, str::FromStr};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Func {
    pub body: unsafe extern "C" fn(usize, *mut *mut Atom) -> *mut Atom,
}

impl Func {
    pub fn from_fn(body: unsafe extern "C" fn(usize, *mut *mut Atom) -> *mut Atom) -> Self {
        Self { body }
    }
    // pub fn jit(jit: &mut Jit, defun: Atom) -> Result<Self> {
    //     let body = unsafe { std::mem::transmute::<_, _>(jit.compile(todo!(), defun)?) };
    //     Self { body }.okay()
    // }

    pub fn call(&self, args: Vec<Atom>) -> Atom {
        let args = args
            .into_iter()
            .map(|a| a.boxed().leak() as *mut Atom)
            .collect::<Vec<*mut Atom>>();
        let (count, args) = (args.len(), args.leak().as_mut_ptr());
        unsafe { (&*(self.body)(count, args)).clone() }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ body: {:?} }}", self.body as *const u8)
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
