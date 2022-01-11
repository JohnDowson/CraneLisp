use crate::{function::Func, mem};
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};
mod symbol;
use once_cell::unsync::Lazy;
pub use symbol::*;
mod utils;
pub use utils::*;

pub static mut NULL: Lazy<mem::Ref> = Lazy::new(|| mem::alloc(Atom::Null));

pub struct Pair {
    pub car: mem::Ref,
    pub cdr: mem::Ref,
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Pair")
            .field(&self.car.clone().deref())
            .field(&self.cdr.clone().deref())
            .finish()
    }
}

pub enum Atom {
    FRef(usize),
    Error(ErrorCode),
    Null,
    Int(i64),
    Float(f64),
    Pair(Pair),
    Func(&'static Func),
    Symbol(SymId),
    String(String),
    Return(mem::Ref),
    Macro(&'static Func),
    Quoted(mem::Ref),
}

impl Clone for Atom {
    fn clone(&self) -> Self {
        match self {
            Atom::FRef(inner) => Self::FRef(*inner),
            Atom::Error(inner) => Self::Error(*inner),
            Atom::Null => Self::Null,
            Atom::Int(inner) => Self::Int(*inner),
            Atom::Float(inner) => Self::Float(*inner),
            Atom::Pair(Pair { car, cdr }) => Self::Pair(Pair {
                car: car.clone(),
                cdr: cdr.clone(),
            }),
            Atom::Func(inner) => Self::Func(*inner),
            Atom::Symbol(inner) => Self::Symbol(*inner),
            Atom::String(inner) => Self::String(inner.clone()),
            Atom::Return(inner) => Self::Return(inner.clone()),
            Atom::Macro(inner) => Self::Macro(*inner),
            Atom::Quoted(inner) => Self::Quoted(inner.clone()),
        }
    }
}

impl Atom {
    pub const TRUE: Self = Self::Int(1);
    pub const FALSE: Self = Self::Null;

    pub fn as_fref(&self) -> Option<usize> {
        if let Self::FRef(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_error(&self) -> Option<&ErrorCode> {
        if let Self::Error(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the atom is [`Null`].
    ///
    /// [`Null`]: Atom::Null
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    /// Returns `true` if the atom is [`Error`].
    ///
    /// [`Error`]: Atom::Error
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(..))
    }

    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        if let Self::Float(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_pair(&self) -> Option<&Pair> {
        if let Self::Pair(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_pair_mut(&mut self) -> Option<&mut Pair> {
        if let Self::Pair(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn pair(car: mem::Ref, cdr: mem::Ref) -> Self {
        Self::Pair(Pair { car, cdr })
    }

    pub fn pair_alloc(car: Atom, cdr: Atom) -> Self {
        Self::Pair(Pair {
            car: mem::alloc(car),
            cdr: mem::alloc(cdr),
        })
    }

    pub fn as_func(&self) -> Option<&&'static Func> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_symbol(&self) -> Option<&SymId> {
        if let Self::Symbol(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::FRef(r) => write!(f, "FRef({:?})", r),
            Atom::Error(e) => write!(f, "Error::{:?}", e),
            Atom::Null => write!(f, "Null"),
            Atom::Int(i) => write!(f, "i{:?}", i),
            Atom::Float(i) => write!(f, "f{:?}", i),
            Atom::Pair(Pair { car, cdr }) => write!(f, "({:?} . {:?})", car.deref(), cdr.deref()),
            Atom::Func(func) => write!(f, "Func {:?}", func),
            Atom::Symbol(s) => write!(f, "Symbol {:?}", s),
            Atom::String(s) => write!(f, "String {:?}", s),
            Atom::Return(r) => write!(f, "Return({:?})", r),
            Atom::Macro(r) => write!(f, "Macro({:?})", r),
            Atom::Quoted(r) => write!(f, "Quoted({:?})", r.deref()),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::FRef(r) => write!(f, "FRef({:?})", r),
            Atom::Error(e) => write!(f, "{:?}", e),
            Atom::Null => write!(f, "Null"),
            Atom::Int(i) => write!(f, "{}", i),
            Atom::Float(i) => write!(f, "{}", i),
            Atom::Pair(Pair { car, cdr }) => write!(f, "({} . {})", car.deref(), cdr.deref()),
            Atom::Func(func) => write!(f, "{:?}", func),
            Atom::Symbol(s) => write!(f, "{:?}", s),
            Atom::String(s) => write!(f, "{:?}", s),
            Atom::Return(r) => write!(f, "Return({})", r.deref()),
            Atom::Macro(r) => write!(f, "Macro({:?})", r),
            Atom::Quoted(r) => write!(f, "{}", r.deref()),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(u64)]
pub enum ErrorCode {
    Arity = 0,
    Type = 1,
}
