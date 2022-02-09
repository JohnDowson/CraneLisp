use crate::mem;
use std::{
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};
mod symbol;
pub use symbol::*;
mod utils;
pub use utils::*;

#[derive(Clone)]
pub struct Pair {
    pub car: mem::Ref,
    pub cdr: mem::Ref,
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Pair")
            .field(&self.car)
            .field(&self.cdr)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub args: Vec<SymId>,
    pub iptr: usize,
}

pub enum Atom {
    FRef(Option<usize>),
    Error(ErrorCode),
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Pair(Pair),
    Func(Func),
    Symbol(SymId),
    String(Rc<String>),
    Macro(&'static ()),
    Ptr(mem::Ref),
}

impl Clone for Atom {
    fn clone(&self) -> Self {
        match self {
            Atom::FRef(inner) => Self::FRef(*inner),
            Atom::Error(inner) => Self::Error(*inner),
            Atom::Null => Self::Null,
            Atom::Int(inner) => Self::Int(*inner),
            Atom::Float(inner) => Self::Float(*inner),
            Atom::Bool(inner) => Self::Bool(*inner),
            Atom::Pair(Pair { car, cdr }) => Self::Pair(Pair {
                car: car.clone(),
                cdr: cdr.clone(),
            }),
            Atom::Func(inner) => Self::Func(inner.clone()),
            Atom::Symbol(inner) => Self::Symbol(*inner),
            Atom::String(inner) => Self::String(inner.clone()),
            Atom::Macro(inner) => Self::Macro(*inner),
            Atom::Ptr(inner) => Self::Ptr(inner.clone()),
        }
    }
}

impl Atom {
    pub fn as_fref(&self) -> Option<Option<usize>> {
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

    pub fn truthy(&self) -> bool {
        if let Some(b) = self.as_bool() {
            b
        } else {
            !self.is_null()
        }
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

    pub fn pair_alloc_static(car: Atom, cdr: Atom) -> Self {
        Self::Pair(Pair {
            car: mem::static_alloc(car),
            cdr: mem::static_alloc(cdr),
        })
    }

    pub fn pair_alloc(car: Atom, cdr: Atom) -> Self {
        Self::Pair(Pair {
            car: mem::alloc(car),
            cdr: mem::alloc(cdr),
        })
    }

    pub fn new_func(args: Vec<SymId>, iptr: usize) -> Self {
        Self::Func(Func { args, iptr })
    }

    pub fn as_func(&self) -> Option<Func> {
        if let Self::Func(v) = self {
            Some(v.clone())
        } else {
            None
        }
    }

    pub fn as_symbol(&self) -> Option<SymId> {
        if let Self::Symbol(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the atom is [`FRef`].
    ///
    /// [`FRef`]: Atom::FRef
    pub fn is_fref(&self) -> bool {
        matches!(self, Self::FRef(..))
    }

    /// Returns `true` if the atom is [`Bool`].
    ///
    /// [`Bool`]: Atom::Bool
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the atom is [`Symbol`].
    ///
    /// [`Symbol`]: Atom::Symbol
    pub fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(..))
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
            Atom::Bool(b) => write!(f, "{:?}", b),
            Atom::Pair(Pair { car, cdr }) => write!(f, "({:?} . {:?})", car.deref(), cdr.deref()),
            Atom::Func(func) => write!(f, "Func {:?}", func),
            Atom::Symbol(s) => write!(f, "Symbol {:?}", s),
            Atom::String(s) => write!(f, "String {:?}", s),
            Atom::Macro(r) => write!(f, "Macro({:?})", r),
            Atom::Ptr(r) => write!(f, "Ptr({:?})", r),
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
            Atom::Bool(b) => write!(f, "{}", b),
            Atom::Pair(Pair { car, cdr }) => write!(f, "({} . {})", car.deref(), cdr.deref()),
            Atom::Func(func) => write!(f, "{:?}", func),
            Atom::Symbol(s) => write!(f, "{:?}", s),
            Atom::String(s) => write!(f, "{:?}", s),
            Atom::Macro(r) => write!(f, "Macro({:?})", r),
            Atom::Ptr(r) => write!(f, "&{:?}", r),
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(u64)]
pub enum ErrorCode {
    Arity = 0,
    Type = 1,
}
