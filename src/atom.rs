use crate::{symbol::SymId, vm::closure::RuntimeFn};
use cl_alloc::{Mark, Ref};
use somok::Somok;
use std::{fmt::Debug, rc::Rc};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct FuncId(pub usize);
pub type AtomRef<const ID: u32> = Ref<Atom<ID>, ID, 2>;

#[derive(PartialEq, Clone)]
pub enum Atom<const ID: u32> {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Pair(AtomRef<ID>, AtomRef<ID>),
    Symbol(SymId),
    Func(Rc<RuntimeFn>),
    Place(Place<ID>),
}

#[derive(PartialEq, Clone)]
pub enum Place<const ID: u32> {
    Stack(u32),
    Heap(AtomRef<ID>),
}

impl<const ID: u32> Debug for Place<ID> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stack(arg0) => f.debug_tuple("Stack").field(arg0).finish(),
            Self::Heap(arg0) => f.debug_tuple("Heap").field(arg0).finish(),
        }
    }
}

impl<const ID: u32> std::fmt::Debug for Atom<ID> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Pair(_, _) => f.debug_tuple("Pair").finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::Func(arg0) => f.debug_tuple("Func").field(arg0).finish(),
            Self::Place(p) => f.debug_tuple("Place").field(p).finish(),
        }
    }
}

impl<const ID: u32> Mark<ID, 2> for Atom<ID> {
    fn refs(&self) -> [Option<AtomRef<ID>>; 2] {
        if let Self::Pair(car, cdr) = self {
            [car.clone().some(), cdr.clone().some()]
        } else if let Self::Place(Place::Heap(r)) = self {
            [r.clone().some(), None]
        } else {
            [None, None]
        }
    }
}
