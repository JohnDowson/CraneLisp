use crate::value::{Pair, SymId};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct FuncId(pub usize);

#[derive(Debug)]
pub enum Atom {
    Null,
    Int(i64),
    Float(f64),
    Pair(Pair),
    Symbol(SymId),
    Func(FuncId),
}

impl Atom {
    pub fn tag(&self) -> u64 {
        match self {
            Atom::Null => 0,
            Atom::Int(_) => 1,
            Atom::Float(_) => 2,
            Atom::Pair(_) => 3,
            Atom::Symbol(_) => 4,
            Atom::Func(_) => 5,
        }
    }

    pub fn from_atom(atom: &crate::value::Atom) -> Atom {
        match atom {
            crate::value::Atom::Null => Self::Null,
            crate::value::Atom::Int(i) => Self::Int(*i),
            crate::value::Atom::Float(f) => Self::Float(*f),
            crate::value::Atom::Pair(p) => Self::Pair(p.clone()),
            crate::value::Atom::Symbol(s) => Self::Symbol(*s),
            _ => todo!(),
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}
