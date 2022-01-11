#![feature(box_into_inner)]
#![feature(box_patterns)]
#![feature(box_syntax)]

#[macro_export]
macro_rules! syntax {
    ($kind:tt, $($spans:expr),+) => {
        {
            //let mut vec = vec![];
            let vec = vec![$($spans),*];
            //$(vec.push($spans);),*
            crate::errors::CranelispError::Syntax(
            crate::errors::SyntaxError {
                kind: crate::errors::SyntaxErrorKind::$kind,
                spans: vec,
            })
        }
    };
    ($kind:tt) => {
        {crate::errors::CranelispError::Syntax(
            crate::errors::SyntaxError {
                kind: crate::errors::SyntaxErrorKind::$kind,
                spans: vec![],
            })
        }
    };
}
pub mod errors;
pub mod eval;
pub mod function;
// pub mod jit;
pub mod lexer;
pub mod libcl;
pub mod mem;
pub mod parser;
pub mod value;

pub use errors::*;
use fnv::{FnvBuildHasher, FnvHashMap};
use function::Func;
use indexmap::IndexSet;
use somok::{Leaksome, Somok};
use std::ops::{Index, IndexMut};
use value::{intern, Atom, SymId};

pub type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;
pub type Result<T, E = CranelispError> = std::result::Result<T, E>;
#[derive(Debug)]
pub struct Env<'a> {
    parent: Option<&'a mut Env<'a>>,
    inner: FnvHashMap<SymId, Atom>,
}

impl<'this> Env<'this> {
    pub fn insert(&mut self, sym: SymId, atom: Atom) {
        self.inner.insert(sym, atom);
    }

    pub fn try_get(&self, index: SymId) -> Option<&Atom> {
        self.inner.get(&index).or_else(|| {
            if let Some(parent) = &self.parent {
                parent.try_get(index)
            } else {
                None
            }
        })
    }

    pub fn try_get_mut(&mut self, index: SymId) -> Option<&mut Atom> {
        self.inner.get_mut(&index).or_else(|| {
            if let Some(parent) = &mut self.parent {
                parent.try_get_mut(index)
            } else {
                None
            }
        })
    }

    pub fn fork(&'this mut self) -> Env<'this> {
        Env::<'this> {
            parent: Some(self),
            inner: Default::default(),
        }
    }
}
impl<'a> Index<SymId> for Env<'a> {
    type Output = Atom;

    fn index(&self, index: SymId) -> &Self::Output {
        self.inner.get(&index).unwrap_or_else(|| {
            if let Some(parent) = self.parent {
                &parent[index]
            } else {
                panic!("Symbol not found")
            }
        })
    }
}
impl<'a> IndexMut<SymId> for Env<'a> {
    fn index_mut(&mut self, index: SymId) -> &mut Self::Output {
        self.inner.get_mut(&index).unwrap_or_else(|| {
            if let Some(parent) = &mut self.parent {
                &mut parent[index]
            } else {
                panic!("Symbol not found")
            }
        })
    }
}

pub fn setup<'a>() -> Env<'a> {
    let mut env = Env {
        parent: None,
        inner: Default::default(),
    };
    env.insert(
        intern("+"),
        Atom::Func(Func::from_fn(libcl::add).boxed().leak()),
    );
    env.insert(
        intern("-"),
        Atom::Func(Func::from_fn(libcl::sub).boxed().leak()),
    );
    env
}

#[derive(Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    //pub source_id: &'static str,
}

impl Span {
    pub fn new(start: usize, end: usize /*source_id: &'static str*/) -> Self {
        Self {
            start,
            end,
            //source_id,
        }
    }
    pub fn point(point: usize /*source_id: &'static str*/) -> Self {
        Self {
            start: point,
            end: point,
            //source_id,
        }
    }
    pub fn merge(mut first: Self, second: Self) -> Self {
        first.end = second.end;
        first
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}..{}]", /* , &self.source_id*/
            &self.start, &self.end
        )
    }
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}
