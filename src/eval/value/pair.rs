use somok::{Leaksome, Somok};

use super::Atom;
use std::fmt::{Debug, Display};

pub fn list(mut vs: Vec<Atom>) -> Atom {
    let mut p = Atom::new_pair(
        Pair {
            car: vs.remove(0).boxed().leak(),
            cdr: Atom::NULL.boxed().leak(),
        }
        .boxed()
        .leak(),
    )
    .boxed()
    .leak();
    let po = p as *mut Atom;
    for v in vs {
        let p2 = Atom::new_pair(
            cons(v.boxed().leak(), Atom::NULL.boxed().leak())
                .boxed()
                .leak(),
        )
        .boxed()
        .leak();
        unsafe { (*p.as_pair()).cdr = p2 };
        p = p2;
    }
    unsafe { *po }
}

pub fn cons(v1: *mut Atom, v2: *mut Atom) -> Pair {
    Pair::new(v1, v2)
}
pub fn head(v: &Atom) -> *mut Atom {
    unsafe { *v.as_pair() }.car
}
pub fn tail(v: &Atom) -> *mut Atom {
    unsafe { *v.as_pair() }.cdr
}
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Pair {
    pub car: *mut Atom,
    pub cdr: *mut Atom,
}

impl Pair {
    pub fn new(car: *mut Atom, cdr: *mut Atom) -> Self {
        Self { car, cdr }
    }
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "( {:?} . {:?})", &*self.car, &*self.cdr) }
    }
}
impl Display for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "( {} . {})", &*self.car, &*self.cdr) }
    }
}
