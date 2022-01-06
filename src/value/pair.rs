use somok::{Leaksome, Somok};

use super::{Atom, Tag};
use std::fmt::{Debug, Display};

pub fn list(mut vs: Vec<Atom>) -> Atom {
    let mut p = Atom::new_pair(
        Pair {
            car: vs.remove(0).boxed().leak(),
            cdr: Atom::NULL.boxed().leak(),
        }
        .boxed(),
    )
    .boxed()
    .leak();
    let po = p as *mut Atom;
    for v in vs {
        let p2 = cons(v.boxed().leak(), Atom::NULL.boxed().leak())
            .boxed()
            .leak();
        unsafe { (*p.as_pair()).cdr = p2 };
        p = p2;
    }
    unsafe { *po }
}

pub fn cons(v1: *mut Atom, v2: *mut Atom) -> Atom {
    Atom::new_pair(Pair::new(v1, v2).boxed())
}

/// # Safety
/// unsafe lol
pub unsafe fn car(v: *mut Atom) -> *mut Atom {
    (*(*v).as_pair()).car
}

/// # Safety
/// unsafe lol
pub unsafe fn cdr(v: *mut Atom) -> *mut Atom {
    (*(*v).as_pair()).cdr
}

pub fn scar(v: &mut Atom) -> &mut Atom {
    unsafe { &mut *(*v.as_pair()).car }
}

pub fn scdr(v: &mut Atom) -> &mut Atom {
    unsafe { &mut *(*v.as_pair()).cdr }
}

pub fn ocar(v: Atom) -> Atom {
    unsafe { *(*v.as_pair()).car }
}

pub fn ocdr(v: Atom) -> Atom {
    unsafe { *(*v.as_pair()).cdr }
}

pub fn map(l: Atom, op: impl Fn(Atom) -> Atom) -> Atom {
    let mut buf = Vec::new();
    let mut val = ocar(l);
    let mut next = ocdr(l);
    loop {
        buf.push(op(val));
        if matches!(next.tag, Tag::Null | Tag::Error) {
            break;
        }
        val = ocar(next);
        next = ocdr(next);
    }
    list(buf)
}

pub fn map_to_vec(l: Atom, op: impl Fn(Atom) -> Atom) -> Vec<Atom> {
    let mut buf = Vec::new();
    let mut val = ocar(l);
    let mut next = ocdr(l);
    loop {
        buf.push(op(val));
        if matches!(next.tag, Tag::Null | Tag::Error) {
            break;
        }
        val = ocar(next);
        next = ocdr(next);
    }
    buf
}

/// # Safety
/// This function is unsafe lol
pub unsafe fn append(l: *mut Atom, v: Atom) {
    let mut next = cdr(l);
    loop {
        if matches!((*next).tag, Tag::Null) {
            break;
        } else {
            next = cdr(next)
        }
    }
    (*(*next).as_pair()).cdr = cons(v.boxed().leak(), Atom::NULL.boxed().leak())
        .boxed()
        .leak();
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

    pub fn new_zeroed() -> Self {
        Self {
            car: Atom::NULL.boxed().leak(),
            cdr: Atom::NULL.boxed().leak(),
        }
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
