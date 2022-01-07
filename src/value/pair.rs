use somok::Somok;

use super::{Atom, Tag};
use std::fmt::{Debug, Display};

pub fn cons(car: Box<Atom>, cdr: Box<Atom>) -> Atom {
    Atom::new_pair(Pair::new(car, cdr).boxed())
}

pub fn car(v: Atom) -> Atom {
    *v.as_pair_owned().car
}

pub fn cdr(v: Atom) -> Atom {
    *v.as_pair_owned().cdr
}

pub fn destruct(v: Atom) -> (Box<Atom>, Box<Atom>) {
    let pair = v.as_pair_owned();
    (pair.car, pair.cdr)
}

/// # Safety
/// unsafe lol
pub unsafe fn pcar(v: *mut Atom) -> *mut Atom {
    Box::into_raw((((&*v).clone()).as_pair_owned()).car)
}

/// # Safety
/// unsafe lol
pub unsafe fn pcdr(v: *mut Atom) -> *mut Atom {
    Box::into_raw(((&*v).clone().as_pair_owned()).cdr)
}

pub fn scar(v: &Atom) -> &Atom {
    &*(*v.as_pair()).car
}

pub fn scdr(v: &Atom) -> &Atom {
    &*(*v.as_pair()).cdr
}

pub fn mcar(v: &mut Atom) -> &mut Atom {
    &mut *(*v.as_pair_mut()).car
}

pub fn mcdr(v: &mut Atom) -> &mut Atom {
    &mut (*v.as_pair_mut()).cdr
}

pub fn append(mut head: Atom, v: Atom) -> Atom {
    let mut next = &mut head;
    while !matches!(scdr(next).tag, Tag::Null) {
        next = mcdr(next);
    }
    *mcdr(next) = cons(v.boxed(), Atom::NULL.boxed());
    head
}

pub fn join(mut head: Atom, head2: Atom) -> Atom {
    let mut next = &mut head;
    while !matches!(scdr(next).tag, Tag::Null) {
        next = mcdr(next);
    }
    *mcdr(next) = head2;
    head
}

pub fn map(mut head: Atom, op: impl Fn(Atom) -> Atom) -> Atom {
    let mut next = &mut head;
    loop {
        if matches!(&next.tag, Tag::Null) {
            break;
        }
        *mcar(next) = op(car(next.clone()));
        next = mcdr(next);
    }
    head
}

pub fn map_to_vec(mut head: Atom, op: impl Fn(Atom) -> Atom) -> Vec<Atom> {
    let mut buf = Vec::new();
    let mut next = &mut head;
    loop {
        if matches!(&next.tag, Tag::Null) {
            break;
        }
        buf.push(op(car(next.clone())));
        next = mcdr(next);
    }
    buf
}

#[repr(C)]
#[derive(Clone)]
pub struct Pair {
    pub car: Box<Atom>,
    pub cdr: Box<Atom>,
}

impl Pair {
    pub fn new(car: Box<Atom>, cdr: Box<Atom>) -> Self {
        Self { car, cdr }
    }

    pub fn new_zeroed() -> Self {
        Self {
            car: Atom::NULL.boxed(),
            cdr: Atom::NULL.boxed(),
        }
    }
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "( {:?} . {:?})", &*self.car, &*self.cdr)
    }
}
impl Display for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "( {} . {})", &*self.car, &*self.cdr)
    }
}
