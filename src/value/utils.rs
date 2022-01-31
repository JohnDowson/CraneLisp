use super::{intern, Atom};
use crate::mem;

pub fn map_to_vec<T>(mut atom: mem::Ref, mut op: impl FnMut(&Atom) -> T) -> Vec<T> {
    let mut res = vec![];
    while let Some(p) = atom.as_pair() {
        res.push(op(&*p.car));
        atom = p.cdr.clone();
    }
    res
}

pub fn length(atom: mem::Ref) -> usize {
    let mut count = 0;
    let mut next = atom;
    while let Some(pair) = next.as_pair() {
        count += 1;
        next = pair.cdr.clone();
    }
    count
}

pub fn nth(mut val: mem::Ref, mut n: isize) -> mem::Ref {
    let mut res = null!();
    while let Some(p) = val.as_pair() {
        res = p.car.clone();
        val = p.cdr.clone();
        n -= 1;
        if n < 0 {
            break;
        }
    }
    res
}

pub fn restn(mut val: mem::Ref, mut n: isize) -> mem::Ref {
    let mut res = null!();
    while let Some(p) = val.as_pair() {
        res = p.cdr.clone();
        val = p.cdr.clone();
        n -= 1;
        if n < 0 {
            break;
        }
    }
    res
}

pub fn quoted(val: mem::Ref) -> mem::Ref {
    mem::alloc(Atom::pair_alloc(
        Atom::Symbol(intern("quote")),
        Atom::pair(val, null!()),
    ))
}
pub fn quasi(val: mem::Ref) -> mem::Ref {
    mem::alloc(Atom::pair_alloc(
        Atom::Symbol(intern("quasi")),
        Atom::pair(val, null!()),
    ))
}
pub fn paste(val: mem::Ref) -> mem::Ref {
    mem::alloc(Atom::pair_alloc(
        Atom::Symbol(intern("paste")),
        Atom::pair(val, null!()),
    ))
}
