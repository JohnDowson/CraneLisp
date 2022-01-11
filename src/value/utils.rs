use std::ops::{Deref, DerefMut};

use crate::mem;

use super::Atom;

pub fn collect(atom: mem::Ref) -> Vec<Atom> {
    let mut atoms = Vec::new();
    let mut next = atom;
    while let Some(pair) = next.as_pair() {
        atoms.push(pair.car.deref().clone());
        next = pair.cdr.clone();
    }
    atoms
}

pub fn map_to_vec<T>(atom: mem::Ref, mut op: impl FnMut(&Atom) -> T) -> Vec<T> {
    let mut atoms = Vec::new();
    let mut next = atom;
    while let Some(pair) = next.as_pair() {
        atoms.push(op(pair.car.deref()));
        next = pair.cdr.clone();
    }
    atoms
}

pub fn map(atom: mem::Ref, mut op: impl FnMut(Atom) -> Atom) -> Atom {
    let mut next = atom.clone();
    while let Some(pair) = next.as_pair_mut() {
        *pair.car.deref_mut() = op(pair.car.deref().clone());
        next = pair.cdr.clone();
    }
    atom.deref().clone()
}
