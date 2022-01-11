use std::ops::Deref;

use crate::{
    env::Env,
    mem,
    value::{Atom, ErrorCode},
};

pub fn add(atom: mem::Ref, _env: &mut Env) -> Atom {
    let a = atom.as_pair().unwrap().car.deref();
    if let Atom::Int(a) = a {
        let mut a = *a;
        let mut next = atom.as_pair().unwrap().cdr.deref();
        while let Some(pair) = next.as_pair() {
            match pair.car.deref() {
                Atom::Int(i) => a += i,
                Atom::Float(f) => a += *f as i64,
                _ => return Atom::Error(ErrorCode::Type),
            }
            next = pair.cdr.deref();
        }
        Atom::Int(a)
    } else if let Atom::Float(a) = a {
        let mut a = *a;
        let mut next = atom.as_pair().unwrap().cdr.deref();
        while let Some(pair) = next.as_pair() {
            match pair.car.deref() {
                Atom::Int(i) => a += *i as f64,
                Atom::Float(f) => a += f,
                _ => return Atom::Error(ErrorCode::Type),
            }
            next = pair.cdr.deref();
        }
        Atom::Float(a)
    } else {
        Atom::Error(ErrorCode::Type)
    }
}

pub fn sub(atom: mem::Ref, _env: &mut Env) -> Atom {
    let a = atom.as_pair().unwrap().car.deref();
    if let Atom::Int(a) = a {
        let mut a = *a;
        let mut next = atom.as_pair().unwrap().cdr.deref();
        while let Some(pair) = next.as_pair() {
            match pair.car.deref() {
                Atom::Int(i) => a -= i,
                Atom::Float(f) => a -= *f as i64,
                _ => return Atom::Error(ErrorCode::Type),
            }
            next = pair.cdr.deref();
        }
        Atom::Int(a)
    } else if let Atom::Float(a) = a {
        let mut a = *a;
        let mut next = atom.as_pair().unwrap().cdr.deref();
        while let Some(pair) = next.as_pair() {
            match pair.car.deref() {
                Atom::Int(i) => a -= *i as f64,
                Atom::Float(f) => a -= f,
                _ => return Atom::Error(ErrorCode::Type),
            }
            next = pair.cdr.deref();
        }
        Atom::Float(a)
    } else {
        Atom::Error(ErrorCode::Type)
    }
}
