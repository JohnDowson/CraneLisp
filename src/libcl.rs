use std::ops::Deref;

use crate::{
    env::Env,
    mem,
    value::{Atom, ErrorCode},
};

pub fn add(atom: mem::Ref, _env: &mut Env) -> mem::Ref {
    let a = atom.as_pair().unwrap().car.deref();
    let ret = if let Atom::Int(a) = a {
        let mut a = *a;
        let mut next = atom.as_pair().unwrap().cdr.deref();
        while let Some(pair) = next.as_pair() {
            match pair.car.deref() {
                Atom::Int(i) => a += i,
                Atom::Float(f) => a += *f as i64,
                _ => return mem::alloc(Atom::Error(ErrorCode::Type)),
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
                _ => return mem::alloc(Atom::Error(ErrorCode::Type)),
            }
            next = pair.cdr.deref();
        }
        Atom::Float(a)
    } else {
        Atom::Error(ErrorCode::Type)
    };
    mem::alloc(ret)
}

pub fn sub(atom: mem::Ref, _env: &mut Env) -> mem::Ref {
    let a = atom.as_pair().unwrap().car.deref();
    let ret = if let Atom::Int(a) = a {
        let mut a = *a;
        let mut next = atom.as_pair().unwrap().cdr.deref();
        while let Some(pair) = next.as_pair() {
            match pair.car.deref() {
                Atom::Int(i) => a -= i,
                Atom::Float(f) => a -= *f as i64,
                _ => return mem::alloc(Atom::Error(ErrorCode::Type)),
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
                _ => return mem::alloc(Atom::Error(ErrorCode::Type)),
            }
            next = pair.cdr.deref();
        }
        Atom::Float(a)
    } else {
        Atom::Error(ErrorCode::Type)
    };
    mem::alloc(ret)
}

pub fn car(atom: mem::Ref, _env: &mut Env) -> mem::Ref {
    if let Some(pair) = atom.as_pair() {
        pair.car.as_pair().unwrap().car.clone()
    } else {
        mem::alloc(Atom::Error(ErrorCode::Type))
    }
}

pub fn cdr(atom: mem::Ref, _env: &mut Env) -> mem::Ref {
    if let Some(pair) = atom.as_pair() {
        pair.car.as_pair().unwrap().cdr.clone()
    } else {
        mem::alloc(Atom::Error(ErrorCode::Type))
    }
}

pub fn list(_atom: mem::Ref, _env: &mut Env) -> mem::Ref {
    mem::alloc(Atom::pair_alloc(
        Atom::Int(1),
        Atom::pair_alloc(Atom::Int(2), Atom::pair(mem::alloc(Atom::Int(3)), null!())),
    ))
}
