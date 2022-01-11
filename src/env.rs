use fnv::FnvHashMap;
use once_cell::unsync::Lazy;
use slotmap::{DefaultKey, SlotMap};
use somok::{Leaksome, Somok};
use std::ops::{Index, IndexMut};

use crate::{
    function::Func,
    libcl,
    value::{intern, Atom, SymId},
};

pub type EnvId = DefaultKey;

static mut ENVS: Lazy<SlotMap<EnvId, Env>> = Lazy::new(SlotMap::new);

#[derive(Debug)]
pub struct Env {
    parent: Option<EnvId>,
    inner: FnvHashMap<SymId, Atom>,
}

impl Env {
    pub fn insert(&mut self, sym: SymId, atom: Atom) {
        self.inner.insert(sym, atom);
    }

    pub fn try_get(&self, index: SymId) -> Option<&Atom> {
        self.inner.get(&index).or_else(|| {
            if let Some(parent) = self.parent {
                unsafe { ENVS.get(parent).expect("Invalid parent env reference") }.try_get(index)
            } else {
                None
            }
        })
    }

    pub fn try_get_mut(&mut self, index: SymId) -> Option<&mut Atom> {
        self.inner.get_mut(&index).or_else(|| {
            if let Some(parent) = self.parent {
                unsafe { ENVS.get_mut(parent).expect("Invalid parent env reference") }
                    .try_get_mut(index)
            } else {
                None
            }
        })
    }
}

impl Index<SymId> for Env {
    type Output = Atom;

    fn index(&self, index: SymId) -> &Self::Output {
        self.inner.get(&index).unwrap_or_else(|| {
            if let Some(parent) = self.parent {
                &unsafe { ENVS.get(parent) }.expect("Invalid parent env reference")[index]
            } else {
                panic!("Symbol not found")
            }
        })
    }
}
impl IndexMut<SymId> for Env {
    fn index_mut(&mut self, index: SymId) -> &mut Self::Output {
        self.inner.get_mut(&index).unwrap_or_else(|| {
            if let Some(parent) = self.parent {
                &mut unsafe { ENVS.get_mut(parent) }.expect("Invalid parent env reference")[index]
            } else {
                panic!("Symbol not found")
            }
        })
    }
}

pub fn setup() -> EnvId {
    let mut env = Env {
        parent: None,
        inner: Default::default(),
    };
    env.insert(intern("nil"), Atom::Null);
    env.insert(intern("t"), Atom::Int(1));
    env.insert(
        intern("+"),
        Atom::Func(Func::from_fn(libcl::add).boxed().leak()),
    );
    env.insert(
        intern("-"),
        Atom::Func(Func::from_fn(libcl::sub).boxed().leak()),
    );
    unsafe { ENVS.insert(env) }
}

pub fn get_env(env: EnvId) -> &'static mut Env {
    unsafe { ENVS.get_mut(env).expect("Invalid parent env reference") }
}

pub fn fork_env(env: EnvId) -> EnvId {
    let new = Env {
        parent: Some(env),
        inner: Default::default(),
    };
    unsafe { ENVS.insert(new) }
}

pub fn unfork_env(env: EnvId) {
    unsafe {
        ENVS.remove(env);
    }
}
