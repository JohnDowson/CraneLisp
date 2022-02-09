use fnv::FnvHashMap;
use once_cell::unsync::Lazy;
use slotmap::SlotMap;
use std::ops::{Index, IndexMut};

use crate::{mem, value::SymId, EnvId};

static mut ENVS: Lazy<SlotMap<EnvId, Env>> = Lazy::new(SlotMap::new);

pub fn setup() -> EnvId {
    unsafe {
        ENVS.clear();
        ENVS.insert(Env::default())
    }
}

#[derive(Debug, Default)]
pub struct Env {
    parent: Option<EnvId>,
    inner: FnvHashMap<SymId, mem::Ref>,
}

impl Env {
    pub fn insert(&mut self, sym: SymId, atom: mem::Ref) {
        self.inner.insert(sym, atom);
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<SymId, mem::Ref> {
        self.inner.iter()
    }

    pub fn insert_toplevel(&self, sym: SymId, atom: mem::Ref) {
        let mut parent = self.parent;
        let mut last = self.parent;
        while let Some(p) = parent {
            last = parent;
            parent = get_env(p).parent;
        }
        get_env(last.unwrap()).insert(sym, atom);
    }

    pub fn try_get(&self, index: SymId) -> Option<&mem::Ref> {
        self.inner.get(&index).or_else(|| {
            if let Some(parent) = self.parent {
                unsafe { ENVS.get(parent).expect("Invalid parent env reference") }.try_get(index)
            } else {
                None
            }
        })
    }

    pub fn try_get_mut(&mut self, index: SymId) -> Option<&mut mem::Ref> {
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
    type Output = mem::Ref;

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

pub fn mark(env: EnvId) {
    let env = get_env(env);
    for (_, aref) in env.inner.iter() {
        mem::mark(aref.clone());
    }
    if let Some(env) = env.parent {
        mark(env)
    }
}

pub fn unfork_env(env: EnvId) -> Option<EnvId> {
    let parent = get_env(env).parent;
    unsafe {
        ENVS.remove(env);
    }
    parent
}
