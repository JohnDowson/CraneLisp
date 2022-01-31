use std::ops::{Index, IndexMut};

use crate::{mem, value::SymId, EnvId};
use fnv::FnvHashMap;
use slotmap::SlotMap;

#[derive(Debug)]
pub struct Env {
    parent: Option<EnvId>,
    inner: FnvHashMap<SymId, mem::Ref>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            parent: None,
            inner: Default::default(),
        }
    }

    pub fn parent<'s, 'p>(&'s self, store: &'p SlotMap<EnvId, Env>) -> Option<&'p Env> {
        if let Some(key) = self.parent {
            store.get(key)
        } else {
            None
        }
    }

    pub fn parent_mut<'s, 'p>(&'s self, store: &'p mut SlotMap<EnvId, Env>) -> Option<&'p mut Env> {
        if let Some(key) = self.parent {
            store.get_mut(key)
        } else {
            None
        }
    }

    pub fn insert(&mut self, sym: SymId, atom: mem::Ref) {
        self.inner.insert(sym, atom);
    }

    pub fn get(&self, index: SymId, store: &SlotMap<EnvId, Env>) -> Option<mem::Ref> {
        self.inner.get(&index).cloned().or_else(|| {
            if let Some(parent) = self.parent {
                store.get(parent).and_then(|e| e.get(index, store))
            } else {
                None
            }
        })
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<SymId> for Env {
    type Output = mem::Ref;

    fn index(&self, index: SymId) -> &Self::Output {
        self.inner.get(&index).expect("Symbol not found")
    }
}

impl IndexMut<SymId> for Env {
    fn index_mut(&mut self, index: SymId) -> &mut Self::Output {
        self.inner.get_mut(&index).expect("Symbol not found")
    }
}
