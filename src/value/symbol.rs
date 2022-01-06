use super::Atom;
use smol_str::SmolStr;
use somok::{Leaksome, Somok};
use std::{fmt::Debug, ops::Deref};

#[derive(Clone)]
pub struct Symbol {
    pub name: SmolStr,
    pub val: *mut Atom,
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("")
            .field("name", &self.name)
            .field("val", unsafe { &*self.val })
            .finish()
    }
}

impl Eq for Symbol {}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl Symbol {
    pub fn new(name: impl Into<SmolStr>) -> Self {
        Self {
            name: name.into(),
            val: Atom::NULL.boxed().leak(),
        }
    }
    pub fn new_with_atom(name: impl Into<SmolStr>, atom: Atom) -> Self {
        Self {
            name: name.into(),
            val: atom.boxed().leak(),
        }
    }
}
impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &*self.name
    }
}
