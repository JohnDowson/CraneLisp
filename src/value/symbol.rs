use crate::FnvIndexSet;
use once_cell::unsync::Lazy;
use smol_str::SmolStr;
use std::{fmt::Debug, ops::Deref};

static mut INTERNER: Lazy<FnvIndexSet<SmolStr>> = Lazy::new(FnvIndexSet::default);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymId(pub usize);

impl Debug for SymId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.deref())
    }
}

impl SymId {
    pub fn inner(&self) -> usize {
        self.0
    }
}

impl Deref for SymId {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        retrieve(self)
    }
}

pub fn retrieve<'a>(sym: &SymId) -> &'a SmolStr {
    unsafe { &INTERNER[sym.0] }
}

pub fn intern(sym: impl Into<SmolStr>) -> SymId {
    let (id, _) = unsafe { INTERNER.insert_full(sym.into()) };
    SymId(id)
}
