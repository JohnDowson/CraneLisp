use fnv::FnvHashMap;

use crate::symbol::SymId;

use super::asm::Ins;

pub struct Closure {
    pub upvalues: Vec<Upvalue>,
    pub assembly: Vec<Ins>,
    pub scope: Scope,
    pub parent: Option<usize>,
    pub bp: usize,
    pub sp: usize,
}

#[derive(Debug)]
pub struct Scope {
    pub vars: FnvHashMap<SymId, usize>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: Default::default(),
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Upvalue {}
