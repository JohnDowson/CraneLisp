use std::cell::{RefCell, RefMut};

use super::{asm::Ins, atom::Atom};
use crate::SymId;
use fnv::FnvHashMap;

pub type ConstTable = Vec<Atom>;

#[derive(Debug, Clone, Copy)]
pub enum FnProto {
    Func(&'static RuntimeFn),
    Closure(&'static RuntimeClosure),
}

impl FnProto {
    pub fn arity(&self) -> i16 {
        match self {
            FnProto::Func(f) => f.arity,
            FnProto::Closure(c) => c.fun.arity,
        }
    }
    pub fn assembly(&self) -> &'static [u8] {
        match self {
            FnProto::Func(f) => f.assembly,
            FnProto::Closure(c) => c.fun.assembly,
        }
    }
    pub fn locals(&self) -> &'static FnvHashMap<SymId, Local> {
        match self {
            FnProto::Func(f) => &f.locals,
            FnProto::Closure(c) => &c.fun.locals,
        }
    }
    pub fn upvalues_unresolved(&self) -> &'static [Upvalue] {
        match self {
            FnProto::Func(f) => &f.upvalues,
            FnProto::Closure(c) => &c.fun.upvalues,
        }
    }
    pub fn const_table(&self) -> &'static [Atom] {
        match self {
            FnProto::Func(f) => &f.const_table,
            FnProto::Closure(c) => &c.fun.const_table,
        }
    }
    pub fn fun(&self) -> &'static RuntimeFn {
        match self {
            FnProto::Func(f) => f,
            FnProto::Closure(c) => c.fun,
        }
    }
    pub fn upvalues(&mut self) -> RefMut<Vec<Atom>> {
        match self {
            FnProto::Func(_) => panic!("Tried to get upvalues of a function"),
            FnProto::Closure(c) => c.upvalues.borrow_mut(),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct RuntimeFn {
    pub arity: i16,
    pub assembly: &'static [u8],
    pub locals: FnvHashMap<SymId, Local>,
    pub upvalues: Vec<Upvalue>,
    pub const_table: ConstTable,
}

#[derive(PartialEq, Clone, Debug)]
pub struct RuntimeClosure {
    pub fun: &'static RuntimeFn,
    pub upvalues: RefCell<Vec<Atom>>,
}

#[derive(Clone, Copy)]
pub struct NativeFn {
    arity: i16,
    fun: fn(&[Atom]) -> Atom,
    name: &'static str,
}

impl NativeFn {
    pub fn new(arity: i16, fun: fn(&[Atom]) -> Atom, name: &'static str) -> Self {
        Self { arity, fun, name }
    }
    pub fn call(&self, a: &[Atom]) -> Atom {
        (self.fun)(a)
    }
}

impl std::fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFn")
            .field("arity", &self.arity)
            .field("name", &self.name)
            .finish()
    }
}

impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && (self.fun as usize) == (other.fun as usize)
    }
}

impl std::fmt::Debug for RuntimeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RuntimeFn")
            .field("locals", &self.locals)
            .field("const_table", &self.const_table)
            .finish()
    }
}
#[derive(PartialEq, Debug, Clone)]
pub struct Func {
    pub arity: i16,
    pub assembly: Vec<Ins>,
    pub id: usize,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
    pub bp: usize,
    pub sp: usize,
    pub const_table: ConstTable,
    pub locals: FnvHashMap<SymId, Local>,
    pub upvalues: Vec<Upvalue>,
    pub loops: Vec<Vec<ReturnToPatch>>,
    pub closure: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ReturnToPatch {
    pub return_loc: isize,
    pub ins_loc: isize,
}

impl ReturnToPatch {
    pub fn new(return_loc: isize, ins_loc: isize) -> Self {
        Self {
            return_loc,
            ins_loc,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Local {
    pub id: usize,
    pub captured: bool,
}

impl Local {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            captured: false,
        }
    }
}
#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Upvalue {
    pub id: usize,
    pub local: bool,
}

impl Upvalue {
    pub fn new(id: usize, local: bool) -> Self {
        Self { id, local }
    }
}
