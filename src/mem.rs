use std::ops::Deref;

use crate::value::{Atom, Pair};
use once_cell::unsync::Lazy;
use somok::Somok;

static mut HEAP: Lazy<Heap> = Lazy::new(Heap::default);

pub fn debug() {
    unsafe {
        println!("{:?}", &HEAP.inner);
    }
}

pub fn alloc(atom: Atom) -> Ref {
    unsafe { HEAP.alloc(atom) }
}
pub fn drop(rf: Ref) {
    unsafe {
        HEAP.drop(rf);
    }
}

pub fn mark(aref: Ref) {
    unsafe {
        HEAP.mark(aref);
    }
}

pub fn collect() {
    for (i, (a, marker)) in unsafe { HEAP.inner.iter_mut().enumerate() } {
        if !*marker && !a.is_fref() {
            drop(Ref(i))
        }
        *marker = false;
    }
}

#[derive(Clone, Eq, PartialEq)]
#[repr(C)]
pub struct Ref(usize);

impl std::fmt::Debug for Ref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{:?}:{:?}", self.0, self.deref())
    }
}

impl std::ops::Deref for Ref {
    type Target = Atom;

    fn deref(&self) -> &Self::Target {
        unsafe { HEAP.get(self) }
    }
}

impl std::ops::DerefMut for Ref {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { HEAP.get_mut(self) }
    }
}

impl Ref {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct Stack {
    inner: Vec<Atom>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            inner: Vec::with_capacity(1024),
        }
    }

    pub fn push(&mut self, atom: Atom) {
        self.inner.push(atom);
    }

    pub fn pop(&mut self) -> Option<Atom> {
        self.inner.pop()
    }

    pub fn forget(&mut self) {
        self.inner.pop();
    }

    pub fn peek(&mut self) -> Option<&Atom> {
        self.inner.last()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Atom> {
        self.inner.iter()
    }
}

#[derive(Clone)]
pub struct Heap {
    inner: Vec<(Atom, bool)>,
    fref_head: Option<usize>,
}

impl std::fmt::Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heap")
            .field("inner", &self.inner.iter().enumerate().collect::<Vec<_>>())
            .field("fref_head", &self.fref_head)
            .finish()
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            fref_head: None,
        }
    }

    pub fn alloc(&mut self, atom: Atom) -> Ref {
        if let Some(head) = self.fref_head {
            self.fref_head = self.inner[head].0.as_fref().unwrap();

            self.inner[head] = (atom, false);

            Ref(head)
        } else {
            self.inner.push((atom, false));
            Ref(self.inner.len() - 1)
        }
    }

    pub fn drop(&mut self, aref: Ref) {
        if let Some(head) = self.fref_head {
            self.inner[aref.0] = (Atom::FRef(head.some()), false);
            self.fref_head = Some(aref.0);
        } else {
            self.inner[aref.0] = (Atom::FRef(None), false);
            self.fref_head = Some(aref.0);
        }
    }

    pub fn get(&self, rf: &Ref) -> &Atom {
        &self.inner.get(rf.as_usize()).expect("Invalid ref").0
    }

    pub fn get_mut(&mut self, rf: &mut Ref) -> &mut Atom {
        &mut self.inner.get_mut(rf.as_usize()).expect("Invalid ref").0
    }

    pub fn mark(&mut self, rf: Ref) {
        let (atom, marker) = self.inner.get_mut(rf.as_usize()).expect("Invalid ref");
        if *marker {
            return;
        }
        *marker = true;
        match atom {
            Atom::Pair(Pair { car, cdr }) => {
                let (car, cdr) = (car.clone(), cdr.clone());
                self.mark(car);
                self.mark(cdr)
            }
            Atom::Ptr(ptr) => {
                let ptr = ptr.clone();
                self.mark(ptr)
            }
            _ => (),
        }
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}
