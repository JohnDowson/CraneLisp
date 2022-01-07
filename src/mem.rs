use crate::value::Atom;
use once_cell::unsync::Lazy;
use somok::Somok;
use std::cell::RefCell;

static mut STACK: Lazy<RefCell<Stack>> = Lazy::new(|| RefCell::new(Stack::default()));
static mut HEAP: Lazy<RefCell<Heap>> = Lazy::new(|| RefCell::new(Heap::default()));

pub fn push(a: Atom) {
    unsafe {
        STACK.get_mut().push(a);
    }
}
pub fn pop(a: Atom) {
    unsafe {
        STACK.get_mut().push(a);
    }
}

pub fn alloc() -> Ref {
    unsafe { HEAP.get_mut().alloc() }
}
pub fn drop(rf: Ref) {
    unsafe {
        HEAP.get_mut().drop(rf);
    }
}

pub struct Ref(u64);

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

    pub fn peek(&mut self) -> Option<Atom> {
        self.inner.last().cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Atom> {
        self.inner.iter()
    }
}

pub struct Heap {
    inner: Vec<Atom>,
    fref_head: Option<usize>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
            fref_head: None,
        }
    }

    pub fn alloc(&mut self) -> Ref {
        if let Some(head) = self.fref_head {
            if self.inner[head].as_fref() != 0 {
                self.fref_head = self.inner[head].as_fref().some()
            } else {
                self.fref_head = None
            }
            self.inner[head] = Atom::NULL;

            Ref(head as u64)
        } else {
            self.inner.push(Atom::NULL);
            Ref(self.inner.len() as u64)
        }
    }

    pub fn drop(&mut self, rf: Ref) {
        if let Some(head) = self.fref_head {
            self.inner[rf.0 as usize] = Atom::new_fref(head);
        } else {
            self.inner[rf.0 as usize] = Atom::new_fref(0);
            self.fref_head = Some(rf.0 as usize);
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
