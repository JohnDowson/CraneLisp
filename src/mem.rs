use crate::value::{scar, scdr, Atom};
use once_cell::unsync::Lazy;
use somok::Somok;
use std::cell::RefCell;

static mut STACK: Lazy<RefCell<Stack>> = Lazy::new(|| RefCell::new(Stack::default()));
static mut HEAP: Lazy<RefCell<Heap>> = Lazy::new(|| RefCell::new(Heap::default()));

pub fn debug() {
    unsafe {
        println!("{:?}\n{:?}", STACK.borrow(), HEAP.borrow());
    }
}

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

pub fn alloc(atom: Atom) -> Ref {
    unsafe { HEAP.get_mut().alloc(atom) }
}
pub fn drop(rf: Ref) {
    unsafe {
        HEAP.get_mut().drop(rf);
    }
}

pub fn collect() {
    for atom in unsafe { STACK.borrow() }.iter() {
        mark(atom)
    }
}

fn mark(atom: &Atom) {
    match atom.tag {
        crate::value::Tag::Pair => {
            mark(scar(atom));
            mark(scdr(atom));
        }
        crate::value::Tag::Return => unsafe {
            mark(atom.value.Return.as_ref());
        },
        _ => (),
    }
}
#[derive(Clone, Debug, Eq, PartialEq)]
#[repr(C)]
pub struct Ref(usize);

impl Ref {
    pub fn deref(self) -> Atom {
        unsafe { HEAP.borrow().get(self) }
    }

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

    pub fn peek(&mut self) -> Option<Atom> {
        self.inner.last().cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Atom> {
        self.inner.iter()
    }
}

#[derive(Debug)]
pub struct Heap {
    inner: Vec<(Atom, bool)>,
    fref_head: Option<usize>,
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
            if self.inner[head].0.as_fref().as_usize() != 0 {
                self.fref_head = self.inner[head].0.as_fref().as_usize().some()
            } else {
                self.fref_head = None
            }
            self.inner[head] = (atom, false);

            Ref(head)
        } else {
            self.inner.push((atom, false));
            Ref(self.inner.len() - 1)
        }
    }

    pub fn drop(&mut self, rf: Ref) {
        if let Some(head) = self.fref_head {
            self.inner[rf.0 as usize] = (Atom::new_fref(head), false);
        } else {
            self.inner[rf.0 as usize] = (Atom::new_fref(0), false);
            self.fref_head = Some(rf.0 as usize);
        }
    }

    pub fn get(&self, rf: Ref) -> Atom {
        self.inner
            .get(rf.as_usize())
            .cloned()
            .expect("Invalid ref")
            .0
    }

    pub fn mark(&mut self, rf: Ref) {
        self.inner.get_mut(rf.as_usize()).expect("Invalid ref").1 = true
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
