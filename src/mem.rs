use crate::value::Atom;
use once_cell::unsync::Lazy;

static mut STACK: Lazy<Stack> = Lazy::new(Stack::default);
static mut HEAP: Lazy<Heap> = Lazy::new(Heap::default);

pub fn debug() {
    unsafe {
        println!("{:?}\n{:?}", &STACK, &HEAP);
    }
}

pub fn push(a: Atom) {
    unsafe {
        STACK.push(a);
    }
}
pub fn pop(a: Atom) {
    unsafe {
        STACK.push(a);
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

pub fn collect() {
    for atom in unsafe { &STACK }.iter() {
        mark(atom)
    }
}

fn mark(_atom: &Atom) {}

#[derive(Clone, Debug, Eq, PartialEq)]
#[repr(C)]
pub struct Ref(usize);

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
            if self.inner[head].0.as_fref().unwrap() != 0 {
                self.fref_head = self.inner[head].0.as_fref()
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
            self.inner[rf.0 as usize] = (Atom::FRef(head), false);
        } else {
            self.inner[rf.0 as usize] = (Atom::FRef(0), false);
            self.fref_head = Some(rf.0 as usize);
        }
    }

    pub fn get(&self, rf: &Ref) -> &Atom {
        &self.inner.get(rf.as_usize()).expect("Invalid ref").0
    }

    pub fn get_mut(&mut self, rf: &Ref) -> &mut Atom {
        &mut self.inner.get_mut(rf.as_usize()).expect("Invalid ref").0
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
