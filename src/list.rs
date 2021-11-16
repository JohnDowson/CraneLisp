use std::fmt::Debug;

use somok::Somok;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct List {
    value: Option<f64>,
    next: Option<*mut List>,
}
impl List {
    /// # Safety
    /// Not safe at all
    pub fn push(mut self, v: f64) -> Self {
        let new = (Box::leak(Box::new(Self {
            value: v.some(),
            next: None,
        })) as *mut Self)
            .some();
        let mut previous = self.next;
        let mut next = self.next;
        while let Some(node) = next {
            previous = next;
            unsafe {
                next = (*node).next;
            }
        }
        unsafe {
            match previous {
                Some(previous) => (*previous).next = new,
                None => self.next = new,
            }
        }
        self
    }
    pub fn tail(&self) -> Option<Self> {
        unsafe { self.next.map(|f| *f) }
    }
    pub fn head(&self) -> Self {
        *self
    }
    pub fn new(v: f64) -> Self {
        Self {
            value: v.some(),
            next: None,
        }
    }
}

impl Debug for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        unsafe {
            let mut next = Some(std::mem::transmute::<&Self, *mut Self>(self));
            while let Some(node) = next {
                write!(f, ", {:?}", (*node).value)?;
                next = (*node).next
            }
        }
        write!(f, "]")
    }
}
