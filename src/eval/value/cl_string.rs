use core::slice;
use std::{
    alloc::LayoutError,
    fmt::{Debug, Display},
    ops::Deref,
    str::FromStr,
};

use somok::Somok;
use thiserror::Error;

use super::{Atom, Tag};

#[derive(Error, Debug)]
pub enum CLStringError {
    #[error("{0}")]
    Layout(#[from] LayoutError),
    #[error("CLString cannot hold more than u32::MAX bytes")]
    Capacity,
}

#[repr(C)]
pub struct CLString {
    size: u32,
    capacity: u32,
    inner: *mut u8,
}

impl Clone for CLString {
    fn clone(&self) -> Self {
        Self::from_str(self.as_str()).unwrap()
    }
}

impl CLString {
    pub fn new() -> Self {
        Self {
            size: 0,
            capacity: 0,
            inner: std::ptr::null_mut(),
        }
    }
    pub fn from_string(s: String) -> Self {
        let s = s.into_bytes();
        Self::from_buffer(s)
    }
    fn from_buffer(s: Vec<u8>) -> Self {
        let (size, capacity) = {
            if s.capacity() > u32::MAX as usize {
                panic!("CLString over capacity")
            }
            (s.len() as u32, s.capacity() as u32)
        };
        let inner = s.leak().as_mut_ptr();
        Self {
            size,
            capacity,
            inner,
        }
    }
    pub fn len(&self) -> usize {
        self.size as usize
    }
    pub fn capacity(&self) -> usize {
        self.capacity as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_bytes()) }
    }
    pub fn into_string(self) -> String {
        self.as_str().into()
    }
    pub fn as_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.inner, self.len()) }
    }
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.inner, self.capacity()) }
    }
    pub fn replaced(&self, pat: &str, replacement: &str) -> Self {
        use kmp::kmp_find as find;
        if let Some(start) = find(pat.as_bytes(), self.as_bytes()) {
            let end = start + pat.len();
            let new_len = (self.len() - pat.len()) + replacement.len();
            // Realloc panics when the string is over capacity
            let mut new_buff = Vec::with_capacity(new_len);
            new_buff.extend_from_slice(&self.as_bytes()[..start]);
            new_buff.extend_from_slice(replacement.as_bytes());
            new_buff.extend_from_slice(&self.as_bytes()[end..]);
            Self::from_buffer(new_buff)
        } else {
            self.clone()
        }
    }
    pub fn replace(&mut self, pat: &str, replacement: &str) {
        *self = self.replaced(pat, replacement);
    }
    pub fn as_ptr(&self) -> *mut u8 {
        self.inner
    }
    pub fn format(&self, values: &[Atom]) -> Self {
        let mut replaced = self.clone();
        for value in values {
            let pat = match value.tag {
                Tag::Null => "%0",
                Tag::Int => "%d",
                Tag::Float => "%f",
                Tag::Ptr => "%p",
                Tag::Pair => "%t",
                Tag::Func => "%p",
                Tag::Symbol => "%y",
                Tag::String => "%s",
                Tag::Return => "%p",
            };
            let replacement = format!("{}", value);
            replaced = replaced.replaced(pat, &replacement)
        }
        replaced
    }
}
impl Deref for CLString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Drop for CLString {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.inner, self.len(), self.capacity());
        }
    }
}

impl Default for CLString {
    fn default() -> Self {
        Self::new()
    }
}

impl FromStr for CLString {
    type Err = CLStringError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use std::alloc::{alloc, Layout};
        let size = s.len();
        if size > u32::MAX as usize {
            return CLStringError::Capacity.error();
        }
        let (inner, capacity) = unsafe {
            let layout = Layout::from_size_align(size, 1)?;
            let capacity = layout.size();
            if capacity > u32::MAX as usize {
                return CLStringError::Capacity.error();
            }
            let buf = slice::from_raw_parts_mut(alloc(layout), s.len());
            buf.copy_from_slice(s.as_bytes());
            (buf.as_mut_ptr(), capacity as u32)
        };
        Self {
            size: size as u32,
            capacity,
            inner,
        }
        .okay()
    }
}

impl Display for CLString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
impl Debug for CLString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CLString")
            .field("size", &self.size)
            .field("capacity", &self.capacity)
            .field("str", &self.as_str())
            .finish()
    }
}
