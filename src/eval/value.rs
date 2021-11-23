use crate::function::Func;
use somok::Somok;
use std::{
    ffi::{CStr, CString},
    fmt::{Debug, Display},
};

pub fn cons(v1: Value, v2: Value) -> Value {
    let tail = Box::leak(Box::new(Pair {
        value: v2,
        next: Value::NULL,
    }));
    let head = Pair {
        value: v1,
        next: Value {
            tag: Tag::Ptr,
            value: tail as *mut _ as _,
        },
    };

    Value::new_pair(Box::leak(Box::new(head)))
}
pub fn head(v: Value) -> Value {
    let value = v.as_pair().value;
    Value::new_pair(Box::leak(Box::new(Pair {
        value,
        next: Value::NULL,
    })))
}
pub fn tail(v: Value) -> Value {
    v.as_pair().next
}
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Pair {
    pub value: Value,
    pub next: Value,
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.value)
            .field(&self.next)
            .finish()
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Value {
    pub tag: Tag,
    value: u64,
}
impl Value {
    pub const NULL: Self = Self {
        tag: Tag::Null,
        value: 0,
    };
    pub const TRUE: Self = Self {
        tag: Tag::Null,
        value: 0,
    };
    pub const FALSE: Self = Self {
        tag: Tag::Null,
        value: 0,
    };

    pub fn new_int(value: i64) -> Self {
        Self {
            tag: Tag::Int,
            value: value as u64,
        }
    }
    pub fn new_float(value: f64) -> Self {
        Self {
            tag: Tag::Float,
            value: value.to_bits(),
        }
    }
    pub fn new_pair(value: *mut Pair) -> Self {
        Self {
            tag: Tag::Pair,
            value: value as u64,
        }
    }
    pub fn new_ptr<T>(value: *mut T) -> Self {
        Self {
            tag: Tag::Ptr,
            value: value as u64,
        }
    }
    pub fn new_func(value: Func) -> Self {
        Self {
            tag: Tag::Func,
            value: Box::leak(Box::new(value)) as *mut _ as u64,
        }
    }
    pub fn new_bool(value: bool) -> Self {
        Self {
            tag: Tag::Bool,
            value: value as u64,
        }
    }
    pub fn new_string(value: impl Into<Vec<u8>>) -> Self {
        let cstring = CString::new(value).unwrap().into_raw();
        Self {
            tag: Tag::String,
            value: cstring as u64,
        }
    }

    pub fn as_int(&self) -> i64 {
        match self.tag {
            Tag::Null => self.value as _,
            Tag::Int => self.value as _,
            Tag::Float => panic!("Can't cast Float to Int"),
            Tag::Ptr => panic!("Can't cast Ptr to Int"),
            Tag::Pair => panic!("Can't cast Pair to Int"),
            Tag::Func => panic!("Can't cast Func to Int"),
            Tag::Bool => panic!("Can't cast Bool to Int"),
            Tag::String => todo!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self.tag {
            Tag::Null => f64::from_bits(self.value),
            Tag::Int => panic!("Can't cast Int to Float"),
            Tag::Float => f64::from_bits(self.value),
            Tag::Ptr => panic!("Can't cast Ptr to Float"),
            Tag::Pair => panic!("Can't cast Pair to Float"),
            Tag::Func => panic!("Can't cast Func to Float"),
            Tag::Bool => panic!("Can't cast Bool to Float"),
            Tag::String => todo!(),
        }
    }

    pub fn as_ptr<T>(&self) -> *mut T {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to Ptr"),
            Tag::Int => panic!("Can't cast Int to Ptr"),
            Tag::Float => panic!("Can't cast Float to Ptr"),
            Tag::Ptr => self.value as *mut T,
            Tag::Pair => panic!("Can't cast Pair to Ptr"),
            Tag::Func => panic!("Can't cast Func to Ptr"),
            Tag::Bool => panic!("Can't cast Bool to Ptr"),
            Tag::String => todo!(),
        }
    }

    pub fn as_pair(&self) -> &'static mut Pair {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to Pair"),
            Tag::Int => panic!("Can't cast Int to Pair"),
            Tag::Float => panic!("Can't cast Float to Pair"),
            Tag::Ptr => panic!("Can't cast Ptr to Pair"),
            Tag::Pair => unsafe { std::mem::transmute(self.value) },
            Tag::Func => panic!("Can't cast Func to Pair"),
            Tag::Bool => panic!("Can't cast Bool to Pair"),
            Tag::String => todo!(),
        }
    }

    pub fn as_func(&self) -> *mut Func {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to Func"),
            Tag::Int => panic!("Can't cast Int to Func"),
            Tag::Float => panic!("Can't cast Float to Func"),
            Tag::Ptr => panic!("Can't cast Ptr to Func"),
            Tag::Pair => panic!("Can't cast Pair to Func"),
            Tag::Func => unsafe { std::mem::transmute(self.value) },
            Tag::Bool => panic!("Can't cast Bool to Func"),
            Tag::String => todo!(),
        }
    }
    pub fn as_bool(&self) -> bool {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to Bool"),
            Tag::Int => panic!("Can't cast Int to Bool"),
            Tag::Float => panic!("Can't cast Float to Bool"),
            Tag::Ptr => panic!("Can't cast Ptr to Bool"),
            Tag::Pair => panic!("Can't cast Pair to Bool"),
            Tag::Func => panic!("Can't cast Func to Bool"),
            Tag::Bool => self.value != 0,
            Tag::String => todo!(),
        }
    }
    pub fn as_string(&self) -> &CStr {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to String"),
            Tag::Int => panic!("Can't cast Int to String"),
            Tag::Float => panic!("Can't cast Float to String"),
            Tag::Ptr => panic!("Can't cast Ptr to String"),
            Tag::Pair => panic!("Can't cast Pair to String"),
            Tag::Func => panic!("Can't cast Func to String"),
            Tag::Bool => panic!("Can't cast Bool to String"),
            Tag::String => unsafe { CStr::from_ptr(self.value as *mut i8) },
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ", self.tag)?;
        match self.tag {
            Tag::Null => ().okay(),
            Tag::Int => write!(f, "{:?}", self.as_int()),
            Tag::Float => write!(f, "{:?}", self.as_float()),
            Tag::Ptr => write!(f, "{:?}", unsafe { *self.as_ptr::<Value>() }),
            Tag::Pair => write!(f, "{:?}", self.as_pair()),
            Tag::Func => ().okay(),
            Tag::Bool => write!(f, "{:?}", self.as_bool()),
            Tag::String => write!(f, "{:?}", self.as_string()),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag {
            Tag::Null => write!(f, "Null"),
            Tag::Int => write!(f, "{:?}", self.as_int()),
            Tag::Float => write!(f, "{:?}", self.as_float()),
            Tag::Ptr => write!(f, "{:?}", unsafe { *self.as_ptr::<Value>() }),
            Tag::Pair => write!(f, "{:?}", self.as_pair()),
            Tag::Func => write!(f, "Function"),
            Tag::Bool => write!(f, "{:?}", self.as_bool()),
            Tag::String => write!(f, "{:?}", self.as_string()),
        }
    }
}

#[repr(i64)]
#[derive(Clone, Copy, Debug)]
pub enum Tag {
    Null = 0,
    Int = 1,
    Float = 2,
    Ptr = 3,
    Pair = 4,
    Func = 5,
    Bool = 6,
    String = 7,
}
