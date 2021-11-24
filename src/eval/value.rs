use crate::function::Func;
use somok::{Leaksome, Somok};
use std::{
    ffi::{CStr, CString},
    fmt::{Debug, Display},
};

pub fn cons(v1: Atom, v2: Atom) -> Atom {
    let v1 = v1.boxed().leak();
    let v2 = v2.boxed().leak();
    let pair = Pair::new(v1, v2);

    Atom::new_pair(pair.boxed().leak())
}
pub fn head(v: &Atom) -> *mut Atom {
    unsafe { *v.as_pair() }.car
}
pub fn tail(v: &Atom) -> *mut Atom {
    unsafe { *v.as_pair() }.cdr
}
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Pair {
    pub car: *mut Atom,
    pub cdr: *mut Atom,
}

impl Pair {
    pub fn new(car: *mut Atom, cdr: *mut Atom) -> Self {
        Self { car, cdr }
    }
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { write!(f, "( {:?} . {:?})", &*self.car, &*self.cdr) }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Atom {
    pub tag: Tag,
    value: u64,
}
impl Atom {
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
    pub fn new_return(value: Atom) -> Self {
        let value = value.boxed().leak() as *mut Atom as u64;
        Self {
            tag: Tag::Return,
            value,
        }
    }

    pub fn as_int(&self) -> i64 {
        match self.tag {
            Tag::Null => 0,
            Tag::Int => self.value as _,
            Tag::Float => self.value as f64 as _,
            Tag::Ptr => self.value as _,
            Tag::Pair => panic!("Can't cast Pair to Int"),
            Tag::Func => panic!("Can't cast Func to Int"),
            Tag::Bool => self.value as _,
            Tag::String => todo!(),
            Tag::Return => todo!(),
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
            Tag::Return => todo!(),
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
            Tag::Return => self.value as *mut T,
        }
    }

    pub fn as_pair(&self) -> *mut Pair {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to Pair"),
            Tag::Int => panic!("Can't cast Int to Pair"),
            Tag::Float => panic!("Can't cast Float to Pair"),
            Tag::Ptr => panic!("Can't cast Ptr to Pair"),
            Tag::Pair => self.value as *mut _,
            Tag::Func => panic!("Can't cast Func to Pair"),
            Tag::Bool => panic!("Can't cast Bool to Pair"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
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
            Tag::Return => todo!(),
        }
    }
    pub fn as_bool(&self) -> bool {
        match self.tag {
            Tag::Null => false,
            Tag::Int => self.value != 0,
            Tag::Float => (self.value as f64) >= 0.0,
            Tag::Ptr => panic!("Can't cast Ptr to Bool"),
            Tag::Pair => panic!("Can't cast Pair to Bool"),
            Tag::Func => panic!("Can't cast Func to Bool"),
            Tag::Bool => self.value != 0,
            Tag::String => todo!(),
            Tag::Return => todo!(),
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
            Tag::Return => todo!(),
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ", self.tag)?;
        match self.tag {
            Tag::Null => ().okay(),
            Tag::Int => write!(f, "{:?}", self.as_int()),
            Tag::Float => write!(f, "{:?}", self.as_float()),
            Tag::Ptr => write!(f, "{:?}", unsafe { *self.as_ptr::<Atom>() }),
            Tag::Pair => write!(f, "{:?}", unsafe { &*self.as_pair() }),
            Tag::Func => write!(f, "{:?}", unsafe { *self.as_func() }),
            Tag::Bool => write!(f, "{:?}", self.as_bool()),
            Tag::String => write!(f, "{:?}", self.as_string()),
            Tag::Return => write!(f, "{:?}", unsafe { *self.as_ptr::<Atom>() }),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag {
            Tag::Null => write!(f, "Null"),
            Tag::Int => write!(f, "{:?}", self.as_int()),
            Tag::Float => write!(f, "{:?}", self.as_float()),
            Tag::Ptr => write!(f, "{:?}", unsafe { &*self.as_ptr::<Atom>() }),
            Tag::Pair => write!(f, "{:?}", unsafe { &*self.as_pair() }),
            Tag::Func => write!(f, "Function"),
            Tag::Bool => write!(f, "{:?}", self.as_bool()),
            Tag::String => write!(f, "{:?}", self.as_string()),
            Tag::Return => write!(f, "{:?}", unsafe { &*self.as_ptr::<Atom>() }),
        }
    }
}

#[repr(i64)]
#[derive(Clone, Copy)]
pub enum Tag {
    Null = 0,
    Int = 1,
    Float = 2,
    Ptr = 3,
    Pair = 4,
    Func = 5,
    Bool = 6,
    String = 7,
    Return = 8,
}

impl Debug for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Ptr => write!(f, "Ptr"),
            Self::Pair => write!(f, "Pair"),
            Self::Func => write!(f, "Func"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Return => write!(f, "Return"),
        }
    }
}
