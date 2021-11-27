use crate::function::Func;
use smol_str::SmolStr;
use somok::{Leaksome, Somok};
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};
mod cl_string;
pub use cl_string::CLString;
pub use cl_string::CLVector;
mod pair;
pub use pair::*;

#[derive(Clone)]
pub struct Symbol {
    pub name: SmolStr,
    pub val: *mut Atom,
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Symbol")
            .field("name", &self.name)
            .field("val", unsafe { &*self.val })
            .finish()
    }
}

impl Eq for Symbol {}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl Symbol {
    pub fn new(name: impl Into<SmolStr>) -> Self {
        Self {
            name: name.into(),
            val: Atom::NULL.boxed().leak(),
        }
    }
    pub fn new_with_atom(name: impl Into<SmolStr>, atom: Atom) -> Self {
        Self {
            name: name.into(),
            val: atom.boxed().leak(),
        }
    }
}
impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &*self.name
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
        tag: Tag::Int,
        value: 1,
    };
    pub const FALSE: Self = Self::NULL;

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
    pub fn new_pair(value: &mut Pair) -> Self {
        Self {
            tag: Tag::Pair,
            value: value as *mut _ as u64,
        }
    }
    pub fn new_ptr<T>(value: &mut T) -> Self {
        Self {
            tag: Tag::Ptr,
            value: value as *mut _ as u64,
        }
    }
    pub fn new_func(value: Func) -> Self {
        Self {
            tag: Tag::Func,
            value: Box::leak(Box::new(value)) as *mut _ as u64,
        }
    }
    pub fn new_bool(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::NULL
        }
    }
    pub fn new_string(value: CLString) -> Self {
        Self {
            tag: Tag::String,
            value: value.as_ptr() as u64,
        }
    }
    pub fn new_symbol(value: &Symbol) -> Self {
        Self {
            tag: Tag::Symbol,
            value: value as *const _ as u64,
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
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
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
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
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
            Tag::String => todo!(),
            Tag::Return => self.value as *mut T,
            Tag::Symbol => todo!(),
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
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
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
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
        }
    }
    pub fn as_bool(&self) -> bool {
        match self.tag {
            Tag::Null => false,
            Tag::Int => true,
            Tag::Float => true,
            Tag::Ptr => true,
            Tag::Pair => true,
            Tag::Func => true,
            Tag::String => true,
            Tag::Return => true,
            Tag::Symbol => self.as_string() == "t",
        }
    }
    pub fn as_string(&self) -> &str {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to String"),
            Tag::Int => panic!("Can't cast Int to String"),
            Tag::Float => panic!("Can't cast Float to String"),
            Tag::Ptr => panic!("Can't cast Ptr to String"),
            Tag::Pair => panic!("Can't cast Pair to String"),
            Tag::Func => panic!("Can't cast Func to String"),
            Tag::String => unsafe { &*(self.value as *mut CLString) },
            Tag::Return => panic!("Can't cast Return to String"),
            Tag::Symbol => self.as_symbol(),
        }
    }
    pub fn as_symbol(&self) -> &Symbol {
        match self.tag {
            Tag::Null => panic!("Can't cast Null to String"),
            Tag::Int => panic!("Can't cast Int to String"),
            Tag::Float => panic!("Can't cast Float to String"),
            Tag::Ptr => panic!("Can't cast Ptr to String"),
            Tag::Pair => panic!("Can't cast Pair to String"),
            Tag::Func => panic!("Can't cast Func to String"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => unsafe { &*(self.value as *mut Symbol) },
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
            Tag::String => write!(f, "{:?}", self.as_string()),
            Tag::Return => write!(f, "{:?}", unsafe { *self.as_ptr::<Atom>() }),
            Tag::Symbol => write!(f, "{:?}", self.as_symbol()),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag {
            Tag::Null => write!(f, "Null"),
            Tag::Int => write!(f, "{}", self.as_int()),
            Tag::Float => write!(f, "{}", self.as_float()),
            Tag::Ptr => write!(f, "{}", unsafe { &*self.as_ptr::<Atom>() }),
            Tag::Pair => write!(f, "{}", unsafe { &*self.as_pair() }),
            Tag::Func => write!(f, "Function"),
            Tag::String => write!(f, "{}", self.as_string()),
            Tag::Return => write!(f, "{}", unsafe { &*self.as_ptr::<Atom>() }),
            Tag::Symbol => write!(f, "{}", self.as_string()),
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
    Symbol = 6,
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
            Self::String => write!(f, "String"),
            Self::Return => write!(f, "Return"),
            Tag::Symbol => write!(f, "Symbol"),
        }
    }
}
