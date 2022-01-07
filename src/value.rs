use crate::{function::Func, mem};
use somok::Somok;
use std::{
    fmt::{Debug, Display},
    fs::File,
    mem::ManuallyDrop,
    os::unix::prelude::RawFd,
};
mod symbol;
pub use symbol::*;
mod cl_string;
pub use cl_string::CLString;
pub use cl_string::CLVector;
mod pair;
pub use pair::*;
#[repr(C)]
pub struct Atom {
    pub tag: Tag,
    pub value: Value,
}

impl Clone for Atom {
    fn clone(&self) -> Self {
        unsafe {
            Self {
                tag: self.tag,
                value: match self.tag {
                    Tag::FRef => Value {
                        FRef: self.value.FRef.clone(),
                    },
                    Tag::Error => Value {
                        Error: self.value.Error,
                    },
                    Tag::Null => Value {
                        Null: self.value.Null,
                    },
                    Tag::Int => Value {
                        Int: self.value.Int,
                    },
                    Tag::Float => Value {
                        Float: self.value.Float,
                    },
                    Tag::Pair => Value {
                        Pair: self.value.Pair.clone(),
                    },
                    Tag::Func => Value {
                        Func: self.value.Func.clone(),
                    },
                    Tag::Symbol => Value {
                        Symbol: self.value.Symbol.clone(),
                    },
                    Tag::String => Value {
                        String: self.value.String.clone(),
                    },
                    Tag::Return => Value {
                        Return: self.value.Return.clone(),
                    },
                    Tag::Port => todo!(),
                },
            }
        }
    }
}

#[allow(non_snake_case)]
pub union Value {
    pub FRef: ManuallyDrop<mem::Ref>,
    pub Error: u64,
    pub Null: u8,
    pub Int: i64,
    pub Float: f64,
    pub Pair: ManuallyDrop<Box<Pair>>,
    pub Func: ManuallyDrop<Box<Func>>,
    pub Symbol: ManuallyDrop<Box<Symbol>>,
    pub String: ManuallyDrop<Box<CLString>>,
    pub Return: ManuallyDrop<Box<Atom>>,
    pub Port: ManuallyDrop<Box<File>>,
}

impl Atom {
    pub const NULL: Self = Self {
        tag: Tag::Null,
        value: Value { Null: 0 },
    };
    pub const TRUE: Self = Self {
        tag: Tag::Int,
        value: Value { Int: 1 },
    };
    pub const FALSE: Self = Self::NULL;

    pub fn new_int(value: i64) -> Self {
        Self {
            tag: Tag::Int,
            value: Value { Int: value },
        }
    }
    pub fn new_float(value: f64) -> Self {
        Self {
            tag: Tag::Float,
            value: Value { Float: value },
        }
    }
    pub fn new_pair(value: Box<Pair>) -> Self {
        Self {
            tag: Tag::Pair,
            value: Value {
                Pair: ManuallyDrop::new(value),
            },
        }
    }
    pub fn new_func(value: Func) -> Self {
        Self {
            tag: Tag::Func,
            value: Value {
                Func: ManuallyDrop::new(value.boxed()),
            },
        }
    }
    pub fn new_bool(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }
    pub fn new_string(value: CLString) -> Self {
        Self {
            tag: Tag::String,
            value: Value {
                String: ManuallyDrop::new(value.boxed()),
            },
        }
    }
    pub fn new_symbol(value: Box<Symbol>) -> Self {
        Self {
            tag: Tag::Symbol,
            value: Value {
                Symbol: ManuallyDrop::new(value),
            },
        }
    }
    pub fn new_return(value: Atom) -> Self {
        Self {
            tag: Tag::Return,
            value: Value {
                Return: ManuallyDrop::new(value.boxed()),
            },
        }
    }

    pub fn new_error(error_code: ErrorCode) -> Self {
        Self {
            tag: Tag::Error,
            value: Value {
                Error: error_code as u64,
            },
        }
    }

    pub fn new_port(_fd: RawFd) -> Self {
        todo!()
    }

    pub fn new_fref(fref: usize) -> Self {
        Self {
            tag: Tag::FRef,
            value: Value { Error: fref as u64 },
        }
    }
    pub fn as_fref(&self) -> mem::Ref {
        match self.tag {
            Tag::FRef => unsafe { (&*self.value.FRef).clone() },
            _ => todo!(),
        }
    }

    pub fn as_int(&self) -> i64 {
        unsafe {
            match self.tag {
                Tag::FRef => todo!(),
                Tag::Error => todo!(),
                Tag::Null => 0,
                Tag::Int => self.value.Int,
                Tag::Float => self.value.Float as _,
                Tag::Pair => panic!("Can't cast Pair to Int"),
                Tag::Func => panic!("Can't cast Func to Int"),
                Tag::String => todo!(),
                Tag::Return => todo!(),
                Tag::Symbol => todo!(),
                Tag::Port => todo!(),
            }
        }
    }

    pub fn as_float(&self) -> f64 {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to Float"),
            Tag::Int => panic!("Can't cast Int to Float"),
            Tag::Float => unsafe { self.value.Float },
            Tag::Pair => panic!("Can't cast Pair to Float"),
            Tag::Func => panic!("Can't cast Func to Float"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
            Tag::Port => todo!(),
        }
    }

    pub fn as_pair(&self) -> &Pair {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to Pair"),
            Tag::Int => panic!("Can't cast Int to Pair"),
            Tag::Float => panic!("Can't cast Float to Pair"),
            Tag::Pair => unsafe { self.value.Pair.as_ref() },
            Tag::Func => panic!("Can't cast Func to Pair"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
            Tag::Port => todo!(),
        }
    }

    pub fn as_pair_owned(self) -> Pair {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to Pair"),
            Tag::Int => panic!("Can't cast Int to Pair"),
            Tag::Float => panic!("Can't cast Float to Pair"),
            Tag::Pair => unsafe {
                Box::into_inner(ManuallyDrop::into_inner(self.value.Pair.clone()))
            },
            Tag::Func => panic!("Can't cast Func to Pair"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
            Tag::Port => todo!(),
        }
    }

    pub fn as_pair_mut(&mut self) -> &mut Pair {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to Pair"),
            Tag::Int => panic!("Can't cast Int to Pair"),
            Tag::Float => panic!("Can't cast Float to Pair"),
            Tag::Pair => unsafe { (*self.value.Pair).as_mut() },
            Tag::Func => panic!("Can't cast Func to Pair"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
            Tag::Port => todo!(),
        }
    }

    pub fn as_func(&self) -> &Func {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to Func"),
            Tag::Int => panic!("Can't cast Int to Func"),
            Tag::Float => panic!("Can't cast Float to Func"),
            Tag::Pair => panic!("Can't cast Pair to Func"),
            Tag::Func => unsafe { self.value.Func.as_ref() },
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => todo!(),
            Tag::Port => todo!(),
        }
    }
    pub fn as_bool(&self) -> bool {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => false,
            _ => true,
        }
    }
    pub fn as_string(&self) -> &str {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to String"),
            Tag::Int => panic!("Can't cast Int to String"),
            Tag::Float => panic!("Can't cast Float to String"),
            Tag::Pair => panic!("Can't cast Pair to String"),
            Tag::Func => panic!("Can't cast Func to String"),
            Tag::String => unsafe { self.value.String.as_ref() },
            Tag::Return => panic!("Can't cast Return to String"),
            Tag::Symbol => self.as_symbol(),
            Tag::Port => todo!(),
        }
    }
    pub fn as_symbol(&self) -> &Symbol {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => panic!("Can't cast Null to Symbol"),
            Tag::Int => panic!("Can't cast Int to Symbol"),
            Tag::Float => panic!("Can't cast Float to Symbol"),
            Tag::Pair => panic!("Can't cast Pair to Symbol"),
            Tag::Func => panic!("Can't cast Func to Symbol"),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Symbol => unsafe { self.value.Symbol.as_ref() },
            Tag::Port => todo!(),
        }
    }

    pub fn as_error(&self) -> ErrorCode {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => unsafe { self.value.Error.into() },
            Tag::Null => todo!(),
            Tag::Int => todo!(),
            Tag::Float => todo!(),
            Tag::Pair => todo!(),
            Tag::Func => todo!(),
            Tag::Symbol => todo!(),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Port => todo!(),
        }
    }

    pub fn as_port(&self) -> RawFd {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => todo!(),
            Tag::Null => todo!(),
            Tag::Int => todo!(),
            Tag::Float => todo!(),
            Tag::Pair => todo!(),
            Tag::Func => todo!(),
            Tag::Symbol => todo!(),
            Tag::String => todo!(),
            Tag::Return => todo!(),
            Tag::Port => todo!(),
        }
    }
}

impl Drop for Atom {
    fn drop(&mut self) {
        match self.tag {
            Tag::Pair => unsafe { ManuallyDrop::drop(&mut self.value.Pair) },
            Tag::Func => unsafe { ManuallyDrop::drop(&mut self.value.Func) },
            Tag::Symbol => unsafe { ManuallyDrop::drop(&mut self.value.Symbol) },
            Tag::String => unsafe { ManuallyDrop::drop(&mut self.value.String) },
            Tag::Return => unsafe { ManuallyDrop::drop(&mut self.value.Return) },
            Tag::Port => todo!(),
            _ => (),
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ", self.tag)?;
        match self.tag {
            Tag::FRef => write!(f, "{:?}", unsafe { &*self.value.FRef }),
            Tag::Error => write!(f, "{:?}", self.as_error()),
            Tag::Null => ().okay(),
            Tag::Int => write!(f, "{:?}", self.as_int()),
            Tag::Float => write!(f, "{:?}", self.as_float()),
            Tag::Pair => write!(f, "{:?}", self.as_pair()),
            Tag::Func => write!(f, "{:?}", self.as_func()),
            Tag::String => write!(f, "{:?}", self.as_string()),
            Tag::Return => write!(f, "{:?}", unsafe {
                ManuallyDrop::into_inner(self.value.Return.clone())
            }),
            Tag::Symbol => write!(f, "{:?}", self.as_symbol()),
            Tag::Port => write!(f, "{:?}", self.as_port()),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag {
            Tag::FRef => todo!(),
            Tag::Error => write!(f, "Error {:?}", self.as_error()),
            Tag::Null => write!(f, "Null"),
            Tag::Int => write!(f, "{}", self.as_int()),
            Tag::Float => write!(f, "{}", self.as_float()),
            Tag::Pair => write!(f, "{}", self.as_pair()),
            Tag::Func => write!(f, "Function"),
            Tag::String => write!(f, "{}", self.as_string()),
            Tag::Return => write!(f, "{}", unsafe {
                ManuallyDrop::into_inner(self.value.Return.clone())
            }),
            Tag::Symbol => write!(f, "{}", self.as_string()),
            Tag::Port => write!(f, "{}", self.as_port()),
        }
    }
}

#[repr(i32)]
#[derive(Clone, Copy)]
pub enum Tag {
    FRef = -2,
    Error = -1,
    Null = 0,
    Int = 1,
    Float = 2,
    Pair = 3,
    Func = 4,
    Symbol = 5,
    String = 6,
    Return = 7,
    Port = 8,
}

impl Debug for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FRef => write!(f, "FRef"),
            Self::Error => write!(f, "Error"),
            Self::Null => write!(f, "Null"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Pair => write!(f, "Pair"),
            Self::Func => write!(f, "Func"),
            Self::String => write!(f, "String"),
            Self::Return => write!(f, "Return"),
            Self::Symbol => write!(f, "Symbol"),
            Self::Port => write!(f, "Port"),
        }
    }
}

#[derive(Debug)]
#[repr(u64)]
pub enum ErrorCode {
    Arity = 0,
    Type = 1,
}

impl From<u64> for ErrorCode {
    fn from(n: u64) -> Self {
        match n {
            0 => Self::Arity,
            1 => Self::Type,
            _ => panic!("Invalid error code"),
        }
    }
}
