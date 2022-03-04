use super::{
    closure::{NativeFn, RuntimeClosure, RuntimeFn},
    memman::{allocate, mark},
    Error, Result,
};
use cl_alloc::ObjectHeader;
use enumn::N;
use somok::{Leaksome, Somok};
use std::{
    fmt::Debug,
    ops::{Add, Deref, DerefMut, Div, Mul, Sub},
    ptr::NonNull,
};

#[repr(u8)]
#[derive(Debug)]
pub enum Tag {
    NaN = 0b000,
    Null = 0b001,
    Int = 0b010,
    Undefined = 0b011,
    Uint = 0b100,
    Obj = 0b101,
    Sym = 0b110,
    Ptr = 0b111,
    Float,
}

#[derive(Clone, Copy)]
pub struct Atom(u64);

impl Default for Atom {
    fn default() -> Self {
        Self::undefined()
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match (self.get_tag(), other.get_tag()) {
            (Tag::NaN, _) => false,
            (_, Tag::NaN) => false,
            (Tag::Undefined, _) => false,
            (_, Tag::Undefined) => false,
            (Tag::Ptr, _) => false,
            (_, Tag::Ptr) => false,
            (Tag::Null, Tag::Null) => true,
            (Tag::Null, _) => false,
            (_, Tag::Null) => false,
            (Tag::Int, Tag::Int) => self.as_int().eq(&other.as_int()),
            (Tag::Int, _) => false,
            (_, Tag::Int) => false,
            (Tag::Uint, Tag::Uint) => self.as_uint().eq(&other.as_uint()),
            (Tag::Uint, _) => false,
            (_, Tag::Uint) => false,
            (Tag::Obj, Tag::Obj) => todo!(), //unsafe { (&*self.as_obj()).eq(&*other.as_obj()) },
            (Tag::Obj, _) => false,
            (_, Tag::Obj) => false,
            (Tag::Sym, _) => false,
            (_, Tag::Sym) => false,
            (Tag::Float, Tag::Float) => self.as_float().eq(&other.as_float()),
        }
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;
        match (self.get_tag(), other.get_tag()) {
            (Tag::NaN, _) => None,
            (_, Tag::NaN) => None,
            (Tag::Undefined, _) => None,
            (_, Tag::Undefined) => None,
            (Tag::Ptr, _) => None,
            (_, Tag::Ptr) => None,
            (Tag::Null, Tag::Null) => Some(Ordering::Equal),
            (Tag::Null, _) => None,
            (_, Tag::Null) => None,
            (Tag::Int, Tag::Int) => self.as_int().partial_cmp(&other.as_int()),
            (Tag::Int, _) => None,
            (_, Tag::Int) => None,
            (Tag::Uint, Tag::Uint) => self.as_uint().partial_cmp(&other.as_uint()),
            (Tag::Uint, _) => None,
            (_, Tag::Uint) => None,
            (Tag::Obj, Tag::Obj) => todo!(),
            (Tag::Obj, _) => None,
            (_, Tag::Obj) => None,
            (Tag::Sym, _) => None,
            (_, Tag::Sym) => None,
            (Tag::Float, Tag::Float) => self.as_float().partial_cmp(&other.as_float()),
        }
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_tag() {
            Tag::NaN => write!(f, "NaN"),
            Tag::Null => write!(f, "Null"),
            Tag::Int => write!(f, "i{:?}", self.as_int()),
            Tag::Undefined => write!(f, "Undefined"),
            Tag::Uint => write!(f, "u{:?}", self.as_uint()),
            Tag::Obj => {
                let ptr = self.as_obj();
                let dbg = match ptr.type_tag() {
                    Type::Atom => ptr.debug::<Atom>(),
                    Type::Pair => ptr.debug::<Pair>(),
                    Type::Str => todo!(),
                    Type::Vector => todo!(),
                    Type::Func => ptr.debug::<&'static RuntimeFn>(),
                    Type::NativeFunc => ptr.debug::<NativeFn>(),
                    Type::Closure => ptr.debug::<&'static RuntimeClosure>(),
                };
                write!(f, "&{:?}", dbg)
            }
            Tag::Sym => write!(f, "Symbol {:?}", self.as_sym()),
            Tag::Ptr => write!(f, "Ptr {:?}", self.as_ptr()),
            Tag::Float => write!(f, "f{:?}", self.as_float()),
        }
    }
}
#[allow(dead_code)]
impl Atom {
    const MASK_SIGN: u64 = 0x8000000000000000;
    const MASK_EXPONENT: u64 = 0x7ff0000000000000;
    const MASK_QUIET: u64 = 0x0008000000000000;
    const MASK_TYPE: u64 = 0x0007000000000000;
    const MASK_SIGNATURE: u64 = 0xffff000000000000;
    const MASK_PTR_PAYLOAD: u64 = 0xFFFFFFFFFFFF;

    const MASK_TYPE_NAN: u64 = 0b000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_NULL: u64 = 0b001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_INT: u64 = 0b010_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_UNDEFINED: u64 =
        0b011_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_UINT: u64 = 0b100_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_OBJ: u64 = 0b101_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_SYM: u64 = 0b110_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
    const MASK_TYPE_PTR: u64 = 0b111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;

    pub fn bitwise_eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    pub fn new_float(f: f64) -> Self {
        Self(f.to_bits())
    }

    pub fn new_int(i: i32) -> Self {
        let val = i as u32 as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_INT);
        Self(val)
    }

    pub fn new_uint(i: u32) -> Self {
        let val = i as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_UINT);
        Self(val)
    }

    pub fn new_obj(i: NonNull<ObjectHeader>) -> Self {
        let val = i.as_ptr() as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_OBJ);
        Self(val)
    }

    pub fn new_sym(i: usize) -> Self {
        let val = i as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_SYM);
        Self(val)
    }

    pub fn undefined() -> Self {
        let val = Self::MASK_EXPONENT | Self::MASK_TYPE_UNDEFINED;
        Self(val)
    }

    pub fn null() -> Self {
        let val = Self::MASK_EXPONENT | Self::MASK_TYPE_NULL;
        Self(val)
    }

    pub fn get_tag(&self) -> Tag {
        if !self.0 & Self::MASK_EXPONENT != 0 {
            return Tag::Float;
        }
        let ty = (self.0 & Self::MASK_TYPE) >> 48;
        match ty {
            0b000 => Tag::NaN,
            0b001 => Tag::Null,
            0b010 => Tag::Int,
            0b011 => Tag::Undefined,
            0b100 => Tag::Uint,
            0b101 => Tag::Obj,
            0b110 => Tag::Sym,
            0b111 => Tag::Ptr,
            _ => panic!(),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self.get_tag() {
            Tag::Float | Tag::NaN => f64::from_bits(self.0),
            tag => panic!("Can't cast {:?} as Float", tag),
        }
    }

    pub fn as_int(&self) -> i32 {
        match self.get_tag() {
            Tag::Int => self.0 as i32,
            tag => panic!("Can't cast {:?} as Int", tag),
        }
    }

    pub fn as_uint(&self) -> u32 {
        match self.get_tag() {
            Tag::Uint => self.0 as u32,
            tag => panic!("Can't cast {:?} as Uint", tag),
        }
    }

    pub fn as_obj(&self) -> NonNull<ObjectHeader> {
        match self.get_tag() {
            Tag::Obj => unsafe {
                NonNull::new_unchecked(
                    (self.0 as u64 & Self::MASK_PTR_PAYLOAD) as *mut ObjectHeader,
                )
            },
            tag => panic!("Can't cast {:?} as Obj", tag),
        }
    }

    pub fn as_sym(&self) -> usize {
        match self.get_tag() {
            Tag::Sym => self.0 as u32 as usize,
            tag => panic!("Can't cast {:?} as Sym", tag),
        }
    }

    pub fn as_ptr(&self) -> *mut u8 {
        match self.get_tag() {
            Tag::Ptr => (self.0 as u64 & Self::MASK_PTR_PAYLOAD) as _,
            tag => panic!("Can't cast {:?} as Sym", tag),
        }
    }

    pub fn mark(&mut self) {
        match self.get_tag() {
            Tag::Ptr => {
                if !mark(self.as_ptr()) {
                    return;
                }
                self.as_obj().type_tag();
            }
            Tag::Obj => {
                let ty = self.as_obj().type_tag();
                match ty {
                    Type::Atom => self.as_obj().to_object::<Atom>().mark(),
                    Type::Pair => {
                        let mut pair = self.as_obj().to_object::<Pair>();
                        pair.car.mark();
                        pair.cdr.mark();
                    }
                    Type::Str => todo!(),
                    Type::Vector => todo!(),
                    Type::Func => todo!(),
                    Type::NativeFunc => todo!(),
                    Type::Closure => todo!(),
                };
            }
            _ => (),
        }
    }

    pub fn truthy(&self) -> bool {
        !matches!(self.get_tag(), Tag::Null)
    }
}

impl Add for Atom {
    type Output = Result<Atom>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self.get_tag(), rhs.get_tag()) {
            (Tag::Int, Tag::Int) => Atom::new_int(self.as_int() + rhs.as_int()),
            (Tag::Int, Tag::Uint) => Atom::new_int(self.as_int() + rhs.as_uint() as i32),
            (Tag::Int, Tag::Float) => Atom::new_int(self.as_int() + rhs.as_float() as i32),
            (Tag::Int, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Int, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Uint, Tag::Int) => Atom::new_uint(self.as_uint() + rhs.as_int() as u32),
            (Tag::Uint, Tag::Uint) => Atom::new_uint(self.as_uint() + rhs.as_uint()),
            (Tag::Uint, Tag::Float) => Atom::new_uint(self.as_uint() + rhs.as_float() as u32),
            (Tag::Uint, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Uint, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Float, Tag::Int) => Atom::new_float(self.as_float() + rhs.as_int() as f64),
            (Tag::Float, Tag::Uint) => Atom::new_float(self.as_float() + rhs.as_uint() as f64),
            (Tag::Float, Tag::Float) => Atom::new_float(self.as_float() + rhs.as_float()),
            (Tag::Float, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Float, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Obj) => {
                let this = self.as_obj();
                let rhs = rhs.as_obj();
                if this.is::<Atom>() && rhs.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    let rhs = rhs.downcast::<Atom>();
                    (this + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Int) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Uint) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Float) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this + rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (a_tag, b_tag) => {
                return Error::TypeMismatch(format!(
                    "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                    a_tag, b_tag
                ))
                .error();
            }
        }
        .okay()
    }
}

impl Sub for Atom {
    type Output = Result<Atom>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self.get_tag(), rhs.get_tag()) {
            (Tag::Int, Tag::Int) => Atom::new_int(self.as_int() - rhs.as_int()),
            (Tag::Int, Tag::Uint) => Atom::new_int(self.as_int() - rhs.as_uint() as i32),
            (Tag::Int, Tag::Float) => Atom::new_int(self.as_int() - rhs.as_float() as i32),
            (Tag::Int, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Int, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Uint, Tag::Int) => Atom::new_uint(self.as_uint() - rhs.as_int() as u32),
            (Tag::Uint, Tag::Uint) => Atom::new_uint(self.as_uint() - rhs.as_uint()),
            (Tag::Uint, Tag::Float) => Atom::new_uint(self.as_uint() - rhs.as_float() as u32),
            (Tag::Uint, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Uint, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Float, Tag::Int) => Atom::new_float(self.as_float() - rhs.as_int() as f64),
            (Tag::Float, Tag::Uint) => Atom::new_float(self.as_float() - rhs.as_uint() as f64),
            (Tag::Float, Tag::Float) => Atom::new_float(self.as_float() - rhs.as_float()),
            (Tag::Float, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Float, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Obj) => {
                let this = self.as_obj();
                let rhs = rhs.as_obj();
                if this.is::<Atom>() && rhs.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    let rhs = rhs.downcast::<Atom>();
                    (this - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Int) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Uint) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Float) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this - rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (a_tag, b_tag) => {
                return Error::TypeMismatch(format!(
                    "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                    a_tag, b_tag
                ))
                .error();
            }
        }
        .okay()
    }
}

impl Div for Atom {
    type Output = Result<Atom>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self.get_tag(), rhs.get_tag()) {
            (Tag::Int, Tag::Int) => Atom::new_int(self.as_int() / rhs.as_int()),
            (Tag::Int, Tag::Uint) => Atom::new_int(self.as_int() / rhs.as_uint() as i32),
            (Tag::Int, Tag::Float) => Atom::new_int(self.as_int() / rhs.as_float() as i32),
            (Tag::Int, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Int, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Uint, Tag::Int) => Atom::new_uint(self.as_uint() / rhs.as_int() as u32),
            (Tag::Uint, Tag::Uint) => Atom::new_uint(self.as_uint() / rhs.as_uint()),
            (Tag::Uint, Tag::Float) => Atom::new_uint(self.as_uint() / rhs.as_float() as u32),
            (Tag::Uint, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Uint, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Float, Tag::Int) => Atom::new_float(self.as_float() / rhs.as_int() as f64),
            (Tag::Float, Tag::Uint) => Atom::new_float(self.as_float() / rhs.as_uint() as f64),
            (Tag::Float, Tag::Float) => Atom::new_float(self.as_float() / rhs.as_float()),
            (Tag::Float, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Float, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Obj) => {
                let this = self.as_obj();
                let rhs = rhs.as_obj();
                if this.is::<Atom>() && rhs.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    let rhs = rhs.downcast::<Atom>();
                    (this / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Int) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Uint) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Float) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this / rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (a_tag, b_tag) => {
                return Error::TypeMismatch(format!(
                    "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                    a_tag, b_tag
                ))
                .error();
            }
        }
        .okay()
    }
}

impl Mul for Atom {
    type Output = Result<Atom>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self.get_tag(), rhs.get_tag()) {
            (Tag::Int, Tag::Int) => Atom::new_int(self.as_int() * rhs.as_int()),
            (Tag::Int, Tag::Uint) => Atom::new_int(self.as_int() * rhs.as_uint() as i32),
            (Tag::Int, Tag::Float) => Atom::new_int(self.as_int() * rhs.as_float() as i32),
            (Tag::Int, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Int, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Uint, Tag::Int) => Atom::new_uint(self.as_uint() * rhs.as_int() as u32),
            (Tag::Uint, Tag::Uint) => Atom::new_uint(self.as_uint() * rhs.as_uint()),
            (Tag::Uint, Tag::Float) => Atom::new_uint(self.as_uint() * rhs.as_float() as u32),
            (Tag::Uint, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Uint, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Float, Tag::Int) => Atom::new_float(self.as_float() * rhs.as_int() as f64),
            (Tag::Float, Tag::Uint) => Atom::new_float(self.as_float() * rhs.as_uint() as f64),
            (Tag::Float, Tag::Float) => Atom::new_float(self.as_float() * rhs.as_float()),
            (Tag::Float, Tag::Obj) => {
                let rhs = rhs.as_obj();
                if rhs.is::<Atom>() {
                    let rhs = rhs.downcast::<Atom>();
                    (self * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Float, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Obj) => {
                let this = self.as_obj();
                let rhs = rhs.as_obj();
                if this.is::<Atom>() && rhs.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    let rhs = rhs.downcast::<Atom>();
                    (this * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Obj)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Int) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Uint) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (Tag::Obj, Tag::Float) => {
                let this = self.as_obj();
                if this.is::<Atom>() {
                    let this = this.downcast::<Atom>();
                    (this * rhs)?
                } else {
                    return Error::TypeMismatch(
                        "Expected (Float | Int, Float | Int), found (Obj, Int)".to_string(),
                    )
                    .error();
                }
            }
            (a_tag, b_tag) => {
                return Error::TypeMismatch(format!(
                    "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                    a_tag, b_tag
                ))
                .error();
            }
        }
        .okay()
    }
}

#[derive(Clone)]
pub struct Pair {
    pub car: Atom,
    pub cdr: Atom,
}

impl Debug for Pair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "( {:?} . {:?} )", &self.car, &self.cdr)
    }
}

impl Pair {
    pub fn new(car: Atom, cdr: Atom) -> Self {
        Self { car, cdr }
    }
}

pub trait LispType {
    fn type_tag() -> Type;
}
impl LispType for Atom {
    fn type_tag() -> Type {
        Type::Atom
    }
}
impl LispType for Pair {
    fn type_tag() -> Type {
        Type::Pair
    }
}
impl LispType for &'static RuntimeFn {
    fn type_tag() -> Type {
        Type::Func
    }
}
impl LispType for NativeFn {
    fn type_tag() -> Type {
        Type::NativeFunc
    }
}
impl LispType for &'static RuntimeClosure {
    fn type_tag() -> Type {
        Type::Closure
    }
}

#[derive(Debug, PartialEq, Eq, N)]
#[repr(usize)]
pub enum Type {
    Atom,
    Pair,
    Str,
    Vector,
    Func,
    NativeFunc,
    Closure,
}
impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Atom => 8,
            Type::Pair => 16,
            Type::Str => 24,
            Type::Vector => 24,
            Type::Func => std::mem::size_of::<&'static RuntimeFn>(),
            Type::NativeFunc => std::mem::size_of::<NativeFn>(),
            Type::Closure => std::mem::size_of::<RuntimeClosure>(),
        }
    }
}

/// This is a reference type
#[repr(transparent)]
pub struct Object<T>(*mut T);
impl<T: LispType + 'static> Object<T> {
    pub fn from_ptr(p: *mut T) -> Self {
        Self(p)
    }
    pub fn new(v: T) -> Self {
        allocate(v).expect("Memory allocation failure")
    }
    pub fn new_static(v: T) -> Self {
        Self(v.boxed().leak())
    }

    pub fn type_tag(&self) -> Type {
        T::type_tag()
    }
}
impl<T> Deref for Object<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}
impl<T> DerefMut for Object<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

pub trait Downcast {
    fn downcast<T>(self) -> T
    where
        Self: Sized,
        T: LispType + Clone + 'static,
    {
        self.to_object::<T>().deref().clone()
    }

    fn type_tag(self) -> Type;

    fn is<T>(self) -> bool
    where
        Self: Sized,
        T: LispType,
    {
        self.type_tag() == T::type_tag()
    }

    fn to_object<T>(self) -> Object<T>
    where
        T: LispType + 'static;

    fn debug<'s, T>(self) -> &'s dyn Debug
    where
        T: LispType + Debug + 'static;
}

impl Downcast for NonNull<ObjectHeader> {
    fn type_tag(self) -> Type {
        Type::n(unsafe { &*self.as_ptr() }.ty()).expect("Invalid type")
    }

    fn to_object<T>(self) -> Object<T>
    where
        T: LispType + 'static,
    {
        let ty = self.type_tag();
        if ty == T::type_tag() {
            unsafe { Object::from_ptr(self.as_ptr().add(1).cast::<T>()) }
        } else {
            panic!(
                "Invalid downcast, can't cast `{:?}` to `{:?}`",
                ty,
                T::type_tag()
            )
        }
    }

    fn debug<'s, T>(self) -> &'s dyn Debug
    where
        T: LispType + Debug + 'static,
    {
        let ty = self.type_tag();
        if ty == T::type_tag() {
            unsafe { &*self.as_ptr().add(1).cast::<T>() }
        } else {
            panic!(
                "Invalid downcast, can't cast `{:?}` to `{:?}`",
                ty,
                T::type_tag()
            )
        }
    }
}

#[used]
static ASSERT_HT_SIZE: [(); 8] = [(); std::mem::size_of::<Type>()];

#[test]
fn test() {
    use crate::vm::memman::allocate_raw_and_store;
    let p = allocate_raw_and_store(Pair::new(Atom::new_float(4.20), Atom::new_int(69))).unwrap();

    println!("{:?}", p);
    let a = Atom::new_obj(p);
    let mut pair = a.as_obj().to_object::<Pair>();

    println!("0x{:0x?}", a.0);
    println!("{:?}", a.as_obj());
    println!("{:?}", a);
    pair.car = Atom::new_uint(96);
    println!("{:?}", a);
    panic!("{:?}", std::mem::size_of::<Type>());
}
