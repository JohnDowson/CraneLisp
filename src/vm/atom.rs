use std::rc::Rc;

use super::closure::RuntimeFn;

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
            (Tag::Obj, Tag::Obj) => unsafe { (&*self.as_obj()).eq(&*other.as_obj()) },
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

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_tag() {
            Tag::NaN => write!(f, "NaN"),
            Tag::Null => write!(f, "Null"),
            Tag::Int => write!(f, "i{:?}", self.as_int()),
            Tag::Undefined => write!(f, "Undefined"),
            Tag::Uint => write!(f, "u{:?}", self.as_uint()),
            Tag::Obj => write!(f, "{:?}", unsafe { &*self.as_obj() }),
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

    pub fn new_obj(i: *mut Object) -> Self {
        let val = i as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_OBJ);
        Self(val)
    }

    pub fn new_sym(i: usize) -> Self {
        let val = i as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_SYM);
        Self(val)
    }

    pub fn new_ptr(i: *mut Atom) -> Self {
        let val = i as u64 | (Self::MASK_EXPONENT | Self::MASK_TYPE_PTR);
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

    pub fn as_obj(&self) -> *mut Object {
        match self.get_tag() {
            Tag::Obj => (self.0 as u64 & Self::MASK_PTR_PAYLOAD) as _,
            tag => panic!("Can't cast {:?} as Obj", tag),
        }
    }

    pub fn as_sym(&self) -> usize {
        match self.get_tag() {
            Tag::Sym => self.0 as u32 as usize,
            tag => panic!("Can't cast {:?} as Sym", tag),
        }
    }

    pub fn as_ptr(&self) -> *mut Atom {
        match self.get_tag() {
            Tag::Ptr => (self.0 as u64 & Self::MASK_PTR_PAYLOAD) as _,
            tag => panic!("Can't cast {:?} as Sym", tag),
        }
    }

    pub fn mark(&mut self) {
        match self.get_tag() {
            Tag::Obj => unsafe {
                (&mut *self.as_obj()).mark();
            },

            Tag::Ptr => unsafe {
                (&mut *self.as_ptr()).mark();
            },
            _ => (),
        }
    }

    pub fn truthy(&self) -> bool {
        !matches!(self.get_tag(), Tag::Null)
    }
}

pub struct Object {
    pub mark: bool,
    inner: HeapType,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Object {
    pub fn new_pair(car: Atom, cdr: Atom) -> Self {
        Self {
            mark: false,
            inner: HeapType::Pair(Pair { car, cdr }),
        }
    }
    pub fn new_func(fun: Rc<RuntimeFn>) -> Self {
        Self {
            mark: false,
            inner: HeapType::Func(fun),
        }
    }
    pub fn new_native_func(fun: fn(&[Atom]) -> Atom) -> Self {
        Self {
            mark: false,
            inner: HeapType::NativeFunc(fun),
        }
    }

    pub fn mark(&mut self) {
        if self.mark {
            return;
        }
        self.mark = true;
        match &mut self.inner {
            HeapType::Pair(Pair { car, cdr }) => {
                car.mark();
                cdr.mark()
            }
            HeapType::Str {} => (),
            HeapType::Vector {} => (),
            HeapType::Func(_) => (),
            HeapType::NativeFunc(_) => (),
        }
    }

    pub fn is_pair(&self) -> bool {
        matches!(self.inner, HeapType::Pair { .. })
    }

    pub fn is_str(&self) -> bool {
        matches!(self.inner, HeapType::Str { .. })
    }

    pub fn is_vector(&self) -> bool {
        matches!(self.inner, HeapType::Vector { .. })
    }

    pub fn is_func(&self) -> bool {
        matches!(self.inner, HeapType::Func(..))
    }

    pub fn is_native_func(&self) -> bool {
        matches!(self.inner, HeapType::NativeFunc(..))
    }

    pub fn as_pair(&self) -> &Pair {
        if let HeapType::Pair(v) = &self.inner {
            v
        } else {
            panic!()
        }
    }

    pub fn as_func(&self) -> Rc<RuntimeFn> {
        if let HeapType::Func(v) = &self.inner {
            v.clone()
        } else {
            panic!()
        }
    }

    pub fn as_native_func(&self) -> fn(&[Atom]) -> Atom {
        if let HeapType::NativeFunc(v) = &self.inner {
            *v
        } else {
            panic!()
        }
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Object({:?}): {:?}", self.mark, self.inner)
    }
}

pub enum HeapType {
    Pair(Pair),
    Str {},
    Vector {},
    Func(Rc<RuntimeFn>),
    NativeFunc(fn(&[Atom]) -> Atom),
}

impl PartialEq for HeapType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pair(l0), Self::Pair(r0)) => l0 == r0,
            (Self::Func(l0), Self::Func(r0)) => l0 == r0,
            (Self::NativeFunc(l0), Self::NativeFunc(r0)) => (*l0) as usize == (*r0) as usize,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(PartialEq)]
pub struct Pair {
    pub car: Atom,
    pub cdr: Atom,
}

const _ASSERT_HT_SIZE: [(); 24] = [(); std::mem::size_of::<HeapType>()];

impl std::fmt::Debug for HeapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pair(Pair { car, cdr }) => write!(f, "({:?} . {:?})", car, cdr),
            Self::Str {} => f.debug_struct("Str").finish(),
            Self::Vector {} => f.debug_struct("Vector").finish(),
            Self::Func(fun) => f
                .debug_struct("Func")
                .field("const_table", &fun.const_table)
                .field("locals", &fun.locals)
                .finish(),
            Self::NativeFunc(_) => f.debug_struct("NativeFunc").finish(),
        }
    }
}

#[test]
fn test() {
    use somok::Leaksome;

    let p = Box::new(Object {
        mark: true,
        inner: HeapType::Pair(Pair {
            car: Atom::new_uint(69),
            cdr: Atom::new_int(420),
        }),
    })
    .leak() as *mut _;

    println!("{:?}", p);
    let a = Atom::new_obj(p);
    println!("0x{:0x?}", a.0);
    println!("{:?}", a.as_obj());
    println!("{:?}", a);

    panic!("{:?}", std::mem::size_of::<Object>());
}
