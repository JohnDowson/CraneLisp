use std::rc::Rc;

use crate::vm::{
    atom::{Object, Tag},
    memman::alloc,
};

use self::{atom::Atom, closure::RuntimeFn, memman::sweep};
use smol_str::SmolStr;
use somok::Somok;
use tinyvec::ArrayVec;

pub mod asm;
pub mod atom;
pub mod closure;
pub mod memman;
pub mod translate;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Illegal,
    NotEnoughBytes,
    IpOutOfBounds,
    StackOverflow,
    StackUnderflow,
    CallStackOverflow,
    CallStackUnderflow,
    StackNoValue,
    TypeMismatch(String),
    UndefinedLocal,
    InvalidValueTag,
    PlaceInvalid,
}

#[derive(Debug, Clone)]
pub struct Frame {
    fp: Rc<RuntimeFn>,
    ip: isize,
    sp: isize,
    bp: isize,
}

pub struct Vm {
    pub code: &'static [u8],
    pub cf: Rc<RuntimeFn>,
    pub reminder: u64,
    pub ip: isize,
    pub call_stack: Vec<Frame>,
    pub sp: isize,
    pub bp: isize,
    pub stack: Vec<Option<Atom>>,
    pub symbol_table: Vec<SmolStr>,
}

impl Vm {
    pub fn new(main: Rc<RuntimeFn>, symbol_table: Vec<SmolStr>) -> Self {
        let code = main.assembly;
        Vm {
            code,
            cf: main,
            reminder: 0,
            ip: 0,
            call_stack: Vec::new(),
            sp: 0,
            bp: 0,
            stack: Vec::new(),
            symbol_table,
        }
    }

    pub fn execute(&mut self) -> Result<bool> {
        let op = self.decode()?;
        #[cfg(debug_assertions)]
        {
            let ip = self.ip;
            println!("stack: {:?}", &self.stack);
            println!("{:?}:\t {:?}", ip, &op);
        }
        match op {
            Op::Push(id) => {
                let constant = self.cf.const_table[id as usize];
                self.push(constant)
            }
            Op::Pop => {
                self.pop().unwrap();
            }
            Op::LSet(slot) => {
                let val = self.pop()?;
                self.stack[(self.bp + slot as isize) as usize] = val.some();
            }
            Op::LGet(slot) => {
                let val =
                    self.stack[(self.bp + slot as isize) as usize].ok_or(Error::UndefinedLocal)?;
                self.push(val)
            }
            Op::Jump(offset) => {
                let offset = offset as isize;
                self.ip += offset
            }
            Op::FJump(offset) => {
                if !self.pop()?.truthy() {
                    let offset = offset as isize;
                    self.ip += offset
                }
            }
            Op::TJump(offset) => {
                if self.pop()?.truthy() {
                    let offset = offset as isize;
                    self.ip += offset
                }
            }
            Op::Call(argc) => self.call(argc)?,
            Op::Return => {
                let frame = self.call_stack.pop().unwrap();
                self.code = frame.fp.assembly;
                self.ip = frame.ip;
                self.sp = frame.sp + 1;
                self.bp = frame.bp;
            }
            Op::Halt => return true.okay(),
            Op::Add => {
                let (b, a) = (self.pop()?, self.pop()?);
                let res = match (a.get_tag(), b.get_tag()) {
                    (Tag::Int, Tag::Int) => Atom::new_int(a.as_int() + b.as_int()),
                    (Tag::Int, Tag::Uint) => Atom::new_int(a.as_int() + b.as_uint() as i32),
                    (Tag::Int, Tag::Float) => Atom::new_int(a.as_int() + b.as_float() as i32),
                    (Tag::Uint, Tag::Int) => Atom::new_uint(a.as_uint() + b.as_int() as u32),
                    (Tag::Uint, Tag::Uint) => Atom::new_uint(a.as_uint() + b.as_uint()),
                    (Tag::Uint, Tag::Float) => Atom::new_uint(a.as_uint() + b.as_float() as u32),
                    (Tag::Float, Tag::Int) => Atom::new_float(a.as_float() + b.as_int() as f64),
                    (Tag::Float, Tag::Uint) => Atom::new_float(a.as_float() + b.as_uint() as f64),
                    (Tag::Float, Tag::Float) => Atom::new_float(a.as_float() + b.as_float()),
                    (a_tag, b_tag) => {
                        return Error::TypeMismatch(format!(
                            "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                            a_tag, b_tag
                        ))
                        .error();
                    }
                };
                self.push(res);
            }
            Op::Sub => {
                let (b, a) = (self.pop()?, self.pop()?);
                let res = match (a.get_tag(), b.get_tag()) {
                    (Tag::Int, Tag::Int) => Atom::new_int(a.as_int() - b.as_int()),
                    (Tag::Int, Tag::Uint) => Atom::new_int(a.as_int() - b.as_uint() as i32),
                    (Tag::Int, Tag::Float) => Atom::new_int(a.as_int() - b.as_float() as i32),
                    (Tag::Uint, Tag::Int) => Atom::new_uint(a.as_uint() - b.as_int() as u32),
                    (Tag::Uint, Tag::Uint) => Atom::new_uint(a.as_uint() - b.as_uint()),
                    (Tag::Uint, Tag::Float) => Atom::new_uint(a.as_uint() - b.as_float() as u32),
                    (Tag::Float, Tag::Int) => Atom::new_float(a.as_float() - b.as_int() as f64),
                    (Tag::Float, Tag::Uint) => Atom::new_float(a.as_float() - b.as_uint() as f64),
                    (Tag::Float, Tag::Float) => Atom::new_float(a.as_float() - b.as_float()),
                    (a_tag, b_tag) => {
                        return Error::TypeMismatch(format!(
                            "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                            a_tag, b_tag
                        ))
                        .error()
                    }
                };
                self.push(res);
            }
            Op::Car => {
                let atom = self.pop()?;

                if let Tag::Obj = atom.get_tag() {
                    let obj = unsafe { &*atom.as_obj() };
                    if obj.is_pair() {
                        self.push(obj.as_pair().car)
                    } else {
                        return Error::TypeMismatch(format!("Expected Pair, found {:?}", atom))
                            .error();
                    }
                } else {
                    return Error::TypeMismatch(format!("Expected Pair, found {:?}", atom)).error();
                }
            }
            Op::Cdr => {
                let atom = self.pop()?;

                if let Tag::Obj = atom.get_tag() {
                    let obj = unsafe { &*atom.as_obj() };
                    if obj.is_pair() {
                        self.push(obj.as_pair().cdr)
                    } else {
                        return Error::TypeMismatch(format!("Expected Pair, found {:?}", atom))
                            .error();
                    }
                } else {
                    return Error::TypeMismatch(format!("Expected Pair, found {:?}", atom)).error();
                }
            }
            Op::Cons => {
                let car = self.pop()?;
                let cdr = self.pop()?;
                let pair = self.alloc(Object::new_pair(car, cdr));
                self.push(pair);
            }
        }
        false.okay()
    }

    fn alloc(&mut self, obj: Object) -> Atom {
        let atom = Atom::new_obj(alloc(obj));
        self.mark();
        sweep();
        atom
    }

    fn mark(&mut self) {
        for atom in self.stack.iter_mut().flatten() {
            atom.mark()
        }
    }

    fn call(&mut self, argc: u32) -> Result<()> {
        let atom = self.pop().unwrap();
        if let Tag::Obj = atom.get_tag() {
            let obj = unsafe { &*atom.as_obj() };
            if obj.is_func() {
                let func = obj.as_func();
                self.sp -= argc as isize;
                let frame = Frame {
                    fp: self.cf.clone(),
                    ip: self.ip,
                    sp: self.sp,
                    bp: self.bp,
                };
                self.bp = self.sp;
                self.sp = 0;
                self.code = func.assembly;
                self.cf = func;
                self.call_stack.push(frame);
                self.ip = 0;
            } else if obj.is_native_func() {
                let mut args: ArrayVec<[Atom; 256]> = ArrayVec::new();
                let func = obj.as_native_func();
                for _ in 0..argc {
                    args.push(self.pop()?);
                }
                self.push(func(&args));
            }
        } else {
            return Error::TypeMismatch(format!("Expected Func, found {:?}", atom)).error();
        };
        ().okay()
    }

    pub fn decode(&mut self) -> Result<Op> {
        let res = self.code[self.ip as usize..(self.ip + 8) as usize]
            .try_into()
            .map(Op::from_be_bytes)
            .map_err(|_| Error::Illegal)?;
        self.ip += 8;
        res
    }

    pub fn push(&mut self, val: Atom) {
        let asp = self.bp + self.sp;
        self.sp += 1;
        if asp >= self.stack.len() as isize {
            self.stack.push(val.some());
        } else {
            self.stack[asp as usize] = val.some();
        }
    }
    pub fn pop(&mut self) -> Result<Atom> {
        self.sp -= 1;
        let asp = self.bp + self.sp;
        let mut res = None;
        std::mem::swap(&mut self.stack[asp as usize], &mut res);
        res.ok_or(Error::StackNoValue)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Push(u32),
    Pop,

    LSet(u32),
    LGet(u32),

    Jump(i32),
    FJump(i32),
    TJump(i32),

    Call(u32),
    Return,
    Halt,

    Add,
    Sub,

    Car,
    Cdr,
    Cons,
}
impl Op {
    pub fn from_be_bytes(bytes: [u8; 8]) -> Result<Self> {
        match bytes[0] {
            0 => {
                let const_idx = bytes[4..]
                    .try_into()
                    .map(u32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::Push(const_idx)
            }
            1 => Self::Pop,
            2 => {
                let sp = bytes[4..]
                    .try_into()
                    .map(u32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::LSet(sp)
            }
            3 => {
                let sp = bytes[4..]
                    .try_into()
                    .map(u32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::LGet(sp)
            }
            4 => {
                let sp = bytes[4..]
                    .try_into()
                    .map(i32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::Jump(sp)
            }
            5 => {
                let sp = bytes[4..]
                    .try_into()
                    .map(i32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::FJump(sp)
            }
            6 => {
                let sp = bytes[4..]
                    .try_into()
                    .map(i32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::TJump(sp)
            }
            7 => {
                let argc = bytes[4..]
                    .try_into()
                    .map(u32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::Call(argc)
            }
            8 => Self::Return,
            9 => Self::Halt,
            10 => Self::Add,
            11 => Self::Sub,

            12 => Self::Car,
            13 => Self::Cdr,
            14 => Self::Cons,

            15.. => return Error::Illegal.error(),
        }
        .okay()
    }
}
