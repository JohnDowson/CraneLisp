use std::{cell::RefCell, ops::DerefMut};

use self::{
    atom::{Atom, Downcast, LispType, Pair},
    closure::{FnProto, NativeFn, RuntimeClosure, RuntimeFn},
    memman::allocate_raw_and_store,
};
use crate::vm::atom::Tag;
use smol_str::SmolStr;
use somok::{Leaksome, Somok};
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
    AllocationFailure,
}

#[derive(Debug, Clone)]
pub struct Frame {
    fp: FnProto,
    ip: isize,
    sp: isize,
    bp: isize,
}

pub struct Vm {
    pub code: &'static [u8],
    pub cf: FnProto,
    pub reminder: u64,
    pub ip: isize,
    pub call_stack: Vec<Frame>,
    pub sp: isize,
    pub bp: isize,
    pub stack: Vec<Option<Atom>>,
    pub symbol_table: Vec<SmolStr>,
}

impl Vm {
    pub fn new(main: &'static RuntimeFn, symbol_table: Vec<SmolStr>) -> Self {
        let code = main.assembly;
        Vm {
            code,
            cf: FnProto::Func(main),
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
            println!(
                "sp: {:?}, bp: {:?}, asp: {:?}",
                self.sp,
                self.bp,
                self.sp + self.bp
            );
            println!("stack: {:?}", &self.stack); //&self.stack[..self.sp as usize]);
            println!("{:?}:\t {:?}", self.ip, &op);
        }
        match op {
            Op::Push(id) => {
                let constant = self.cf.const_table()[id as usize];
                self.push(constant)
            }
            Op::Pop => {
                self.pop()?;
            }
            Op::LSet(slot) => {
                let val = self.pop()?;
                self.stack[slot as usize] = val.some();
            }
            Op::LGet(slot) => {
                let val = self.stack[slot as usize].ok_or(Error::UndefinedLocal)?;
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
                self.code = frame.fp.assembly();
                let ret = self.pop()?;
                self.cf = frame.fp;
                self.ip = frame.ip;
                self.sp = frame.sp;
                self.bp = frame.bp;
                self.push(ret);
            }
            Op::Halt => return true.okay(),
            Op::Add => {
                let (b, a) = (self.pop()?, self.pop()?);
                let res = (a + b)?;
                self.push(res);
            }
            Op::Sub => {
                let (b, a) = (self.pop()?, self.pop()?);
                let res = (a - b)?;
                self.push(res);
            }
            Op::Car => {
                let atom = self.pop()?;

                if let Tag::Obj = atom.get_tag() {
                    let obj = atom.as_obj();
                    if obj.is::<Pair>() {
                        self.push(obj.downcast::<Pair>().car);
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
                    let obj = atom.as_obj();
                    if obj.is::<Pair>() {
                        self.push(obj.downcast::<Pair>().cdr)
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
                let pair = self
                    .alloc(Pair::new(car, cdr))
                    .ok_or(Error::AllocationFailure)?;
                self.push(pair);
            }
            Op::UpvalGet(id) => {
                let a = self.cf.upvalues()[id as usize];
                self.push(a);
            }
            Op::UpvalSet(id) => {
                let a = self.pop()?;
                *self.cf.upvalues()[id as usize]
                    .as_obj()
                    .to_object::<Atom>()
                    .deref_mut() = a;
            }
            Op::CloseOver => {
                let f = self.pop()?;
                let f = f.as_obj().downcast::<&'static RuntimeFn>();
                let c = RuntimeClosure {
                    fun: f,
                    upvalues: RefCell::new(vec![]),
                }
                .boxed()
                .leak();
                for upval in &f.upvalues {
                    let at = if upval.local {
                        let val = self.stack[upval.id].ok_or(Error::UndefinedLocal)?;
                        self.alloc(val).ok_or(Error::AllocationFailure)?
                    } else {
                        self.cf.upvalues()[upval.id]
                    };
                    c.upvalues.borrow_mut().push(at);
                    self.stack[upval.id] = at.some();
                }
                let clos = self.alloc(&*c).ok_or(Error::AllocationFailure)?;
                self.push(clos)
            }
        }
        false.okay()
    }

    fn alloc<T: LispType + 'static>(&mut self, obj: T) -> Option<Atom> {
        //self.mark();
        //sweep();
        let ptr = allocate_raw_and_store(obj)?;
        let atom = Atom::new_obj(ptr);

        atom.some()
    }

    fn _mark(&mut self) {
        for atom in self.stack.iter_mut().flatten() {
            atom.mark()
        }
    }

    fn call(&mut self, argc: u32) -> Result<()> {
        let atom = self.pop()?;
        if let Tag::Obj = atom.get_tag() {
            let obj = atom.as_obj();
            if obj.is::<&'static RuntimeFn>() {
                let func = obj.downcast::<&'static RuntimeFn>();
                self.sp -= argc as isize;
                let frame = Frame {
                    fp: self.cf,
                    ip: self.ip,
                    sp: self.sp,
                    bp: self.bp,
                };
                self.bp += self.sp;
                self.sp = 0;
                self.code = func.assembly;
                self.cf = FnProto::Func(func);
                self.call_stack.push(frame);
                self.ip = 0;
            } else if obj.is::<&'static RuntimeClosure>() {
                let closure = obj.downcast::<&'static RuntimeClosure>();
                self.sp -= argc as isize;
                let frame = Frame {
                    fp: self.cf,
                    ip: self.ip,
                    sp: self.sp,
                    bp: self.bp,
                };
                self.bp += self.sp;
                self.sp = 0;
                self.code = closure.fun.assembly;
                self.cf = FnProto::Closure(closure);
                self.call_stack.push(frame);
                self.ip = 0;
            } else if obj.is::<NativeFn>() {
                let mut args: ArrayVec<[Atom; 256]> = ArrayVec::new();
                let func = obj.downcast::<NativeFn>();
                for _ in 0..argc {
                    args.push(self.pop()?);
                }
                self.push(func.call(&args));
            } else {
                return Error::TypeMismatch(format!("Expected Func, found {:?}", atom)).error();
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

    pub fn push(&mut self, v: Atom) {
        let asp = self.bp + self.sp;
        self.sp += 1;
        if asp >= self.stack.len() as isize {
            self.stack.push(v.some());
        } else {
            self.stack[asp as usize] = v.some();
        }
    }
    pub fn pop(&mut self) -> Result<Atom> {
        self.sp -= 1;
        let asp = self.bp + self.sp;
        if asp < self.bp {
            return Error::StackUnderflow.error();
        }
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

    UpvalGet(u32),
    UpvalSet(u32),

    CloseOver,
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

            15 => {
                let slot = bytes[4..]
                    .try_into()
                    .map(u32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::UpvalGet(slot)
            }
            16 => {
                let slot = bytes[4..]
                    .try_into()
                    .map(u32::from_be_bytes)
                    .map_err(|_| Error::Illegal)?;
                Self::UpvalSet(slot)
            }
            17 => Self::CloseOver,

            18.. => return Error::Illegal.error(),
        }
        .okay()
    }
}
