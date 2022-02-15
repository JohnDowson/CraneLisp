use smol_str::SmolStr;
use somok::Somok;

use crate::atom::Atom;

pub mod asm;
pub mod closure;
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
    InvalidValueTag,
}

#[derive(Debug, Copy, Clone)]
pub struct Frame {
    ip: isize,
    sp: isize,
    bp: isize,
}

pub struct Vm {
    pub code: Vec<u8>,
    // pub arg_regs: [Atom<0>; 32],
    pub reminder: u64,
    pub ip: isize,
    pub call_stack: Vec<Frame>,
    pub sp: isize,
    pub bp: isize,
    pub stack: Vec<Option<Atom<0>>>,
    pub const_table: Vec<Atom<0>>,
    pub symbol_table: Vec<SmolStr>,
}

impl Vm {
    pub fn new(code: Vec<u8>, const_table: Vec<Atom<0>>, symbol_table: Vec<SmolStr>) -> Vm {
        Vm {
            code,
            // arg_regs: [Atom::<0>::Null; 32],
            reminder: 0,
            ip: 0,
            call_stack: Vec::new(),
            sp: 0,
            bp: 0,
            stack: vec![None; 16],
            const_table,
            symbol_table,
        }
    }

    pub fn execute(&mut self) -> Result<bool> {
        let ip = self.ip;
        let op = self.decode()?;
        println!("ip: {:?},\top: {:?}", ip, &op);
        match op {
            Op::Push(id) => {
                let constant = self.const_table[id as usize];
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
                let val = self.stack[(self.bp + slot as isize) as usize].unwrap();
                self.push(val)
            }
            Op::Jump(_) => todo!(),
            Op::FJump(_) => todo!(),
            Op::TJump(_) => todo!(),
            Op::Call(argc) => self.call(argc)?,
            Op::Return => {
                let frame = self.call_stack.pop().unwrap();
                self.ip = frame.ip;
                self.sp = frame.sp + 1;
                self.bp = frame.bp;
            }
            Op::Halt => return true.okay(),
            Op::Add => {
                let (b, a) = (self.pop()?, self.pop()?);
                let res = match (a, b) {
                    (Atom::Int(a), Atom::Int(b)) => Atom::Int(a + b),
                    (Atom::Int(a), Atom::Float(b)) => Atom::Int(a + b as i64),
                    (Atom::Float(a), Atom::Int(b)) => Atom::Float(a + b as f64),
                    (Atom::Float(a), Atom::Float(b)) => Atom::Float(a + b),
                    _ => {
                        return Error::TypeMismatch(format!(
                            "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                            a, b
                        ))
                        .error()
                    }
                };
                self.push(res);
            }
            Op::Sub => {
                let (b, a) = (self.pop()?, self.pop()?);
                let res = match (a, b) {
                    (Atom::Int(a), Atom::Int(b)) => Atom::Int(a - b),
                    (Atom::Int(a), Atom::Float(b)) => Atom::Int(a - b as i64),
                    (Atom::Float(a), Atom::Int(b)) => Atom::Float(a - b as f64),
                    (Atom::Float(a), Atom::Float(b)) => Atom::Float(a - b),
                    _ => {
                        return Error::TypeMismatch(format!(
                            "Expected (Float | Int, Float | Int), found ({:?}, {:?})",
                            a, b
                        ))
                        .error()
                    }
                };
                self.push(res);
            } // Op::LReg(reg) => {
              //     let val = self.arg_regs[reg as usize];
              //     self.push(val)
              // }
        }
        false.okay()
    }

    fn call(&mut self, argc: u32) -> Result<()> {
        let atom = self.pop().unwrap();
        if let Atom::Func(func) = atom {
            self.ip -= argc as isize;
            let frame = Frame {
                ip: self.ip,
                sp: self.sp,
                bp: self.bp,
            };
            self.call_stack.push(frame);
            self.ip = func as isize;
        } else {
            return Error::TypeMismatch(format!("Expected Func, found {:?}", atom)).error();
        };
        ().okay()
    }

    fn decode(&mut self) -> Result<Op> {
        let res = self.code[self.ip as usize..(self.ip + 8) as usize]
            .try_into()
            .map(Op::from_be_bytes)
            .map_err(|_| Error::Illegal)?;
        self.ip += 8;
        res
    }

    pub fn push(&mut self, val: Atom<0>) {
        let asp = self.bp + self.sp;
        self.sp += 1;
        self.stack[asp as usize] = val.some();
    }
    pub fn pop(&mut self) -> Result<Atom<0>> {
        self.sp -= 1;
        let asp = self.bp + self.sp;
        let mut res = None;
        std::mem::swap(&mut self.stack[asp as usize], &mut res);
        res.ok_or(Error::StackNoValue)
    }
}

#[derive(Debug, Clone, Copy)]
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
    // LReg(u32),
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
            // 12 => {
            //     let reg = bytes[4..]
            //         .try_into()
            //         .map(u32::from_be_bytes)
            //         .map_err(|_| Error::Illegal)?;
            //     Self::LReg(reg)
            // }
            12.. => return Error::Illegal.error(),
        }
        .okay()
    }
}
