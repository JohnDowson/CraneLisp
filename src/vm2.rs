use self::asm::Ins;
use crate::value::SymId;
use somok::Somok;
use tinyvec::ArrayVec;

pub mod asm;
pub mod atom;
pub mod closure;
pub mod translate;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Illegal(usize, u8),
    NotEnoughBytes(usize),
    IpOutOfBounds(usize),
    AllocationNotFound,
    StackUnderflow,
    TypeMismatch,
    InvalidValueTag,
}

#[repr(C)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Null,
    Int(i64),
    Symbol(SymId),
    Pair(u32, u32),
    Place(u32),
    Func(usize),
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Int(i) => write!(f, "i{i}"),
            Self::Symbol(s) => write!(f, "{s:?}"),
            Self::Pair(car, cdr) => write!(f, "({car}.{cdr})"),
            Self::Place(p) => write!(f, "p{p}"),
            Self::Func(fun) => write!(f, "f{fun}"),
        }
    }
}
impl Default for Value {
    fn default() -> Self {
        Self::Null
    }
}
impl Value {
    pub fn as_bytes(self) -> ArrayVec<[u8; 9]> {
        let mut res = ArrayVec::new();
        match self {
            Value::Null => res.extend([0; 9]),
            Value::Int(i) => {
                res.push(1);
                res.extend(i.to_be_bytes())
            }
            Value::Symbol(s) => {
                res.push(2);
                res.extend(s.0.to_be_bytes())
            }
            Value::Pair(car, cdr) => {
                res.push(3);
                res.extend(car.to_be_bytes());
                res.extend(cdr.to_be_bytes());
            }
            Value::Place(p) => {
                res.push(4);
                res.extend(p.to_be_bytes());
                res.extend([0u8; 4]);
            }
            Value::Func(p) => {
                res.push(5);
                res.extend(p.to_be_bytes());
            }
        }
        res
    }
    pub fn from_bytes(bytes: &[u8]) -> Result<Self> {
        assert_eq!(bytes.len(), 9);

        match bytes[0] {
            0 => Self::Null,
            1 => Self::Int(bytes[1..].try_into().map(i64::from_be_bytes).unwrap()),
            2 => Self::Symbol(SymId(
                bytes[1..].try_into().map(usize::from_be_bytes).unwrap(),
            )),
            3 => Self::Pair(
                bytes[1..5].try_into().map(u32::from_be_bytes).unwrap(),
                bytes[5..9].try_into().map(u32::from_be_bytes).unwrap(),
            ),
            4 => Self::Place(bytes[1..5].try_into().map(u32::from_be_bytes).unwrap()),
            5 => Self::Func(bytes[1..9].try_into().map(usize::from_be_bytes).unwrap()),
            _ => return Error::InvalidValueTag.error(),
        }
        .okay()
    }
    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(v) = self {
            Some(*v)
        } else {
            None
        }
    }
    pub fn car(&self) -> Option<usize> {
        if let Self::Pair(car, _) = self {
            Some(*car as usize)
        } else {
            None
        }
    }
    pub fn cdr(&self) -> Option<usize> {
        if let Self::Pair(_, cdr) = self {
            Some(*cdr as usize)
        } else {
            None
        }
    }
    pub fn as_place(&self) -> Option<usize> {
        if let Self::Place(v) = self {
            Some(*v as usize)
        } else {
            None
        }
    }

    /// Returns `true` if the value is [`Null`].
    ///
    /// [`Null`]: Value::Null
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn as_func(&self) -> Option<&usize> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[test]
fn test() {
    let r = Value::from_bytes(&[0, 0, 0, 0, 0, 0, 0, 0, 0]).unwrap();
    assert_eq!(r, Value::Null);
    let r = Value::from_bytes(&[1, 0, 0, 0, 0, 0, 0, 0, 69]).unwrap();
    assert_eq!(r, Value::Int(69));

    let r = Value::Int(69).as_bytes();
    let r = Value::from_bytes(&r).unwrap();
    assert_eq!(r, Value::Int(69));
}

struct Allocation {
    offset: u32,
    size: u32,
}
impl Allocation {
    pub fn new(offset: u32, size: u32) -> Allocation {
        Allocation { offset, size }
    }
}

pub struct Vm {
    pub code: Vec<u8>,
    pub registers: [Value; 32],
    pub r#return: Value,
    reminder: i64,
    pub ip: usize,
    pub bp: isize,
    pub sp: isize,
    pub stack: [Value; 16],
    pub heap: Vec<u8>,
    allocations: Vec<Allocation>,
    free: Vec<Allocation>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            code: Default::default(),
            registers: [Default::default(); 32],
            r#return: Default::default(),
            reminder: 0,
            ip: 0,
            bp: 0,
            sp: 0,
            stack: [Default::default(); 16],
            heap: Default::default(),
            allocations: Default::default(),
            free: Default::default(),
        }
    }

    pub fn with_code(code: Vec<u8>) -> Self {
        Self {
            code,
            ..Default::default()
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut halt = false;
        while !halt {
            halt = self.execute()?;
        }
        ().okay()
    }

    pub fn execute(&mut self) -> Result<bool> {
        use Opcode::*;
        if self.ip >= self.code.len() {
            return Error::IpOutOfBounds(self.ip).error();
        }
        #[cfg(debug_assertions)]
        {
            let dis = self.disassemble()?;
            println!("{}:\t{}", self.ip, dis);
        };

        match self.decode() {
            Halt => return true.okay(),

            Load => {
                let (reg, imm) = (self.decode_register(), self.decode_val()?);
                self.registers[reg] = imm;
            }
            Alloc => {
                self.alloc()?;
            }
            Dealloc => {
                self.dealloc()?;
            }

            Pop => {
                let reg = self.decode_register();
                self.registers[reg] = self.pop()?;
            }
            Push => {
                let reg = self.decode_register();
                let val = self.registers[reg];
                self.push(val)
            }
            IPush => {
                let val = self.decode_val()?;
                self.push(val)
            }
            Peek => {
                let (reg, slot) = (self.decode_register(), self.decode_register());
                let slot = self.registers[slot].as_place().ok_or(Error::TypeMismatch)?;
                self.registers[reg] = self.stack[slot];
            }
            LPeek => {
                let (reg, slot) = (self.decode_register(), self.decode_register());
                let slot = self.registers[self.bp as usize + slot]
                    .as_place()
                    .ok_or(Error::TypeMismatch)?;
                self.registers[reg] = self.stack[slot];
            }
            SStore => {
                let (slot, val) = (self.decode_register(), self.decode_register());
                let slot = self.registers[slot].as_place().ok_or(Error::TypeMismatch)?;
                self.stack[slot] = self.registers[val];
            }

            MemS => {
                let (adr, cont) = (self.decode_register(), self.decode_register());
                let adr = self.registers[adr].as_place().ok_or(Error::TypeMismatch)?;
                let cont = self.registers[cont];
                for (h_byte, v_byte) in (&mut self.heap[adr..adr + 9])
                    .iter_mut()
                    .zip(cont.as_bytes())
                {
                    *h_byte = v_byte;
                }
            }
            MemL => {
                let (reg, adr) = (self.decode_register(), self.decode_register());
                let adr = self.registers[adr].as_place().ok_or(Error::TypeMismatch)?;
                let res = Value::from_bytes(&self.heap[adr..adr + 9])?;
                self.registers[reg] = res
            }

            Jump => {
                let reg = self.decode_register();
                let target = self.registers[reg].as_place().ok_or(Error::TypeMismatch)?;
                self.ip = target;
            }
            JumpF => {
                let reg = self.decode_register();
                let offset = self.registers[reg].as_place().ok_or(Error::TypeMismatch)?;
                self.ip += offset;
            }
            JumpB => {
                let reg = self.decode_register();
                let offset = self.registers[reg].as_place().ok_or(Error::TypeMismatch)?;
                self.ip -= offset;
            }

            Call => {
                let reg = self.decode_register();
                let target = self.registers[reg].as_place().ok_or(Error::TypeMismatch)?;

                // Do I need to save registers?
                self.bp = self.sp;
                self.push(Value::Place(self.sp as u32));
                self.push(Value::Place(self.ip as u32));
                self.push(Value::Place(self.bp as u32));
                self.sp = 3;
                self.ip = target;
            }
            Return => {
                let reg = self.decode_register();
                self.r#return = self.registers[reg];

                self.bp = self.pop()?.as_place().ok_or(Error::TypeMismatch)? as isize;
                self.ip = self.pop()?.as_place().ok_or(Error::TypeMismatch)?;
                self.sp = self.pop()?.as_place().ok_or(Error::TypeMismatch)? as isize;
            }

            JumpT => {
                let (reg, offset) = (self.decode_register(), self.decode_register());
                let offset = self.registers[offset]
                    .as_place()
                    .ok_or(Error::TypeMismatch)?;
                if !self.registers[reg].is_null() {
                    self.ip += offset;
                }
            }

            Eqz => {
                let (reg, a) = (self.decode_register(), self.decode_register());
                self.registers[reg] = if self.registers[a] == Value::Int(0) {
                    Value::Int(1)
                } else {
                    Value::Null
                }
            }
            Nez => {
                let (reg, a) = (self.decode_register(), self.decode_register());
                self.registers[reg] = if self.registers[a] != Value::Int(0) {
                    Value::Int(1)
                } else {
                    Value::Null
                }
            }

            Eq => {
                self.binary_bool_op(|a, b| a == b)?;
            }
            Ne => {
                self.binary_bool_op(|a, b| a != b)?;
            }
            Gt => {
                self.binary_bool_op(|a, b| a > b)?;
            }
            Lt => {
                self.binary_bool_op(|a, b| a < b)?;
            }
            Ge => {
                self.binary_bool_op(|a, b| a >= b)?;
            }
            Le => {
                self.binary_bool_op(|a, b| a <= b)?;
            }

            Add => {
                self.binary_int_op(|a, b| a + b)?;
            }
            Sub => {
                self.binary_int_op(|a, b| a - b)?;
            }
            Mul => {
                self.binary_int_op(|a, b| a * b)?;
            }
            Div => {
                let (out, a, b) = (
                    self.decode_register(),
                    self.decode_register(),
                    self.decode_register(),
                );
                let (a, b) = (
                    self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
                    self.registers[b].as_int().ok_or(Error::TypeMismatch)?,
                );
                self.registers[out] = Value::Int(a / b);
                self.reminder = a % b;
            }

            IAdd => {
                let (out, a, b) = (
                    self.decode_register(),
                    self.decode_register(),
                    self.decode_val()?,
                );
                let (a, b) = (
                    self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
                    b.as_int().ok_or(Error::TypeMismatch)?,
                );
                self.registers[out] = Value::Int(a + b);
            }
            ISub => {
                let (out, a, b) = (
                    self.decode_register(),
                    self.decode_register(),
                    self.decode_val()?,
                );
                let (a, b) = (
                    self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
                    b.as_int().ok_or(Error::TypeMismatch)?,
                );
                self.registers[out] = Value::Int(a - b);
            }
            IMul => {
                let (out, a, b) = (
                    self.decode_register(),
                    self.decode_register(),
                    self.decode_val()?,
                );
                let (a, b) = (
                    self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
                    b.as_int().ok_or(Error::TypeMismatch)?,
                );
                self.registers[out] = Value::Int(a * b);
            }
            IDiv => {
                let (out, a, b) = (
                    self.decode_register(),
                    self.decode_register(),
                    self.decode_val()?,
                );
                let (a, b) = (
                    self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
                    b.as_int().ok_or(Error::TypeMismatch)?,
                );
                self.registers[out] = Value::Int(a / b);
                self.reminder = a % b;
            }

            Car => {
                let (reg, pair) = (self.decode_register(), self.decode_register());
                let place = self.registers[pair].car().ok_or(Error::TypeMismatch)?;
                self.registers[reg] = Value::Place(place as u32)
            }
            Cdr => {
                let (reg, pair) = (self.decode_register(), self.decode_register());
                let place = self.registers[pair].cdr().ok_or(Error::TypeMismatch)?;
                self.registers[reg] = Value::Place(place as u32)
            }

            PInc => {
                let reg = self.decode_register();
                let place = self.registers[reg].as_place().ok_or(Error::TypeMismatch)?;
                self.registers[reg] = Value::Place(place as u32 + 1)
            }
            PDec => {
                let reg = self.decode_register();
                let place = self.registers[reg].as_place().ok_or(Error::TypeMismatch)?;
                self.registers[reg] = Value::Place(place as u32 - 1)
            }
            POffset => {
                let (reg, offset) = (self.decode_register(), self.decode_register());

                let place = self.registers[reg].as_place().ok_or(Error::TypeMismatch)? as i32;
                let offset = self.registers[offset].as_int().ok_or(Error::TypeMismatch)? as i32;
                self.registers[reg] = Value::Place((place + offset) as u32)
            }
            PIOffset => {
                let (reg, offset) = (self.decode_register(), self.decode_val()?);

                let place = self.registers[reg].as_place().ok_or(Error::TypeMismatch)? as i32;
                let offset = offset.as_int().ok_or(Error::TypeMismatch)? as i32;

                self.registers[reg] = Value::Place((place + offset) as u32)
            }

            Illegal => return Error::Illegal(self.ip, self.code[self.ip]).error(),
        }
        false.okay()
    }

    fn push(&mut self, val: Value) {
        self.stack[(self.bp + self.sp) as usize] = val;
        self.sp += 1;
    }

    fn pop(&mut self) -> Result<Value> {
        self.sp -= 1;
        if self.sp < 0 {
            return Error::StackUnderflow.error();
        }
        self.stack[(self.bp + self.sp) as usize].okay()
    }

    fn binary_int_op(&mut self, op: impl Fn(i64, i64) -> i64) -> Result<()> {
        let (out, a, b) = (
            self.decode_register(),
            self.decode_register(),
            self.decode_register(),
        );
        let (a, b) = (
            self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
            self.registers[b].as_int().ok_or(Error::TypeMismatch)?,
        );
        self.registers[out] = Value::Int(op(a, b));
        ().okay()
    }

    fn binary_bool_op(&mut self, op: impl Fn(i64, i64) -> bool) -> Result<()> {
        let (out, a, b) = (
            self.decode_register(),
            self.decode_register(),
            self.decode_register(),
        );
        let (a, b) = (
            self.registers[a].as_int().ok_or(Error::TypeMismatch)?,
            self.registers[b].as_int().ok_or(Error::TypeMismatch)?,
        );
        self.registers[out] = if op(a, b) { Value::Int(1) } else { Value::Null };
        ().okay()
    }

    fn dealloc(&mut self) -> Result<()> {
        let reg = self.decode_register();
        let offset = self.registers[reg].as_place().ok_or(Error::TypeMismatch)? as u32;
        let i = self
            .allocations
            .iter()
            .enumerate()
            .find_map(|(i, a)| if a.offset == offset { i.some() } else { None })
            .ok_or(Error::AllocationNotFound)?;
        let allocation = self.allocations.remove(i);
        self.free.push(allocation);
        Ok(())
    }

    fn alloc(&mut self) -> Result<()> {
        let (reg, size) = (self.decode_register(), self.decode_register());
        let size = self.registers[size].as_int().ok_or(Error::TypeMismatch)? as u32;
        if let Some(allocation) =
            self.free.iter().enumerate().find_map(
                |(i, a)| {
                    if a.size >= size {
                        i.some()
                    } else {
                        None
                    }
                },
            )
        {
            let allocation = self.free.remove(allocation);
            self.registers[reg] = Value::Place(allocation.offset);
            self.allocations.push(allocation);
        } else {
            let offset = self.heap.len() as u32;
            self.registers[reg] = Value::Place(offset);
            self.allocations.push(Allocation::new(offset, size));
            self.heap.resize((offset + size) as usize, 0);
        }
        ().okay()
    }

    fn decode(&mut self) -> Opcode {
        self.ip += 1;
        self.code[self.ip - 1].into()
    }

    fn decode_register(&mut self) -> usize {
        self.ip += 1;
        self.code[self.ip - 1] as usize
    }

    fn decode_val(&mut self) -> Result<Value> {
        let res = Value::from_bytes(&self.code[self.ip..self.ip + 9]);
        self.ip += 9;
        res
    }

    fn disassemble(&mut self) -> Result<String> {
        use Opcode::*;
        let ip = self.ip;
        let res = match self.decode() {
            Halt => Ins::Halt.to_string().okay(),
            Load => Ins::Load(self.decode_register() as u8, self.decode_val()?)
                .to_string()
                .okay(),
            Alloc => Ins::Alloc(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            Dealloc => Ins::Dealloc(self.decode_register() as u8)
                .to_string()
                .okay(),

            Pop => Ins::Pop(self.decode_register() as u8).to_string().okay(),
            Push => Ins::Push(self.decode_register() as u8).to_string().okay(),
            IPush => Ins::IPush(self.decode_val()?).to_string().okay(),
            Peek => Ins::Peek(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            LPeek => Ins::LPeek(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            SStore => Ins::SStore(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),

            MemS => Ins::MemS(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            MemL => Ins::MemL(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),

            Jump => Ins::Jump(self.decode_register() as u8).to_string().okay(),
            JumpF => Ins::JumpF(self.decode_register() as u8).to_string().okay(),
            JumpB => Ins::JumpB(self.decode_register() as u8).to_string().okay(),

            JumpT => Ins::JumpT(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),

            Call => Ins::Call(self.decode_register() as u8).to_string().okay(),
            Return => Ins::Return(self.decode_register() as u8).to_string().okay(),

            Eqz => Ins::Eqz(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            Nez => Ins::Nez(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),

            Eq => Ins::Eq(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Ne => Ins::Ne(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Gt => Ins::Gt(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Lt => Ins::Lt(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Ge => Ins::Ge(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Le => Ins::Le(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),

            Add => Ins::Add(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Sub => Ins::Sub(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Mul => Ins::Mul(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),
            Div => Ins::Div(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
            .to_string()
            .okay(),

            IAdd => Ins::IAdd(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_val()?,
            )
            .to_string()
            .okay(),
            ISub => Ins::ISub(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_val()?,
            )
            .to_string()
            .okay(),
            IMul => Ins::IMul(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_val()?,
            )
            .to_string()
            .okay(),
            IDiv => Ins::IDiv(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_val()?,
            )
            .to_string()
            .okay(),

            Car => Ins::Car(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            Cdr => Ins::Cdr(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),

            PInc => Ins::PInc(self.decode_register() as u8).to_string().okay(),
            PDec => Ins::PDec(self.decode_register() as u8).to_string().okay(),
            POffset => Ins::POffset(self.decode_register() as u8, self.decode_register() as u8)
                .to_string()
                .okay(),
            PIOffset => Ins::PIOffset(self.decode_register() as u8, self.decode_val()?)
                .to_string()
                .okay(),

            Illegal => "Illegal".to_string().okay(),
        };
        self.ip = ip;
        res
    }
}
impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum Opcode {
    Halt = 0,

    Load,
    Alloc,
    Dealloc,

    Pop,
    Push,
    IPush,
    Peek,
    LPeek,
    SStore,

    MemS,
    MemL,

    Jump,
    JumpF,
    JumpB,

    JumpT,

    Call,
    Return,

    Eqz,
    Nez,

    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,

    Add,
    Sub,
    Mul,
    Div,

    IAdd,
    ISub,
    IMul,
    IDiv,

    Car,
    Cdr,

    PInc,
    PDec,
    POffset,
    PIOffset,

    Illegal = u8::MAX,
}
impl Opcode {
    const HALT: u8 = Self::Halt as u8;

    const LOAD: u8 = Self::Load as u8;
    const ALLOC: u8 = Self::Alloc as u8;
    const DEALLOC: u8 = Self::Dealloc as u8;

    const POP: u8 = Self::Pop as u8;
    const PUSH: u8 = Self::Push as u8;
    const IPUSH: u8 = Self::IPush as u8;
    const PEEK: u8 = Self::Peek as u8;
    const LPEEK: u8 = Self::LPeek as u8;
    const SSTORE: u8 = Self::SStore as u8;

    const MEMS: u8 = Self::MemS as u8;
    const MEML: u8 = Self::MemL as u8;

    const JUMP: u8 = Self::Jump as u8;
    const JUMPF: u8 = Self::JumpF as u8;
    const JUMPB: u8 = Self::JumpB as u8;

    const CALL: u8 = Self::Call as u8;
    const RETURN: u8 = Self::Return as u8;

    const JUMPT: u8 = Self::JumpT as u8;

    const EQZ: u8 = Self::Eqz as u8;
    const NEZ: u8 = Self::Nez as u8;

    const EQ: u8 = Self::Eq as u8;
    const NE: u8 = Self::Ne as u8;
    const GT: u8 = Self::Gt as u8;
    const LT: u8 = Self::Lt as u8;
    const GE: u8 = Self::Ge as u8;
    const LE: u8 = Self::Le as u8;

    const ADD: u8 = Self::Add as u8;
    const SUB: u8 = Self::Sub as u8;
    const MUL: u8 = Self::Mul as u8;
    const DIV: u8 = Self::Div as u8;

    const IADD: u8 = Self::IAdd as u8;
    const ISUB: u8 = Self::ISub as u8;
    const IMUL: u8 = Self::IMul as u8;
    const IDIV: u8 = Self::IDiv as u8;

    const CAR: u8 = Self::Car as u8;
    const CDR: u8 = Self::Cdr as u8;

    const PINC: u8 = Self::PInc as u8;
    const PDEC: u8 = Self::PDec as u8;
    const POFFSET: u8 = Self::POffset as u8;
    const PIOFFSET: u8 = Self::PIOffset as u8;

    const ILLEGAL: u8 = Self::Illegal as u8;
}

impl From<u8> for Opcode {
    fn from(byte: u8) -> Opcode {
        use Opcode::*;

        match byte {
            Self::HALT => Halt,

            Self::LOAD => Load,
            Self::ALLOC => Alloc,
            Self::DEALLOC => Dealloc,

            Self::POP => Pop,
            Self::PUSH => Push,
            Self::IPUSH => Push,
            Self::PEEK => Peek,
            Self::LPEEK => LPeek,
            Self::SSTORE => SStore,

            Self::MEMS => MemS,
            Self::MEML => MemL,

            Self::JUMP => Jump,
            Self::JUMPF => JumpF,
            Self::JUMPB => JumpB,

            Self::CALL => Call,
            Self::RETURN => Return,

            Self::JUMPT => JumpT,

            Self::EQZ => Eqz,
            Self::NEZ => Nez,

            Self::EQ => Eq,
            Self::NE => Ne,
            Self::GT => Gt,
            Self::LT => Lt,
            Self::GE => Ge,
            Self::LE => Le,

            Self::ADD => Add,
            Self::SUB => Sub,
            Self::MUL => Mul,
            Self::DIV => Div,

            Self::IADD => IAdd,
            Self::ISUB => ISub,
            Self::IMUL => IMul,
            Self::IDIV => IDiv,

            Self::CAR => Car,
            Self::CDR => Cdr,

            Self::PINC => PInc,
            Self::PDEC => PDec,
            Self::POFFSET => POffset,
            Self::PIOFFSET => PIOffset,

            Self::PIOFFSET..=Self::ILLEGAL => Illegal,
        }
    }
}
