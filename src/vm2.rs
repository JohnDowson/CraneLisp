use self::asm::Ins;
use crate::symbol::SymId;
use somok::Somok;

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

pub struct Vm {
    pub code: Vec<u8>,
    pub registers: [Value; 32],
    pub r#return: Value,
    reminder: i64,
    pub ip: usize,
    pub bp: isize,
    pub sp: isize,
    pub stack: [Value; 16],
}

impl Vm {
    pub fn new() -> Self {
        Self {
            code: Default::default(),
            registers: [Value::Null; 32],
            r#return: Value::Null,
            reminder: 0,
            ip: 0,
            bp: 0,
            sp: 0,
            stack: [Value::Null; 16],
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
            println!("\t{}:\t{}", self.ip, dis);
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

            JumpT => {
                let (reg, offset) = (self.decode_register(), self.decode_register());
                let offset = self.registers[offset]
                    .as_place()
                    .ok_or(Error::TypeMismatch)?;
                if !self.registers[reg].is_null() {
                    self.ip += offset;
                }
            }
            JumpN => {
                let (reg, offset) = (self.decode_register(), self.decode_register());
                let offset = self.registers[offset]
                    .as_place()
                    .ok_or(Error::TypeMismatch)?;
                if self.registers[reg].is_null() {
                    self.ip += offset;
                }
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
            Cons => {
                let (reg, car, cdr) = (
                    self.decode_register(),
                    self.decode_register(),
                    self.decode_register(),
                );
                let car = self.registers[car].as_place().ok_or(Error::TypeMismatch)? as u32;
                let cdr = self.registers[cdr].as_place().ok_or(Error::TypeMismatch)? as u32;
                self.registers[reg] = Value::Pair(car, cdr)
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
            JumpN => Ins::JumpN(self.decode_register() as u8, self.decode_register() as u8)
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
            Cons => Ins::Cons(
                self.decode_register() as u8,
                self.decode_register() as u8,
                self.decode_register() as u8,
            )
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
    JumpN,

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
    Cons,

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

    const JUMPT: u8 = Self::JumpT as u8;
    const JUMPN: u8 = Self::JumpN as u8;

    const CALL: u8 = Self::Call as u8;
    const RETURN: u8 = Self::Return as u8;

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
    const CONS: u8 = Self::Cons as u8;

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

            Self::JUMPT => JumpT,
            Self::JUMPN => JumpN,

            Self::CALL => Call,
            Self::RETURN => Return,

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
            Self::CONS => Cons,

            Self::PINC => PInc,
            Self::PDEC => PDec,
            Self::POFFSET => POffset,
            Self::PIOFFSET => PIOffset,

            Self::PIOFFSET..=Self::ILLEGAL => Illegal,
        }
    }
}
