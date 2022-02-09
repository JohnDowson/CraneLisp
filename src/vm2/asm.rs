use fnv::FnvHashMap;
use tinyvec::ArrayVec;

use crate::vm2::Value;

use super::{atom::FuncId, Opcode};

pub fn assemble(code: Vec<Ins>) -> Vec<u8> {
    let mut res = Vec::new();
    let mut labels = FnvHashMap::default();
    let mut label_refs = FnvHashMap::default();
    let mut code_len = 0;
    for ins in code.iter() {
        let ins_start = code_len;
        code_len += ins.size();
        if ins.is_load_label() {
            let (reg, label) = ins.as_load_label().unwrap();

            let ins_end = code_len;

            label_refs
                .entry(label)
                .or_insert_with(Vec::new)
                .push((reg, ins_start, ins_end));
            continue;
        }

        if ins.is_label() {
            labels.insert(ins.as_label().unwrap(), code_len);
            continue;
        }
    }
    for ins in code {
        res.extend(ins.assemble());
    }
    for (label, refs) in label_refs {
        for (reg, start, end) in refs {
            let slice = &mut res[start..end];
            slice[0] = Opcode::LOAD;
            slice[1] = reg;
            let label_loc = labels.get(&label).copied().unwrap();
            let bytes = Value::Place(label_loc as u32).as_bytes();
            for (i, byte) in bytes.into_iter().enumerate() {
                slice[i + 2] = byte;
            }
        }
    }
    res
}
#[cfg(test)]
mod test {
    use super::super::Value as V;
    use super::super::Vm;
    use super::Ins::*;
    use crate::vm2::asm::assemble;
    use crate::vm2::atom::FuncId;
    use std::assert_matches::assert_matches;
    #[test]
    fn test_assemble() {
        let code = vec![Load(1, V::Int(60)), Load(2, V::Int(9)), Add(0, 1, 2), Halt];
        let code = assemble(code);
        let mut vm = Vm::with_code(code);
        let res = vm.run();
        assert_matches!(res, Ok(()));
        assert_matches!(vm.registers, [V::Int(69), ..])
    }

    #[test]
    fn push_pop() {
        let code = vec![Load(1, V::Int(69)), Push(1), Pop(0), Halt];
        let code = assemble(code);
        let mut vm = Vm::with_code(code);
        let res = vm.run();
        assert_matches!(res, Ok(()));
        assert_matches!(vm.registers, [V::Int(69), ..])
    }

    #[test]
    fn store_load() {
        let code = vec![
            Load(31, V::Int(18)),
            Alloc(31, 31),
            Load(30, V::Int(60)),
            MemS(31, 30),
            MemL(29, 31),
            PIOffset(31, V::Int(9)),
            Load(30, V::Int(9)),
            MemS(31, 30),
            MemL(28, 31),
            Add(0, 28, 29),
            Halt,
        ];
        let code = assemble(code);
        println!("{code:?}");
        let mut vm = Vm::with_code(code);
        let res = vm.run();
        assert_matches!(res, Ok(()));
        assert_matches!(vm.registers, [V::Int(69), ..]);
    }

    #[test]
    fn call_return() {
        let code = vec![
            LoadLabel(0, FuncId(0)),
            Load(1, V::Int(42)),
            Call(0),
            Load(2, V::Int(24)),
            Halt,
            Label(FuncId(0)),
            Load(1, V::Int(69)),
            Return(1),
        ];
        let code = assemble(code);
        let mut vm = Vm::with_code(code);
        let res = vm.run();
        assert_matches!(res, Ok(()));
        assert_matches!(vm.registers, [V::Place(36), V::Int(69), V::Int(24), ..]);
        assert_eq!(vm.r#return, V::Int(69));
    }

    #[test]
    fn test() {
        let code = vec![
            Load(1, V::Int(1)),
            Load(31, V::Place(1)),
            Peek(1, 31),
            Push(1),
            Pop(0),
            Halt,
        ];
        println!("{code:?}");
        let code = assemble(code);
        println!("{code:?}");
        let mut vm = Vm::with_code(code);
        let res = vm.run();
        assert_matches!(res, Ok(()));
        assert_matches!(vm.registers, [V::Null, ..]);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Ins {
    Halt,

    Load(u8, Value),
    Alloc(u8, u8),
    Dealloc(u8),

    MemS(u8, u8),
    MemL(u8, u8),

    Pop(u8),
    Push(u8),
    IPush(Value),
    Peek(u8, u8),
    LPeek(u8, u8),
    SStore(u8, u8),

    Jump(u8),
    JumpF(u8),
    JumpB(u8),

    Call(u8),
    Return(u8),

    JumpT(u8, u8),

    Eqz(u8, u8),
    Nez(u8, u8),

    Eq(u8, u8, u8),
    Ne(u8, u8, u8),
    Gt(u8, u8, u8),
    Lt(u8, u8, u8),
    Ge(u8, u8, u8),
    Le(u8, u8, u8),

    Add(u8, u8, u8),
    Sub(u8, u8, u8),
    Mul(u8, u8, u8),
    Div(u8, u8, u8),
    IAdd(u8, u8, Value),
    ISub(u8, u8, Value),
    IMul(u8, u8, Value),
    IDiv(u8, u8, Value),

    Car(u8, u8),
    Cdr(u8, u8),

    PInc(u8),
    PDec(u8),
    POffset(u8, u8),
    PIOffset(u8, Value),

    Label(FuncId),
    LoadLabel(u8, FuncId),
}

impl std::fmt::Display for Ins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ins::Halt => write!(f, "Halt"),
            Ins::Load(reg, imm) => write!(f, "Load ${} #{}", reg, imm),
            Ins::Alloc(reg, size) => write!(f, "Alloc ${} ${}", reg, size),

            Ins::Dealloc(reg) => write!(f, "Dealloc ${}", reg),

            Ins::Pop(reg) => write!(f, "Pop ${}", reg),
            Ins::Push(reg) => write!(f, "Push ${}", reg),
            Ins::IPush(imm) => write!(f, "IPush #{}", imm),
            Ins::Peek(reg, slot) => write!(f, "Peek ${} ${}", reg, slot),
            Ins::LPeek(reg, slot) => write!(f, "LPeek ${} ${}", reg, slot),
            Ins::SStore(slot, value) => write!(f, "SStore ${} ${}", slot, value),

            Ins::MemS(loc, val) => write!(f, "MemS ${} ${}", loc, val),
            Ins::MemL(reg, loc) => write!(f, "MemL ${} ${}", reg, loc),

            Ins::Jump(t) => write!(f, "Jump ${}", t),
            Ins::JumpF(t) => write!(f, "JumpF ${}", t),
            Ins::JumpB(t) => write!(f, "JumpB ${}", t),

            Ins::Call(t) => write!(f, "Call ${}", t),
            Ins::Return(t) => write!(f, "Return ${}", t),

            Ins::JumpT(reg, t) => write!(f, "JumpT ${} ${}", reg, t),

            Ins::Eqz(reg, a) => write!(f, "Eq ${} ${}", reg, a),
            Ins::Nez(reg, a) => write!(f, "Ne ${} ${}", reg, a),

            Ins::Eq(reg, a, b) => write!(f, "Eq ${} ${} ${}", reg, a, b),
            Ins::Ne(reg, a, b) => write!(f, "Ne ${} ${} ${}", reg, a, b),
            Ins::Gt(reg, a, b) => write!(f, "Gt ${} ${} ${}", reg, a, b),
            Ins::Lt(reg, a, b) => write!(f, "Lt ${} ${} ${}", reg, a, b),
            Ins::Ge(reg, a, b) => write!(f, "Ge ${} ${} ${}", reg, a, b),
            Ins::Le(reg, a, b) => write!(f, "Le ${} ${} ${}", reg, a, b),

            Ins::Add(reg, a, b) => write!(f, "Add ${} ${} ${}", reg, a, b),
            Ins::Sub(reg, a, b) => write!(f, "Sub ${} ${} ${}", reg, a, b),
            Ins::Mul(reg, a, b) => write!(f, "Mul ${} ${} ${}", reg, a, b),
            Ins::Div(reg, a, b) => write!(f, "Div ${} ${} ${}", reg, a, b),
            Ins::IAdd(reg, a, b) => write!(f, "IAdd ${} ${} #{}", reg, a, b),
            Ins::ISub(reg, a, b) => write!(f, "ISub ${} ${} #{}", reg, a, b),
            Ins::IMul(reg, a, b) => write!(f, "IMul ${} ${} #{}", reg, a, b),
            Ins::IDiv(reg, a, b) => write!(f, "IDiv ${} ${} #{}", reg, a, b),

            Ins::Car(reg, pair) => write!(f, "Car ${} ${}", reg, pair),
            Ins::Cdr(reg, pair) => write!(f, "Cdr ${} ${}", reg, pair),

            Ins::PInc(reg) => write!(f, "PInc ${}", reg),
            Ins::PDec(reg) => write!(f, "PDec ${} ", reg),
            Ins::POffset(reg, offset) => write!(f, "POffset ${} ${}", reg, offset),
            Ins::PIOffset(reg, offset) => write!(f, "PIOffset ${} #{}", reg, offset),

            Ins::Label(l) => write!(f, "Label {:?}", l),
            Ins::LoadLabel(reg, l) => write!(f, "LoadLabel ${} @{:?}", reg, l),
        }
    }
}

impl Ins {
    pub fn assemble(self) -> ArrayVec<[u8; 16]> {
        use super::Opcode as Oc;
        let mut res = ArrayVec::new();
        match self {
            Ins::Halt => res.push(Oc::HALT),
            Ins::Load(reg, imm) => {
                res.push(Oc::LOAD);
                res.push(reg);
                res.extend(imm.as_bytes());
            }
            Ins::Alloc(reg, size) => {
                res.push(Oc::ALLOC);
                res.push(reg);
                res.push(size);
            }
            Ins::Dealloc(reg) => {
                res.push(Oc::DEALLOC);
                res.push(reg);
            }

            Ins::Pop(reg) => {
                res.push(Oc::POP);
                res.push(reg);
            }
            Ins::Push(reg) => {
                res.push(Oc::PUSH);
                res.push(reg);
            }
            Ins::IPush(imm) => {
                res.push(Oc::IPUSH);
                res.extend(imm.as_bytes());
            }
            Ins::Peek(reg, slot) => {
                res.push(Oc::PEEK);
                res.push(reg);
                res.push(slot);
            }
            Ins::LPeek(reg, slot) => {
                res.push(Oc::LPEEK);
                res.push(reg);
                res.push(slot);
            }
            Ins::SStore(slot, val) => {
                res.push(Oc::PUSH);
                res.push(slot);
                res.push(val);
            }

            Ins::MemS(loc, val) => {
                res.push(Oc::MEMS);
                res.push(loc);
                res.push(val);
            }
            Ins::MemL(reg, loc) => {
                res.push(Oc::MEML);
                res.push(reg);
                res.push(loc);
            }

            Ins::Jump(t) => {
                res.push(Oc::JUMP);
                res.push(t);
            }
            Ins::JumpF(t) => {
                res.push(Oc::JUMPF);
                res.push(t);
            }
            Ins::JumpB(t) => {
                res.push(Oc::JUMPB);
                res.push(t);
            }

            Ins::Call(t) => {
                res.push(Oc::CALL);
                res.push(t);
            }
            Ins::Return(t) => {
                res.push(Oc::RETURN);
                res.push(t);
            }

            Ins::JumpT(reg, t) => {
                res.push(Oc::JUMPT);
                res.push(reg);
                res.push(t);
            }

            Ins::Eqz(reg, a) => {
                res.push(Oc::EQZ);
                res.push(reg);
                res.push(a);
            }
            Ins::Nez(reg, a) => {
                res.push(Oc::NEZ);
                res.push(reg);
                res.push(a);
            }

            Ins::Eq(reg, a, b) => {
                res.push(Oc::EQ);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Ne(reg, a, b) => {
                res.push(Oc::NE);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Gt(reg, a, b) => {
                res.push(Oc::GT);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Lt(reg, a, b) => {
                res.push(Oc::LT);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Ge(reg, a, b) => {
                res.push(Oc::GE);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Le(reg, a, b) => {
                res.push(Oc::LE);
                res.push(reg);
                res.push(a);
                res.push(b);
            }

            Ins::Add(reg, a, b) => {
                res.push(Oc::ADD);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Sub(reg, a, b) => {
                res.push(Oc::SUB);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Mul(reg, a, b) => {
                res.push(Oc::MUL);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::Div(reg, a, b) => {
                res.push(Oc::DIV);
                res.push(reg);
                res.push(a);
                res.push(b);
            }
            Ins::IAdd(reg, a, b) => {
                res.push(Oc::IADD);
                res.push(reg);
                res.push(a);
                res.extend(b.as_bytes());
            }
            Ins::ISub(reg, a, b) => {
                res.push(Oc::ISUB);
                res.push(reg);
                res.push(a);
                res.extend(b.as_bytes());
            }
            Ins::IMul(reg, a, b) => {
                res.push(Oc::IMUL);
                res.push(reg);
                res.push(a);
                res.extend(b.as_bytes());
            }
            Ins::IDiv(reg, a, b) => {
                res.push(Oc::IDIV);
                res.push(reg);
                res.push(a);
                res.extend(b.as_bytes());
            }

            Ins::Car(reg, pair) => {
                res.push(Oc::CAR);
                res.push(reg);
                res.push(pair);
            }
            Ins::Cdr(reg, pair) => {
                res.push(Oc::CDR);
                res.push(reg);
                res.push(pair);
            }

            Ins::PInc(reg) => {
                res.push(Oc::PINC);
                res.push(reg);
            }
            Ins::PDec(reg) => {
                res.push(Oc::PDEC);
                res.push(reg);
            }
            Ins::POffset(reg, offset) => {
                res.push(Oc::POFFSET);
                res.push(reg);
                res.push(offset);
            }
            Ins::PIOffset(reg, offset) => {
                res.push(Oc::PIOFFSET);
                res.push(reg);
                res.extend(offset.as_bytes());
            }

            Ins::Label(_) => {}
            Ins::LoadLabel(..) => res.extend([Oc::ILLEGAL; Self::Load(0, Value::Int(0)).size()]),
        }
        res
    }

    pub const fn size(&self) -> usize {
        match self {
            Ins::Halt => 1,

            Ins::Load(_, _) => 11,
            Ins::Alloc(_, _) => 3,
            Ins::Dealloc(_) => 2,

            Ins::MemS(_, _) => 3,
            Ins::MemL(_, _) => 3,

            Ins::Pop(_) => 2,
            Ins::Push(_) => 2,
            Ins::IPush(_) => 10,
            Ins::Peek(_, _) => 3,
            Ins::LPeek(_, _) => 3,
            Ins::SStore(_, _) => 3,

            Ins::Jump(_) => 2,
            Ins::JumpF(_) => 2,
            Ins::JumpB(_) => 2,

            Ins::Call(_) => 2,
            Ins::Return(_) => 2,

            Ins::JumpT(_, _) => 3,

            Ins::Eqz(_, _) => 3,
            Ins::Nez(_, _) => 3,

            Ins::Eq(_, _, _) => 4,
            Ins::Ne(_, _, _) => 4,
            Ins::Gt(_, _, _) => 4,
            Ins::Lt(_, _, _) => 4,
            Ins::Ge(_, _, _) => 4,
            Ins::Le(_, _, _) => 4,

            Ins::Add(_, _, _) => 4,
            Ins::Sub(_, _, _) => 4,
            Ins::Mul(_, _, _) => 4,
            Ins::Div(_, _, _) => 4,

            Ins::IAdd(_, _, _) => 4,
            Ins::ISub(_, _, _) => 4,
            Ins::IMul(_, _, _) => 4,
            Ins::IDiv(_, _, _) => 4,

            Ins::Car(_, _) => 3,
            Ins::Cdr(_, _) => 3,

            Ins::PInc(_) => 2,
            Ins::PDec(_) => 2,
            Ins::POffset(_, _) => 3,
            Ins::PIOffset(_, _) => 11,

            Ins::Label(_) => 0,
            Ins::LoadLabel(_, _) => Self::size(&Self::Load(0, Value::Int(0))),
        }
    }

    /// Returns `true` if the ins is [`Label`].
    ///
    /// [`Label`]: Ins::Label
    pub fn is_label(&self) -> bool {
        matches!(self, Self::Label(..))
    }

    /// Returns `true` if the ins is [`LoadLabel`].
    ///
    /// [`LoadLabel`]: Ins::LoadLabel
    pub fn is_load_label(&self) -> bool {
        matches!(self, Self::LoadLabel(..))
    }

    pub fn as_label(&self) -> Option<FuncId> {
        if let Self::Label(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_load_label(&self) -> Option<(u8, FuncId)> {
        if let Self::LoadLabel(reg, label) = self {
            Some((*reg, *label))
        } else {
            None
        }
    }
}
