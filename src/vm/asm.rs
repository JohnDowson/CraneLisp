use super::Op;
use crate::atom::Atom;
use fnv::FnvHashMap;
use smol_str::SmolStr;
use tinyvec::ArrayVec;

#[derive(Debug, Clone, Copy)]
pub enum Ins {
    Label(usize),
    PushLambda(usize),
    Op(Op),
}

impl Ins {
    pub fn size(&self) -> usize {
        match self {
            Ins::Label(_) => 0,
            Ins::PushLambda(_) => 8,
            Ins::Op(_) => 8,
        }
    }
    fn assemble(self) -> ArrayVec<[u8; 8]> {
        let mut res = ArrayVec::new();
        match self {
            Ins::Label(_) => {}
            Ins::PushLambda(_) => res.extend([0; 8]),
            Ins::Op(op) => match op {
                Op::Push(constant) => {
                    res.push(0);
                    res.extend([0; 3]);
                    res.extend(constant.to_be_bytes())
                }
                Op::Pop => {
                    res.push(1);
                    res.extend([0; 7])
                }
                Op::LSet(slot) => {
                    res.push(2);
                    res.extend([0; 3]);
                    res.extend(slot.to_be_bytes())
                }
                Op::LGet(slot) => {
                    res.push(3);
                    res.extend([0; 3]);
                    res.extend(slot.to_be_bytes())
                }
                Op::Jump(offset) => {
                    res.push(4);
                    res.extend([0; 3]);
                    res.extend(offset.to_be_bytes())
                }
                Op::FJump(offset) => {
                    res.push(5);
                    res.extend([0; 3]);
                    res.extend(offset.to_be_bytes())
                }
                Op::TJump(offset) => {
                    res.push(6);
                    res.extend([0; 3]);
                    res.extend(offset.to_be_bytes())
                }
                Op::Call(argc) => {
                    res.push(7);
                    res.extend([0; 3]);
                    res.extend(argc.to_be_bytes())
                }
                Op::Return => {
                    res.push(8);
                    res.extend([0; 7])
                }
                Op::Halt => {
                    res.push(9);
                    res.extend([0; 7])
                }
                Op::Add => {
                    res.push(10);
                    res.extend([0; 7])
                }
                Op::Sub => {
                    res.push(11);
                    res.extend([0; 7])
                } // Op::LReg(reg) => {
                  //     res.push(12);
                  //     res.extend([0; 3]);
                  //     res.extend(reg.to_be_bytes())
                  // }
            },
        }
        res
    }
}

pub fn assemble(
    code: Vec<Ins>,
    mut const_table: Vec<Atom<0>>,
    symbol_table: Vec<SmolStr>,
) -> (Vec<u8>, Vec<Atom<0>>, Vec<SmolStr>) {
    let mut res = Vec::new();
    let mut labels = FnvHashMap::default();
    let mut label_refs = FnvHashMap::default();
    let mut code_len = 0;
    for ins in code.iter() {
        let ins_start = code_len;
        code_len += ins.size();
        if let Ins::PushLambda(label) = ins {
            let ins_end = code_len;

            label_refs
                .entry(*label)
                .or_insert_with(Vec::new)
                .push((ins_start, ins_end));
            continue;
        }

        if let Ins::Label(label) = ins {
            labels.insert(*label, code_len);
            continue;
        }
    }
    for ins in code {
        res.extend(ins.assemble());
    }
    for (label, refs) in label_refs {
        for (start, end) in refs {
            let slice = &mut res[start..end];
            slice[0] = 0;
            let label_loc = labels.get(&label).copied().unwrap();
            let bytes = (const_table.len() as u32).to_be_bytes();
            const_table.push(Atom::<0>::Func(label_loc as u64));
            for (i, byte) in bytes.into_iter().enumerate() {
                slice[i + 4] = byte;
            }
        }
    }
    (res, const_table, symbol_table)
}
