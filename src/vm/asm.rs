use super::{
    atom::Atom,
    atom::Object,
    closure::{Closure, RuntimeFn},
    memman::alloc,
    Op,
};
use fnv::FnvHashMap;
use somok::{Leaksome, Somok};
use std::rc::Rc;
use tinyvec::ArrayVec;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ins {
    PatchMe,
    PushLambda(usize),
    Op(Op),
}

impl Ins {
    pub fn size(&self) -> usize {
        match self {
            Ins::PatchMe => 0,
            Ins::PushLambda(_) => 8,
            Ins::Op(_) => 8,
        }
    }
    fn assemble(self) -> ArrayVec<[u8; 8]> {
        let mut res = ArrayVec::new();
        match self {
            Ins::PatchMe => panic!("PatchMe encountered when assembling!"),
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
                }
                Op::Car => {
                    res.push(12);
                    res.extend([0; 7])
                }
                Op::Cdr => {
                    res.push(13);
                    res.extend([0; 7])
                }
                Op::Cons => {
                    res.push(14);
                    res.extend([0; 7])
                }
            },
        }
        res
    }
}

pub fn assemble(closures: Vec<Closure>) -> FnvHashMap<usize, Rc<RuntimeFn>> {
    let (childless, has_children): (Vec<_>, _) =
        closures.into_iter().partition(|c| c.children.is_empty());
    let mut assembled = FnvHashMap::default();

    for closure in childless {
        let mut res = Vec::new();
        for ins in closure.assembly {
            res.extend(ins.assemble());
        }
        let func = RuntimeFn {
            assembly: res.into_boxed_slice().leak(),
            locals: closure.locals,
            upvalues: closure.upvalues,
            const_table: closure.const_table,
        };
        assembled.insert(closure.id, Rc::new(func));
    }

    let (mut all_children_assembled, mut not_all_children_assembled): (Vec<_>, _) = has_children
        .into_iter()
        .partition(|c| c.children.iter().all(|c| assembled.contains_key(c)));
    loop {
        for mut closure in all_children_assembled {
            let mut res = Vec::new();
            for ins in closure.assembly {
                if let Ins::PushLambda(id) = ins {
                    let fun = assembled.get(&id).cloned().unwrap();
                    let mut const_id = None;
                    for i in 0..closure.const_table.len() {
                        if closure.const_table[i]
                            == Atom::new_obj(alloc(Object::new_func(fun.clone())))
                        {
                            const_id = i.some();
                            break;
                        }
                    }
                    if const_id.is_none() {
                        const_id = closure.const_table.len().some();
                        closure
                            .const_table
                            .push(Atom::new_obj(alloc(Object::new_func(fun))));
                    }

                    let ins = Ins::Op(Op::Push(const_id.unwrap() as u32));
                    res.extend(ins.assemble());
                } else {
                    res.extend(ins.assemble());
                }
            }
            let func = RuntimeFn {
                assembly: res.into_boxed_slice().leak(),
                locals: closure.locals,
                upvalues: closure.upvalues,
                const_table: closure.const_table,
            };
            assembled.insert(closure.id, Rc::new(func));
        }
        (all_children_assembled, not_all_children_assembled) = not_all_children_assembled
            .into_iter()
            .partition(|c| c.children.iter().all(|c| assembled.contains_key(c)));
        if all_children_assembled.is_empty() && not_all_children_assembled.is_empty() {
            break;
        }
    }
    assembled
}
