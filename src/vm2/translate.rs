use super::atom::Atom;
use super::closure::Closure;
use super::Value;
use super::{asm::Ins, atom::FuncId};
use crate::{
    mem,
    value::{length, map_to_vec, nth, restn, SymId},
};
use somok::Somok;
use std::ops::Deref;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    OutOfRegisters,
    OutOfUsedRegisters,
    UndefinedVariable,
    NoScopes,
    NotAPlace,
    UnexpectedAtom,
    NoPatchFrame,
}

pub enum Returnable {
    Closure,
    Loop,
}

pub struct Patch {
    loc: usize,
    start: Option<usize>,
    end: Option<usize>,
}
impl Patch {
    fn new(loc: usize) -> Self {
        Self {
            loc,
            start: None,
            end: None,
        }
    }
    fn new_with_start(loc: usize, start: usize) -> Self {
        Self {
            loc,
            start: start.some(),
            end: None,
        }
    }
    fn new_with_end(loc: usize, end: usize) -> Self {
        Self {
            loc,
            start: None,
            end: end.some(),
        }
    }
    fn new_full(loc: usize, start: usize, end: usize) -> Self {
        Self {
            loc,
            start: start.some(),
            end: end.some(),
        }
    }
}

pub struct Translator {
    active_closure: usize,
    next_label: usize,
    closures: Vec<Closure>,
    returnables: Vec<Returnable>,
    patch_me: Vec<Vec<Patch>>,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            active_closure: 0,
            next_label: 0,
            closures: vec![Closure {
                upvalues: Default::default(),
                assembly: Default::default(),
                scope: Default::default(),
                parent: Default::default(),
                bp: 0,
                sp: 0,
            }],
            returnables: Default::default(),
            patch_me: Default::default(),
        }
    }
    pub fn emit(&self) -> Vec<Ins> {
        let mut res = vec![Ins::LoadLabel(0, FuncId(0)), Ins::Call(0), Ins::Halt];
        for closure in &self.closures {
            res.extend(closure.assembly.clone());
        }
        res
    }

    pub fn translate_top_level(&mut self, atom: mem::Ref) -> Result<()> {
        self.closure_mut().sp = 2;
        self.returnables.push(Returnable::Closure);
        let label = self.next_func_id();

        self.ins(Ins::Label(label));

        self.translate(atom)?;

        self.ins(Ins::Pop(0));
        self.ins(Ins::Return(0));
        self.returnables.pop();
        ().okay()
    }

    fn translate_fn(&mut self, args: mem::Ref, body: mem::Ref) -> Result<()> {
        self.returnables.push(Returnable::Closure);
        self.new_closure();
        let args = map_to_vec(args, |a| a.as_symbol().ok_or(Error::UnexpectedAtom))
            .into_iter()
            .collect::<Result<Vec<SymId>>>()?;

        for arg in args {
            self.closure_mut().scope.vars.insert(arg, 0);
        }

        let label = self.next_func_id();

        self.ins(Ins::Label(label));

        self.translate_begin(body)?;

        self.ins(Ins::Pop(0));
        self.ins(Ins::Return(0));
        self.returnables.pop();
        self.pop_closure();
        ().okay()
    }

    fn translate(&mut self, atom: mem::Ref) -> Result<()> {
        match Atom::from_atom(atom.deref()) {
            Atom::Int(i) => {
                self.ins(Ins::Load(1, Value::Int(i)));
                self.ins(Ins::Push(1));
            }
            Atom::Symbol(sym) if &*sym == "nil" => {
                self.ins(Ins::Load(1, Value::Null));
                self.ins(Ins::Push(1));
            }
            Atom::Symbol(sym) if &*sym == "t" => {
                self.ins(Ins::Load(1, Value::Int(1)));
                self.ins(Ins::Push(1));
            }
            Atom::Symbol(sym) => {
                let var = self.get_local(&sym).ok_or(Error::UndefinedVariable)?;
                self.ins(Ins::Load(31, Value::Place(var as u32)));
                self.ins(Ins::Peek(1, 31));
                self.ins(Ins::Push(1))
            }
            Atom::Null => {
                self.ins(Ins::Load(1, Value::Null));
                self.ins(Ins::Push(1));
            }
            Atom::Pair(p) => match Atom::from_atom(&*(p.car)) {
                Atom::Symbol(s) => match &*s {
                    "+" => {
                        let argc = length(p.cdr.clone()) as isize;
                        for i in 0..argc {
                            self.translate(nth(p.cdr.clone(), i))?;
                        }

                        self.ins(Ins::Pop(1));
                        self.ins(Ins::Pop(2));
                        self.ins(Ins::Add(1, 1, 2));
                        self.ins(Ins::Push(1));
                    }
                    "begin" => {
                        self.translate_begin(restn(atom.clone(), 0))?;
                    }
                    "quote" => {
                        self.translate_quote(nth(p.cdr.clone(), 0));
                    }
                    "fn" => self.translate_fn(nth(p.cdr.clone(), 0), nth(p.cdr.clone(), 1))?,
                    "set" => match Atom::from_atom(nth(p.cdr.clone(), 0).deref()) {
                        Atom::Pair(_) => todo!("Handle places"),
                        Atom::Symbol(sym) => {
                            if let Some(var) = self.get_local(&sym) {
                                self.translate(nth(p.cdr.clone(), 1))?;
                                self.ins(Ins::Load(31, Value::Place(var as u32)));
                                self.ins(Ins::SStore(31, 1))
                            } else if let Some(_var) = self.get_outer(&sym) {
                                todo!()
                            } else {
                                self.translate(nth(p.cdr.clone(), 1))?;
                                let _var = self.new_local(sym);
                            }
                        }
                        _ => return Error::UnexpectedAtom.error(),
                    },
                    "return" => {
                        if let Some(r) = self.returnables.last() {
                            match r {
                                Returnable::Closure => {
                                    self.ins(Ins::Return(1));
                                }
                                Returnable::Loop => {
                                    let dummy_load = self.closure().assembly.len();
                                    self.ins(Ins::Load(31, Value::Null));
                                    self.ins(Ins::JumpF(31));
                                    let ret_end = self.current_byte_offset();
                                    self.patch_me
                                        .last_mut()
                                        .ok_or(Error::NoPatchFrame)?
                                        .push(Patch::new_with_start(dummy_load, ret_end));
                                }
                            }
                        }
                    }
                    "loop" => {
                        self.returnables.push(Returnable::Loop);
                        self.patch_me.push(Default::default());
                        let loop_start = self.current_byte_offset();
                        self.translate(nth(p.cdr.clone(), 0))?;

                        self.ins(Ins::Pop(1));
                        let dummy_load = self.closure().assembly.len();
                        self.ins(Ins::Load(31, Value::Null));
                        self.ins(Ins::JumpB(31));
                        let loop_end = self.current_byte_offset();
                        let offset = loop_end - loop_start;
                        self.backpatch(dummy_load, Ins::Load(31, Value::Place((offset) as u32)));
                        for patch in self.patch_me.pop().ok_or(Error::NoPatchFrame)? {
                            if let Patch {
                                loc,
                                start: Some(start),
                                end: _,
                            } = patch
                            {
                                let offset = loop_end - start;
                                self.backpatch(loc, Ins::Load(31, Value::Place((offset) as u32)));
                            }
                        }
                        self.returnables.pop();
                    }
                    "if" => {
                        self.translate(nth(p.cdr.clone(), 0))?;
                        self.ins(Ins::Pop(31));

                        let dummy_load = self.closure().assembly.len();
                        self.ins(Ins::Load(31, Value::Null));

                        self.ins(Ins::JumpN(1, 31));
                        let start = self.current_byte_offset();
                        self.translate(nth(p.cdr.clone(), 1))?;
                        let dummy_load2 = self.closure().assembly.len();
                        self.ins(Ins::Load(31, Value::Null));
                        self.ins(Ins::JumpF(31));

                        let end_t = self.current_byte_offset();

                        let offset = end_t - start;
                        self.backpatch(dummy_load, Ins::Load(31, Value::Place((offset) as u32)));
                        self.translate(nth(p.cdr.clone(), 2))?;
                        let end = self.current_byte_offset();
                        let offset = end - end_t;
                        self.backpatch(dummy_load2, Ins::Load(31, Value::Place((offset) as u32)));
                    }
                    a => todo!("{a}"),
                },
                a => todo!("{a:?}"),
            },
            a => todo!("{a:?}"),
        };
        ().okay()
    }

    fn translate_begin(&mut self, atom: mem::Ref) -> Result<()> {
        match length(atom.clone()) {
            0 => ().okay(),
            1 => self.translate(nth(atom, 0)),
            _ => {
                self.translate(nth(atom.clone(), 0))?;
                // self.closure_mut()
                // .assembly
                // .cond_pop(|ins| matches!(ins, Ins::Push(_)));
                self.translate_begin(atom.as_pair().unwrap().cdr.clone())
            }
        }
    }

    fn translate_quote(&mut self, atom: mem::Ref) -> Result<()> {
        match Atom::from_atom(&*atom) {
            Atom::Null => self.ins(Ins::Load(1, Value::Null)),
            Atom::Int(i) => self.ins(Ins::Load(1, Value::Int(i))),
            Atom::Float(_f) => todo!("Implement floats"),
            Atom::Pair(_) => self.translate_list(atom, true)?,
            Atom::Symbol(s) => self.ins(Ins::Load(1, Value::Symbol(s))),
            Atom::Func(_) => unreachable!("This would require atom to be evaluated"),
        };
        self.ins(Ins::Push(1));
        ().okay()
    }

    fn translate_list(&mut self, atom: mem::Ref, quoted: bool) -> Result<()> {
        use Ins::*;

        let pair = match &*atom {
            crate::value::Atom::Null => {
                self.ins(Ins::Load(1, Value::Null));
                return ().okay();
            }
            crate::value::Atom::Pair(p) => p,
            _ => return Error::UnexpectedAtom.error(),
        };
        if quoted {
            let car = match Atom::from_atom(&*pair.car) {
                Atom::Null => Value::Null,
                Atom::Int(i) => Value::Int(i),
                Atom::Float(_) => todo!("Implement floats"),
                Atom::Pair(_) => todo!(),
                Atom::Symbol(s) => Value::Symbol(s),
                Atom::Func(_) => unreachable!("This would require atom to be evaluated"),
            };
            let cdr = match Atom::from_atom(&*pair.cdr) {
                Atom::Null => Value::Null,
                Atom::Int(i) => Value::Int(i),
                Atom::Float(_) => todo!("Implement floats"),
                Atom::Pair(_) => todo!(),
                Atom::Symbol(s) => Value::Symbol(s),
                Atom::Func(_) => unreachable!("This would require atom to be evaluated"),
            };
            // Load car, alloc, store car
            // Load cdr, alloc, store cdr
            // cons car, cdr
            dbg! {(&car, &cdr)};
            self.ins(Load(31, car));
            self.ins(Load(30, Value::Int(9)));
            self.ins(Alloc(30, 30));
            self.ins(MemS(30, 31));

            self.ins(Load(29, cdr));
            self.ins(Load(28, Value::Int(9)));
            self.ins(Alloc(28, 28));
            self.ins(MemS(28, 29));

            self.ins(Cons(1, 30, 28));
        } else {
        }
        ().okay()
    }

    fn current_byte_offset(&self) -> usize {
        self.closure()
            .assembly
            .iter()
            .fold(0, |acc, ins| acc + ins.size())
    }

    fn backpatch(&mut self, loc: usize, ins: Ins) {
        self.closure_mut().assembly[loc] = ins;
    }

    fn next_func_id(&mut self) -> FuncId {
        let fid = FuncId(self.next_label);
        self.next_label += 1;
        fid
    }

    fn get_local(&self, sym: &SymId) -> Option<usize> {
        if let Some(&var) = self.closure().scope.vars.get(sym) {
            return var.some();
        }
        None
    }
    fn get_outer(&self, sym: &SymId) -> Option<usize> {
        let mut parent = self.closure().parent;
        while let Some(p) = parent {
            let closure = &self.closures[p];
            if let Some(&var) = closure.scope.vars.get(sym) {
                return var.some();
            }
            parent = closure.parent
        }
        None
    }

    fn new_bp(&self) -> usize {
        let closure = self.closure();
        let mut bp = closure.sp;
        let mut parent = closure.parent;
        while let Some(p) = parent {
            let closure = &self.closures[p];
            bp += closure.sp;
            parent = closure.parent
        }
        bp
    }

    fn new_local(&mut self, sym: SymId) -> usize {
        let slot = self.new_bp();
        self.closure_mut().scope.vars.insert(sym, slot);
        slot
    }

    fn pop_closure(&mut self) {
        self.active_closure = if let Some(p) = self.closure().parent {
            p
        } else {
            0
        }
    }

    fn new_closure(&mut self) {
        let bp = self.new_bp();
        self.closures.push(Closure {
            upvalues: Default::default(),
            assembly: Default::default(),
            scope: Default::default(),
            parent: Some(self.active_closure),
            bp,
            sp: 0,
        })
    }

    fn closure(&self) -> &Closure {
        &self.closures[self.active_closure]
    }

    fn closure_mut(&mut self) -> &mut Closure {
        &mut self.closures[self.active_closure]
    }

    fn ins(&mut self, ins: Ins) {
        match ins {
            Ins::Pop(_) => self.closure_mut().sp -= 1,
            Ins::Push(_) => self.closure_mut().sp += 1,
            Ins::IPush(_) => self.closure_mut().sp += 1,
            _ => (),
        }
        self.closures[self.active_closure].assembly.push(ins)
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}
