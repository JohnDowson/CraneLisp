use super::atom::Atom;
use super::closure::Closure;
use super::Value;
use super::{asm::Ins, atom::FuncId};
use crate::value::intern;
use crate::{
    mem,
    value::{length, map_to_vec, nth, restn, SymId},
};
use somok::{CondPop, Somok};
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
}

pub struct Translator {
    active_closure: usize,
    next_label: usize,
    closures: Vec<Closure>,
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
        let label = self.next_func_id();

        self.ins(Ins::Label(label));

        self.translate(atom)?;

        self.ins(Ins::Pop(0));
        self.ins(Ins::Return(0));
        ().okay()
    }

    fn translate_fn(&mut self, args: mem::Ref, body: mem::Ref) -> Result<()> {
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
        ().okay()
    }

    fn translate(&mut self, atom: mem::Ref) -> Result<()> {
        match Atom::from_atom(atom.deref()) {
            Atom::Int(i) => {
                self.ins(Ins::Load(1, Value::Int(i)));
                self.ins(Ins::Push(1));
            }
            Atom::Symbol(sym) => {
                let var = self.get_local(&sym).ok_or(Error::UndefinedVariable)?;
                self.ins(Ins::Load(31, Value::Place(var as u32)));
                self.ins(Ins::Peek(1, 31));
                self.ins(Ins::Push(1))
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
                        todo!()
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
                    "loop" => {
                        todo!()
                    }
                    "if" => {
                        todo!()
                    }
                    a => todo!("{a:?}"),
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
