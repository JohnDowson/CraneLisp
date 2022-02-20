use std::fmt::Debug;

use super::asm::Ins;
use super::atom2::Atom;
use super::closure::{Closure, Local, ReturnToPatch};
use super::Op;
use crate::parser::expr::Expr;
use crate::symbol::SymId;
use crate::Span;
use smol_str::SmolStr;
use somok::Somok;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
#[derive(Debug)]
pub enum ErrorKind {
    UndefinedVariable,
    NoScopes,
    NotAPlace,
    UnexpectedAtom,
    ArityMismatch,
    RestMustBeLast,
    InvalidIf,
    InvalidLoop,
}
use ErrorKind::*;
fn error<T>(kind: ErrorKind, span: Span) -> Result<T> {
    Error { kind, span }.error()
}

pub struct Translator {
    active_closure: usize,
    next_func_id: usize,
    closures: Vec<Closure>,
    symbol_table: Vec<SmolStr>,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            active_closure: 0,
            next_func_id: 1,
            closures: vec![Closure {
                assembly: Default::default(),
                id: 0,
                parent: Default::default(),
                children: Default::default(),
                bp: 0,
                sp: 0,
                const_table: Default::default(),
                locals: Default::default(),
                upvalues: Default::default(),
                loops: Default::default(),
            }],
            symbol_table: Default::default(),
        }
    }

    pub fn emit(self) -> (Vec<Closure>, Vec<SmolStr>) {
        (self.closures, self.symbol_table)
    }

    pub fn translate_toplevel(&mut self, expr: &Expr) -> Result<()> {
        self.translate(expr)?;

        self.ins(Op::Halt);

        ().okay()
    }

    fn translate(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Symbol(s, span) => match *s {
                "t" => self.push_const(Atom::new_uint(1)),
                "f" => self.push_const(Atom::null()),
                "nil" => self.push_const(Atom::null()),
                sym => {
                    let sym = self.intern(sym);
                    let local = self.get_local(&sym);
                    let outer = self.get_upvalue(&sym);
                    if let Some(local) = local {
                        self.ins(Op::LGet(local as u32));
                    } else if let Some(_outer) = outer {
                        todo!()
                    } else {
                        return error(UndefinedVariable, *span);
                    }
                }
            },
            Expr::Float(f, _) => self.push_const(Atom::new_float(*f)),
            Expr::Integer(i, _) => self.push_const(Atom::new_int(*i)),
            Expr::Null(_) => self.push_const(Atom::null()),
            Expr::List(exprs, _span) => {
                if exprs.is_empty() {
                    self.push_const(Atom::null());
                }
                let (first, rest) = exprs.split_at(1);
                match first.first() {
                    Some(Expr::Symbol(s, span)) => match *s {
                        "fn" => self.translate_lambda(&rest[0], &rest[1])?,
                        "begin" => self.translate_begin(rest)?,
                        "set" => {
                            if let Expr::Symbol(sym, _span) = rest[0] {
                                let sym = self.intern(sym);
                                let local = self.get_local(&sym);
                                let outer = self.get_upvalue(&sym);
                                if let Some(local) = local {
                                    self.translate(&rest[1])?;
                                    self.ins(Op::LSet(local as u32))
                                } else if let Some(_outer) = outer {
                                    todo!()
                                } else {
                                    self.new_local(sym);
                                    self.translate(&rest[1])?;
                                }
                            }
                        }
                        "+" => {
                            if let [a, b] = rest {
                                self.translate(a)?;
                                self.translate(b)?;
                                self.ins(Op::Add)
                            } else {
                                return error(ArityMismatch, *span);
                            }
                        }
                        "-" => {
                            if let [a, b] = rest {
                                self.translate(a)?;
                                self.translate(b)?;
                                self.ins(Op::Sub)
                            } else {
                                return error(ArityMismatch, *span);
                            }
                        }
                        "=" => {
                            if rest.len() >= 2 {
                                for arg in rest {
                                    self.translate(arg)?;
                                }
                                self.native_call(rest.len() as u32, |atoms| {
                                    let mut atoms = atoms.iter();
                                    let first = atoms.next().unwrap();
                                    if atoms.all(|a| first == a) {
                                        Atom::new_int(1)
                                    } else {
                                        Atom::null()
                                    }
                                });
                            } else {
                                return error(ArityMismatch, *span);
                            }
                        }
                        "<" => {
                            if let [a, b] = rest {
                                self.translate(a)?;
                                self.translate(b)?;
                                self.native_call(2, |atoms| {
                                    if let [a, b] = atoms {
                                        if a < b {
                                            Atom::new_int(1)
                                        } else {
                                            Atom::null()
                                        }
                                    } else {
                                        Atom::undefined()
                                    }
                                });
                            } else {
                                return error(ArityMismatch, *span);
                            }
                        }
                        "loop" => {
                            if let [body] = rest {
                                self.push_loop();
                                let start = self.current_byte_offset();
                                self.translate(body)?;
                                self.ins(Op::Pop);
                                let end = self.current_byte_offset();
                                self.ins(Op::Jump(((start - end) - 8) as i32));
                                let patches = self.pop_loop();
                                for ReturnToPatch {
                                    return_loc,
                                    ins_loc,
                                } in patches
                                {
                                    self.closure_mut().assembly[ins_loc as usize] =
                                        Ins::Op(Op::Jump(((end - return_loc) + 8) as i32));
                                }
                            } else {
                                return error(InvalidLoop, *span);
                            }
                        }
                        "return" => {
                            if let [val] = rest {
                                self.translate(val)?;
                            }
                            if self.in_loop() {
                                self.push_return_patch(
                                    self.current_byte_offset() + 8,
                                    self.current_offset(),
                                );
                                self.patchme()
                            } else {
                                self.ins(Op::Return);
                            }
                        }
                        "if" => {
                            if let [cond, truth, lie] = rest {
                                self.translate(cond)?;
                                let patch_fjump = self.current_offset();
                                self.patchme();
                                let start = self.current_byte_offset();
                                self.translate(truth)?;

                                let patch_ejump = self.current_offset();
                                self.patchme();
                                let truth_end = self.current_byte_offset();

                                let lie_start = self.current_byte_offset();
                                self.closure_mut().assembly[patch_fjump as usize] =
                                    Ins::Op(Op::FJump((lie_start - start) as i32));
                                self.translate(lie)?;
                                let end = self.current_byte_offset();
                                self.closure_mut().assembly[patch_ejump as usize] =
                                    Ins::Op(Op::Jump((end - truth_end) as i32));
                            } else {
                                return error(InvalidIf, *span);
                            }
                        }
                        _function => {
                            let argc = rest.len();
                            for arg in rest {
                                self.translate(arg)?;
                            }
                            self.translate(&first[0])?;
                            self.ins(Op::Call(argc as u32))
                        }
                    },
                    Some(lambda @ Expr::List(es, _))
                        if matches!(es.first(), Some(Expr::Symbol("fn", ..))) =>
                    {
                        let argc = rest.len();
                        for arg in rest {
                            self.translate(arg)?;
                        }
                        self.translate(lambda)?;
                        self.ins(Op::Call(argc as u32));
                    }
                    Some(e) => return error(UnexpectedAtom, e.span()),
                    None => unreachable!(),
                };
            }
            Expr::String(_s, _span) => todo!(),
        }
        ().okay()
    }

    fn translate_begin(&mut self, body: &[Expr]) -> Result<()> {
        for expr in body {
            self.translate(expr)?;
        }
        ().okay()
    }

    fn translate_lambda(&mut self, args: &Expr, body: &Expr) -> Result<()> {
        self.new_closure();

        if let Expr::List(args, _) = args {
            let mut rest = false;
            let mut i = 0;
            loop {
                if i == args.len() {
                    break;
                }
                let arg = &args[i];
                if let Expr::Symbol(sym, _) = arg {
                    let id = self.intern(sym);
                    if *sym == "&rest" {
                        rest = true;
                        i += 1;
                        continue;
                    } else {
                        self.new_local(id);
                        self.closure_mut().sp += 1;
                    }
                    if rest {
                        if i != args.len() - 1 {
                            return error(RestMustBeLast, arg.span());
                        }
                        self.new_local(id);
                        self.closure_mut().sp += 1;
                        break;
                    }
                } else {
                    return error(UnexpectedAtom, arg.span());
                }
                i += 1;
            }
        } else if let Expr::Null(_) = args {
        } else {
            return error(UnexpectedAtom, args.span());
        };

        if let Expr::List(..) = body {
        } else {
            return error(UnexpectedAtom, body.span());
        };

        self.translate_begin(&[body.clone()])?;

        self.ins(Op::Return);

        let fid = self.closure().id;
        self.pop_closure();
        self.push_lambda(fid);

        ().okay()
    }

    fn push_const(&mut self, constant: Atom) {
        let closure = self.closure_mut();
        closure.sp += 1;
        for i in 0..closure.const_table.len() {
            if closure.const_table[i] == constant {
                self.ins(Op::Push(i as u32));
                return;
            }
        }
        let id = closure.const_table.len();
        closure.const_table.push(constant);
        self.ins(Op::Push(id as u32));
    }

    fn intern(&mut self, sym: &str) -> SymId {
        for i in 0..self.symbol_table.len() {
            if self.symbol_table[i] == sym {
                return SymId(i);
            }
        }
        let i = self.symbol_table.len();
        self.symbol_table.push(sym.into());
        SymId(i)
    }

    fn current_byte_offset(&self) -> isize {
        self.closure().assembly.iter().fold(0, |acc, _| acc + 8)
    }

    fn current_offset(&self) -> isize {
        self.closure().assembly.iter().fold(0, |acc, _| acc + 1)
    }

    fn get_local(&self, sym: &SymId) -> Option<usize> {
        if let Some(var) = self.closure().locals.get(sym) {
            return var.id.some();
        }
        None
    }

    fn new_local(&mut self, sym: SymId) -> usize {
        let slot = self.closure().sp;
        self.closure_mut().locals.insert(sym, Local::new(slot));
        slot
    }

    fn get_upvalue(&self, sym: &SymId) -> Option<usize> {
        let mut parent = self.closure().parent;
        while let Some(p) = parent {
            let closure = &self.closures[p];
            if let Some(var) = closure.locals.get(sym) {
                return var.id.some();
            }
            parent = closure.parent
        }
        None
    }

    fn _new_upvalue(&mut self, _index: usize, _is_local: bool) -> usize {
        for (_sym, _uv) in self.closure().upvalues.iter().enumerate() {}
        0
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

    fn push_loop(&mut self) {
        self.closure_mut().loops.push(Vec::new());
    }

    fn pop_loop(&mut self) -> Vec<ReturnToPatch> {
        self.closure_mut().loops.pop().unwrap()
    }

    fn push_return_patch(&mut self, return_loc: isize, ins_loc: isize) {
        self.closure_mut()
            .loops
            .last_mut()
            .unwrap()
            .push(ReturnToPatch::new(return_loc, ins_loc))
    }

    fn in_loop(&self) -> bool {
        !self.closure().loops.is_empty()
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
        let id = self.next_func_id();
        self.closure_mut().children.push(id);
        self.closures.push(Closure {
            id,
            assembly: Default::default(),
            parent: Some(self.active_closure),
            children: Default::default(),
            bp,
            sp: 0,
            const_table: Default::default(),
            locals: Default::default(),
            upvalues: Default::default(),
            loops: Default::default(),
        });
        self.active_closure = self.closures.len() - 1;
    }

    fn closure(&self) -> &Closure {
        &self.closures[self.active_closure]
    }

    fn closure_mut(&mut self) -> &mut Closure {
        &mut self.closures[self.active_closure]
    }

    fn ins(&mut self, ins: Op) {
        match ins {
            Op::Push(_) => self.closure_mut().sp += 1,
            Op::Pop => self.closure_mut().sp -= 1,
            Op::LSet(_) => self.closure_mut().sp -= 1,
            Op::LGet(_) => self.closure_mut().sp += 1,
            Op::Add => self.closure_mut().sp -= 1,
            Op::Sub => self.closure_mut().sp -= 1,
            Op::Call(argc) => self.closure_mut().sp -= argc as usize,
            _ => (),
        }
        self.closure_mut().assembly.push(Ins::Op(ins))
    }

    fn patchme(&mut self) {
        self.closure_mut().assembly.push(Ins::PatchMe)
    }

    fn push_lambda(&mut self, label: usize) {
        self.closure_mut().sp += 1;
        self.closure_mut().assembly.push(Ins::PushLambda(label))
    }

    fn next_func_id(&mut self) -> usize {
        let fid = self.next_func_id;
        self.next_func_id += 1;
        fid
    }

    fn native_call(&mut self, argc: u32, func: fn(&[Atom]) -> Atom) {
        self.ins(Op::NativeCall(argc));
        self.closure_mut().assembly.push(Ins::Native(func as usize));
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}
