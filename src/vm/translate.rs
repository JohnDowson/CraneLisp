use super::asm::Ins;
use super::closure::Closure;
use super::Op;
use crate::atom::Atom;
use crate::parser::expr::Expr;
use crate::symbol::SymId;
use crate::Span;
use smol_str::SmolStr;
use somok::Somok;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}
#[derive(Debug)]
pub enum ErrorKind {
    UndefinedVariable,
    NoScopes,
    NotAPlace,
    UnexpectedAtom,
    ArityMismatch,
    RestMustBeLast,
}
use ErrorKind::*;
fn error<T>(kind: ErrorKind, span: Span) -> Result<T> {
    Error { kind, span }.error()
}

pub type ConstTable<const ID: u32> = Vec<Atom<ID>>;

pub struct Translator {
    active_closure: usize,
    next_label: usize,
    closures: Vec<Closure>,
    const_table: ConstTable<0>,
    symbol_table: Vec<SmolStr>,
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
            const_table: Default::default(),
            symbol_table: Default::default(),
        }
    }

    pub fn emit(self) -> (Vec<Ins>, ConstTable<0>, Vec<SmolStr>) {
        let mut res = vec![Ins::PushLambda(0), Ins::Op(Op::Call(0)), Ins::Op(Op::Halt)];

        for closure in self.closures {
            res.extend(closure.assembly.iter().copied());
        }
        (res, self.const_table, self.symbol_table)
    }

    pub fn translate_toplevel(&mut self, expr: &Expr) -> Result<()> {
        self.label();

        self.translate(expr)?;

        self.ins(Op::Return);

        ().okay()
    }

    pub fn translate(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Symbol(s, span) => match *s {
                "t" => self.push_const(Atom::Bool(true)),
                "f" => self.push_const(Atom::Bool(false)),
                "nil" => self.push_const(Atom::Null),
                sym => {
                    let sym = self.intern(sym);
                    let local = self.get_local(&sym).ok_or_else(|| Error {
                        kind: UndefinedVariable,
                        span: *span,
                    })?;
                    self.ins(Op::LGet(local as u32));
                }
            },
            Expr::Float(f, _) => self.push_const(Atom::Float(*f)),
            Expr::Integer(i, _) => self.push_const(Atom::Int(*i)),
            Expr::Bool(b, _) => self.push_const(Atom::Bool(*b)),
            Expr::Null(_) => self.push_const(Atom::Null),
            Expr::List(exprs, _span) => {
                if exprs.is_empty() {
                    self.push_const(Atom::Null);
                }
                let (first, rest) = exprs.split_at(1);
                match first.first() {
                    Some(Expr::Symbol(s, span)) => match *s {
                        "fn" => self.translate_lambda(&rest[0], &rest[1])?,
                        "begin" => self.translate_begin(rest)?,
                        "set" => {}
                        "+" => {
                            if let [a, b] = rest {
                                self.translate(a)?;
                                self.translate(b)?;
                                self.ins(Op::Add)
                            } else {
                                return error(ArityMismatch, *span);
                            }
                        }
                        _ => todo!("Function invocation"),
                    },
                    Some(e @ Expr::List(es, _))
                        if matches!(es.first(), Some(Expr::Symbol("fn", ..))) =>
                    {
                        self.translate(&rest[0])?;
                        self.translate(&rest[1])?;
                        self.translate(e)?;
                        self.ins(Op::Call(2));
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
        self.label();

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
                        // self.ins(Op::LReg(i as u32));
                    }
                    if rest {
                        if i != args.len() - 1 {
                            return error(RestMustBeLast, arg.span());
                        }
                        self.new_local(id);
                        // self.ins(Op::LReg((i - 1) as u32));
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

        self.pop_closure();
        self.push_lambda(self.next_label - 1);

        ().okay()
    }

    fn push_const(&mut self, constant: Atom<0>) {
        self.closure_mut().sp += 1;
        for i in 0..self.const_table.len() {
            if self.const_table[i] == constant {
                self.ins(Op::Push(i as u32));
                return;
            }
        }
        self.ins(Op::Push(self.const_table.len() as u32));
        self.const_table.push(constant);
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

    fn _current_byte_offset(&self) -> usize {
        self.closure()
            .assembly
            .iter()
            .fold(0, |acc, ins| acc + ins.size())
    }

    fn get_local(&self, sym: &SymId) -> Option<usize> {
        if let Some(&var) = self.closure().scope.vars.get(sym) {
            return var.some();
        }
        None
    }
    fn _get_outer(&self, sym: &SymId) -> Option<usize> {
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
        let slot = self.closure().sp;
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
            // Op::LReg(_) => self.closure_mut().sp += 1,
            Op::Call(argc) => self.closure_mut().sp -= argc as usize,
            _ => (),
        }
        self.closure_mut().assembly.push(Ins::Op(ins))
    }

    fn push_lambda(&mut self, label: usize) {
        self.closure_mut().assembly.push(Ins::PushLambda(label))
    }

    fn label(&mut self) {
        let label = self.next_label();
        self.closure_mut().assembly.push(Ins::Label(label))
    }

    fn next_label(&mut self) -> usize {
        let label = self.next_label;
        self.next_label += 1;
        label
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test() {
    panic! {"{:?}", vec![1].split_at(1)}
}
