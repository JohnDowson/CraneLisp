use crate::{
    mem,
    value::{
        length, map_to_vec, nth, restn,
        Atom::{self, *},
        SymId,
    },
    vm::Opcode,
};
use fnv::FnvHashMap;
use somok::{CondPop, Somok};
use std::ops::Deref;

pub struct Translator {
    label_id: usize,
    return_labels: FnvHashMap<SymId, usize>,
    funcs: Vec<(Vec<SymId>, Vec<Opcode>)>,
}

impl Translator {
    pub fn new() -> Self {
        Self {
            label_id: 0,
            return_labels: Default::default(),
            funcs: Default::default(),
        }
    }
    pub fn translate(&mut self, atom: mem::Ref) -> Vec<Opcode> {
        match atom.deref() {
            Symbol(symbol) if &**symbol == "t" => self.translate_const(Bool(true)),
            Symbol(symbol) if &**symbol == "f" => self.translate_const(Bool(false)),
            Symbol(symbol) => self.translate_var(*symbol),
            atom @ (Int(_) | Float(_) | Bool(_)) => self.translate_const(atom.clone()),
            Pair(p) => match p.car.deref() {
                Symbol(s) => match &**s {
                    "quote" => self.translate_const(nth(atom, 1).deref().clone()),
                    "begin" => self.translate_begin(p.cdr.clone()),
                    "set" => seq([
                        self.translate(nth(atom.clone(), 2)),
                        self.translate(nth(atom, 1)),
                        vec![Opcode::Set, Opcode::Push(Atom::Null)],
                    ]),
                    "place" => {
                        let mut ops = self.translate(nth(atom.clone(), 1));
                        ops.cond_pop(|op| matches!(op, Opcode::Load));
                        ops
                    }
                    "if" => self.translate_if(p.cdr.clone()),
                    "loop" => {
                        self.translate_loop(nth(atom.clone(), 2), nth(atom, 1).as_symbol().unwrap())
                    }
                    "return" => self.translate_return(nth(atom, 1).as_symbol().unwrap()),
                    "fn" => self.translate_fn(nth(atom.clone(), 1), restn(atom, 1)),
                    "car" => seq([
                        self.translate(nth(atom, 1)),
                        vec![Opcode::Car, Opcode::Load],
                    ]),
                    "cdr" => seq([
                        self.translate(nth(atom, 1)),
                        vec![Opcode::Cdr, Opcode::Load],
                    ]),
                    _ => self.translate_call(*s, p.cdr.clone()),
                },
                a => todo!("{:?}", a),
            },
            _ => todo!(),
        }
    }

    fn translate_return(&mut self, label: SymId) -> Vec<Opcode> {
        let l = *self.return_labels.get(&label).unwrap();
        vec![Opcode::Jump(l)]
    }

    fn translate_loop(&mut self, body: mem::Ref, label: SymId) -> Vec<Opcode> {
        let l1 = self.gen_label();
        let l2 = self.gen_label();
        self.return_labels.insert(label, l2);
        seq([
            vec![Opcode::Label(l1)],
            self.translate(body),
            vec![Opcode::Jump(l1), Opcode::Label(l2)],
        ])
    }

    fn translate_var(&mut self, symbol: SymId) -> Vec<Opcode> {
        vec![
            Opcode::Push(Atom::Symbol(symbol)),
            Opcode::Lvar,
            Opcode::Load,
        ]
    }

    fn translate_const(&mut self, atom: Atom) -> Vec<Opcode> {
        vec![Opcode::Push(atom)]
    }

    fn translate_begin(&mut self, atom: mem::Ref) -> Vec<Opcode> {
        match length(atom.clone()) {
            0 => self.translate_const(Atom::Null),
            1 => self.translate(nth(atom, 0)),
            _ => seq([
                self.translate(nth(atom.clone(), 0)),
                vec![Opcode::Pop],
                self.translate_begin(atom.as_pair().unwrap().cdr.clone()),
            ]),
        }
    }

    fn translate_if(&mut self, atom: mem::Ref) -> Vec<Opcode> {
        let cond = nth(atom.clone(), 0);
        let truth = nth(atom.clone(), 1);
        let lie = nth(atom, 2);
        let l1 = self.gen_label();
        let l2 = self.gen_label();
        seq([
            self.translate(cond),
            vec![Opcode::FJump(l1)],
            self.translate(truth),
            vec![Opcode::Jump(l2)],
            vec![Opcode::Label(l1)],
            self.translate(lie),
            vec![Opcode::Label(l2)],
        ])
    }

    fn gen_label(&mut self) -> usize {
        let label = self.label_id;
        self.label_id += 1;
        label
    }

    fn translate_call(&mut self, symbol: SymId, args: mem::Ref) -> Vec<Opcode> {
        let argc = length(args.clone());
        let args = self.translate_args(args);
        let primitive = PRIMITIVE_FNS.contains(&&*symbol);
        if primitive {
            seq([args, vec![primitive_opcode(&*symbol)]])
        } else {
            seq([
                vec![Opcode::Save(args.len())],
                args,
                vec![Opcode::Push(Atom::Symbol(symbol)), Opcode::Call(argc)],
            ])
        }
    }
    fn translate_args(&mut self, atom: mem::Ref) -> Vec<Opcode> {
        match length(atom.clone()) {
            0 => vec![],
            1 => self.translate(nth(atom, 0)),
            _ => seq([
                self.translate(nth(atom.clone(), 0)),
                self.translate_args(atom.as_pair().unwrap().cdr.clone()),
            ]),
        }
    }

    fn translate_fn(&mut self, args: mem::Ref, body: mem::Ref) -> Vec<Opcode> {
        let l = self.gen_label();
        let code = seq([
            vec![Opcode::Label(l)],
            self.translate_begin(body),
            vec![Opcode::Swap, Opcode::Return],
        ]);
        let args = map_to_vec(args, |a| a.as_symbol().unwrap());
        self.funcs.push((args, code));
        vec![Opcode::Pushf(l)]
    }

    pub fn compile(self, mut code: Vec<Opcode>) -> Vec<Opcode> {
        use Opcode::*;
        code.push(Halt);
        let mut funcs = vec![];
        for func in self.funcs {
            let i = code.len()
                - code.iter().fold(
                    0,
                    |acc, o| if matches!(o, Label(_)) { acc + 1 } else { acc },
                );
            code.extend(func.1);
            funcs.push((i, func.0));
        }
        loop {
            let maybe_label = code.iter().enumerate().find_map(|(i, o)| {
                if let Label(id) = o {
                    Some((i, *id))
                } else {
                    None
                }
            });
            if let Some((i, id)) = maybe_label {
                code.remove(i);
                for id in code.iter_mut().filter_map(|op| match op {
                    Jump(ji) | TJump(ji) | FJump(ji) => {
                        if *ji == id {
                            Some(ji)
                        } else {
                            None
                        }
                    }
                    Pushf(ji) => {
                        if *ji == id {
                            *op = Push(Atom::new_func(
                                {
                                    funcs
                                        .iter()
                                        .find_map(
                                            |(a, args)| {
                                                if *a == i {
                                                    args.some()
                                                } else {
                                                    None
                                                }
                                            },
                                        )
                                        .unwrap()
                                        .clone()
                                },
                                i,
                            ));
                        }
                        None
                    }
                    _ => None,
                }) {
                    *id = i
                }
            } else {
                break;
            }
        }
        code
    }
}

const PRIMITIVE_FNS: [&str; 7] = ["+", "-", "=", "<", ">", "<=", ">="];

fn primitive_opcode(sym: &str) -> Opcode {
    match sym {
        "+" => Opcode::Add,
        "-" => Opcode::Sub,
        "=" => Opcode::Eq,
        "<" => Opcode::Lt,
        ">" => Opcode::Gt,
        "<=" => Opcode::Le,
        ">=" => Opcode::Ge,
        _ => unreachable!(),
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}
fn seq<const N: usize>(i: [Vec<Opcode>; N]) -> Vec<Opcode> {
    i.into_iter().flatten().collect()
}
