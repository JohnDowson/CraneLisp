use crate::{
    env::{self, fork_env, get_env, unfork_env},
    mem::{self, *},
    value::{
        Atom::{self, *},
        Pair,
    },
    EnvId,
};
use somok::Somok;
use std::ops::{Deref, DerefMut};
use Opcode::*;
use VmError::*;

#[derive(Debug)]
pub enum VmError {
    StackUnderflow(usize),
    TypeError(usize, std::string::String),
    NoEnv(usize),
    Undefined(usize),
}

#[derive(Debug)]
pub struct VM {
    stack: Stack,
    active_env: EnvId,
    code: Vec<Opcode>,
    ip: usize,
    cc: usize,
    gc_period: usize,
}

impl VM {
    pub fn new(code: Vec<Opcode>, gc_period: usize, active_env: EnvId) -> VM {
        VM {
            stack: Default::default(),
            active_env,
            code,
            ip: 0,
            cc: 0,
            gc_period,
        }
    }

    pub fn run(&mut self) -> Result<Atom, VmError> {
        'outer: loop {
            loop {
                // dbg! {(&self.stack,&self.code[self.ip])};
                if self.ip >= self.code.len() {
                    panic!("Instruction pointer out of bounds {}", self.ip)
                }
                if self.gc_period != 0 && self.cc >= self.gc_period {
                    self.cc = 0;
                    self.mark();
                    mem::collect();
                }
                self.cc += 1;
                match &self.code[self.ip] {
                    Push(a) => self.stack.push(a.clone()),
                    Pop => {
                        self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                    }
                    Dup => {
                        let a = self.stack.peek().cloned().ok_or(StackUnderflow(self.ip))?;
                        self.stack.push(a);
                    }
                    Swap => {
                        let a = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                        let b = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                        self.stack.push(a);
                        self.stack.push(b);
                    }

                    Store => {
                        if let Some(atom) = self.stack.pop() {
                            let aref = mem::alloc(atom);
                            self.stack.push(Ptr(aref))
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Load => {
                        if let Some(atom) = self.stack.pop() {
                            if let Ptr(aref) = atom {
                                self.stack.push((*aref).clone())
                            } else {
                                return TypeError(
                                    self.ip,
                                    format!("Expected Ptr, got {:?}", &atom),
                                )
                                .error();
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }

                    Lvar => {
                        let env = env::get_env(self.active_env);
                        let symbol = {
                            let atom = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                            atom.as_symbol()
                                .ok_or_else(|| TypeError(self.ip, "".into()))?
                        };
                        self.stack.push(
                            env.try_get(symbol)
                                .cloned()
                                .map(Ptr)
                                .ok_or(Undefined(self.ip))?,
                        )
                    }
                    Set => {
                        let atom = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                        let value = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                        let mut aref = if let Symbol(s) = atom {
                            let env = { env::get_env(self.active_env) };
                            let aref = env.try_get(s).cloned();
                            if let Some(aref) = aref {
                                aref
                            } else {
                                let aref = mem::alloc(Null);
                                let env = env::get_env(self.active_env);
                                env.insert(s, aref.clone());
                                aref
                            }
                        } else if let Ptr(r) = atom {
                            r
                        } else {
                            return TypeError(
                                self.ip,
                                format!("Expected Ptr or Symbol, got {:?}", atom),
                            )
                            .error();
                        };
                        let place = aref.deref_mut();
                        *place = value
                    }

                    Car => {
                        let car = self
                            .stack
                            .pop()
                            .ok_or(StackUnderflow(self.ip))?
                            .as_pair()
                            .ok_or_else(|| {
                                TypeError(
                                    self.ip,
                                    format!("Expected Pair, got {:?}", self.stack.peek()),
                                )
                            })?
                            .car
                            .clone();
                        self.stack.push(Ptr(car));
                    }
                    Cdr => {
                        let cdr = self
                            .stack
                            .pop()
                            .ok_or(StackUnderflow(self.ip))?
                            .as_pair()
                            .ok_or_else(|| {
                                TypeError(
                                    self.ip,
                                    format!("Expected Pair, got {:?}", self.stack.peek()),
                                )
                            })?
                            .cdr
                            .clone();
                        self.stack.push(Ptr(cdr));
                    }

                    Add => {
                        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
                            match (b, a) {
                                (Int(a), Int(b)) => self.stack.push(Int(a + b)),
                                (Int(a), Float(b)) => self.stack.push(Int(a + b as i64)),
                                (Float(a), Int(b)) => self.stack.push(Float(a + b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Float(a + b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Sub => {
                        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
                            match (b, a) {
                                (Int(a), Int(b)) => self.stack.push(Int(a - b)),
                                (Int(a), Float(b)) => self.stack.push(Int(a - b as i64)),
                                (Float(a), Int(b)) => self.stack.push(Float(a - b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Float(a - b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Eq => {
                        if let (Some(b), Some(a)) = (self.stack.pop(), self.stack.pop()) {
                            match (a, b) {
                                (Int(a), Int(b)) => self.stack.push(Bool(a == b)),
                                (Int(a), Float(b)) => self.stack.push(Bool((a as f64) == b)),
                                (Float(a), Int(b)) => self.stack.push(Bool(a == b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Bool(a == b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Lt => {
                        if let (Some(b), Some(a)) = (self.stack.pop(), self.stack.pop()) {
                            match (a, b) {
                                (Int(a), Int(b)) => self.stack.push(Bool(a < b)),
                                (Int(a), Float(b)) => self.stack.push(Bool((a as f64) < b)),
                                (Float(a), Int(b)) => self.stack.push(Bool(a < b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Bool(a < b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Gt => {
                        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
                            match (b, a) {
                                (Int(a), Int(b)) => self.stack.push(Bool(a > b)),
                                (Int(a), Float(b)) => self.stack.push(Bool((a as f64) > b)),
                                (Float(a), Int(b)) => self.stack.push(Bool(a > b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Bool(a > b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Le => {
                        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
                            match (b, a) {
                                (Int(a), Int(b)) => self.stack.push(Bool(a >= b)),
                                (Int(a), Float(b)) => self.stack.push(Bool((a as f64) >= b)),
                                (Float(a), Int(b)) => self.stack.push(Bool(a >= b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Bool(a >= b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }
                    Ge => {
                        if let (Some(a), Some(b)) = (self.stack.pop(), self.stack.pop()) {
                            match (b, a) {
                                (Int(a), Int(b)) => self.stack.push(Bool(a <= b)),
                                (Int(a), Float(b)) => self.stack.push(Bool((a as f64) <= b)),
                                (Float(a), Int(b)) => self.stack.push(Bool(a <= b as f64)),
                                (Float(a), Float(b)) => self.stack.push(Bool(a <= b)),
                                (a, b) => {
                                    return TypeError(
                                        self.ip,
                                        format!("Expected Int or Float found {} {}", a, b),
                                    )
                                    .error()
                                }
                            }
                        } else {
                            return StackUnderflow(self.ip).error();
                        }
                    }

                    Jump(ip) => {
                        self.ip = *ip;
                        break;
                    }
                    FJump(ip) => {
                        let atom = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                        if !atom.truthy() {
                            self.ip = *ip;
                            break;
                        }
                    }
                    TJump(ip) => {
                        let atom = self.stack.pop().ok_or(StackUnderflow(self.ip))?;
                        if atom.truthy() {
                            self.ip = *ip;
                            break;
                        }
                    }
                    Call(argc) => {
                        let mut argc = *argc;
                        let func = self
                            .stack
                            .pop()
                            .ok_or(StackUnderflow(self.ip))?
                            .as_symbol()
                            .ok_or_else(|| TypeError(self.ip, "Expected Symbol".into()))?;
                        let func = env::get_env(self.active_env)
                            .try_get(func)
                            .unwrap()
                            .deref()
                            .as_func()
                            .unwrap();
                        self.active_env = fork_env(self.active_env);
                        let env = get_env(self.active_env);
                        for sym in func.args.iter() {
                            argc -= 1;
                            env.insert(
                                *sym,
                                alloc(self.stack.pop().ok_or(StackUnderflow(self.ip))?),
                            );
                        }
                        assert_eq!(argc, 0);
                        self.ip = func.iptr - 1;
                    }
                    Return => {
                        let func = self
                            .stack
                            .pop()
                            .ok_or(StackUnderflow(self.ip))?
                            .as_func()
                            .ok_or_else(|| TypeError(self.ip, "Expected Func".into()))?;
                        self.active_env = unfork_env(self.active_env).ok_or(NoEnv(self.ip))?;
                        self.ip = func.iptr - 1;
                    }
                    // todo: save should aware of the number of argumets
                    Save(skip) => self.stack.push(Atom::new_func(vec![], self.ip + skip + 3)),

                    Label(_) => panic!("Compiler error: label in final output"),
                    Pushf(_) => panic!("Compiler error: pushf in final output"),
                    Halt => break 'outer,
                }
                self.ip += 1;
            }
        }
        self.stack.pop().unwrap_or(Null).okay()
        //self.stack.pop().ok_or(StackUnderflow(self.ip))
    }

    fn mark(&self) {
        for atom in self.stack.iter() {
            match atom {
                Pair(Pair { car, cdr }) => {
                    mem::mark(car.clone());
                    mem::mark(cdr.clone())
                }
                Ptr(ptr) => mem::mark(ptr.clone()),
                _ => (),
            }
        }
        for opcode in self.code.iter() {
            if let Push(a) = opcode {
                match a {
                    Pair(Pair { car, cdr }) => {
                        mem::mark(car.clone());
                        mem::mark(cdr.clone())
                    }
                    Ptr(ptr) => mem::mark(ptr.clone()),
                    _ => (),
                }
            }
        }
        env::mark(self.active_env)
    }
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Push(Atom),
    Pop,
    Dup,
    Swap,

    Store,
    Load,

    Lvar,
    Set,

    Car,
    Cdr,

    Add,
    Sub,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,

    Jump(usize),
    FJump(usize),
    TJump(usize),

    Call(usize),
    Return,
    Save(usize),

    Label(usize),
    Pushf(usize),
    Halt,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::env::setup;
    use std::assert_matches::assert_matches;

    #[test]
    pub fn add() {
        let mut vm = VM::new(vec![Push(Int(1)), Push(Int(1)), Add, Halt], 0, setup());
        assert_matches!(vm.run().unwrap(), Int(2))
    }

    #[test]
    pub fn sub() {
        let mut vm = VM::new(vec![Push(Int(1)), Push(Int(1)), Sub, Halt], 0, setup());
        assert_matches!(vm.run().unwrap(), Int(0))
    }

    #[test]
    pub fn store() {
        let mut vm = VM::new(vec![Push(Int(1)), Store, Load, Halt], 0, setup());
        assert_matches!(vm.run().unwrap(), Int(1))
    }

    #[test]
    pub fn set() {
        let mut vm = VM::new(
            vec![
                Push(Int(1)),
                Store,
                Dup,
                Push(Int(2)),
                Swap,
                Set,
                Load,
                Halt,
            ],
            0,
            setup(),
        );
        let r = vm.run().unwrap();
        assert_matches!(r, Int(2))
    }
}
