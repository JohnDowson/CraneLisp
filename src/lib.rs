#[macro_export]
macro_rules! syntax {
    ($kind:tt, $($spans:expr),+) => {
        {
            //let mut vec = vec![];
            let vec = vec![$($spans),*];
            //$(vec.push($spans);),*
            crate::errors::CranelispError::Syntax(
            crate::errors::SyntaxError {
                kind: crate::errors::SyntaxErrorKind::$kind,
                spans: vec,
            })
        }
    };
    ($kind:tt) => {
        {crate::errors::CranelispError::Syntax(
            crate::errors::SyntaxError {
                kind: crate::errors::SyntaxErrorKind::$kind,
                spans: vec![],
            })
        }
    };
}
pub mod errors;
pub mod eval;
pub mod function;
pub mod jit;
pub mod lexer;
pub mod list;
pub mod parser;

use std::ops::Deref;

pub use errors::*;
use eval::Atom;
use fnv::FnvHashMap;
use smol_str::SmolStr;
use somok::{Leaksome, Somok};
pub type Result<T, E = CranelispError> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
pub struct Symbol {
    name: SmolStr,
}
impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &*self.name
    }
}

#[derive(Default, Debug)]
pub struct Env {
    pub env: FnvHashMap<usize, Atom>,
    symbol_ids: FnvHashMap<SmolStr, usize>,
    symbols: Vec<Symbol>,
    last_symbol_id: Option<usize>,
}

impl Env {
    pub fn insert_symbol_str(&mut self, sym: &'static str) -> usize {
        let symbol_exists = self.symbol_ids.keys().any(|s| sym == *s);
        if symbol_exists {
            *self.symbol_ids.get(sym).unwrap()
        } else {
            self.symbols.push(Symbol { name: sym.into() });
            let id = if let Some(mut id) = self.last_symbol_id {
                id += 1;
                self.last_symbol_id = Some(id);
                id
            } else {
                self.last_symbol_id = 0.some();
                0
            };
            self.symbol_ids.insert(sym.into(), id);
            id
        }
    }
    pub fn insert_symbol(&mut self, sym: SmolStr) -> usize {
        let symbol_exists = self.symbol_ids.keys().any(|s| sym == *s);
        if symbol_exists {
            *self.symbol_ids.get(&*sym).unwrap()
        } else {
            self.symbols.push(Symbol { name: sym.clone() });
            let id = if let Some(mut id) = self.last_symbol_id {
                id += 1;
                self.last_symbol_id = Some(id);
                id
            } else {
                self.last_symbol_id = 0.some();
                0
            };
            self.symbol_ids.insert(sym, id);
            id
        }
    }
    pub fn lookup_symbol(&self, sym_id: usize) -> Option<Symbol> {
        self.symbols.get(sym_id).cloned()
    }
    pub fn insert_value(&mut self, id: usize, value: Atom) {
        self.env.insert(id, value);
    }
    pub fn lookup_value(&self, sym_id: usize) -> Option<Atom> {
        self.env.get(&sym_id).copied()
    }
}
trait TryRemove {
    type Item;
    fn try_remove(&mut self, i: usize) -> Option<Self::Item>;
}
impl<T> TryRemove for Vec<T> {
    type Item = T;

    fn try_remove(&mut self, i: usize) -> Option<Self::Item> {
        if i < self.len() {
            self.remove(i).some()
        } else {
            None
        }
    }
}

#[derive(Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    //pub source_id: &'static str,
}

impl Span {
    pub fn new(start: usize, end: usize /*source_id: &'static str*/) -> Self {
        Self {
            start,
            end,
            //source_id,
        }
    }
    pub fn point(point: usize /*source_id: &'static str*/) -> Self {
        Self {
            start: point,
            end: point,
            //source_id,
        }
    }
    pub fn merge(mut first: Self, second: Self) -> Self {
        first.end = second.end;
        first
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}..{}]", /* , &self.source_id*/
            &self.start, &self.end
        )
    }
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

pub mod libcl {
    use somok::{Leaksome, Somok};

    use crate::eval::value::{Atom, Tag};

    #[no_mangle]
    pub extern "C" fn cl_print(ret: &mut Atom, a: &Atom) {
        println!("{}", a);
        *ret = Atom::NULL
    }
    #[no_mangle]
    pub extern "C" fn cl_eprint(ret: &mut Atom, a: &Atom) {
        eprintln!("{:?}", a);
        *ret = Atom::NULL
    }

    #[no_mangle]
    pub extern "C" fn cl_alloc_value() -> *mut Atom {
        Atom::NULL.boxed().leak()
    }

    #[no_mangle]
    pub extern "C" fn add(ret: &mut Atom, a: &Atom, b: &Atom) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Atom::new_int(a.as_int() + b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Atom::new_int(a.as_int() + b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Atom::new_float(a.as_float() + b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Atom::new_float(a.as_float() + b.as_float()),
            _ => eprintln!("Can't add non-numbers {:?} + {:?}", a, b),
        }
    }

    #[no_mangle]
    pub extern "C" fn sub(ret: &mut Atom, a: &Atom, b: &Atom) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Atom::new_int(a.as_int() - b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Atom::new_int(a.as_int() - b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Atom::new_float(a.as_float() - b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Atom::new_float(a.as_float() - b.as_float()),
            _ => eprintln!("Can't subtract non-numbers"),
        }
    }

    #[no_mangle]
    pub extern "C" fn less_than(ret: &mut Atom, a: &Atom, b: &Atom) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Atom::new_bool(a.as_int() < b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Atom::new_bool(a.as_int() < b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Atom::new_bool(a.as_float() < b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Atom::new_bool(a.as_float() < b.as_float()),
            _ => eprintln!("Can't subtract non-numbers"),
        }
    }

    #[no_mangle]
    pub extern "C" fn more_than(ret: &mut Atom, a: &Atom, b: &Atom) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Atom::new_bool(a.as_int() > b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Atom::new_bool(a.as_int() > b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Atom::new_bool(a.as_float() > b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Atom::new_bool(a.as_float() > b.as_float()),
            _ => eprintln!("Can't subtract non-numbers"),
        }
    }
}
