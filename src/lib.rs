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
pub use errors::*;
use eval::Value;
use fnv::FnvHashMap;
use somok::Somok;

pub type Result<T, E = CranelispError> = std::result::Result<T, E>;

#[derive(Default, Debug)]
pub struct Env {
    pub env: FnvHashMap<usize, Value>,
    pub symbols: Vec<String>,
}

impl Env {
    pub fn insert_symbol(&mut self, sym: String) -> usize {
        self.symbols
            .iter()
            .position(|s| s == &sym)
            .unwrap_or_else(|| {
                let id = self.symbols.len();
                self.symbols.push(sym);
                id
            })
    }
    pub fn lookup_symbol(&self, sym_id: usize) -> Option<&String> {
        self.symbols.get(sym_id)
    }
    pub fn insert_value(&mut self, id: usize, value: Value) {
        self.env.insert(id, value);
    }
    pub fn lookup_value(&self, sym_id: usize) -> Option<&Value> {
        self.env.get(&sym_id)
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

    use crate::eval::value::{Tag, Value};

    #[no_mangle]
    pub extern "C" fn cl_print(ret: &mut Value, a: &Value) {
        println!("{}", a);
        *ret = Value::NULL
    }
    #[no_mangle]
    pub extern "C" fn cl_eprint(ret: &mut Value, a: &Value) {
        eprintln!("{:?}", a);
        *ret = Value::NULL
    }

    #[no_mangle]
    pub extern "C" fn cl_alloc_value(_ret: &mut Value) {
        Value::NULL.boxed().leak();
    }

    #[no_mangle]
    pub extern "C" fn add(ret: &mut Value, a: &Value, b: &Value) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Value::new_int(a.as_int() + b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Value::new_int(a.as_int() + b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Value::new_float(a.as_float() + b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Value::new_float(a.as_float() + b.as_float()),
            _ => eprintln!("Can't add non-numbers {:?} + {:?}", a, b),
        }
    }

    #[no_mangle]
    pub extern "C" fn sub(ret: &mut Value, a: &Value, b: &Value) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Value::new_int(a.as_int() - b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Value::new_int(a.as_int() - b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Value::new_float(a.as_float() - b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Value::new_float(a.as_float() - b.as_float()),
            _ => eprintln!("Can't subtract non-numbers"),
        }
    }

    #[no_mangle]
    pub extern "C" fn less_than(ret: &mut Value, a: &Value, b: &Value) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Value::new_bool(a.as_int() < b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Value::new_bool(a.as_int() < b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Value::new_bool(a.as_float() < b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Value::new_bool(a.as_float() < b.as_float()),
            _ => eprintln!("Can't subtract non-numbers"),
        }
    }

    #[no_mangle]
    pub extern "C" fn more_than(ret: &mut Value, a: &Value, b: &Value) {
        match (a.tag, b.tag) {
            (Tag::Int, Tag::Int) => *ret = Value::new_bool(a.as_int() > b.as_int()),
            (Tag::Int, Tag::Float) => *ret = Value::new_bool(a.as_int() > b.as_float() as i64),
            (Tag::Float, Tag::Int) => *ret = Value::new_bool(a.as_float() > b.as_int() as f64),
            (Tag::Float, Tag::Float) => *ret = Value::new_bool(a.as_float() > b.as_float()),
            _ => eprintln!("Can't subtract non-numbers"),
        }
    }
}
