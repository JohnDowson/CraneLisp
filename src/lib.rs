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

use std::{cell::RefCell, collections::BTreeSet};

pub use errors::*;
use eval::{value::Symbol, Atom};
use once_cell::unsync::Lazy;
use smol_str::SmolStr;
use somok::{Leaksome, Somok};
pub type Result<T, E = CranelispError> = std::result::Result<T, E>;

static mut ENV: Lazy<RefCell<BTreeSet<Symbol>>> =
    Lazy::new(|| RefCell::new(std::collections::BTreeSet::default()));

pub fn intern(sym: SmolStr) -> SmolStr {
    unsafe {
        ENV.get_mut().insert(Symbol {
            name: sym.clone(),
            val: Atom::NULL.boxed().leak(),
        });
        sym
    }
}

pub fn set_value(sym: Symbol) {
    unsafe {
        ENV.get_mut().replace(sym);
    }
}

pub fn lookup_value(sym: SmolStr) -> Option<*mut Atom> {
    unsafe {
        ENV.get_mut()
            .get(&Symbol {
                name: sym,
                val: Atom::NULL.boxed().leak(),
            })
            .map(|s| s.val)
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

#[allow(clippy::missing_safety_doc)]
pub mod libcl {
    use somok::{Leaksome, Somok};

    use crate::eval::value::{head, tail, Atom, Tag};

    #[no_mangle]
    pub unsafe extern "C" fn car(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 1 {
            return Atom::NULL.boxed().leak();
        }
        match (*atoms).tag {
            Tag::Null => atoms,
            Tag::Pair => head(&*atoms),
            _ => todo!(),
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn cdr(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 1 {
            return Atom::NULL.boxed().leak();
        }
        match (*atoms).tag {
            Tag::Null => atoms,
            Tag::Pair => tail(&*atoms),
            _ => todo!(),
        }
    }

    pub unsafe extern "C" fn cons(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::NULL.boxed().leak();
        }
        let (a, b) = (atoms, atoms.add(1));

        Atom::new_pair(crate::eval::value::cons(a, b).boxed().leak())
            .boxed()
            .leak()
    }

    pub unsafe extern "C" fn setf(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::NULL.boxed().leak();
        }
        let (place, new) = (atoms, atoms.add(1));
        *place = *new;

        Atom::NULL.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_print(count: usize, atoms: *mut Atom) -> *mut Atom {
        for i in 0..count {
            print!("{}", &*atoms.add(i));
        }
        println!();
        Atom::NULL.boxed().leak()
    }
    #[no_mangle]
    pub unsafe extern "C" fn cl_eprint(count: usize, atoms: *mut Atom) -> *mut Atom {
        for i in 0..count {
            print!("{}", &*atoms.add(i));
        }
        println!();
        Atom::NULL.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_alloc_value(_: usize, _: *mut Atom) -> *mut Atom {
        Atom::NULL.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn add(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::ERROR.boxed().leak();
        }

        let mut a = *atoms;

        for i in 1..count {
            let b = &*atoms.add(i);
            a = match (a.tag, b.tag) {
                (Tag::Int, Tag::Int) => Atom::new_int(a.as_int() + b.as_int()),
                (Tag::Int, Tag::Float) => Atom::new_int(a.as_int() + b.as_float() as i64),
                (Tag::Float, Tag::Int) => Atom::new_float(a.as_float() + b.as_int() as f64),
                (Tag::Float, Tag::Float) => Atom::new_float(a.as_float() + b.as_float()),
                _ => panic!("Can't add non-numbers {:?} + {:?}", a, b),
            };
        }
        a.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn sub(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::ERROR.boxed().leak();
        }

        let mut a = *atoms;

        for i in 1..count {
            let b = &*atoms.add(i);
            a = match (a.tag, b.tag) {
                (Tag::Int, Tag::Int) => Atom::new_int(a.as_int() - b.as_int()),
                (Tag::Int, Tag::Float) => Atom::new_int(a.as_int() - b.as_float() as i64),
                (Tag::Float, Tag::Int) => Atom::new_float(a.as_float() - b.as_int() as f64),
                (Tag::Float, Tag::Float) => Atom::new_float(a.as_float() - b.as_float()),
                _ => panic!("Can't sutract non-numbers {:?} + {:?}", a, b),
            };
        }
        a.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn less_than(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::ERROR.boxed().leak();
        }

        let mut a = *atoms;

        for i in 1..count {
            let b = &*atoms.add(i);
            a = match (a.tag, b.tag) {
                (Tag::Int, Tag::Int) => Atom::new_int(a.as_int() - b.as_int()),
                (Tag::Int, Tag::Float) => Atom::new_int(a.as_int() - b.as_float() as i64),
                (Tag::Float, Tag::Int) => Atom::new_float(a.as_float() - b.as_int() as f64),
                (Tag::Float, Tag::Float) => Atom::new_float(a.as_float() - b.as_float()),
                _ => panic!("Can't compare non-numbers {:?} + {:?}", a, b),
            };
        }
        a.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn more_than(count: usize, atoms: *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::ERROR.boxed().leak();
        }
        let mut a = *atoms;

        for i in 1..count {
            let b = &*atoms.add(i);
            a = match (a.tag, b.tag) {
                (Tag::Int, Tag::Int) => Atom::new_int(a.as_int() - b.as_int()),
                (Tag::Int, Tag::Float) => Atom::new_int(a.as_int() - b.as_float() as i64),
                (Tag::Float, Tag::Int) => Atom::new_float(a.as_float() - b.as_int() as f64),
                (Tag::Float, Tag::Float) => Atom::new_float(a.as_float() - b.as_float()),
                _ => panic!("Can't compare non-numbers {:?} + {:?}", a, b),
            };
        }
        a.boxed().leak()
    }
}
