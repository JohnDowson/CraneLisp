#![feature(box_into_inner)]
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
// pub mod jit;
pub mod lexer;
pub mod mem;
pub mod parser;
pub mod value;

use std::{cell::RefCell, collections::BTreeSet};

pub use errors::*;
use once_cell::unsync::Lazy;
use smol_str::SmolStr;
use somok::Somok;
use value::{Atom, Symbol};
pub type Result<T, E = CranelispError> = std::result::Result<T, E>;

static mut ENV: Lazy<RefCell<BTreeSet<Symbol>>> =
    Lazy::new(|| RefCell::new(std::collections::BTreeSet::default()));

pub fn setup() {
    set_value(value::Symbol::new_with_atom(
        "+",
        Atom::new_func(function::Func::from_fn(libcl::add)),
    ));
    set_value(value::Symbol::new_with_atom(
        "-",
        Atom::new_func(function::Func::from_fn(libcl::sub)),
    ));
    set_value(value::Symbol::new_with_atom("t", Atom::TRUE));
    set_value(value::Symbol::new_with_atom("nil", Atom::NULL));
}

pub fn dump_env() {
    unsafe { println!("{:#?}", ENV.borrow()) }
}

pub fn intern(sym: SmolStr) -> Symbol {
    let sym = Symbol {
        name: sym,
        val: Atom::NULL.boxed(),
    };
    unsafe {
        if let Some(sym) = ENV.borrow().get(&sym) {
            sym.clone()
        } else {
            ENV.get_mut().insert(sym.clone());
            sym
        }
    }
}

pub fn symbol_defined(sym: SmolStr) -> bool {
    unsafe { ENV.borrow().contains(&Symbol::new(sym)) }
}

pub fn set_value(sym: Symbol) {
    unsafe {
        ENV.get_mut().replace(sym);
    }
}

pub fn lookup_value<'a>(sym: SmolStr) -> Option<&'a Atom> {
    unsafe {
        ENV.get_mut()
            .get(&Symbol {
                name: sym,
                val: Atom::NULL.boxed(),
            })
            .map(|s| s.val.as_ref())
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
    use std::{
        fs::{File, OpenOptions},
        io::{BufRead, BufReader},
        os::unix::prelude::{FromRawFd, IntoRawFd},
    };

    use crate::value::{self, Atom, CLString, ErrorCode::*, Tag};
    use somok::{Leaksome, Somok};

    #[no_mangle]
    pub unsafe extern "C" fn car(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count != 1 {
            return Atom::new_error(Arity).boxed().leak();
        }
        match (**atoms).tag {
            Tag::Null => *atoms,
            Tag::Pair => value::pcar(*atoms),
            _ => todo!(),
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn cdr(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count != 1 {
            return Atom::new_error(Arity).boxed().leak();
        }
        match (**atoms).tag {
            Tag::Null => *atoms,
            Tag::Pair => value::pcdr(*atoms),
            _ => todo!(),
        }
    }

    pub unsafe extern "C" fn cons(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::new_error(Arity).boxed().leak();
        }
        let (a, b) = (*atoms, *atoms.add(1));

        crate::value::cons(Box::from_raw(a), Box::from_raw(b))
            .boxed()
            .leak()
    }

    pub unsafe extern "C" fn setf(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count != 2 {
            return Atom::new_error(Arity).boxed().leak();
        }
        let (place, new) = (*atoms, (&**atoms.add(1)));
        *place = new.clone();

        new.clone().boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_print(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        for i in 0..count {
            let atom = &**atoms.add(i);
            print!("{}", atom);
        }
        println!();
        Atom::NULL.boxed().leak()
    }
    #[no_mangle]
    pub unsafe extern "C" fn cl_eprint(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        for i in 0..count {
            print!("{}", &**atoms.add(i));
        }
        println!();
        Atom::NULL.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_open(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 1 {
            return Atom::new_error(Arity).boxed().leak();
        }
        let a = &**atoms;
        match a.tag {
            Tag::String => {
                let fd = OpenOptions::new()
                    .read(true)
                    .write(true)
                    .create(true)
                    .open(a.as_string())
                    .unwrap()
                    .into_raw_fd();
                Atom::new_port(fd).boxed().leak()
            }
            _ => Atom::new_error(Type).boxed().leak(),
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_close(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 1 {
            return Atom::new_error(Arity).boxed().leak();
        }
        let a = &**atoms;
        match a.tag {
            Tag::Port => {
                File::from_raw_fd(a.as_port());
                Atom::NULL.boxed().leak()
            }
            _ => Atom::new_error(Type).boxed().leak(),
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_readline(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 1 {
            return Atom::new_error(Arity).boxed().leak();
        }
        let a = &**atoms;
        match a.tag {
            Tag::Port => {
                let file = File::from_raw_fd(a.as_port());
                let s = BufReader::new(file).lines().next().unwrap().unwrap();
                Atom::new_string(CLString::from_string(s)).boxed().leak()
            }
            _ => Atom::new_error(Type).boxed().leak(),
        }
    }

    #[no_mangle]
    pub unsafe extern "C" fn cl_alloc_value(_: usize, _: *mut *mut Atom) -> *mut Atom {
        Atom::NULL.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn add(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 2 {
            return Atom::new_error(Arity).boxed().leak();
        }

        let mut a = (&**atoms).clone();

        for i in 1..count {
            let b = &**(atoms.add(i));
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
    pub unsafe extern "C" fn sub(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 2 {
            return Atom::new_error(Arity).boxed().leak();
        }

        let mut a = (&**atoms).clone();

        for i in 1..count {
            let b = &**atoms.add(i);
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
    pub unsafe extern "C" fn less_than(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 2 {
            return Atom::new_error(Arity).boxed().leak();
        }

        let mut a = (&**atoms).clone();

        for i in 1..count {
            let b = &**atoms.add(i);
            a = match (a.tag, b.tag) {
                (Tag::Int, Tag::Int) => Atom::new_bool(a.as_int() < b.as_int()),
                (Tag::Int, Tag::Float) => Atom::new_bool(a.as_int() < b.as_float() as i64),
                (Tag::Float, Tag::Int) => Atom::new_bool(a.as_float() < b.as_int() as f64),
                (Tag::Float, Tag::Float) => Atom::new_bool(a.as_float() < b.as_float()),
                _ => panic!("Can't compare non-numbers {:?} + {:?}", a, b),
            };
        }
        a.boxed().leak()
    }

    #[no_mangle]
    pub unsafe extern "C" fn more_than(count: usize, atoms: *mut *mut Atom) -> *mut Atom {
        if count < 2 {
            return Atom::new_error(Arity).boxed().leak();
        }
        let mut a = (&**atoms).clone();

        for i in 1..count {
            let b = &**atoms.add(i);
            a = match (a.tag, b.tag) {
                (Tag::Int, Tag::Int) => Atom::new_bool(a.as_int() > b.as_int()),
                (Tag::Int, Tag::Float) => Atom::new_bool(a.as_int() > b.as_float() as i64),
                (Tag::Float, Tag::Int) => Atom::new_bool(a.as_float() > b.as_int() as f64),
                (Tag::Float, Tag::Float) => Atom::new_bool(a.as_float() > b.as_float()),
                _ => panic!("Can't compare non-numbers {:?} + {:?}", a, b),
            };
        }
        a.boxed().leak()
    }
}
