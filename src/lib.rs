pub mod errors;
pub mod eval;
pub mod function;
pub mod jit;
pub mod lexer;
pub mod parser;
use std::ops::Range;

pub use errors::*;
use eval::Value;
use fnv::FnvHashMap;
pub type Env = FnvHashMap<String, Value>;
pub type Result<T, E = CranelispError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub source_id: &'static str,
}
impl Span {
    pub fn new(start: usize, end: usize, source_id: &'static str) -> Self {
        Self {
            start,
            end,
            source_id,
        }
    }
    pub fn point(point: usize, source_id: &'static str) -> Self {
        Self {
            start: point,
            end: point,
            source_id,
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = str;

    fn source(&self) -> &Self::SourceId {
        self.source_id
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

mod libcl {

    #[no_mangle]
    #[export_name = "print"]
    pub extern "C" fn cl_print(n: f64) -> f64 {
        println!("{}", n);
        0.0
    }
    #[no_mangle]
    #[export_name = "+"]
    pub extern "C" fn plus(a: f64, b: f64) -> f64 {
        a + b
    }
    #[no_mangle]
    #[export_name = "-"]
    pub extern "C" fn minus(a: f64, b: f64) -> f64 {
        a - b
    }
    #[no_mangle]
    #[export_name = "<"]
    pub extern "C" fn less_than(a: f64, b: f64) -> f64 {
        if a < b {
            1.0
        } else {
            0.0
        }
    }
    #[no_mangle]
    #[export_name = ">"]
    pub extern "C" fn more_than(a: f64, b: f64) -> f64 {
        if a > b {
            1.0
        } else {
            0.0
        }
    }
}
