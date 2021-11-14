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
        {
            crate::errors::SyntaxError {
                kind: crate::errors::SyntaxErrorKind::$kind,
                spans: vec![],
            }
        }
    };
}

pub mod errors;
//pub mod eval;
//pub mod function;
//pub mod jit;
pub mod lexer;
pub mod parser;

pub use errors::*;
//use fnv::FnvHashMap;
//pub type Env = FnvHashMap<String, Value>;
pub type Result<T, E = CranelispError> = std::result::Result<T, E>;

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
