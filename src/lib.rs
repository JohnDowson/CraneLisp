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
pub type Span = Range<usize>;

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
