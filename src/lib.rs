#![feature(box_into_inner)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(assert_matches)]
#![feature(destructuring_assignment)]

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

pub mod atom;
pub mod errors;
pub mod lexer;
pub mod mem;
pub mod parser;
pub mod symbol;
//pub mod vm2;
pub mod vm;

pub use errors::*;
use slotmap::DefaultKey;

use fnv::FnvBuildHasher;
use indexmap::IndexSet;

pub type EnvId = DefaultKey;
pub type FnvIndexSet<T> = IndexSet<T, FnvBuildHasher>;
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
