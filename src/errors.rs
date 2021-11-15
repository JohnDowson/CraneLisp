use std::fmt::Debug;
use std::fmt::Display;

use somok::Somok;

use crate::{Result, Span};

#[derive(Debug, thiserror::Error)]
pub enum CranelispError {
    #[error("Syntax error {0:?}")]
    Syntax(SyntaxError),
    //#[error("Type error {0}")]
    //Type(String),
    //#[error("Lookup error {0}")]
    //Lookup(String),
    #[error("{0}")]
    IO(#[from] std::io::Error),
    #[error("EOF")]
    EOF,
    #[error("Unexpected EOF: expected {1}")]
    UnexpectedEOF(Span, String),
    #[error("Couldn't get input: {0}")]
    ReplIO(#[from] rustyline::error::ReadlineError),
    #[error("")]
    Eval(EvalError),
    #[error("Couldn't get input: {0}")]
    JIT(#[from] cranelift_module::ModuleError),
}

pub fn syntax<T>(error: SyntaxError) -> Result<T> {
    CranelispError::Syntax(error).error()
}

pub fn eval<T>(error: EvalError) -> Result<T> {
    CranelispError::Eval(error).error()
}

#[derive(Debug)]
pub enum EvalError {
    Undefined(String, Span),
    ArityMismatch,
    InvalidSignature(String),
    UnexpectedVirtualFunction(Span),
}

#[derive(Debug)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub spans: Vec<(Span, String)>,
}

#[derive(Debug)]
pub enum SyntaxErrorKind {
    UnmatchedParen,
    UnexpectedCharacter,
    InvalidLiteral,
    UnexpectedToken,
    UnexpectedExpression,

    FunctionHasNoBody,
    FunctionHasNoArglist,
    InvalidDefun,

    MissingType,
    UnknownType,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SyntaxError::{:?}", &self.kind)
    }
}
