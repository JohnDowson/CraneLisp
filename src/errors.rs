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
pub enum SyntaxError {
    UnmatchedParen(Span, Span),
    UnexpectedCharacter(Span, char),
    InvalidLiteral(Span, String),
    UnexpectedToken(Span, String),

    FunctionHasNoBody(Span, Span),
    FunctionHasNoArglist(Span, Span),
    InvalidDefun(Span, Span, Span),

    MissingType(Span),
    UnknownType(Span, String),
}
impl SyntaxError {
    pub fn spans(&self) -> Vec<Span> {
        match self {
            SyntaxError::UnmatchedParen(a, b, ..) => vec![a.clone(), b.clone()],
            SyntaxError::UnexpectedCharacter(a, ..) => vec![a.clone()],
            SyntaxError::InvalidLiteral(a, ..) => vec![a.clone()],
            SyntaxError::UnexpectedToken(a, ..) => vec![a.clone()],

            SyntaxError::FunctionHasNoBody(a, b) => vec![a.clone(), b.clone()],
            SyntaxError::FunctionHasNoArglist(a, b) => vec![a.clone(), b.clone()],
            SyntaxError::InvalidDefun(a, b, c) => vec![a.clone(), b.clone(), c.clone()],

            SyntaxError::MissingType(b) => vec![b.clone()],
            SyntaxError::UnknownType(a, ..) => vec![a.clone()],
        }
    }
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            SyntaxError::UnmatchedParen(_, _) => "Unmatched paren".into(),
            SyntaxError::UnexpectedCharacter(_, c) => format!("Unexpected character {}", c),
            SyntaxError::InvalidLiteral(_, s) => format!("Invalid numeric literal {}", s),
            SyntaxError::UnexpectedToken(_, s) => format!("Unexpected token {}", s),

            SyntaxError::FunctionHasNoBody(_, _) => "Functions must have body".to_string(),
            SyntaxError::FunctionHasNoArglist(_, _) => "Functions must have arglist".to_string(),
            SyntaxError::InvalidDefun(..) => "Functions must have arglist and body".to_string(),

            SyntaxError::MissingType(..) => "Expected type".to_string(),
            SyntaxError::UnknownType(_, s) => format!("Unknown type {}", s),
        };
        write!(f, "{}", msg)
    }
}
