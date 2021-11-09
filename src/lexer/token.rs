use crate::Span;
use std::fmt::Debug;
#[derive(Clone)]
pub enum Token {
    LParen(usize),
    RParen(usize),
    Number(f64, Span),
    Symbol(String, Span),
    String(String, Span),
    Quote(usize),
    Defun(usize),
}

impl Token {
    pub fn span(&self) -> Span {
        self.span_start()..self.span_end()
    }
    pub fn span_start(&self) -> usize {
        match self {
            Token::LParen(s) => *s,
            Token::RParen(s) => *s,
            Token::Number(_, s) => s.start,
            Token::Symbol(_, s) => s.start,
            Token::String(_, s) => s.start,
            Token::Quote(s) => *s,
            Token::Defun(s) => *s,
        }
    }
    pub fn span_end(&self) -> usize {
        match self {
            Token::LParen(s) => *s,
            Token::RParen(s) => *s,
            Token::Number(_, s) => s.end,
            Token::Symbol(_, s) => s.end,
            Token::String(_, s) => s.end,
            Token::Quote(s) => *s,
            Token::Defun(s) => *s,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Token::LParen(_) => write!(fmt, "(",),
            Token::RParen(_) => write!(fmt, ")",),
            Token::Number(n, _) => write!(fmt, "{:?}", n,),
            Token::Symbol(s, _) => write!(fmt, "{}", s,),
            Token::String(s, _) => write!(fmt, "{:?}", s,),
            Token::Quote(_) => write!(fmt, "'"),
            Token::Defun(_) => write!(fmt, ":"),
        }
    }
}
