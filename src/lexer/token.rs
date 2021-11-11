use crate::{eval::Type, Span};
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
    Eof(usize),
    Type(Type, Span),
    Loop(Span),
    If(Span),
    Return(Span),
    Let(Span),
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
            Token::Eof(s) => *s,
            Token::Type(_, s) => s.start,
            Token::Loop(s) => s.start,
            Token::If(s) => s.start,
            Token::Return(s) => s.start,
            Token::Let(s) => s.start,
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
            Token::Eof(s) => *s,
            Token::Type(_, s) => s.end,
            Token::Loop(s) => s.end,
            Token::If(s) => s.end,
            Token::Return(s) => s.end,
            Token::Let(s) => s.end,
        }
    }

    pub fn symbol_inner(self) -> String {
        match self {
            Token::Symbol(s, _) => s,
            _ => panic!("Called sumbol_inner on non symbol token"),
        }
    }

    pub fn is_valued(&self) -> bool {
        match self {
            Token::LParen(_) => true,
            Token::Number(_, _) => true,
            Token::Symbol(_, _) => true,
            Token::String(_, _) => true,
            Token::Loop(_) => true,
            Token::If(_) => true,
            Token::Return(_) => todo!("I think this is none?"),
            Token::Let(_) => todo!("Is this valued? Dunno"),
            Token::Quote(_) => todo!("Is this valued? Dunno"),
            _ => false,
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
            Token::Defun(_) => write!(fmt, ">"),
            Token::Eof(_) => write!(fmt, "EOF"),
            Token::Type(ty, _) => write!(fmt, ":{:?}", ty),
            Token::Loop(_) => write!(fmt, "loop"),
            Token::If(_) => write!(fmt, "?"),
            Token::Return(_) => write!(fmt, "return"),
            Token::Let(_) => write!(fmt, "let"),
        }
    }
}
