use crate::{function::Type, Span};
use std::fmt::Debug;

#[derive(Debug)]
pub struct Token {
    pub span: Span,
    kind: TokenKind,
    data: TokenData,
}

impl Token {
    fn new(span: Span, kind: TokenKind, data: TokenData) -> Self {
        Self { span, kind, data }
    }

    pub fn lparen(span: Span) -> Self {
        Self::new(span, TokenKind::LParen, TokenData::None)
    }
    pub fn rparen(span: Span) -> Self {
        Self::new(span, TokenKind::RParen, TokenData::None)
    }
    pub fn float(span: Span, num: impl Into<f64>) -> Self {
        Self::new(span, TokenKind::Float, TokenData::Float(num.into()))
    }
    pub fn integer(span: Span, num: impl Into<i64>) -> Self {
        Self::new(span, TokenKind::Integer, TokenData::Integer(num.into()))
    }
    pub fn symbol(span: Span, sym: impl Into<String>) -> Self {
        Self::new(span, TokenKind::Symbol, TokenData::String(sym.into()))
    }
    pub fn string(span: Span, str: impl Into<String>) -> Self {
        Self::new(span, TokenKind::String, TokenData::String(str.into()))
    }
    pub fn Quote(span: Span) -> Self {
        Self::new(span, TokenKind::Quote, TokenData::None)
    }
}
#[derive(Debug)]
pub enum TokenData {
    String(String), // don't forget to replce `/n` with literal newline
    Float(f64),
    Integer(i64),
    None,
}

#[derive(Debug)]
pub enum TokenKind {
    LParen,
    RParen,
    Float,
    Integer,
    Symbol,
    String,
    Quote,
}
