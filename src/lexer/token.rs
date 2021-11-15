use somok::Somok;

use crate::Result;
use crate::Span;
use std::fmt::Debug;

pub struct Token {
    pub span: Span,
    kind: TokenKind,
    data: TokenData,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;
        if matches!(self.data, TokenData::None) {
            ().okay()
        } else {
            write!(f, "({:?})", self.data)
        }
    }
}

impl Token {
    fn new(span: Span, kind: TokenKind, data: TokenData) -> Self {
        Self { span, kind, data }
    }

    pub fn lparen(span: Span) -> Self {
        Self::new(span, TokenKind::LParen, TokenData::None)
    }
    pub fn is_lparen(&self) -> bool {
        matches!(self.kind, TokenKind::LParen)
    }

    pub fn rparen(span: Span) -> Self {
        Self::new(span, TokenKind::RParen, TokenData::None)
    }
    pub fn is_rparen(&self) -> bool {
        matches!(self.kind, TokenKind::RParen)
    }

    pub fn float(span: Span, num: impl Into<f64>) -> Self {
        Self::new(span, TokenKind::Float, TokenData::Float(num.into()))
    }
    pub fn is_float(&self) -> bool {
        matches!(self.kind, TokenKind::Float)
    }
    pub fn extract_float(&self) -> Result<f64> {
        if self.is_float() {
            match self.data {
                TokenData::Float(f) => f.okay(),
                _ => unreachable!(),
            }
        } else {
            syntax!(UnexpectedToken, (self.span, "expected float here".into())).error()
        }
    }

    pub fn integer(span: Span, num: impl Into<i64>) -> Self {
        Self::new(span, TokenKind::Integer, TokenData::Integer(num.into()))
    }
    pub fn is_integer(&self) -> bool {
        matches!(self.kind, TokenKind::Integer)
    }
    pub fn extract_integer(&self) -> Result<i64> {
        if self.is_integer() {
            match self.data {
                TokenData::Integer(i) => i.okay(),
                _ => unreachable!(),
            }
        } else {
            syntax!(UnexpectedToken, (self.span, "expected integer here".into())).error()
        }
    }

    pub fn symbol(span: Span, sym: impl Into<String>) -> Self {
        Self::new(span, TokenKind::Symbol, TokenData::String(sym.into()))
    }
    pub fn is_symbol(&self) -> bool {
        matches!(self.kind, TokenKind::Symbol)
    }
    pub fn extract_symbol(&self) -> Result<String> {
        if self.is_symbol() {
            match &self.data {
                TokenData::String(s) => s.clone().okay(),
                _ => unreachable!(),
            }
        } else {
            syntax!(UnexpectedToken, (self.span, "expected symbol here".into())).error()
        }
    }

    pub fn string(span: Span, str: impl Into<String>) -> Self {
        let str = str.into().replace("\\n", "\n");
        Self::new(span, TokenKind::String, TokenData::String(str))
    }
    pub fn is_string(&self) -> bool {
        matches!(self.kind, TokenKind::String)
    }
    pub fn extract_string(&self) -> Result<String> {
        if self.is_string() {
            match &self.data {
                TokenData::String(s) => s.clone().okay(),
                _ => unreachable!(),
            }
        } else {
            syntax!(UnexpectedToken, (self.span, "expected string here".into())).error()
        }
    }

    pub fn quote(span: Span) -> Self {
        Self::new(span, TokenKind::Quote, TokenData::None)
    }
    pub fn is_quote(&self) -> bool {
        matches!(self.kind, TokenKind::Quote)
    }

    pub fn type_separator(span: Span) -> Self {
        Self::new(span, TokenKind::TypeSeparator, TokenData::None)
    }
    pub fn is_type_separator(&self) -> bool {
        matches!(self.kind, TokenKind::TypeSeparator)
    }

    pub fn eof(span: Span) -> Self {
        Self::new(span, TokenKind::Eof, TokenData::None)
    }
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::Eof)
    }
    pub fn whitespace(span: Span) -> Self {
        Self::new(span, TokenKind::Whitespace, TokenData::None)
    }
    pub fn is_whitespace(&self) -> bool {
        matches!(self.kind, TokenKind::Whitespace)
    }
}

pub enum TokenData {
    String(String), // don't forget to replce `/n` with literal newline
    Float(f64),
    Integer(i64),
    None,
}

impl Debug for TokenData {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(fmt, "{:?}", s),
            Self::Float(f) => write!(fmt, "{:?}", f),
            Self::Integer(i) => write!(fmt, "{:?}", i),
            Self::None => ().okay(),
        }
    }
}

pub enum TokenKind {
    LParen,
    RParen,
    Float,
    Integer,
    Symbol,
    String,
    Quote,
    TypeSeparator,
    Eof,
    Whitespace,
}

impl Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Float => write!(f, "F"),
            Self::Integer => write!(f, "I"),
            Self::Symbol => write!(f, "S"),
            Self::String => write!(f, "Str"),
            Self::Quote => write!(f, "'"),
            Self::TypeSeparator => write!(f, ":"),
            Self::Eof => write!(f, "Eof"),
            Self::Whitespace => write!(f, "WS"),
        }
    }
}
