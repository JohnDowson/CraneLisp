use smol_str::SmolStr;

use crate::Span;
use std::fmt::Debug;

pub enum Token {
    LParen(Span),
    RParen(Span),
    Float(f64, Span),
    Integer(i64, Span),
    Symbol(SmolStr, Span),
    String(String, Span),
    Quote(Span),
    Eof(Span),
    Whitespace(Span),
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LParen(..) => write!(f, "("),
            Self::RParen(..) => write!(f, ")"),
            Self::Float(float, ..) => write!(f, "{:?}", float),
            Self::Integer(int, ..) => write!(f, "{:?}", int),
            Self::Symbol(sym, ..) => write!(f, "{:?}", sym),
            Self::String(str, ..) => write!(f, "\"{:?}\"", str),
            Self::Quote(..) => write!(f, "'"),
            Self::Eof(..) => write!(f, "Eof"),
            Self::Whitespace(..) => write!(f, " "),
        }
    }
}

impl Token {
    pub fn span(&self) -> Span {
        *match self {
            Token::LParen(span) => span,
            Token::RParen(span) => span,
            Token::Float(_, span) => span,
            Token::Integer(_, span) => span,
            Token::Symbol(_, span) => span,
            Token::String(_, span) => span,
            Token::Quote(span) => span,
            Token::Eof(span) => span,
            Token::Whitespace(span) => span,
        }
    }
    pub fn extract_float(&self) -> f64 {
        match self {
            Self::Float(f, ..) => *f,
            _ => panic!(),
        }
    }
    pub fn extract_integer(&self) -> i64 {
        match self {
            Self::Integer(i, ..) => *i,
            _ => panic!(),
        }
    }
    pub fn extract_symbol(&self) -> SmolStr {
        match self {
            Self::Symbol(s, ..) => s.clone(),
            _ => panic!(),
        }
    }
    pub fn extract_string(&self) -> String {
        match self {
            Self::String(s, ..) => s.clone(),
            _ => panic!(),
        }
    }

    /// Returns `true` if the token is [`Whitespace`].
    ///
    /// [`Whitespace`]: Token::Whitespace
    pub fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace(..))
    }

    /// Returns `true` if the token is [`LParen`].
    ///
    /// [`LParen`]: Token::LParen
    pub fn is_lparen(&self) -> bool {
        matches!(self, Self::LParen(..))
    }

    /// Returns `true` if the token is [`RParen`].
    ///
    /// [`RParen`]: Token::RParen
    pub fn is_rparen(&self) -> bool {
        matches!(self, Self::RParen(..))
    }

    /// Returns `true` if the token is [`Float`].
    ///
    /// [`Float`]: Token::Float
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float(..))
    }

    /// Returns `true` if the token is [`Integer`].
    ///
    /// [`Integer`]: Token::Integer
    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Integer(..))
    }

    /// Returns `true` if the token is [`Symbol`].
    ///
    /// [`Symbol`]: Token::Symbol
    pub fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(..))
    }

    /// Returns `true` if the token is [`String`].
    ///
    /// [`String`]: Token::String
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    /// Returns `true` if the token is [`Quote`].
    ///
    /// [`Quote`]: Token::Quote
    pub fn is_quote(&self) -> bool {
        matches!(self, Self::Quote(..))
    }

    /// Returns `true` if the token is [`Eof`].
    ///
    /// [`Eof`]: Token::Eof
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Eof(..))
    }
}
