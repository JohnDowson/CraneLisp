use super::FnArgs;
use crate::{function::Type, lexer::Token, Span};
use std::fmt::Debug;
#[derive(Clone)]
pub enum Expr {
    Symbol(String, Meta),
    Number(f64, Meta),
    List(Vec<Expr>, Meta),
    Quoted(Box<Expr>, Meta),
    Defun(String, FnArgs, Box<Expr>, Type, Meta),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Meta),
    Break(Option<Box<Expr>>, Meta),
    Loop(Box<Expr>, Meta),
    Let(String, Box<Expr>, Meta),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            match self {
                Self::Symbol(arg0, arg1) => {
                    f.debug_tuple("Symbol").field(arg0).field(arg1).finish()
                }
                Self::Number(arg0, arg1) => {
                    f.debug_tuple("Number").field(arg0).field(arg1).finish()
                }
                Self::List(arg0, arg1) => f.debug_tuple("List").field(arg0).field(arg1).finish(),
                Self::Quoted(arg0, arg1) => {
                    f.debug_tuple("Quoted").field(arg0).field(arg1).finish()
                }
                Self::Defun(name, arg0, arg1, arg2, arg3) => f
                    .debug_tuple("Defun")
                    .field(name)
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .field(arg3)
                    .finish(),
                Self::If(arg0, arg1, arg2, arg3) => f
                    .debug_tuple("If")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .field(arg3)
                    .finish(),
                Self::Break(arg0, arg1) => f.debug_tuple("Return").field(arg0).field(arg1).finish(),
                Self::Loop(arg0, arg1) => f.debug_tuple("Loop").field(arg0).field(arg1).finish(),
                Self::Let(arg0, arg1, arg2) => f
                    .debug_tuple("Let")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .finish(),
            }
        } else {
            match self {
                Self::Symbol(arg0, ..) => f.debug_tuple("Symbol").field(arg0).finish(),
                Self::Number(arg0, ..) => f.debug_tuple("Number").field(arg0).finish(),
                Self::List(arg0, ..) => f.debug_tuple("List").field(arg0).finish(),
                Self::Quoted(arg0, ..) => f.debug_tuple("Quoted").field(arg0).finish(),
                Self::Defun(arg0, arg1, arg2, ..) => f
                    .debug_tuple("Defun")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .finish(),
                Self::If(arg0, arg1, arg2, ..) => f
                    .debug_tuple("If")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .finish(),
                Self::Break(arg0, ..) => f.debug_tuple("Return").field(arg0).finish(),
                Self::Loop(arg0, ..) => f.debug_tuple("Loop").field(arg0).finish(),
                Self::Let(arg0, arg1, ..) => f.debug_tuple("Let").field(arg0).field(arg1).finish(),
            }
        }
    }
}

impl Expr {
    pub fn meta(&self) -> Meta {
        match self {
            Expr::Symbol(_, m) => m,
            Expr::Number(_, m) => m,
            Expr::List(_, m) => m,
            Expr::Quoted(_, m) => m,
            Expr::Defun(.., m) => m,
            Expr::If(_, _, _, m) => m,
            Expr::Break(_, m) => m,
            Expr::Loop(_, m) => m,
            Expr::Let(_, _, m) => m,
        }
        .clone()
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::Symbol(_, m) => m,
            Expr::Number(_, m) => m,
            Expr::List(_, m) => m,
            Expr::Quoted(_, m) => m,
            Expr::Defun(.., m) => m,
            Expr::If(_, _, _, m) => m,
            Expr::Break(_, m) => m,
            Expr::Loop(_, m) => m,
            Expr::Let(_, _, m) => m,
        }
        .span
        .clone()
    }

    pub fn is_valued(&self) -> bool {
        match self {
            Expr::Symbol(_, _) => true,
            Expr::Number(_, _) => true,
            Expr::List(_, _) => true,
            Expr::Quoted(_, _) => todo!(),
            Expr::Defun(_, _, _, _, _) => false,
            Expr::If(_, _, _, _) => true,
            Expr::Break(_, _) => todo!(),
            Expr::Loop(_, _) => todo!(),
            Expr::Let(..) => false,
        }
    }

    pub fn number(self) -> f64 {
        match self {
            Expr::Number(n, _) => n,
            _ => unreachable!(),
        }
    }

    pub fn from_token(token: &Token) -> Self {
        match token {
            Token::Number(val, span) => Expr::Number(*val, Meta { span: span.clone() }),
            Token::Symbol(val, span) => Expr::Symbol(val.clone(), Meta { span: span.clone() }),
            _ => panic!(),
        }
    }

    /// Returns `true` if the expr is [`Symbol`].
    ///
    /// [`Symbol`]: Expr::Symbol
    pub fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(..))
    }

    /// Returns `true` if the expr is [`List`].
    ///
    /// [`List`]: Expr::List
    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(..))
    }

    /// Returns inner list of expressions
    /// Panics if `self` is not [`List`].
    ///
    /// [`List`]: Expr::List
    pub fn unwrap_list(self) -> Vec<Expr> {
        match self {
            Expr::List(v, _) => v,
            _ => panic!("Called `as_list` on non-List instance of Expr"),
        }
    }

    /// Returns inner sumbol
    /// Panics if `self` is not [`Symbol`].
    ///
    /// [`Symbol`]: Expr::Symbol
    pub fn unwrap_symbol(self) -> String {
        match self {
            Expr::Symbol(v, _) => v,
            _ => panic!("Called `as_symbol` on non-Symbol instance of Expr"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Meta {
    pub span: Span,
}
