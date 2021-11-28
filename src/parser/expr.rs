use smol_str::SmolStr;

use crate::Span;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone)]
pub enum Args {
    Foldable,
    Arglist(Vec<SmolStr>),
}

#[derive(Debug, Clone)]
pub struct DefunExpr {
    pub name: SmolStr,
    pub args: Args,
    pub body: Expr,
}

#[derive(Clone)]
pub enum Expr {
    Symbol(SmolStr, Span),
    Float(f64, Span),
    Integer(i64, Span),
    List(Vec<Expr>, Span),
    Quoted(Box<Expr>, Span),
    Defun(Box<DefunExpr>, Span),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Span),
    Return(Option<Box<Expr>>, Span),
    Loop(Box<Expr>, Span),
    Let(SmolStr, Box<Expr>, Span),
    String(String, Span),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            match self {
                Self::Symbol(arg0, arg1) => {
                    f.debug_tuple("Symbol").field(arg0).field(arg1).finish()
                }
                Self::Float(arg0, arg1) => f.debug_tuple("Float").field(arg0).field(arg1).finish(),
                Self::Integer(arg0, arg1) => {
                    f.debug_tuple("Integer").field(arg0).field(arg1).finish()
                }
                Self::List(arg0, arg1) => f.debug_tuple("List").field(arg0).field(arg1).finish(),
                Self::Quoted(arg0, arg1) => {
                    f.debug_tuple("Quoted").field(arg0).field(arg1).finish()
                }
                Self::Defun(defun_expr, span) => f
                    .debug_tuple("Defun")
                    .field(defun_expr)
                    .field(span)
                    .finish(),
                Self::If(arg0, arg1, arg2, arg3) => f
                    .debug_tuple("If")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .field(arg3)
                    .finish(),
                Self::Return(arg0, arg1) => {
                    f.debug_tuple("Return").field(arg0).field(arg1).finish()
                }
                Self::Loop(arg0, arg1) => f.debug_tuple("Loop").field(arg0).field(arg1).finish(),
                Self::Let(arg0, arg1, arg2) => f
                    .debug_tuple("Let")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .finish(),

                Self::String(arg0, arg1) => {
                    f.debug_tuple("String").field(arg0).field(arg1).finish()
                }
            }
        } else {
            match self {
                Self::Symbol(arg0, ..) => f.debug_tuple("Symbol").field(arg0).finish(),
                Self::Float(arg0, ..) => f.debug_tuple("Float").field(arg0).finish(),
                Self::Integer(arg0, ..) => f.debug_tuple("Integer").field(arg0).finish(),
                Self::List(arg0, ..) => f.debug_tuple("List").field(arg0).finish(),
                Self::Quoted(arg0, ..) => f.debug_tuple("Quoted").field(arg0).finish(),
                Self::Defun(defun_expr, ..) => f.debug_tuple("Defun").field(defun_expr).finish(),
                Self::If(arg0, arg1, arg2, ..) => f
                    .debug_tuple("If")
                    .field(arg0)
                    .field(arg1)
                    .field(arg2)
                    .finish(),
                Self::Return(arg0, ..) => f.debug_tuple("Return").field(arg0).finish(),
                Self::Loop(arg0, ..) => f.debug_tuple("Loop").field(arg0).finish(),
                Self::Let(arg0, arg1, ..) => f.debug_tuple("Let").field(arg0).field(arg1).finish(),

                Self::String(arg0, ..) => f.debug_tuple("String").field(arg0).finish(),
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Symbol(_, _) => write!(f, "Symbol"),
            Expr::Float(_, _) => write!(f, "Float"),
            Expr::Integer(_, _) => write!(f, "Integer"),
            Expr::List(_, _) => write!(f, "List"),
            Expr::Quoted(q, _) => write!(f, "Quoted {}", &*q),
            Expr::Defun(..) => write!(f, "Defun"),
            Expr::If(_, _, _, _) => write!(f, "If"),
            Expr::Return(_, _) => write!(f, "Break"),
            Expr::Loop(_, _) => write!(f, "Loop"),
            Expr::Let(_, _, _) => write!(f, "Let"),
            Expr::String(_, _) => write!(f, "String"),
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        *match self {
            Expr::Symbol(_, m) => m,
            Expr::Float(_, m) => m,
            Expr::Integer(_, m) => m,
            Expr::List(_, m) => m,
            Expr::Quoted(_, m) => m,
            Expr::Defun(.., m) => m,
            Expr::If(_, _, _, m) => m,
            Expr::Return(_, m) => m,
            Expr::Loop(_, m) => m,
            Expr::Let(_, _, m) => m,
            Expr::String(_, m) => m,
        }
    }

    pub fn is_valued(&self) -> bool {
        match self {
            Expr::Symbol(_, _) => true,
            Expr::Float(_, _) => true,
            Expr::Integer(_, _) => true,
            Expr::List(_, _) => true,
            Expr::Quoted(_, _) => todo!(),
            Expr::Defun(..) => false,
            Expr::If(_, _, _, _) => true,
            Expr::Return(_, _) => todo!(),
            Expr::Loop(_, _) => todo!(),
            Expr::Let(..) => false,
            Expr::String(_, _) => true,
        }
    }

    pub fn float(self) -> f64 {
        match self {
            Expr::Float(n, _) => n,
            _ => unreachable!(),
        }
    }
    pub fn integer(self) -> i64 {
        match self {
            Expr::Integer(n, _) => n,
            _ => unreachable!(),
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

    // /// Returns inner sumbol
    // /// Panics if `self` is not [`Symbol`].
    // ///
    // /// [`Symbol`]: Expr::Symbol
    // pub fn unwrap_symbol(self) -> String {
    //     match self {
    //         Expr::Symbol(v, _) => v,
    //         _ => panic!("Called `as_symbol` on non-Symbol instance of Expr"),
    //     }
    // }
}
