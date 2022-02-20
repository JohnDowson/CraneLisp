use crate::Span;
use std::fmt::{Debug, Display};

#[derive(Clone)]
pub enum Expr<'s> {
    Symbol(&'s str, Span),
    Float(f64, Span),
    Integer(i32, Span),
    List(Vec<Expr<'s>>, Span),
    String(&'s str, Span),
    Null(Span),
}

impl<'s> Debug for Expr<'s> {
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
                Self::String(arg0, arg1) => {
                    f.debug_tuple("String").field(arg0).field(arg1).finish()
                }
                Self::Null(span) => f.debug_tuple("Null").field(span).finish(),
            }
        } else {
            match self {
                Self::Symbol(arg0, ..) => f.debug_tuple("Symbol").field(arg0).finish(),
                Self::Float(arg0, ..) => f.debug_tuple("Float").field(arg0).finish(),
                Self::Integer(arg0, ..) => f.debug_tuple("Integer").field(arg0).finish(),
                Self::List(arg0, ..) => f.debug_tuple("List").field(arg0).finish(),
                Self::String(arg0, ..) => f.debug_tuple("String").field(arg0).finish(),
                Self::Null(..) => write!(f, "Null"),
            }
        }
    }
}

impl<'s> Display for Expr<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Symbol(_, _) => write!(f, "Symbol"),
            Expr::Float(_, _) => write!(f, "Float"),
            Expr::Integer(_, _) => write!(f, "Integer"),
            Expr::List(_, _) => write!(f, "List"),
            Expr::String(_, _) => write!(f, "String"),
            Expr::Null(_) => write!(f, "Null"),
        }
    }
}

impl<'s> Expr<'s> {
    pub fn span(&self) -> Span {
        *match self {
            Expr::Symbol(_, m) => m,
            Expr::Float(_, m) => m,
            Expr::Integer(_, m) => m,
            Expr::List(_, m) => m,
            Expr::String(_, m) => m,
            Expr::Null(m) => m,
        }
    }
}
