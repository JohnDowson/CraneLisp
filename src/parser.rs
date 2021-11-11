use std::fmt::Debug;
use std::ops::Range;

use crate::eval::Type;
use crate::lexer::{Lexer, Token};
use crate::{CranelispError, Result, Span, SyntaxError};
use log::trace;
use somok::Somok;

pub type Arglist = Vec<(String, Type)>;

#[derive(Clone)]
pub enum Expr {
    Symbol(String, Meta),
    Number(f64, Meta),
    List(Vec<Expr>, Meta),
    Quoted(Box<Expr>, Meta),
    Defun(Arglist, Box<Expr>, Type, Meta),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Meta),
    Return(Option<Box<Expr>>, Meta),
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
                Self::Defun(arg0, arg1, arg2, arg3) => f
                    .debug_tuple("Defun")
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
                Self::Return(arg0, ..) => f.debug_tuple("Return").field(arg0).finish(),
                Self::Loop(arg0, ..) => f.debug_tuple("Loop").field(arg0).finish(),
                Self::Let(arg0, arg1, ..) => f.debug_tuple("Let").field(arg0).field(arg1).finish(),
            }
        }
    }
}

impl Expr {
    fn meta(&self) -> Meta {
        match self {
            Expr::Symbol(_, m) => m,
            Expr::Number(_, m) => m,
            Expr::List(_, m) => m,
            Expr::Quoted(_, m) => m,
            Expr::Defun(_, _, _, m) => m,
            Expr::If(_, _, _, m) => m,
            Expr::Return(_, m) => m,
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
            Expr::Defun(_, _, _, m) => m,
            Expr::If(_, _, _, m) => m,
            Expr::Return(_, m) => m,
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
            Expr::Defun(_, _, _, _) => true,
            Expr::If(_, _, _, _) => true,
            Expr::Return(_, _) => todo!(),
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

    fn from_token(token: &Token) -> Result<Self, ()> {
        match token {
            Token::Number(val, span) => Ok(Expr::Number(*val, Meta { span: span.clone() })),
            Token::Symbol(val, span) => Ok(Expr::Symbol(val.clone(), Meta { span: span.clone() })),
            _ => Err(()),
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
    span: Range<usize>,
}

pub struct Parser {
    lexer: Lexer,
    previous_tokens: Vec<Token>,
}

impl Parser {
    pub fn new(src: String) -> Result<Self> {
        Self {
            lexer: Lexer::new(src)?,
            previous_tokens: Vec::new(),
        }
        .okay()
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        match self.lexer.next_token() {
            Ok(token @ Token::LParen(_)) => {
                self.previous_tokens.push(token);
                self.parse_list()
            }

            Ok(token @ Token::RParen(_)) => {
                trace!("Unexpected rparen");
                CranelispError::Syntax(SyntaxError::UnexpectedCharacter(token.span(), ')')).error()
            }

            Ok(token @ Token::Number(..)) => {
                let expr = Expr::from_token(&token).unwrap();
                expr.okay()
            }

            Ok(token @ Token::Symbol(..)) => {
                let expr = Expr::from_token(&token).unwrap();
                expr.okay()
            }

            Ok(token @ Token::Quote(_)) => {
                if let Ok(paren) = self.lexer.next_token() {
                    if matches!(paren, Token::LParen(..)) {
                        self.previous_tokens.push(token);
                        self.previous_tokens.push(paren)
                    } else {
                        return CranelispError::Syntax(SyntaxError::UnexpectedCharacter(
                            token.span(),
                            ')',
                        ))
                        .error();
                    }
                }
                let expr = self.parse_expr()?;
                let meta = expr.meta();
                Expr::Quoted(Box::new(expr), meta).okay()
            }
            Ok(Token::String(..)) => todo!("String exprs are unimplemented"),

            Ok(token @ Token::Defun(..)) => {
                let ret = self.eat_type()?;
                let args = self.eat_arglist()?;
                let body = self.parse_expr()?;
                match body {
                    // This is a hack to test JIT: Function body should b a list
                    body
                    @ (Expr::List(..) | Expr::Number(..) | Expr::Symbol(..) | Expr::If(..)) => {
                        let span = token.span_start()..body.span().end;
                        Expr::Defun(args, Box::new(body), ret, Meta { span }).okay()
                    }
                    body => CranelispError::Syntax(SyntaxError::FunctionHasNoBody(
                        token.span(),
                        body.span(),
                    ))
                    .error(),
                    // => CranelispError::Syntax(
                    //    SyntaxError::FunctionHasNoArglist(token.span(), args.span()),
                }
            }
            Ok(_token @ Token::Type(..)) => {
                todo!("Error for type where no type expected")
            }
            Ok(token @ Token::Loop(..)) => match self.parse_expr()? {
                body @ Expr::List(..) => {
                    let span = token.span();
                    Expr::Loop(Box::new(body), Meta { span })
                }
                .okay(),
                _ => todo!(),
            },
            Ok(token @ Token::If(..)) => {
                match (self.parse_expr()?, self.parse_expr()?, self.parse_expr()?) {
                    (
                        cond @ (Expr::List(..) | Expr::Number(..) | Expr::Symbol(..)),
                        truth
                        @
                        (Expr::List(..)
                        | Expr::Number(..)
                        | Expr::Symbol(..)
                        | Expr::Return(..)),
                        lie
                        @
                        (Expr::List(..)
                        | Expr::Number(..)
                        | Expr::Symbol(..)
                        | Expr::Return(..)),
                    ) => {
                        let span = token.span_start()..lie.span().end;
                        Expr::If(
                            Box::new(cond),
                            Box::new(truth),
                            Box::new(lie),
                            Meta { span },
                        )
                    }
                    .okay(),
                    _ => todo!(),
                }
            }
            Ok(token @ Token::Return(..)) => {
                let mut value = None;
                if self.lexer.peek_token()?.is_valued() {
                    value = Box::new(self.parse_expr()?).some()
                }
                Expr::Return(value, Meta { span: token.span() }).okay()
            }
            Ok(token @ Token::Let(..)) => match (self.lexer.next_token()?, self.parse_expr()?) {
                (sym @ Token::Symbol(_, _), expr) if expr.is_valued() => Expr::Let(
                    sym.symbol_inner(),
                    Box::new(expr),
                    Meta { span: token.span() },
                )
                .okay(),
                _ => todo!("Handle case when there is no let body"),
            },

            Ok(Token::Eof(..)) => CranelispError::EOF.error(),
            Err(e) => e.error(),
        }
    }

    fn _eat_lparen(&mut self) -> Result<Token> {
        match self.lexer.next_token() {
            token @ Ok(Token::LParen(..)) => token,
            Ok(token) => {
                // TODO this should be unexpected token
                CranelispError::Syntax(SyntaxError::MissingType(token.span())).error()?
            }
            Err(_) => todo!(),
        }
    }

    fn eat_arglist(&mut self) -> Result<Arglist> {
        let mut arglist: Arglist = Vec::new();
        let _lparen = match self.lexer.next_token() {
            Ok(token @ Token::LParen(..)) => token,
            Ok(token) => CranelispError::Syntax(SyntaxError::MissingType(token.span())).error()?,
            Err(_) => todo!(),
        };
        loop {
            let symbol = match self.lexer.next_token() {
                Ok(_token @ Token::RParen(..)) => return arglist.okay(),
                Ok(Token::Symbol(s, ..)) => s,
                Ok(token) => {
                    // TODO: General "UnexpectedToken" error
                    return CranelispError::Syntax(SyntaxError::MissingType(token.span())).error();
                }
                Err(_) => todo!(),
            };

            let ty = match self.lexer.next_token() {
                Ok(Token::Type(t, ..)) => t,
                Ok(token) => {
                    // TODO: General "UnexpectedToken" error
                    CranelispError::Syntax(SyntaxError::MissingType(token.span())).error()?
                }
                Err(_) => todo!(),
            };

            arglist.push((symbol, ty));
        }
    }

    fn eat_type(&mut self) -> Result<Type> {
        match self.lexer.next_token() {
            Ok(Token::Type(ty, ..)) => ty.okay(),
            Ok(tok @ Token::Eof(..)) => CranelispError::UnexpectedEOF(
                tok.span(),
                "Unexpected EOF when expecting Type".into(),
            )
            .error(),
            Ok(tok) => CranelispError::Syntax(SyntaxError::MissingType(tok.span())).error(),
            Err(e) => e.error(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr> {
        // Safety: we have just pushed to previous_tokens
        let first_token = self.previous_tokens.last().unwrap().clone();
        let mut exprs = Vec::new();
        loop {
            match self.lexer.peek_token()? {
                Token::RParen(..) => {
                    self.previous_tokens.push(self.lexer.next_token()?);
                    break;
                }
                token @ Token::Eof(..) => {
                    return CranelispError::Syntax(SyntaxError::UnmatchedParen(
                        first_token.span(),
                        token.span(),
                    ))
                    .error();
                }
                _ => {
                    let expr = self.parse_expr()?;
                    exprs.push(expr);
                }
            };
        }
        // Safety: we have just pushed to previous_tokens
        let last_token = self.previous_tokens.last().unwrap();
        Expr::List(
            exprs,
            Meta {
                span: first_token.span_start()..last_token.span_end(),
            },
        )
        .okay()
    }
}
