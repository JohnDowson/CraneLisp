use std::io::Read;
use std::ops::Range;

use crate::lexer::{Lexer, Token};
use crate::{CranelispError, Result, Span, SyntaxError};
use log::trace;
use somok::Somok;

#[derive(Debug, Clone)]
pub enum Expr {
    Symbol(String, Meta),
    Number(f64, Meta),
    List(Vec<Expr>, Meta),
    Quoted(Box<Expr>, Meta),
    Defun(Box<Expr>, Box<Expr>, Meta),
}

impl Expr {
    fn meta(&self) -> Meta {
        match self {
            Expr::Symbol(_, m) => m,
            Expr::Number(_, m) => m,
            Expr::List(_, m) => m,
            Expr::Quoted(_, m) => m,
            Expr::Defun(_, _, m) => m,
        }
        .clone()
    }

    fn span(&self) -> Span {
        match self {
            Expr::Symbol(_, m) => m,
            Expr::Number(_, m) => m,
            Expr::List(_, m) => m,
            Expr::Quoted(_, m) => m,
            Expr::Defun(_, _, m) => m,
        }
        .span
        .clone()
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
    pub fn new(src: impl Read) -> Result<Self> {
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
                self.previous_tokens.push(token);
                expr.okay()
            }

            Ok(token @ Token::Symbol(..)) => {
                let expr = Expr::from_token(&token).unwrap();
                self.previous_tokens.push(token);
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
                let args = self.parse_expr()?;
                let body = self.parse_expr()?;
                match (args, body) {
                    (args @ Expr::List(..), body @ Expr::List(..)) => {
                        let span = token.span_start()..body.span().end;
                        Expr::Defun(Box::new(args), Box::new(body), Meta { span }).okay()
                    }
                    (Expr::List(..), body) => CranelispError::Syntax(
                        SyntaxError::FunctionHasNoBody(token.span(), body.span()),
                    )
                    .error(),
                    (args, Expr::List(..)) => CranelispError::Syntax(
                        SyntaxError::FunctionHasNoArglist(token.span(), args.span()),
                    )
                    .error(),
                    (args, body) => CranelispError::Syntax(SyntaxError::InvalidDefun(
                        token.span(),
                        args.span(),
                        body.span(),
                    ))
                    .error(),
                }
            }
            Ok(Token::Eof(..)) => CranelispError::EOF.error(),
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
