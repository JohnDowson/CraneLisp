#![allow(dead_code)]
use crate::function::Type;
use crate::lexer::{Lexer, Token};
use crate::value::{cons, Atom, Symbol};
use crate::{intern, CranelispError, Result, Span};
use smol_str::SmolStr;
use somok::{Leaksome, Somok};
use std::str::FromStr;
pub type Arglist = Vec<(String, Type)>;

pub struct Parser<'l> {
    lexer: &'l mut Lexer,
}

impl<'l> Parser<'l> {
    pub fn new(lexer: &'l mut Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse_expr(&mut self) -> Result<(Atom, Span)> {
        match self.lexer.next_token()? {
            Token::RParen(span) => {
                syntax!(UnexpectedToken, (span, "Unexpected rparen".into())).error()
            }

            Token::Float(f, span) => {
                let expr = (Atom::new_float(f), span);
                expr.okay()
            }
            Token::Integer(i, span) => {
                let expr = (Atom::new_int(i), span);
                expr.okay()
            }

            token @ Token::LParen(..) => {
                self.skip_whitespace()?;
                self.parse_list(token)
            }
            token @ Token::Quote(..) => {
                let (atom, span) = self.parse_list(token)?;
                (
                    cons(
                        Atom::new_symbol(Symbol::new("quote").boxed())
                            .boxed()
                            .leak(),
                        atom.boxed().leak(),
                    ),
                    span,
                )
                    .okay()
            }

            Token::Symbol(sym, span) => {
                let sym = intern(sym);
                (Atom::new_symbol(sym.boxed()), span).okay()
            }

            Token::String(string, span) => (Atom::new_string(string.into()), span).okay(),

            Token::TypeSeparator(span) => {
                syntax!(UnexpectedToken, (span, "Unexpected type separator".into())).error()
            }

            Token::Whitespace(..) => self.parse_expr(),

            Token::Eof(..) => CranelispError::EOF.error(),
        }
    }

    fn skip_whitespace(&mut self) -> Result<()> {
        loop {
            match self.lexer.peek_token()? {
                token if token.is_whitespace() => {
                    self.lexer.next_token()?;
                    continue;
                }
                _ => return ().okay(),
            }
        }
    }

    fn eat_lparen(&mut self) -> Result<Token> {
        match self.lexer.next_token()? {
            token if token.is_lparen() => token.okay(),
            token if token.is_whitespace() => self.eat_lparen(),
            token => syntax!(
                UnexpectedToken,
                (token.span(), "LParen expected here".into())
            )
            .error(),
        }
    }

    fn eat_rparen(&mut self) -> Result<Token> {
        match self.lexer.next_token()? {
            token if token.is_rparen() => token.okay(),
            token if token.is_whitespace() => self.eat_rparen(),
            token => syntax!(
                UnexpectedToken,
                (token.span(), "RParen expected here".into())
            )
            .error(),
        }
    }

    fn _eat_type(&mut self) -> Result<Type> {
        match self.lexer.next_token()? {
            Token::Symbol(sym, ..) => Type::from_str(&sym)?.okay(),
            Token::Whitespace(..) => self._eat_type(),
            token => syntax!(
                UnexpectedToken,
                (
                    token.span(),
                    format!("Type expected here, found {:?}", token)
                )
            )
            .error(),
        }
    }

    fn _eat_type_separator(&mut self) -> Result<()> {
        match self.lexer.next_token()? {
            Token::TypeSeparator(..) => ().okay(),
            token => syntax!(
                UnexpectedToken,
                (
                    token.span(),
                    format!("Type separator expected here, found {:?}", token)
                )
            )
            .error(),
        }
    }

    fn eat_symbol(&mut self) -> Result<SmolStr> {
        match self.lexer.next_token()? {
            Token::Symbol(sym, ..) => sym.okay(),
            token if token.is_whitespace() => self.eat_symbol(),
            token => syntax!(
                UnexpectedToken,
                (token.span(), "Symbol expected here".into())
            )
            .error(),
        }
    }

    fn parse_list(&mut self, first_token: Token) -> Result<(Atom, Span)> {
        let (mut atom, _) = match self.parse_expr() {
            Ok((val, span)) => (
                cons(val.boxed().leak(), Atom::NULL.boxed().leak())
                    .boxed()
                    .leak(),
                span,
            ),
            Err(e) => match e {
                CranelispError::Syntax(se) => match se.kind {
                    crate::SyntaxErrorKind::UnexpectedRParen => {
                        return (
                            Atom::NULL,
                            Span::merge(first_token.span(), se.spans.last().unwrap().0),
                        )
                            .okay()
                    }
                    _ => return CranelispError::Syntax(se).error(),
                },
                _ => return e.error(),
            },
        };
        let head = atom as *mut Atom;
        loop {
            match self.lexer.peek_token()? {
                token if token.is_whitespace() => {
                    self.lexer.next_token()?;
                }
                token if token.is_rparen() => {
                    self.lexer.next_token()?;
                    return (
                        unsafe { *head },
                        Span::merge(first_token.span(), token.span()),
                    )
                        .okay();
                }
                token if token.is_eof() => {
                    return syntax!(
                        UnmatchedParen,
                        (first_token.span(), "".into()),
                        (token.span(), "expected here".into())
                    )
                    .error();
                }
                _ => {
                    let (next_atom, _) = self.parse_expr()?;
                    let next_atom = cons(next_atom.boxed().leak(), Atom::NULL.boxed().leak())
                        .boxed()
                        .leak();
                    unsafe { (*atom.as_pair()).cdr = next_atom };
                    atom = next_atom
                }
            };
        }
    }
}
