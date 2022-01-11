use std::ops::Deref;

use crate::lexer::{Lexer, Token};
use crate::value::{intern, Atom};
use crate::{mem, CranelispError, Result, Span};
use somok::Somok;

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
                let expr = (Atom::Float(f), span);
                expr.okay()
            }
            Token::Integer(i, span) => {
                let expr = (Atom::Int(i), span);
                expr.okay()
            }

            token @ Token::LParen(..) => {
                self.skip_whitespace()?;
                self.parse_list(token)
            }
            Token::Quote(..) => {
                let (atom, span) = self.parse_expr()?;
                (Atom::Quoted(mem::alloc(atom)), span).okay()
            }

            Token::Symbol(sym, span) => {
                let sym = intern(sym);
                (Atom::Symbol(sym), span).okay()
            }

            Token::String(string, span) => (Atom::String(string), span).okay(),

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

    fn parse_list(&mut self, first_token: Token) -> Result<(Atom, Span)> {
        let head = match self.parse_expr() {
            Ok((val, _)) => Atom::pair_alloc(val, Atom::Null),
            Err(e) => match e {
                CranelispError::Syntax(se) => match se.kind {
                    crate::SyntaxErrorKind::UnexpectedRParen => {
                        return (
                            Atom::Null,
                            Span::merge(first_token.span(), se.spans.last().unwrap().0),
                        )
                            .okay()
                    }
                    _ => return CranelispError::Syntax(se).error(),
                },
                _ => return e.error(),
            },
        };
        let head = mem::alloc(head);
        let mut atom = head.clone();
        loop {
            match self.lexer.peek_token()? {
                token if token.is_whitespace() => {
                    self.lexer.next_token()?;
                }
                token if token.is_rparen() => {
                    self.lexer.next_token()?;
                    return (
                        head.deref().clone(),
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
                    let next_pair = mem::alloc(Atom::pair_alloc(next_atom, Atom::Null));
                    atom.as_pair_mut().unwrap().cdr = next_pair.clone();
                    atom = next_pair;
                }
            };
        }
    }
}
