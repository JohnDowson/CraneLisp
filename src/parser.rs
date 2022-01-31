use std::rc::Rc;

use crate::lexer::{Lexer, Token};
use crate::value::{intern, paste, quasi, quoted, Atom};
use crate::{mem, CranelispError, Result, Span};
use somok::Somok;

pub struct Parser<'l> {
    lexer: &'l mut Lexer,
}

impl<'l> Parser<'l> {
    pub fn new(lexer: &'l mut Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse_expr(&mut self) -> Result<(mem::Ref, Span)> {
        match self.lexer.next_token()? {
            Token::RParen(span) => {
                syntax!(UnexpectedRParen, (span, "Unexpected rparen".into())).error()
            }

            Token::Float(f, span) => {
                let expr = (mem::alloc(Atom::Float(f)), span);
                expr.okay()
            }
            Token::Integer(i, span) => {
                let expr = (mem::alloc(Atom::Int(i)), span);
                expr.okay()
            }

            Token::Bool(b, span) => (mem::alloc(Atom::Bool(b)), span).okay(),

            token @ Token::LParen(..) => {
                self.skip_whitespace()?;
                self.parse_list(token)
            }

            Token::Symbol(sym, span) => {
                let sym = intern(sym);
                (mem::alloc(Atom::Symbol(sym)), span).okay()
            }

            Token::String(string, span) => (mem::alloc(Atom::String(Rc::new(string))), span).okay(),

            Token::Whitespace(..) => self.parse_expr(),

            Token::Eof(..) => CranelispError::EOF.error(),

            Token::Quote(..) => {
                let (atom, span) = self.parse_expr()?;
                (quoted(atom), span).okay()
            }
            Token::Paste(_) => {
                let (atom, span) = self.parse_expr()?;
                (paste(atom), span).okay()
            }
            Token::Quasi(_) => {
                let (atom, span) = self.parse_expr()?;
                (quasi(atom), span).okay()
            }
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

    fn parse_list(&mut self, first_token: Token) -> Result<(mem::Ref, Span)> {
        let head = match self.parse_expr() {
            Ok((val, _)) => Atom::pair(val, null!()),
            Err(e) => match e {
                CranelispError::Syntax(se) => match se.kind {
                    crate::SyntaxErrorKind::UnexpectedRParen => {
                        return (
                            null!(),
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
                    return (head, Span::merge(first_token.span(), token.span())).okay();
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
                    let next_pair = mem::alloc(Atom::pair(next_atom, null!()));
                    atom.as_pair_mut().unwrap().cdr = next_pair.clone();
                    atom = next_pair;
                }
            };
        }
    }
}
