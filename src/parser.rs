use crate::function::Type;
use crate::lexer::{Lexer, Token};
use crate::value::{append, cons, Atom};
use crate::{intern, CranelispError, Result, Span};
use somok::Somok;
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
                (atom, span).okay()
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

    fn parse_list(&mut self, first_token: Token) -> Result<(Atom, Span)> {
        let (mut head, _) = match self.parse_expr() {
            Ok((val, span)) => (cons(val.boxed(), Atom::NULL.boxed()), span),
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
        let atom = &mut head;
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
                    *atom = append(atom.clone(), next_atom)
                }
            };
        }
    }
}
