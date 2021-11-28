use crate::lexer::{Lexer, Token};
use crate::{intern, CranelispError, Result, Span};
use smol_str::SmolStr;
use somok::Somok;
use std::str::FromStr;
mod expr;
use crate::function::Type;
pub use expr::{Args, DefunExpr, Expr};
pub type Arglist = Vec<(String, Type)>;

pub struct Parser<'l> {
    lexer: &'l mut Lexer,
}

impl<'l> Parser<'l> {
    pub fn new(lexer: &'l mut Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        match self.lexer.next_token()? {
            Token::RParen(span) => {
                syntax!(UnexpectedToken, (span, "Unexpected rparen".into())).error()
            }

            Token::Float(f, span) => {
                let expr = Expr::Float(f, span);
                expr.okay()
            }
            Token::Integer(i, span) => {
                let expr = Expr::Integer(i, span);
                expr.okay()
            }

            token @ Token::LParen(..) => {
                self.skip_whitespace()?;
                match self.lexer.peek_token()? {
                    t if matches!(t, Token::Symbol(..)) => match &*t.extract_symbol() {
                        "defun" => {
                            self.lexer.next_token()?;
                            self.skip_whitespace()?;
                            let name = self.eat_symbol()?;
                            self.skip_whitespace()?;
                            let args = self.eat_arglist()?;
                            let expr = self.parse_expr()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            intern(name.clone());
                            Expr::Defun(
                                Box::new(DefunExpr {
                                    name,
                                    args,
                                    body: expr,
                                }),
                                Span::merge(token.span(), rparen.span()),
                            )
                            .okay()
                        }
                        "loop" => {
                            self.lexer.next_token()?;
                            let expr = self.parse_expr()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            Expr::Loop(Box::new(expr), Span::merge(token.span(), rparen.span()))
                                .okay()
                        }
                        "return" => {
                            self.lexer.next_token()?;
                            self.skip_whitespace()?;
                            let next = self.lexer.next();
                            let expr = match self.parse_expr() {
                                Ok(expr) => expr.boxed().some(),
                                Err(_e) => {
                                    self.lexer.rewind_to(next);
                                    None
                                }
                            };
                            let rparen = self.eat_rparen()?;
                            Expr::Return(expr, Span::merge(token.span(), rparen.span())).okay()
                        }
                        "let" => {
                            self.lexer.next_token()?;
                            let name = self.eat_symbol()?;
                            let expr = self.parse_expr()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            intern(name.clone());
                            Expr::Let(
                                name,
                                Box::new(expr),
                                Span::merge(token.span(), rparen.span()),
                            )
                            .okay()
                        }
                        "if" => {
                            self.lexer.next_token()?;
                            let cond = self.parse_expr()?;

                            let truth = self.parse_expr()?;

                            let lie = self.parse_expr()?;

                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            match (true, true, true) {
                                (true, true, true) => Expr::If(
                                    Box::new(cond),
                                    Box::new(truth),
                                    Box::new(lie),
                                    Span::merge(token.span(), rparen.span()),
                                )
                                .okay(),
                                (true, true, false) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        lie.span(),
                                        format!("Unexpected {} where List is expected", lie)
                                    )
                                )
                                .error(),
                                (true, false, true) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        truth.span(),
                                        format!("Unexpected {} where List is expected", truth)
                                    )
                                )
                                .error(),
                                (true, false, false) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        truth.span(),
                                        format!("Unexpected {} where List is expected", truth)
                                    ),
                                    (
                                        lie.span(),
                                        format!("Unexpected {} where List is expected", lie)
                                    )
                                )
                                .error(),
                                (false, true, true) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        cond.span(),
                                        format!("Unexpected {} where List is expected", cond)
                                    )
                                )
                                .error(),
                                (false, true, false) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        cond.span(),
                                        format!("Unexpected {} where List is expected", cond)
                                    ),
                                    (
                                        lie.span(),
                                        format!("Unexpected {} where List is expected", lie)
                                    )
                                )
                                .error(),
                                (false, false, true) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        cond.span(),
                                        format!("Unexpected {} where List is expected", cond)
                                    ),
                                    (
                                        truth.span(),
                                        format!("Unexpected {} where List is expected", truth)
                                    )
                                )
                                .error(),
                                (false, false, false) => syntax!(
                                    UnexpectedExpression,
                                    (
                                        cond.span(),
                                        format!("Unexpected {} where List is expected", cond)
                                    ),
                                    (
                                        truth.span(),
                                        format!("Unexpected {} where List is expected", truth)
                                    ),
                                    (
                                        lie.span(),
                                        format!("Unexpected {} where List is expected", lie)
                                    )
                                )
                                .error(),
                            }
                        }
                        _ => self.parse_list(token),
                    },
                    _ => self.parse_list(token),
                }
            }
            token @ Token::Quote(..) => {
                let expr = self.parse_expr()?;
                let expr_span = expr.span();
                Expr::Quoted(Box::new(expr), Span::merge(token.span(), expr_span)).okay()
            }

            Token::Symbol(sym, span) => {
                intern(sym.clone());
                let expr = Expr::Symbol(sym, span);
                expr.okay()
            }

            Token::String(string, span) => Expr::String(string, span).okay(),

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

    fn eat_arglist(&mut self) -> Result<Args> {
        let mut arglist = Vec::new();
        let _lparen = self.eat_lparen()?;
        loop {
            let symbol = match self.lexer.next_token()? {
                Token::RParen(..) => return Args::Arglist(arglist).okay(),
                Token::Symbol(sym, ..) if sym == "*" => {
                    self.skip_whitespace()?;
                    self.eat_rparen()?;
                    return Args::Foldable.okay();
                }
                Token::Symbol(sym, ..) => sym,

                Token::Whitespace(..) => continue,
                token => {
                    return syntax!(
                        UnexpectedToken,
                        (token.span(), "Symbol expected here".into())
                    )
                    .error()
                }
            };
            self.skip_whitespace()?;
            arglist.push(symbol);
        }
    }

    fn parse_list(&mut self, first_token: Token) -> Result<Expr> {
        // Safety: we have just pushed to previous_tokens
        let mut exprs = Vec::new();
        loop {
            match self.lexer.peek_token()? {
                token if token.is_whitespace() => {
                    self.lexer.next_token()?;
                }
                token if token.is_rparen() => {
                    self.lexer.next_token()?;
                    return Expr::List(exprs, Span::merge(first_token.span(), token.span())).okay();
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
                    let expr = self.parse_expr()?;
                    exprs.push(expr);
                }
            };
        }
    }
}
