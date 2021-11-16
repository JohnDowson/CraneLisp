use std::str::FromStr;

use crate::lexer::{Lexer, Token};
use crate::{CranelispError, Result, Span};
use somok::Somok;
mod expr;
use crate::function::FnArgs;
use crate::function::Type;
pub use expr::Expr;
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
            token if token.is_rparen() => {
                syntax!(UnexpectedToken, (token.span, "Unexpected rparen".into())).error()
            }

            token if token.is_float() => {
                let expr = Expr::Float(token.extract_float()?, token.span);
                expr.okay()
            }
            token if token.is_integer() => {
                let expr = Expr::Integer(token.extract_integer()?, token.span);
                expr.okay()
            }

            token if token.is_lparen() => {
                self.skip_whitespace()?;
                match self.lexer.peek_token()? {
                    t if t.is_symbol() => match &*t.extract_symbol()? {
                        "defun" => {
                            self.lexer.next_token()?;
                            self.skip_whitespace()?;
                            let name = self.eat_symbol()?;
                            self.skip_whitespace()?;
                            self.eat_type_separator()?;
                            self.skip_whitespace()?;
                            let ty = self.eat_type()?;
                            self.skip_whitespace()?;
                            let args = self.eat_arglist()?;
                            let expr = self.parse_expr()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            Expr::Defun(
                                name,
                                args,
                                Box::new(expr),
                                ty,
                                Span::merge(token.span, rparen.span),
                            )
                            .okay()
                        }
                        "loop" => {
                            self.lexer.next_token()?;
                            let expr = self.parse_expr()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            Expr::Loop(Box::new(expr), Span::merge(token.span, rparen.span)).okay()
                        }
                        "return" => {
                            self.lexer.next_token()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            Expr::Return(None, Span::merge(token.span, rparen.span)).okay()
                        }
                        "let" => {
                            self.lexer.next_token()?;
                            let name = self.eat_symbol()?;
                            let expr = self.parse_expr()?;
                            self.skip_whitespace()?;
                            let rparen = self.eat_rparen()?;
                            Expr::Let(name, Box::new(expr), Span::merge(token.span, rparen.span))
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
                                    Span::merge(token.span, rparen.span),
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
            token if token.is_quote() => {
                let expr = self.parse_expr()?;
                let expr_span = expr.span();
                Expr::Quoted(Box::new(expr), Span::merge(token.span, expr_span)).okay()
            }

            token if token.is_symbol() => {
                let expr = Expr::Symbol(token.extract_symbol()?, token.span);
                expr.okay()
            }

            token if token.is_string() => Expr::String(token.extract_string()?, token.span).okay(),

            token if token.is_type_separator() => syntax!(
                UnexpectedToken,
                (token.span, "Unexpected type separator".into())
            )
            .error(),

            token if token.is_whitespace() => self.parse_expr(),

            token if token.is_eof() => CranelispError::EOF.error(),
            what => panic!("{:?}", what),
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
            token => syntax!(UnexpectedToken, (token.span, "LParen expected here".into())).error(),
        }
    }

    fn eat_rparen(&mut self) -> Result<Token> {
        match self.lexer.next_token()? {
            token if token.is_rparen() => token.okay(),
            token if token.is_whitespace() => self.eat_rparen(),
            token => syntax!(UnexpectedToken, (token.span, "RParen expected here".into())).error(),
        }
    }

    fn eat_type(&mut self) -> Result<Type> {
        match self.lexer.next_token()? {
            token if token.is_symbol() => Type::from_str(&token.extract_symbol()?)?.okay(),
            token if token.is_whitespace() => self.eat_type(),
            token => syntax!(
                UnexpectedToken,
                (token.span, format!("Type expected here, found {:?}", token))
            )
            .error(),
        }
    }

    fn eat_type_separator(&mut self) -> Result<()> {
        match self.lexer.next_token()? {
            token if token.is_type_separator() => ().okay(),
            token => syntax!(
                UnexpectedToken,
                (
                    token.span,
                    format!("Type separator expected here, found {:?}", token)
                )
            )
            .error(),
        }
    }

    fn eat_symbol(&mut self) -> Result<String> {
        match self.lexer.next_token()? {
            token if token.is_symbol() => token.extract_symbol(),
            token if token.is_whitespace() => self.eat_symbol(),
            token => syntax!(UnexpectedToken, (token.span, "Symbol expected here".into())).error(),
        }
    }

    fn eat_arglist(&mut self) -> Result<FnArgs> {
        let mut arglist: Arglist = Vec::new();
        let _lparen = self.eat_lparen()?;
        loop {
            let symbol = match self.lexer.next_token()? {
                token if token.is_rparen() => return FnArgs::Arglist(arglist).okay(),
                token if token.is_symbol() && token.extract_symbol()? == "*" => {
                    self.skip_whitespace()?;
                    self.eat_type_separator()?;
                    self.skip_whitespace()?;
                    let ty = self.eat_type()?;
                    self.skip_whitespace()?;
                    self.eat_rparen()?;
                    return FnArgs::Foldable(ty).okay();
                }
                token if token.is_symbol() => token.extract_symbol()?,

                token if token.is_whitespace() => continue,
                token => {
                    return syntax!(UnexpectedToken, (token.span, "Symbol expected here".into()))
                        .error()
                }
            };
            self.skip_whitespace()?;
            self.eat_type_separator()?;
            self.skip_whitespace()?;
            let ty = self.eat_type()?;
            arglist.push((symbol, ty));
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
                    return Expr::List(exprs, Span::merge(first_token.span, token.span)).okay();
                }
                token if token.is_eof() => {
                    return syntax!(
                        UnmatchedParen,
                        (first_token.span, "".into()),
                        (token.span, "expected here".into())
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
