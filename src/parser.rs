use crate::lexer::{Lexer, Token};
use crate::{CranelispError, Result, Span};
use expr::Expr;
use somok::Somok;

pub mod expr;

pub struct Parser<'l, 's> {
    lexer: &'l mut Lexer<'s>,
}

impl<'l, 's, 'p> Parser<'l, 's> {
    pub fn new(lexer: &'l mut Lexer<'s>) -> Self {
        Self { lexer }
    }

    pub fn parse_expr(&'p mut self) -> Result<Expr<'s>> {
        match self.lexer.next_token()? {
            Token::RParen(span) => {
                syntax!(UnexpectedRParen, (span, "Unexpected rparen".into())).error()
            }

            Token::Float(f, span) => {
                let expr = Expr::Float(f, span);
                expr.okay()
            }
            Token::Integer(i, span) => {
                let expr = Expr::Integer(i, span);
                expr.okay()
            }

            Token::Bool(b, span) => Expr::Bool(b, span).okay(),

            token @ Token::LParen(..) => self.parse_list(token),

            Token::Symbol(sym, span) => Expr::Symbol(sym, span).okay(),

            Token::String(string, span) => Expr::String(string, span).okay(),

            Token::Whitespace(..) => self.parse_expr(),

            Token::Eof(..) => CranelispError::EOF.error(),

            Token::Quote(span) => {
                let expr = self.parse_expr()?;
                let end = expr.span();
                Expr::List(
                    vec![Expr::Symbol("quote", span), expr],
                    Span::merge(span, end),
                )
                .okay()
            }
            Token::Paste(span) => {
                let expr = self.parse_expr()?;
                let end = expr.span();
                Expr::List(
                    vec![Expr::Symbol("paste", span), expr],
                    Span::merge(span, end),
                )
                .okay()
            }
            Token::Quasi(span) => {
                let expr = self.parse_expr()?;
                let end = expr.span();
                Expr::List(
                    vec![Expr::Symbol("quasi", span), expr],
                    Span::merge(span, end),
                )
                .okay()
            }
        }
    }

    fn skip_whitespace(&'p mut self) -> Result<()> {
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

    fn parse_list(&'p mut self, first_token: Token) -> Result<Expr<'s>> {
        let start = first_token.span();
        self.skip_whitespace()?;
        if let Token::RParen(span) = self.lexer.peek_token()? {
            self.lexer.next_token()?;
            return Expr::Null(Span::merge(start, span)).okay();
        }
        let mut exprs = Vec::new();
        let end = loop {
            let expr = self.parse_expr()?;
            exprs.push(expr);

            if let Token::RParen(span) = self.lexer.peek_token()? {
                self.lexer.next_token()?;
                break span;
            }
        };
        Expr::List(exprs, Span::merge(start, end)).okay()
    }
}
