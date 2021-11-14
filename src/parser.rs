use crate::lexer::{Lexer, Token};
use crate::parser::expr::Meta;
use crate::{CranelispError, Result, Span};
use somok::Somok;
mod expr;
pub use expr::Expr;

type Type = ();

#[derive(Debug, Clone)]
pub enum FnArgs {
    Arglist(Arglist),
    Foldable(Type),
}
pub type Arglist = Vec<(String, Type)>;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        match self.lexer.next_token()? {
            token if token.is_lparen() => self.parse_list(token),
            token if token.is_lparen() => syntax!(UnexpectedToken, (token.span, "".into())).error(),

            token if token.is_float() => {
                let expr = Expr::Number(token.extract_float()?, Meta { span: token.span });
                expr.okay()
            }
            token if token.is_integer() => todo!("Integer exprs are unimplemented"),

            token if token.is_symbol() => {
                let sym = token.extract_symbol()?;
                match &*sym {
                    "defun" => {
                        let name = self.eat_symbol()?;
                        self.eat_type_separator()?;
                        let ret_t = self.eat_type()?;
                        let arglist = self.eat_arglist()?;
                        let body = self.parse_expr()?;
                        let body_span = body.span();
                        Expr::Defun(
                            name,
                            arglist,
                            Box::new(body),
                            ret_t,
                            Meta {
                                span: Span::merge(token.span, body_span),
                            },
                        )
                        .okay()
                    }
                    "loop" => {
                        todo!()
                    }
                    "if" => {
                        todo!()
                    }
                    _ => {
                        let expr = Expr::Symbol(token.extract_symbol()?, Meta { span: token.span });
                        expr.okay()
                    }
                }
            }
            token if token.is_quote() => todo!("Quoted"),

            token if token.is_string() => todo!("String exprs are unimplemented"),

            token if token.is_eof() => CranelispError::EOF.error(),

            token if token.is_type_separator() => syntax!(
                UnexpectedToken,
                (token.span, "Unexpected type separator".into())
            )
            .error(),

            token if token.is_whitespace() => self.parse_expr(),

            what => todo!("{:?}", what),
        }
    }

    fn eat_lparen(&mut self) -> Result<Token> {
        match self.lexer.next_token()? {
            token if token.is_lparen() => token.okay(),
            token => syntax!(UnexpectedToken, (token.span, "LParen expected here".into())).error(),
        }
    }

    fn eat_rparen(&mut self) -> Result<Token> {
        match self.lexer.next_token()? {
            token if token.is_rparen() => token.okay(),
            token => syntax!(UnexpectedToken, (token.span, "RParen expected here".into())).error(),
        }
    }

    fn eat_type(&mut self) -> Result<()> {
        match self.lexer.next_token()? {
            token if token.is_symbol() => ().okay(),
            token => syntax!(UnexpectedToken, (token.span, "Type expected here".into())).error(),
        }
    }

    fn eat_type_separator(&mut self) -> Result<()> {
        match self.lexer.next_token()? {
            token if token.is_type_separator() => ().okay(),
            token => syntax!(
                UnexpectedToken,
                (token.span, "Type separator expected here".into())
            )
            .error(),
        }
    }

    fn eat_symbol(&mut self) -> Result<String> {
        match self.lexer.next_token()? {
            token if token.is_symbol() => token.extract_symbol(),
            token => syntax!(UnexpectedToken, (token.span, "Symbol expected here".into())).error(),
        }
    }

    fn eat_arglist(&mut self) -> Result<FnArgs> {
        let mut arglist: Arglist = Vec::new();
        let _lparen = self.eat_lparen()?;
        loop {
            let symbol = match self.lexer.next_token()? {
                token if token.is_rparen() => return FnArgs::Arglist(arglist).okay(),
                token if token.is_rparen() && token.extract_string()? == "*" => {
                    self.eat_type_separator()?;
                    let ty = self.eat_type()?;
                    self.eat_rparen()?;
                    return FnArgs::Foldable(ty).okay();
                }
                token if token.is_symbol() => token.extract_symbol()?,
                token => {
                    return syntax!(UnexpectedToken, (token.span, "Symbol expected here".into()))
                        .error()
                }
            };
            let ty = self.eat_type()?;
            arglist.push((symbol, ty));
        }
    }

    fn parse_list(&mut self, first_token: Token) -> Result<Expr> {
        // Safety: we have just pushed to previous_tokens
        let mut exprs = Vec::new();
        loop {
            match self.lexer.peek_token()? {
                token if token.is_rparen() => {
                    return Expr::List(
                        exprs,
                        Meta {
                            span: Span::merge(first_token.span, token.span),
                        },
                    )
                    .okay()
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
