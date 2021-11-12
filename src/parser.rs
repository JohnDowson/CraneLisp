use crate::function::Type;
use crate::lexer::{Lexer, Token};
use crate::parser::expr::Meta;
use crate::{errors, CranelispError, Result, SyntaxError};
use log::trace;
use somok::Somok;
mod expr;
pub use expr::Expr;

#[derive(Debug, Clone)]
pub enum FnArgs {
    Arglist(Arglist),
    Foldable,
}
pub type Arglist = Vec<(String, Type)>;

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
                let name = self.eat_symbol()?;
                let ret = self.eat_type()?;
                let args = self.eat_arglist()?;
                let body = self.parse_expr()?;
                match body {
                    body @ Expr::List(..) => {
                        let span = token.span_start()..body.span().end;
                        Expr::Defun(name, args, Box::new(body), ret, Meta { span }).okay()
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
            Ok(token @ Token::Type(..)) => errors::syntax(SyntaxError::UnexpectedToken(
                token.span(),
                format!("type {:?} when type is not expected", token),
            )),
            Ok(token @ Token::Loop(..)) => match self.parse_expr()? {
                body @ Expr::List(..) => {
                    let span = token.span();
                    Expr::Loop(Box::new(body), Meta { span })
                }
                .okay(),
                _ => errors::syntax(SyntaxError::UnexpectedToken(
                    token.span(),
                    format!("got {:?} when list is expected", token),
                )),
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
                (t1, t2) => errors::syntax(SyntaxError::UnexpectedToken(
                    t1.span_start()..t2.span().end,
                    format!("got ({:?} {:?}) when expecting (symbol list)", t1, t2),
                )),
            },

            Ok(Token::Eof(..)) => CranelispError::EOF.error(),
            Err(e) => e.error(),
        }
    }

    fn _eat_lparen(&mut self) -> Result<Token> {
        match self.lexer.next_token() {
            token @ Ok(Token::LParen(..)) => token,
            Ok(token) => {
                return errors::syntax(SyntaxError::UnexpectedToken(
                    token.span(),
                    format!("got {:?} when expecting lparen", token),
                ));
            }
            Err(_) => todo!(),
        }
    }

    fn eat_arglist(&mut self) -> Result<FnArgs> {
        let mut arglist: Arglist = Vec::new();
        let _lparen = match self.lexer.next_token() {
            Ok(token @ Token::LParen(..)) => token,
            Ok(token) => CranelispError::Syntax(SyntaxError::MissingType(token.span())).error()?,
            Err(_) => todo!(),
        };
        loop {
            let symbol = match self.lexer.next_token() {
                Ok(_token @ Token::RParen(..)) => return FnArgs::Arglist(arglist).okay(),
                Ok(Token::Symbol(s, ..)) if s == "*" => {
                    self.eat_rparen()?;
                    return FnArgs::Foldable.okay();
                }
                Ok(Token::Symbol(s, ..)) => s,
                Ok(token) => {
                    return errors::syntax(SyntaxError::UnexpectedToken(
                        token.span(),
                        format!("got {:?} when expecting symbol", token),
                    ));
                }
                Err(_) => todo!(),
            };

            let ty = match self.lexer.next_token() {
                Ok(Token::Type(t, ..)) => t,
                Ok(token) => {
                    return errors::syntax(SyntaxError::UnexpectedToken(
                        token.span(),
                        format!("got {:?} when expecting type", token),
                    ))
                }
                Err(_) => todo!(),
            };

            arglist.push((symbol, ty));
        }
    }

    fn eat_rparen(&mut self) -> Result<Token> {
        match self.lexer.next_token() {
            Ok(token @ Token::RParen(..)) => token.okay(),

            Ok(token @ Token::Eof(..)) => {
                CranelispError::UnexpectedEOF(token.span(), "when expecting rparen".to_string())
                    .error()
            }
            Ok(t) => errors::syntax(SyntaxError::UnexpectedToken(
                t.span(),
                format!("got {:?} when rparen is expected", t),
            )),
            Err(e) => e.error(),
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

    fn eat_symbol(&mut self) -> Result<String> {
        match self.lexer.next_token() {
            Ok(Token::Symbol(name, ..)) => name.okay(),
            Ok(tok @ Token::Eof(..)) => CranelispError::UnexpectedEOF(
                tok.span(),
                "Unexpected EOF when expecting Symbol".into(),
            )
            .error(),
            Ok(tok) => errors::syntax(SyntaxError::UnexpectedToken(
                tok.span(),
                format!("got {:?} when expecting Symbol", tok),
            )),
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
