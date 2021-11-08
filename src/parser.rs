use std::io::Read;
use std::ops::Range;

use log::{error, trace};

use crate::lexer::{Lexer, Token};
use crate::{CranelispError, Result, SyntaxError, ToOption, ToResult};

#[derive(Debug)]
pub enum Expr {
    Call(String, Vec<Expr>, Meta),
    Symbol(String, Meta),
    Number(f64, Meta),
    List(Vec<Expr>, Meta),
}
impl Expr {
    fn from_token(token: &Token) -> Result<Self, ()> {
        match token {
            Token::Number(val, span) => Ok(Expr::Number(*val, Meta { span: span.clone() })),
            Token::Symbol(val, span) => Ok(Expr::Symbol(val.clone(), Meta { span: span.clone() })),
            _ => Err(()),
        }
    }
}

#[derive(Default, Debug)]
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
    pub fn parse(mut self) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();
        loop {
            match self.parse_expr() {
                Some(Ok(expr)) => exprs.push(expr),
                Some(Err(CranelispError::EOF)) => break,
                Some(Err(e)) => return e.error(),
                None => continue,
            }
        }
        exprs.okay()
    }

    fn parse_expr(&mut self) -> Option<Result<Expr>> {
        match self.lexer.next_token() {
            Ok(token @ Token::LParen(_)) => {
                self.previous_tokens.push(token);
                self.parse_list().some()
            }

            Ok(token @ Token::RParen(_)) => {
                CranelispError::Syntax(SyntaxError::UnexpectedCharacter(token.span(), ')'))
                    .error()
                    .some()
            }

            Ok(token @ Token::Number(..)) => {
                let expr = Expr::from_token(&token).unwrap();
                self.previous_tokens.push(token);
                expr.okay().some()
            }

            Ok(token @ Token::Symbol(..)) => {
                let expr = Expr::from_token(&token).unwrap();
                self.previous_tokens.push(token);
                expr.okay().some()
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
                        .error()
                        .some();
                    }
                }

                self.parse_list().some()
            }

            Ok(token @ Token::Comment(_)) => None,
            Err(e) => e.error().some(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr> {
        //trace!("parsing list\n{:?}", self.lexer);
        // Safety: we have just pushed to previous_tokens
        let first_token = self.previous_tokens.last().unwrap().clone();
        let mut exprs = Vec::new();
        loop {
            if let Some(c) = self.lexer.peek() {
                if *c == ')' {
                    self.previous_tokens.push(self.lexer.next_token()?);
                    break;
                }
            } else {
                let last = self.previous_tokens.last().unwrap();
                return CranelispError::Syntax(SyntaxError::UnmatchedParen(
                    first_token.span(),
                    last.span(),
                ))
                .error();
            };
            if let Some(expr) = self.parse_expr() {
                exprs.push(expr?);
            }
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

// pub fn parse(tokens: &[Token]) -> Result<Expr> {
//     loop {
//         return match tokens.iter().next() {
//             Some(Token::LParen(_)) => parse_call(&tokens[1..]).okay(),
//             Some(Token::RParen(span)) => {
//                 CranelispError::Syntax(*span..span + 1, "Unexpected closing paren".into()).error()
//             }
//             Some(Token::Number(_, span)) => {
//                 CranelispError::Syntax(span.clone(), "Unexpected numeric literal".into()).error()
//             }
//             Some(Token::Symbol(_, span)) => {
//                 CranelispError::Syntax(span.clone(), "Unexpected symbol".into()).error()
//             }
//             Some(Token::Comment(_)) => continue,
//             Some(Token::Quote(_)) => parse_list(&tokens[1..]).okay(),
//             None => CranelispError::EOF.error(),
//         };
//     }
// }

// pub fn parse_call(tokens: &[Token]) -> Expr {
//     Expr::Call("".into(), vec![], Meta::default())
// }
// pub fn parse_list(tokens: &[Token]) -> Expr {
//     todo!()
// }
