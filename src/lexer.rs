use crate::CranelispError;
use crate::Result;
use crate::ToResult;
use log::trace;
use std::ops::Range;
use std::{fmt::Debug, io::Read};
pub enum Token {
    LParen(usize),
    RParen(usize),
    Number(f64, Range<usize>),
    Symbol(String, Range<usize>),
    Comment(String),
    Quote(usize),
}

impl Debug for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Token::LParen(_) => write!(fmt, "(",),
            Token::RParen(_) => write!(fmt, ") ",),
            Token::Number(n, _) => write!(fmt, "{} ", n,),
            Token::Symbol(s, _) => write!(fmt, "{} ", s,),
            Token::Comment(c) => write!(fmt, "# {} ", c,),
            Token::Quote(_) => write!(fmt, "'"),
        }
    }
}

#[derive(Clone)]
pub struct Lexer {
    src: Vec<char>,
    next: usize,
}
impl Debug for Lexer {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(fmt, "next: {}:{:?}, in:", self.next, self.src[self.next])?;
        for c in self.src.iter() {
            write!(fmt, "'{}', ", c)?
        }
        writeln!(fmt)?;
        for (e, _) in self.src.iter().enumerate() {
            write!(fmt, "{:03}, ", e)?
        }
        Ok(())
    }
}

impl Lexer {
    pub fn new<R: Read>(mut reader: R) -> Result<Self> {
        let mut _src = String::new();
        reader.read_to_string(&mut _src)?;
        let _src = Box::leak(_src.into_boxed_str());
        Ok(Self {
            src: _src.chars().collect(),
            next: 0,
        })
    }

    fn consume_number(&mut self) -> Result<Token> {
        let start = self.next;
        let number = self.consume_until(&[' ', ')', '\n']);
        let end = self.next;
        trace!("Consuming Number at location {:?}", start..end);
        Token::Number(number.parse()?, start..end).okay()
    }

    fn consume_symbol(&mut self) -> Result<Token> {
        let start = self.next;
        let symbol = self.consume_until(&[' ', ')', '\n']);
        let end = self.next;
        trace!("Consuming Symbol at location {:?}", start..end);
        Ok(Token::Symbol(symbol, start..end))
    }

    pub fn next_token(&mut self) -> Result<Token> {
        if let Some(char) = self.peek() {
            trace!("{:?}", &*self);

            match char {
                c if c.is_whitespace() => {
                    trace!("Consuming whitespace at location {}", self.next);
                    self.consume();
                    self.next_token()
                }
                '(' => {
                    trace!("Consuming LParen at location {}", self.next);
                    self.consume();
                    Token::LParen(self.next - 1).okay()
                }
                ')' => {
                    trace!("Consuming RParen at location {}", self.next);
                    self.consume();
                    Token::RParen(self.next - 1).okay()
                }
                c if c.is_numeric() => self.consume_number(),
                c if c.is_alphanumeric() => self.consume_symbol(),
                '#' => {
                    trace!("Consuming Comment at location {}", self.next);
                    let comment = self.consume_until(&['\n']);
                    Token::Comment(comment).okay()
                }
                '\'' => {
                    trace!("Consuming Quote at location {}", self.next);
                    self.consume();
                    Token::Quote(self.next - 1).okay()
                }
                c => CranelispError::Syntax(
                    self.next..self.next,
                    format!("Unexpected character {:?}", c),
                )
                .error(),
            }
        } else {
            CranelispError::EOF.error()
        }
    }

    fn peek(&self) -> Option<&char> {
        self.src.get(self.next)
    }

    fn consume_until(&mut self, del: &[char]) -> String {
        let mut v = vec![];
        while let Some(&c) = self.peek() {
            if del.contains(&c) {
                return v.into_iter().collect();
            } else {
                self.consume();
                v.push(c);
            };
        }
        "".into()
    }

    fn consume(&mut self) {
        self.next += 1
    }
}
