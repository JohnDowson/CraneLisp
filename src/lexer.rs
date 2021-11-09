use crate::CranelispError;
use crate::Result;
use crate::SyntaxError;
use log::trace;
use somok::Somok;
use std::ops::Range;
use std::{fmt::Debug, io::Read};

#[derive(Clone)]
pub enum Token {
    LParen(usize),
    RParen(usize),
    Number(f64, Range<usize>),
    Symbol(String, Range<usize>),
    Quote(usize),
}

impl Token {
    pub fn span(&self) -> Range<usize> {
        self.span_start()..self.span_end()
    }
    pub fn span_start(&self) -> usize {
        match self {
            Token::LParen(s) => *s,
            Token::RParen(s) => *s,
            Token::Number(_, s) => s.start,
            Token::Symbol(_, s) => s.start,
            Token::Quote(s) => *s,
        }
    }
    pub fn span_end(&self) -> usize {
        match self {
            Token::LParen(s) => *s,
            Token::RParen(s) => *s,
            Token::Number(_, s) => s.end,
            Token::Symbol(_, s) => s.end,
            Token::Quote(s) => *s,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Token::LParen(_) => write!(fmt, "(",),
            Token::RParen(_) => write!(fmt, ")",),
            Token::Number(n, _) => write!(fmt, "{:?}", n,),
            Token::Symbol(s, _) => write!(fmt, "{:?}", s,),
            Token::Quote(_) => write!(fmt, "'"),
        }
    }
}

#[derive(Clone)]
pub struct Lexer {
    src: Vec<char>,
    _src: *mut str,
    next: usize,
}
impl Debug for Lexer {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        if self.next == self.src.len() {
            return write!(fmt, "Finished");
        }
        writeln!(fmt, "next: {}:{:?}, in:", self.next, self.src[self.next])?;
        for c in self.src.iter() {
            write!(fmt, "{:?}, ", c)?
        }
        writeln!(fmt)?;
        for (e, _) in self.src.iter().enumerate() {
            write!(fmt, "{:03}, ", e)?
        }
        Ok(())
    }
}

impl Drop for Lexer {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self._src);
        }
    }
}

impl Lexer {
    pub fn new<R: Read>(mut reader: R) -> Result<Self> {
        let mut _src = String::new();
        reader.read_to_string(&mut _src)?;
        let _src = Box::leak(_src.into_boxed_str());
        Self {
            src: _src.chars().collect(),
            _src,
            next: 0,
        }
        .okay()
    }

    fn consume_number(&mut self) -> Result<Token> {
        let start = self.next;
        let mut number = String::new();
        match self.peek() {
            Some('.') => {
                self.consume();
                number.push('.');
                number.push_str(&self.consume_until(|c| !c.is_numeric()));
            }
            Some(c) if c.is_numeric() => {
                let integer = self.consume_until(|c| !c.is_numeric());
                let mut decimal = String::new();
                match self.peek() {
                    Some('.') => {
                        self.consume();
                        if let Some(c) = self.peek() {
                            if !c.is_numeric() {
                                return CranelispError::Syntax(SyntaxError::InvalidLiteral(
                                    start..self.next,
                                    format!(" {:?}", c),
                                ))
                                .error();
                            }
                        }
                        decimal = self.consume_until(|c| !c.is_numeric());
                    }
                    Some(c) if c.is_whitespace() => {}
                    Some(c) => {
                        return CranelispError::Syntax(SyntaxError::InvalidLiteral(
                            start..self.next,
                            format!(" {:?}", c),
                        ))
                        .error();
                    }
                    None => (),
                }
                number = [integer, decimal].join(".")
            }
            Some(c) => {
                return CranelispError::Syntax(SyntaxError::InvalidLiteral(
                    start..self.next,
                    format!(" {:?}", c),
                ))
                .error();
            }
            None => (),
        }
        let end = self.next;
        Token::Number(
            number.parse().map_err(|_| {
                CranelispError::Syntax(SyntaxError::InvalidLiteral(start..end, number))
            })?,
            start..end,
        )
        .okay()
    }

    fn consume_symbol(&mut self) -> Result<Token> {
        let start = self.next;
        let symbol = self.consume_until(|c| c.is_whitespace() || ['(', ')'].contains(&c));
        let end = self.next;
        // trace!(
        //     "Consuming Symbol: {:?} at location {:?}",
        //     symbol,
        //     start..end
        // );
        Ok(Token::Symbol(symbol, start..end))
    }

    pub fn peek_token(&mut self) -> Result<Token> {
        let next = self.next;
        let token = self.next_token();
        self.next = next;
        token
    }

    pub fn next_token(&mut self) -> Result<Token> {
        if let Some(char) = self.peek() {
            // trace!("{:?}", &*self);

            match char {
                c if c.is_whitespace() => {
                    //trace!("Consuming whitespace at location {}", self.next);
                    self.consume();
                    self.next_token()
                }
                '(' => {
                    //trace!("Consuming LParen at location {}", self.next);
                    self.consume();
                    Token::LParen(self.next - 1).okay()
                }
                ')' => {
                    //trace!("Consuming RParen at location {}", self.next);
                    self.consume();
                    Token::RParen(self.next - 1).okay()
                }
                c if c.is_numeric() || c == &'.' => self.consume_number(),
                c if c.is_alphanumeric() => self.consume_symbol(),
                '#' => {
                    //trace!("Consuming Comment at location {}", self.next);
                    self.consume();
                    let _ = self.consume_until(|c| c == '\n' || c == '#');
                    if matches!(self.peek(), Some('#')) {
                        self.consume()
                    };
                    self.next_token()
                }
                '\'' => {
                    //trace!("Consuming Quote at location {}", self.next);
                    self.consume();
                    Token::Quote(self.next - 1).okay()
                }
                c => CranelispError::Syntax(SyntaxError::UnexpectedCharacter(
                    self.next..self.next,
                    *c,
                ))
                .error(),
            }
        } else {
            CranelispError::EOF.error()
        }
    }

    pub fn peek(&self) -> Option<&char> {
        self.src.get(self.next)
    }

    fn consume_until(&mut self, del: impl Fn(char) -> bool) -> String {
        let mut v = vec![];
        while let Some(&c) = self.peek() {
            if del(c) {
                return v.into_iter().collect();
            } else {
                self.consume();
                v.push(c);
            };
        }
        v.into_iter().collect()
    }

    fn consume(&mut self) {
        self.next += 1
    }
}
