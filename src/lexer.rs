use crate::Result;
use crate::Span;

use somok::Somok;
use std::fmt::Debug;
mod token;
pub use token::Token;

#[derive(Clone)]
pub struct Lexer {
    src: Vec<char>,
    _src: *mut str,
    next: usize,
}

impl Lexer {
    pub fn new(_src: String) -> Result<Self> {
        let _src = Box::leak(_src.into_boxed_str());
        Self {
            src: _src.chars().collect(),
            _src,
            next: 0,
        }
        .okay()
    }

    pub fn finished(&self) -> bool {
        self.next >= self.src.len()
    }

    fn consume_negative_number(&mut self) -> Result<Token> {
        let start = self.next;
        self.consume();
        let number = self.consume_number()?;
        match number {
            Token::Float(f, _) => Token::Float(-f, Span::new(start, number.span().end)).okay(),
            Token::Integer(i, _) => Token::Integer(-i, Span::new(start, number.span().end)).okay(),
            _ => unreachable!(),
        }
    }

    fn consume_number(&mut self) -> Result<Token> {
        let start = self.next;
        let mut fp = false;
        let mut number = String::new();
        match self.peek() {
            Some('.') => {
                self.consume();
                number.push('.');
                number.push_str(&self.consume_until(|c| !c.is_numeric()));
                let end = self.next - 1;
                let num = number
                    .parse::<f64>()
                    .map_err(|e| syntax!(InvalidLiteral, (Span::new(start, end), e.to_string())))?;
                return Token::Float(num, Span::new(start, end)).okay();
            }
            Some(c) if c.is_numeric() => {
                let integer = self.consume_until(|c| !c.is_numeric());
                let mut decimal = String::new();
                match self.peek() {
                    Some('.') => {
                        fp = true;
                        self.consume();
                        if let Some(c) = self.peek() {
                            if !c.is_numeric() && c != &')' && !c.is_whitespace() {
                                return syntax!(
                                    InvalidLiteral,
                                    (Span::new(start, self.next), "".to_string())
                                )
                                .error();
                            }
                        }
                        decimal = self.consume_until(|c| !c.is_numeric());
                    }
                    Some(c) if c.is_whitespace() || c == &')' => {}
                    Some(c) => {
                        eprintln!("invalid literal{:?}", c.is_whitespace());
                        return syntax!(
                            InvalidLiteral,
                            (
                                Span::new(start, self.next),
                                "Invalid decimal part here".into()
                            )
                        )
                        .error();
                    }
                    None => (),
                }
                if fp {
                    number = [integer, decimal].join(".")
                } else {
                    return Token::Integer(
                        integer.parse::<i64>().map_err(|e| {
                            syntax!(InvalidLiteral, (Span::new(start, self.next), e.to_string()))
                        })?,
                        Span::new(start, self.next),
                    )
                    .okay();
                }
            }
            Some(_) => {
                return syntax!(
                    InvalidLiteral,
                    (
                        Span::point(self.next),
                        "Invalid numeric literal here".to_string()
                    )
                )
                .error();
            }
            None => (),
        }
        let end = self.next;
        Token::Float(
            number
                .parse::<f64>()
                .map_err(|e| syntax!(InvalidLiteral, (Span::new(start, end), e.to_string())))?,
            Span::new(start, self.next),
        )
        .okay()
    }

    fn consume_symbol(&mut self) -> Result<Token> {
        let start = self.next;
        let symbol = self.consume_until(|c| c.is_whitespace() || ['(', ')'].contains(&c));
        let end = self.next;
        Ok(Token::Symbol(symbol.into(), Span::new(start, end)))
    }

    pub fn peek_token(&mut self) -> Result<Token> {
        let next = self.next;
        let token = self.next_token();
        self.next = next;
        token
    }

    pub fn next_token(&mut self) -> Result<Token> {
        if let Some(char) = self.peek() {
            match char {
                c if c.is_whitespace() => {
                    let start = self.next;
                    self.consume_until(|c| !c.is_whitespace());
                    let end = self.next - 1;
                    Token::Whitespace(Span::new(start, end)).okay()
                }
                '\'' => {
                    self.consume();
                    Token::Quote(Span::point(self.next - 1)).okay()
                }
                ',' => {
                    self.consume();
                    Token::Paste(Span::point(self.next - 1)).okay()
                }
                '`' => {
                    self.consume();
                    Token::Quasi(Span::point(self.next - 1)).okay()
                }
                '#' => {
                    self.consume();
                    self.consume_until(|c| c == '\n');
                    self.next_token()
                }
                '"' => {
                    let start = self.next;
                    self.consume();
                    let string = self.consume_until(|c| c == '"');
                    let end = self.next;
                    self.consume();
                    Token::String(string, Span::new(start, end)).okay()
                }
                '(' => {
                    self.consume();
                    Token::LParen(Span::point(self.next - 1)).okay()
                }
                ')' => {
                    self.consume();
                    Token::RParen(Span::point(self.next - 1)).okay()
                }
                c if c.is_numeric() || c == &'.' => self.consume_number(),
                '-' if matches!(self.peekn(1), Some(c) if c.is_numeric()) => {
                    self.consume_negative_number()
                }
                c if !c.is_numeric() && !c.is_whitespace() => self.consume_symbol(),

                c => syntax!(
                    UnexpectedCharacter,
                    (Span::point(self.next), format!("{:?}", c))
                )
                .error(),
            }
        } else {
            Token::Eof(Span::point(self.next)).okay()
        }
    }

    fn peek(&self) -> Option<&char> {
        self.peekn(0)
    }

    fn peekn(&self, n: usize) -> Option<&char> {
        self.src.get(self.next + n)
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
        if self.next == self.src.len() {
            return;
        }
        self.next += 1
    }

    pub fn collect(&mut self) -> Vec<Token> {
        let mut tt = Vec::new();
        while let Ok(t) = self.next_token() {
            if matches!(t, Token::Eof(..)) {
                tt.push(t);
                break;
            }
            tt.push(t);
        }
        tt
    }
    pub fn rewind(&mut self) {
        self.next = 0;
    }
    pub fn rewind_to(&mut self, n: usize) {
        self.next = n;
    }
    pub fn next(&self) -> usize {
        self.next
    }
}

impl Debug for Lexer {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
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
