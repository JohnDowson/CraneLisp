use crate::function::Type;
use crate::CranelispError;
use crate::Result;
use crate::Span;
use crate::SyntaxError;

use somok::Somok;
use std::fmt::Debug;
mod token;
pub use token::Token;

#[derive(Clone)]
pub struct Lexer {
    src: Vec<char>,
    _src: *mut str,
    src_id: &'static str,
    next: usize,
}

impl Lexer {
    pub fn new(_src: String, src_id: &'static str) -> Result<Self> {
        let _src = Box::leak(_src.into_boxed_str());
        Self {
            src: _src.chars().collect(),
            _src,
            src_id,
            next: 0,
        }
        .okay()
    }

    fn consume_negative_number(&mut self) -> Result<Token> {
        let start = self.next;
        self.consume();
        match self.consume_number()? {
            Token::Number(n, span) => Token::Number(-n, start..span.end).okay(),
            _ => unreachable!(),
        }
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
                            if !c.is_numeric() && c != &')' && !c.is_whitespace() {
                                return CranelispError::Syntax(SyntaxError::InvalidLiteral(
                                    start..self.next,
                                    format!(" {:?}", c),
                                ))
                                .error();
                            }
                        }
                        decimal = self.consume_until(|c| !c.is_numeric());
                    }
                    Some(c) if c.is_whitespace() || c == &')' => {}
                    Some(c) => {
                        eprintln!("invalid literal{:?}", c.is_whitespace());
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

    fn is_loop(&self) -> bool {
        matches!(
            (self.peekn(1), self.peekn(2), self.peekn(3)),
            (Some('o'), Some('o'), Some('p'))
        )
    }

    fn is(&self, tok: &str) -> bool {
        tok.chars()
            .enumerate()
            .all(|(i, c)| self.peekn(i) == Some(&c))
    }

    pub fn next_token(&mut self) -> Result<Token> {
        if let Some(char) = self.peek() {
            // trace!("{:?}", &*self);

            match char {
                c if c.is_whitespace() => {
                    self.consume();
                    self.next_token()
                }
                ':' => {
                    todo!("Maybe have type separator token")
                }
                '"' => {
                    let start = self.next;
                    self.consume();
                    let string = self.consume_until(|c| c == '"');
                    let end = self.next;
                    Token::string(Span::new(start, end, self.src_id), string).okay()
                }
                '(' => {
                    //trace!("Consuming LParen at location {}", self.next);
                    self.consume();
                    Token::lparen(Span::point(self.next - 1, self.src_id)).okay()
                }
                ')' => {
                    //trace!("Consuming RParen at location {}", self.next);
                    self.consume();
                    Token::rparen(Span::point(self.next - 1, self.src_id)).okay()
                }
                c if c.is_numeric() || c == &'.' => self.consume_number(),
                '-' if matches!(self.peekn(1), Some(c) if c.is_numeric()) => {
                    self.consume_negative_number()
                }
                c if !c.is_numeric() && !c.is_whitespace() => self.consume_symbol(),

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
            Token::Eof(self.next).okay()
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
