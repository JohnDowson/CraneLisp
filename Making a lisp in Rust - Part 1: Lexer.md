# Making a lisp in Rust - Part 1: Lexer
I've tried writing a lisp before, but never got much past polish notation calculator phase, in part due to misdesigned parsing pipeline.
This time around I started from the ground up thinking about error propagation and reporting, handled by the excellent [Ariadne][ariadne] crate.

So, let's start with our token type, remembering to include source location of each token:

```rust
pub enum Token {
    LParen(usize),
    RParen(usize),
    Number(f64, Range<usize>),
    Symbol(String, Range<usize>),
    Comment(String),
    Quote(usize),
}
```
##### Foreshadowing: this will turn out to be pretty unergonomic
And while we're at it, let's add a `Debug` impl:
```rust
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
```
Now, let's take a look at `Lexer` definition:
```rust
pub struct Lexer {
    src: Vec<char>,
    next: usize,
}
```
I've decided to go with `Vec<char>` rather than a reader over `[u8]` to allow for unicode support.

Next let's write some useful functionality for our lexer: `peek`, `consume` and `consume_until`.

```rust
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
```

You might've already noticed that both `consume`'s are subtly wrong.

Now on to primary tokenizer function:
```rust
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
```
You might notice peculiar `.okay()` and `.error()` scattered around. That's my tiny utility crate [Somok][somok] which exists because of my instinctive preference for postfix notation for wrappers.

And with that we can tokenize some strings:

```rust
type Result<T, E = CranelispError> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum CranelispError {
    #[error("Syntax error {0:?} {1}")]
    Syntax(Range<usize>, String),
    #[error("IO error {0}")]
    IO(#[from] std::io::Error),
    #[error("InvalidLiteral {0}")]
    InvalidLiteral(#[from] std::num::ParseFloatError),
    #[error("EOF")]
    EOF,
}

fn main() -> Result<()> {
    let env = env_logger::Env::default()
        .filter_or("CL_LOG_LEVEL", "trace")
        .write_style_or("CL_LOG_STYLE", "always");
    env_logger::init_from_env(env);
    const PROG: &str = "(abc 1 2 b '(3 4 c) sad)";
    let mut lexer = Lexer::new(PROG)?;
    while let Ok(t) = lexer.next_token() {
        print!("{:?}", t);
    }
    Ok(())
}
```


[somok]: https://crates.io/crates/somok/1.0.0
[ariadne]: https://crates.io/crates/ariadne/0.0.0