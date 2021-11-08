use std::{
    fmt::{Debug, Write},
    io::Cursor,
    ops::Range,
};
mod lexer;
use lexer::Lexer;

type Result<T, E = CranelispError> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum CranelispError {
    #[error("Syntax error {0:?} {1}")]
    Syntax(Range<usize>, String),
    //#[error("Type error {0}")]
    //Type(String),
    //#[error("Lookup error {0}")]
    //Lookup(String),
    #[error("{0}")]
    IO(#[from] std::io::Error),
    #[error("{0}")]
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
    let mut lexer = Lexer::new(Cursor::new(PROG))?;
    let mut out = String::new();
    while let Ok(t) = lexer.next_token() {
        write!(out, "{:?}", t).unwrap();
    }
    println!("{}", out);
    Ok(())
}

trait ToResult: Sized {
    fn okay(self) -> Result<Self> {
        Ok(self)
    }
    fn error<T>(self) -> Result<T, Self> {
        Err(self)
    }
}
impl<T: Sized> ToResult for T {}
