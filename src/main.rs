use std::{
    fmt::{format, Debug, Display},
    io::Cursor,
    ops::Range,
};
mod lexer;
mod parser;

use crate::parser::Parser;

type Result<T, E = CranelispError> = std::result::Result<T, E>;
type Span = Range<usize>;
#[derive(Debug, thiserror::Error)]
pub enum CranelispError {
    #[error("Syntax error {0:?}")]
    Syntax(SyntaxError),
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
    #[error("Unexpected EOF: expected {1}")]
    UnexpectedEOF(Span, String),
}

#[derive(Debug)]
pub enum SyntaxError {
    UnmatchedParen(Span, Span),
    UnexpectedCharacter(Span, char),
}
impl SyntaxError {
    fn spans(&self) -> Vec<Span> {
        match self {
            SyntaxError::UnmatchedParen(a, b, ..) => vec![a.clone(), b.clone()],
            SyntaxError::UnexpectedCharacter(a, ..) => vec![a.clone()],
        }
    }
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            SyntaxError::UnmatchedParen(_, _) => "Unmatched paren".into(),
            SyntaxError::UnexpectedCharacter(_, c) => format!("Unexpected character {}", c),
        };
        write!(f, "{}", msg)
    }
}

#[macro_export]
macro_rules! if_ {
    ($cond:expr => $t:expr ; $f:expr) => {
        if $cond {
            $t
        } else {
            $f
        }
    };
}

fn main() -> Result<()> {
    let env = env_logger::Env::default()
        .filter_or("CL_LOG_LEVEL", "trace")
        .write_style_or("CL_LOG_STYLE", "always");
    env_logger::init_from_env(env);
    const PROG: &str = "(abc 1 2 b '( #inline# # comment #3 4 c) sad)#comment
    (next)";
    let parser = Parser::new(Cursor::new(PROG))?;
    let tree = parser.parse();
    if let Err(e) = tree {
        provide_diagnostic(e, PROG)
    }
    //println!("{}", out);
    Ok(())
}

fn provide_diagnostic(error: CranelispError, program: &str) {
    match error {
        CranelispError::Syntax(s) => {
            use ariadne::{Label, Report, ReportKind, Source};

            let spans = s.spans().into_iter().zip(std::iter::repeat("Here"));
            let mut report = Report::build(ReportKind::Error, (), 0).with_message(s.to_string());

            for (span, msg) in spans {
                report = report.with_label(Label::new(span).with_message(msg))
            }

            report.finish().eprint(Source::from(program)).unwrap();
        }
        CranelispError::IO(_) => todo!(),
        CranelispError::InvalidLiteral(_) => todo!(),
        CranelispError::EOF => todo!(),
        CranelispError::UnexpectedEOF(_, _) => todo!(),
    }
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
trait ToOption: Sized {
    fn some(self) -> Option<Self> {
        Some(self)
    }
}
impl<T: Sized> ToOption for T {}
