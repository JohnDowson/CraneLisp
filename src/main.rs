use std::{
    fmt::{Debug, Display},
    io::Cursor,
    ops::Range,
};
mod eval;
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
    #[error("EOF")]
    EOF,
    #[error("Unexpected EOF: expected {1}")]
    UnexpectedEOF(Span, String),
}

#[derive(Debug)]
pub enum SyntaxError {
    UnmatchedParen(Span, Span),
    UnexpectedCharacter(Span, char),
    InvalidLiteral(Span, String),
    FunctionHasNoBody(Span, Span),
    FunctionHasNoArglist(Span, Span),
    InvalidDefun(Span, Span, Span),
}
impl SyntaxError {
    fn spans(&self) -> Vec<Span> {
        match self {
            SyntaxError::UnmatchedParen(a, b, ..) => vec![a.clone(), b.clone()],
            SyntaxError::UnexpectedCharacter(a, ..) => vec![a.clone()],
            SyntaxError::InvalidLiteral(a, ..) => vec![a.clone()],
            SyntaxError::FunctionHasNoBody(a, b) => vec![a.clone(), b.clone()],
            SyntaxError::FunctionHasNoArglist(a, b) => vec![a.clone(), b.clone()],
            SyntaxError::InvalidDefun(a, b, c) => vec![a.clone(), b.clone(), c.clone()],
        }
    }
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            SyntaxError::UnmatchedParen(_, _) => "Unmatched paren".into(),
            SyntaxError::UnexpectedCharacter(_, c) => format!("Unexpected character {}", c),
            SyntaxError::InvalidLiteral(_, s) => format!("Invalid numeric literal {}", s),
            SyntaxError::FunctionHasNoBody(_, _) => "Functions must have body".to_string(),
            SyntaxError::FunctionHasNoArglist(_, _) => "Functions must have arglist".to_string(),
            SyntaxError::InvalidDefun(..) => "Functions must have arglist and body".to_string(),
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
    const PROG: &str = ":";
    let parser = Parser::new(Cursor::new(PROG))?;
    let tree = parser.parse();
    if let Err(e) = tree {
        provide_diagnostic(e, PROG)
    } else {
        println!("{:#?}", tree.unwrap());
    }
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
        CranelispError::EOF => todo!(),
        CranelispError::UnexpectedEOF(_, _) => todo!(),
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, CranelispError, SyntaxError};
    use std::io::Cursor;
    #[allow(dead_code)]
    fn test_logger() {
        let env = env_logger::Env::default()
            .filter_or("CL_LOG_LEVEL", "trace")
            .write_style_or("CL_LOG_STYLE", "always");
        env_logger::init_from_env(env);
    }

    #[test]
    fn valid_numeric_literal() {
        let prog = Cursor::new("1.1 2 .3 4.");
        let mut lexer = Lexer::new(prog).unwrap();
        let expected = ["Ok(1.1)", "Ok(2.0)", "Ok(0.3)", "Ok(4.0)"];
        for expected in expected {
            assert_eq!(format!("{:?}", lexer.next_token()), expected)
        }
    }

    #[test]
    fn invalid_numeric_literal() {
        let progs = ["1/1", "2a", "3.b"];

        for prog in progs {
            let mut lexer = Lexer::new(Cursor::new(prog)).unwrap();
            assert!(matches!(
                lexer.next_token(),
                Err(CranelispError::Syntax(SyntaxError::InvalidLiteral(..)))
            ))
        }
    }

    #[test]
    fn number_in_list() {
        let prog = Cursor::new("(1.1 2 .3 4.)");
        let mut lexer = Lexer::new(prog).unwrap();
        let expected = ["Ok(()", "Ok(1.1)", "Ok(2.0)", "Ok(0.3)", "Ok(4.0)", "Ok())"];
        for expected in expected {
            assert_eq!(format!("{:?}", lexer.next_token()), expected)
        }
    }

    #[test]
    fn symbol_standalone() {
        let prog = Cursor::new("sym sym2 sym_-bol");
        let mut lexer = Lexer::new(prog).unwrap();
        let expected = ["Ok(sym)", "Ok(sym2)", "Ok(sym_-bol)"];
        for expected in expected {
            let token = lexer.next_token();
            assert_eq!(format!("{:?}", token), expected)
        }
    }

    #[test]
    fn symbol_in_list() {
        let prog = Cursor::new("(symbol1 symbol2)");
        let mut lexer = Lexer::new(prog).unwrap();
        let expected = ["Ok(()", "Ok(symbol1)", "Ok(symbol2)", "Ok())"];
        for expected in expected {
            let token = dbg! {lexer.next_token()};
            assert_eq!(format!("{:?}", token), expected)
        }
    }

    #[test]
    fn quote() {
        let prog = Cursor::new("'");
        let mut lexer = Lexer::new(prog).unwrap();
        let expected = ["Ok(')"];
        for expected in expected {
            let token = lexer.next_token();
            assert_eq!(format!("{:?}", token), expected)
        }
    }

    #[test]
    fn string() {
        let prog = Cursor::new(r#""Foobar""#);
        let mut lexer = Lexer::new(prog).unwrap();
        let expected = [r#"Ok("Foobar")"#];
        for expected in expected {
            let token = lexer.next_token();
            assert_eq!(format!("{:?}", token), expected)
        }
    }
}
