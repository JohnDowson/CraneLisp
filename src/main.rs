use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    io::Cursor,
    ops::Range,
};
mod eval;
mod lexer;
mod parser;

use eval::Value;
use rustyline::{
    validate::{MatchingBracketValidator, ValidationContext, ValidationResult, Validator},
    Config, EditMode,
};
use rustyline::{Editor, Result as RLResult};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

use crate::{
    eval::{FnBody, Signature, Type},
    parser::Parser,
};

type Env = HashMap<String, Value>;
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
    #[error("Couldn't get input: {0}")]
    ReplIO(#[from] rustyline::error::ReadlineError),
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

#[derive(Completer, Helper, Highlighter, Hinter)]
struct InputValidator {
    brackets: MatchingBracketValidator,
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> RLResult<ValidationResult> {
        self.brackets.validate(ctx)
    }
}

fn main() -> Result<()> {
    let env = env_logger::Env::default()
        .filter_or("CL_LOG_LEVEL", "cranelisp=trace")
        .write_style_or("CL_LOG_STYLE", "always");
    env_logger::init_from_env(env);

    let h = InputValidator {
        brackets: MatchingBracketValidator::new(),
    };
    let config = Config::builder()
        .history_ignore_dups(true)
        .auto_add_history(true)
        .edit_mode(EditMode::Emacs)
        .build();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));
    type EnvLit<const N: usize> = [(String, Value); N];
    let env_lit: EnvLit<2> = [
        (
            "+".to_string(),
            Value::func(
                Signature::build()
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .push_arg(("b".into(), Type::Number))
                    .finish(),
                FnBody::Native(|e| e.get("a").and_then(|a| e.get("b").map(|b| a + b)).unwrap()),
            ),
        ),
        (
            "-".to_string(),
            Value::func(
                Signature::build()
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .push_arg(("b".into(), Type::Number))
                    .finish(),
                FnBody::Native(|e| e.get("a").and_then(|a| e.get("b").map(|b| a - b)).unwrap()),
            ),
        ),
    ];
    let env: Env = env_lit.into_iter().collect();

    loop {
        // (:(a b) (+ a b) 1 2)
        let env = env.clone();
        let prog = rl.readline("> ")?;
        if let "q\n" = &*prog {
            break;
        }
        let tree = {
            let mut parser = Parser::new(Cursor::new(&prog))?;
            parser.parse_expr()
        };
        match tree {
            Err(e) => provide_diagnostic(e, &prog),
            Ok(e) => {
                println!("{:#?}", &e);
                println!("{:#?}", eval::eval(e, env));
            }
        }
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
        _ => todo!("Handle other error kinds"),
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
        let prog = Cursor::new("(1.1 2 .3 4. )");
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
