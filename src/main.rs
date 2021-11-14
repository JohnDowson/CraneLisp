use std::fmt::Debug;

use clap::Parser;
#[allow(dead_code, unused_imports)]
pub use cranelisp::*;
pub use errors::*;
mod repl;

#[derive(clap::Parser)]
struct Args {
    #[clap(short, long)]
    time: bool,
    #[clap(short = 'a', long)]
    dump_ast: bool,
    #[clap(short = 'k', long)]
    dump_tokens: bool,
    source: Option<String>,
}

fn main() -> Result<()> {
    let env = env_logger::Env::default()
        .filter_or("CL_LOG_LEVEL", "cranelisp=trace")
        .write_style_or("CL_LOG_STYLE", "always");
    env_logger::init_from_env(env);

    let args = Args::parse();
    //if let Some(source) = args.exec {
    //    let source = source.join(" ");
    //    eval_source(source, args.time, args.dump_ast, args.dump_tokens)
    //} else if let Some(source) = args.source {
    //    let mut prog = File::open(&source)?;
    //    let source = {
    //        let mut src = String::new();
    //        prog.read_to_string(&mut src)?;
    //        src
    //    };
    //eval_source(source, args.time, args.dump_ast, args.dump_tokens)
    //} else {
    repl::repl(args.time, args.dump_ast, args.dump_tokens)
    //}
}

pub fn provide_diagnostic(error: CranelispError, program: impl Into<ariadne::Source> + Debug) {
    use ariadne::{Label, Report, ReportKind};
    match error {
        CranelispError::Syntax(s) => {
            let spans = s.spans.clone().into_iter();
            let mut report = Report::build(ReportKind::Error, (), 0).with_message(s.to_string());

            for (span, msg) in spans {
                report = report.with_label(Label::new(span).with_message(msg))
            }
            report.finish().eprint(program.into()).unwrap();
        }
        CranelispError::IO(_) => todo!("IO"),
        CranelispError::EOF => todo!("EOF"),
        CranelispError::UnexpectedEOF(_, _) => todo!("UnexpectedEOF"),
        CranelispError::ReplIO(_) => todo!("IO"),
        CranelispError::Eval(e) => {
            dbg!(e);
        }
        CranelispError::JIT(_) => todo!(),
    }
}

// // #[cfg(test)]
// // mod test {
// //     use crate::{lexer::Lexer, CranelispError, SyntaxError};
// //     #[allow(dead_code)]
// //     fn test_logger() {
// //         let env = env_logger::Env::default()
// //             .filter_or("CL_LOG_LEVEL", "trace")
// //             .write_style_or("CL_LOG_STYLE", "always");
// //         env_logger::init_from_env(env);
// //     }

// //     #[test]
// //     fn valid_numeric_literal() {
// //         let prog = "1.1 2 .3 4.".into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = ["Ok(1.1)", "Ok(2.0)", "Ok(0.3)", "Ok(4.0)"];
// //         for expected in expected {
// //             assert_eq!(format!("{:?}", lexer.next_token()), expected)
// //         }
// //     }

// //     #[test]
// //     fn invalid_numeric_literal() {
// //         let progs = ["1/1", "2a", "3.b"];

// //         for prog in progs {
// //             let mut lexer = Lexer::new(prog.into()).unwrap();
// //             assert!(matches!(
// //                 lexer.next_token(),
// //                 Err(CranelispError::Syntax(SyntaxError::InvalidLiteral(..)))
// //             ))
// //         }
// //     }

// //     #[test]
// //     fn number_in_list() {
// //         let prog = "(1.1 2 .3 4.)".into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = ["Ok(()", "Ok(1.1)", "Ok(2.0)", "Ok(0.3)", "Ok(4.0)", "Ok())"];
// //         for expected in expected {
// //             assert_eq!(format!("{:?}", lexer.next_token()), expected)
// //         }
// //         let prog = "(1.1 2 .3 4. )".into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = ["Ok(()", "Ok(1.1)", "Ok(2.0)", "Ok(0.3)", "Ok(4.0)", "Ok())"];
// //         for expected in expected {
// //             assert_eq!(format!("{:?}", lexer.next_token()), expected)
// //         }
// //     }

// //     #[test]
// //     fn symbol_standalone() {
// //         let prog = "sym sym2 sym_-bol".into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = ["Ok(sym)", "Ok(sym2)", "Ok(sym_-bol)"];
// //         for expected in expected {
// //             let token = lexer.next_token();
// //             assert_eq!(format!("{:?}", token), expected)
// //         }
// //     }

// //     #[test]
// //     fn symbol_in_list() {
// //         let prog = "(symbol1 symbol2)".into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = ["Ok(()", "Ok(symbol1)", "Ok(symbol2)", "Ok())"];
// //         for expected in expected {
// //             let token = dbg! {lexer.next_token()};
// //             assert_eq!(format!("{:?}", token), expected)
// //         }
// //     }

// //     #[test]
// //     fn quote() {
// //         let prog = "'".into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = ["Ok(')"];
// //         for expected in expected {
// //             let token = lexer.next_token();
// //             assert_eq!(format!("{:?}", token), expected)
// //         }
// //     }

// //     #[test]
// //     fn string() {
// //         let prog = r#""Foobar""#.into();
// //         let mut lexer = Lexer::new(prog).unwrap();
// //         let expected = [r#"Ok("Foobar")"#];
// //         for expected in expected {
// //             let token = lexer.next_token();
// //             assert_eq!(format!("{:?}", token), expected)
// //         }
// //     }
// // }
