use std::fmt::Debug;

use clap::Parser;
#[allow(dead_code, unused_imports)]
pub use cranelisp::*;
pub use errors::*;
use repl::{eval_source, repl};
mod repl;

#[derive(clap::Parser)]
struct Args {
    #[clap(short, long)]
    time: bool,
    #[clap(short = 'a', long)]
    dump_ast: bool,
    #[clap(short = 'k', long)]
    dump_tokens: bool,
    #[clap(short = 'c', long)]
    dump_clir: bool,
    source: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if let Some(source) = args.source {
        eval_source(
            &source,
            args.time,
            args.dump_ast,
            args.dump_tokens,
            args.dump_clir,
        )
    } else {
        repl(args.time, args.dump_ast, args.dump_tokens, args.dump_clir)
    }
}

pub fn provide_diagnostic(error: &CranelispError, program: impl Into<ariadne::Source> + Debug) {
    use ariadne::{Label, Report, ReportKind};
    match error {
        CranelispError::Syntax(s) => {
            let spans = s.spans().into_iter();
            let mut report = Report::build(ReportKind::Error, (), 0).with_message(s.to_string());

            for (span, msg) in spans {
                report = report.with_label(Label::new(span).with_message(msg))
            }
            report.finish().eprint(program.into()).unwrap();
        }
        CranelispError::IO(_) => todo!("IO"),
        CranelispError::EOF => {}
        CranelispError::UnexpectedEOF(_, _) => todo!("UnexpectedEOF"),
        CranelispError::ReplIO(_) => todo!("IO"),
        CranelispError::Eval(e) => {
            let spans = e.spans();
            let mut report = Report::build(ReportKind::Error, (), 0).with_message(e.to_string());

            for (span, msg) in spans {
                report = report.with_label(Label::new(span).with_message(msg))
            }
            report.finish().eprint(program.into()).unwrap();
        }
        CranelispError::JIT(e) => todo!("{:#?}", e),
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, CranelispError};
    #[test]
    fn invalid_numeric_literal() {
        let progs = ["1/1", "2a", "3.b"];

        for prog in progs {
            let mut lexer = Lexer::new(prog.into()).unwrap();
            assert!(matches!(
                lexer.next_token(),
                Err(CranelispError::Syntax(..))
            ))
        }
    }

    #[test]
    fn number_in_list() {
        let prog = "(1.1 2 .3 4.)".into();
        let tokens = Lexer::new(prog)
            .unwrap()
            .collect()
            .into_iter()
            .filter(|t| !t.is_whitespace() && !t.is_eof())
            .collect::<Vec<_>>();
        let expected = ["(", "(1.1)", "(2)", "(0.3)", "(4.0)", ")"];
        for (i, &expect) in expected.iter().enumerate() {
            assert_eq!(format!("{:?}", tokens[i]), expect)
        }
    }
}
