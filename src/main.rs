use clap::Parser;
pub use cranelisp::*;
pub use errors::*;
use repl::{eval_source, repl};
use std::fmt::Debug;
mod repl;

#[derive(Parser)]
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
        CranelispError::UnexpectedEOF => eprintln!("Unexpected EOF"),
        CranelispError::ReplIO(_) => todo!("IO"),
        CranelispError::Eval(e) => {
            let spans = e.spans();
            let mut report = Report::build(ReportKind::Error, (), 0).with_message(e.to_string());

            for (span, msg) in spans {
                report = report.with_label(Label::new(span).with_message(msg))
            }
            report.finish().eprint(program.into()).unwrap();
        }
    }
}
