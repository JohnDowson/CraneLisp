use std::{fs::File, io::Read, path::Path};

use rustyline::{
    validate::{MatchingBracketValidator, ValidationContext, ValidationResult, Validator},
    Config, EditMode,
};
use rustyline::{Editor, Result as RLResult};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

use crate::{
    eval::{self, FnBody, Signature, Type, Value},
    lexer::Lexer,
    parser::Parser,
    provide_diagnostic, Env, Result,
};
#[derive(Completer, Helper, Highlighter, Hinter)]
struct InputValidator {
    brackets: MatchingBracketValidator,
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> RLResult<ValidationResult> {
        self.brackets.validate(ctx)
    }
}

pub fn repl(time: bool, ast: bool, tt: bool) -> Result<()> {
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

    let env = eval_env();

    loop {
        // (:(a b) (+ a b) 1 2)
        let env = env.clone();
        let src = rl.readline("> ")?;
        if let "q\n" = &*src {
            break;
        }

        if tt {
            println!("{:?}", Lexer::new(src.clone())?.collect())
        }

        let t1 = std::time::Instant::now();

        let tree = {
            let mut parser = Parser::new(src.clone())?;
            parser.parse_expr()
        };
        match tree {
            Err(e) => provide_diagnostic(e, src),
            Ok(e) => {
                if ast {
                    println!("{:#?}", &e)
                }
                let evaluated = eval::eval(e, env);
                let t2 = std::time::Instant::now();
                println!("{:#?}", evaluated);
                if time {
                    println!("Exec time: {:#?}", t2 - t1)
                }
            }
        }
    }
    Ok(())
}

fn eval_env() -> Env {
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
    env_lit.into_iter().collect()
}

pub fn eval_source(src: impl AsRef<Path>, time: bool, ast: bool, tt: bool) -> Result<()> {
    let mut prog = File::open(src.as_ref())?;
    let src = {
        let mut src = String::new();
        prog.read_to_string(&mut src)?;
        src
    };
    let env = eval_env();

    if tt {
        println!("{:?}", Lexer::new(src.clone())?.collect())
    }

    let t1 = std::time::Instant::now();

    let tree = {
        let mut parser = Parser::new(src.clone())?;
        parser.parse_expr()
    };

    match tree {
        Err(e) => provide_diagnostic(e, src),
        Ok(e) => {
            if ast {
                println!("{:#?}", &e)
            }
            let evaluated = eval::eval(e, env);
            let t2 = std::time::Instant::now();
            println!("{:#?}", evaluated);
            if time {
                println!("Exec time: {:#?}", t2 - t1)
            }
        }
    }

    Ok(())
}
