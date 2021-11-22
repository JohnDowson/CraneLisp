use std::{fs::File, io::Read, time::Instant};

use cranelisp::{
    eval::Value,
    function::Func,
    jit::Jit,
    libcl::{add, sub},
    Env,
};
use rustyline::{
    validate::{MatchingBracketValidator, ValidationContext, ValidationResult, Validator},
    Config, EditMode,
};
use rustyline::{Editor, Result as RLResult};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};
use somok::Somok;

use crate::{lexer::Lexer, parser::Parser, provide_diagnostic, Result};

#[derive(Completer, Helper, Highlighter, Hinter)]
struct InputValidator {
    brackets: MatchingBracketValidator,
}

impl Validator for InputValidator {
    fn validate(&self, ctx: &mut ValidationContext) -> RLResult<ValidationResult> {
        self.brackets.validate(ctx)
    }
}

fn eval_env() -> Env {
    let mut env = Env::default();
    let (plus, minus) = (env.insert_symbol("+".into()), env.insert_symbol("-".into()));
    let ev_env = [
        (plus, Value::new_func(Func::from_fn(add))),
        (minus, Value::new_func(Func::from_fn(sub))),
    ]
    .into_iter()
    .collect();
    env.env = ev_env;
    env
}

pub fn repl(time: bool, ast: bool, tt: bool, clir: bool) -> Result<()> {
    let mut env = eval_env();

    let mut jit_builder = Jit::build();
    jit_builder.show_clir = clir;

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
    loop {
        let src = rl.readline("> ")?;
        if let "q\n" = &*src {
            break;
        }
        let t1 = Instant::now();
        let mut lexer = match Lexer::new(src.clone()) {
            Ok(l) => l,
            Err(e) => {
                provide_diagnostic(&e, src);
                continue;
            }
        };
        if tt {
            println!("{:?}", lexer.collect());
            lexer.rewind()
        }

        let tree = match Parser::new(&mut lexer, &mut env).parse_expr() {
            Ok(e) => e,
            Err(e) => {
                provide_diagnostic(&e, src);
                continue;
            }
        };
        if ast {
            println!("{:#?}", tree);
        }

        let evaluated = match {
            let mut jit = jit_builder.finish(&mut env);

            crate::eval::eval(tree, &mut jit)
        } {
            Ok(v) => v,
            Err(e) => {
                provide_diagnostic(&e, src);
                continue;
            }
        };

        let t2 = Instant::now();
        if time {
            println!(": {:?}", t2 - t1);
        }
        println!("{:?}", evaluated);
    }
    Ok(())
}

pub fn eval_source(prog: &str, time: bool, ast: bool, tt: bool, clir: bool) -> Result<()> {
    let src = {
        let mut buf = String::new();
        File::open(prog)?.read_to_string(&mut buf)?;
        buf
    };

    let mut env = eval_env();

    let mut jit_builder = Jit::build();
    jit_builder.show_clir = clir;

    let t1 = Instant::now();
    let mut lexer = match Lexer::new(src.clone()) {
        Ok(l) => l,
        Err(e) => {
            provide_diagnostic(&e, src);
            return e.error();
        }
    };
    if tt {
        println!("{:?}", lexer.collect());
        lexer.rewind()
    }
    while !lexer.finished() {
        let tree = match Parser::new(&mut lexer, &mut env).parse_expr() {
            Ok(e) => e,
            Err(e) => {
                provide_diagnostic(&e, src);
                return e.error();
            }
        };
        if ast {
            println!("{:#?}", tree);
        }

        let t1 = Instant::now();
        let mut jit = jit_builder.finish(&mut env);
        match crate::eval::eval(tree, &mut jit) {
            Ok(v) => println!(": {:?}", v),
            Err(e) => {
                provide_diagnostic(&e, src);
                return e.error();
            }
        };
        let t2 = Instant::now();
        if time {
            println!("exec time {:?}", t2 - t1);
        }
    }
    let t2 = Instant::now();
    if time {
        println!("total time {:?}", t2 - t1);
    }

    Ok(())
}
