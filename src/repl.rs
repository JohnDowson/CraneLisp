use rustyline::{
    validate::{MatchingBracketValidator, ValidationContext, ValidationResult, Validator},
    Config, EditMode,
};
use rustyline::{Editor, Result as RLResult};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

use crate::{
    eval::{self, Value},
    function::{FnBody, Signature, Type},
    jit::Jit,
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
    let mut jit = Jit::default();
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

    let mut env = eval_env();

    loop {
        // (:(a b) (+ a b) 1 2)
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
            Err(e) => provide_diagnostic(e, src), // TODO: handle all erors externaly, including eval errors
            Ok(e) => {
                if ast {
                    println!("{:#?}", &e)
                }
                let evaluated = eval::eval(e, &mut env, &mut jit);
                let t2 = std::time::Instant::now();
                println!("{:#?}", evaluated?);
                if time {
                    println!("Exec time: {:#?}", t2 - t1)
                }
            }
        }
    }
    Ok(())
}

fn eval_env() -> Env {
    //type EnvLit<const N: usize> = [(String, Value); N];
    let env_lit = [
        (
            "+".to_string(),
            Value::func(
                Signature::build()
                    .set_name("+".into())
                    .set_ret(Type::Number)
                    .set_foldable(true)
                    .finish()
                    .unwrap(),
                FnBody::Native((|a: f64, b: f64| a + b) as fn(f64, f64) -> f64 as *const u8),
            ),
        ),
        (
            "-".to_string(),
            Value::func(
                Signature::build()
                    .set_name("-".into())
                    .set_ret(Type::Number)
                    .set_foldable(true)
                    .finish()
                    .unwrap(),
                FnBody::Native((|a: f64, b: f64| a - b) as fn(f64, f64) -> f64 as *const u8),
            ),
        ),
        (
            "*".to_string(),
            Value::func(
                Signature::build()
                    .set_name("*".into())
                    .set_ret(Type::Number)
                    .set_foldable(true)
                    .finish()
                    .unwrap(),
                FnBody::Native((|a: f64, b: f64| a * b) as fn(f64, f64) -> f64 as *const u8),
            ),
        ),
        (
            "/".to_string(),
            Value::func(
                Signature::build()
                    .set_name("/".into())
                    .set_ret(Type::Number)
                    .set_foldable(true)
                    .finish()
                    .unwrap(),
                FnBody::Native((|a: f64, b: f64| a / b) as fn(f64, f64) -> f64 as *const u8),
            ),
        ),
        (
            "<".to_string(),
            Value::func(
                Signature::build()
                    .set_name("<".into())
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .push_arg(("b".into(), Type::Number))
                    .finish()
                    .unwrap(),
                FnBody::Native(
                    (|a: f64, b: f64| if a < b { 1.0 } else { 0.0 }) as fn(f64, f64) -> f64
                        as *const u8,
                ),
            ),
        ),
        (
            ">".to_string(),
            Value::func(
                Signature::build()
                    .set_name(">".into())
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .push_arg(("b".into(), Type::Number))
                    .finish()
                    .unwrap(),
                FnBody::Native(
                    (|a: f64, b: f64| if a > b { 1.0 } else { 0.0 }) as fn(f64, f64) -> f64
                        as *const u8,
                ),
            ),
        ),
        (
            "print".to_string(),
            Value::func(
                Signature::build()
                    .set_name("print".into())
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .finish()
                    .unwrap(),
                FnBody::Native(
                    (|a: f64| {
                        println!("{:?}", a);
                        0.0
                    }) as fn(f64) -> f64 as *const u8,
                ),
            ),
        ),
        (
            "dbg".to_string(),
            Value::func(
                Signature::build()
                    .set_name("dbg".into())
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .finish()
                    .unwrap(),
                FnBody::Native(
                    (|a: f64| {
                        println!("dbg! {:?}", a);
                        0.0
                    }) as fn(f64) -> f64 as *const u8,
                ),
            ),
        ),
        (
            "eq".to_string(),
            Value::func(
                Signature::build()
                    .set_name("eq".into())
                    .set_ret(Type::Number)
                    .push_arg(("a".into(), Type::Number))
                    .push_arg(("b".into(), Type::Number))
                    .finish()
                    .unwrap(),
                FnBody::Native(
                    (|a: f64, b: f64| {
                        if (a - b).abs() < f64::EPSILON {
                            1.0
                        } else {
                            0.0
                        }
                    }) as fn(f64, f64) -> f64 as *const u8,
                ),
            ),
        ),
    ];
    env_lit.into_iter().collect()
}

pub fn eval_source(src: String, time: bool, ast: bool, tt: bool) -> Result<()> {
    let mut jit = Jit::default();
    let mut env = eval_env();

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
            let evaluated = eval::eval(e, &mut env, &mut jit);
            let t2 = std::time::Instant::now();
            println!("{:#?}", evaluated?);
            if time {
                println!("Exec time: {:#?}", t2 - t1)
            }
        }
    }

    Ok(())
}
