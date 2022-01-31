use std::{fs::File, io::Read, time::Instant};

use cranelisp::{
    translate::{self},
    vm::VM,
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

pub fn repl(time: bool, ast: bool, tt: bool, _clir: bool) -> Result<()> {
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

    let env = cranelisp::env::setup();

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

        let tree = match Parser::new(&mut lexer).parse_expr() {
            Ok(e) => e,
            Err(e) => {
                provide_diagnostic(&e, src);
                continue;
            }
        };
        if ast {
            println!("{:#?}", tree);
        }

        let mut translator = translate::Translator::new();
        let code = translator.translate(tree.0);

        println!("{:?}", code);
        let code = translator.compile(code);
        println!("{:?}", code);
        let mut vm = VM::new(code, 100, env);
        let v = vm.run();
        println!("{:?}", v);

        let t2 = Instant::now();
        if time {
            println!("{:?}", t2 - t1);
        }
    }
    Ok(())
}

pub fn eval_source(prog: &str, time: bool, ast: bool, tt: bool, _clir: bool) -> Result<()> {
    let src = {
        let mut buf = String::new();
        File::open(prog)?.read_to_string(&mut buf)?;
        buf
    };

    let env = cranelisp::env::setup();

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
        let tree = match Parser::new(&mut lexer).parse_expr() {
            Ok(e) => e,
            Err(cranelisp::CranelispError::EOF) => continue,
            Err(e) => {
                provide_diagnostic(&e, src);
                return e.error();
            }
        };
        if ast {
            println!("{:#?}", tree);
        }

        let t1 = Instant::now();

        let mut translator = translate::Translator::new();
        let code = translator.translate(tree.0);

        println!("{:?}", code);
        let code = translator.compile(code);
        println!("{:?}", code);
        let mut vm = VM::new(code, 100, env);
        let v = vm.run();
        println!("{:?}", v);

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
