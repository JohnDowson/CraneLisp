use crate::{lexer::Lexer, parser::Parser, provide_diagnostic, Result};
use cranelisp::vm::{self, asm, translate::Translator};
use rustyline::{
    validate::{MatchingBracketValidator, ValidationContext, ValidationResult, Validator},
    Config, EditMode,
};
use rustyline::{Editor, Result as RLResult};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};
use somok::Somok;
use std::{fs::File, io::Read, time::Instant};

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

    loop {
        let src = rl.readline("> ")?;
        if let "q\n" = &*src {
            break;
        }
        let t1 = Instant::now();
        let mut lexer = Lexer::new(&src);
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
        let mut translator = Translator::new();
        match translator.translate_toplevel(&tree) {
            Ok(()) => (),
            Err(e) => {
                provide_diagnostic(&e.into(), src);
                continue;
            }
        }
        let (closures, symbol_table) = translator.emit();
        // println!("{closures:?}");
        let funcs = asm::assemble(closures);
        // println!("{funcs:?}");

        let t2 = Instant::now();

        let mut vm = vm::Vm::new(funcs.get(&0).unwrap().clone(), symbol_table);

        let mut halt = false;
        while !halt {
            halt = vm.execute().unwrap();
        }
        println!("{:?}", vm.stack);

        let t3 = Instant::now();
        if time {
            println!("compile time: {:?}", t2 - t1);
            println!("run time: {:?}", t3 - t2);
            println!("total time: {:?}", t3 - t1);
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

    let t1 = Instant::now();
    let mut lexer = Lexer::new(&src);
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
            println!("{:?}", tree);
        }

        let t1 = Instant::now();

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
