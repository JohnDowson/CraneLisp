use std::time::Instant;

use rustyline::{
    validate::{MatchingBracketValidator, ValidationContext, ValidationResult, Validator},
    Config, EditMode,
};
use rustyline::{Editor, Result as RLResult};
use rustyline_derive::{Completer, Helper, Highlighter, Hinter};

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

    loop {
        let src = rl.readline("> ")?;
        if let "q\n" = &*src {
            break;
        }
        let t1 = Instant::now();
        let mut lexer = match Lexer::new(src.clone(), "repl") {
            Ok(l) => l,
            Err(e) => {
                provide_diagnostic(e, src);
                continue;
            }
        };
        if tt {
            println!("{:#?}", lexer.collect());
            lexer.rewind()
        }
        let t2 = Instant::now();

        let tree = match Parser::new(lexer).parse_expr() {
            Ok(e) => e,
            Err(e) => {
                provide_diagnostic(e, src);
                continue;
            }
        };
        if ast {
            println!("{:#?}", tree);
        }

        if time {
            println!("{:?}", t2 - t1);
        }
    }
    Ok(())
}

// pub fn eval_source(src: String, time: bool, ast: bool, tt: bool) -> Result<()> {
//     let mut jit = Jit::default();
//     let mut env = eval_env();
//     let mut lexer = Lexer::new(src.clone(), "source")?;
//     if tt {
//         println!("{:?}", lexer.collect());
//         lexer.rewind()
//     }
//     let t1 = std::time::Instant::now();

//     let tree = {
//         let mut parser = Parser::new(lexer)?;
//         parser.parse_expr()
//     };

//     match tree {
//         Err(e) => provide_diagnostic(e, src),
//         Ok(e) => {
//             if ast {
//                 println!("{:#?}", &e)
//             }
//             let evaluated = eval::eval(e, &mut env, &mut jit);
//             let t2 = std::time::Instant::now();
//             println!("{:#?}", evaluated?);
//             if time {
//                 println!("Exec time: {:#?}", t2 - t1)
//             }
//         }
//     }

//     Ok(())
// }
