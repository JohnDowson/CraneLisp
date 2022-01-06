use crate::{
    // jit::Jit,
    lookup_value,
    set_value,
    value::{map_to_vec, ocar, ocdr, Atom, Symbol, Tag},
    CranelispError,
    EvalError,
    Result,
    Span,
};
use somok::Somok;

// "loop" => {
//     self.lexer.next_token()?;
//     let expr = self.parse_expr()?;
//     self.skip_whitespace()?;
//     let rparen = self.eat_rparen()?;
//     Expr::Loop(Box::new(expr), Span::merge(token.span(), rparen.span()))
//         .okay()
// }
// "return" => {
//     self.lexer.next_token()?;
//     self.skip_whitespace()?;
//     let next = self.lexer.next();
//     let expr = match self.parse_expr() {
//         Ok(expr) => expr.boxed().some(),
//         Err(_e) => {
//             self.lexer.rewind_to(next);
//             None
//         }
//     };
//     let rparen = self.eat_rparen()?;
//     Expr::Return(expr, Span::merge(token.span(), rparen.span())).okay()
// }
// "let" => {
//     self.lexer.next_token()?;
//     let name = self.eat_symbol()?;
//     let expr = self.parse_expr()?;
//     self.skip_whitespace()?;
//     let rparen = self.eat_rparen()?;
//     intern(name.clone());
//     Expr::Let(
//         name,
//         Box::new(expr),
//         Span::merge(token.span(), rparen.span()),
//     )
//     .okay()
// }
// "if" => {
//     self.lexer.next_token()?;
//     let cond = self.parse_expr()?;

//     let truth = self.parse_expr()?;

//     let lie = self.parse_expr()?;

//     self.skip_whitespace()?;
//     let rparen = self.eat_rparen()?;
//     match (true, true, true) {
//         (true, true, true) => Expr::If(
//             Box::new(cond),
//             Box::new(truth),
//             Box::new(lie),
//             Span::merge(token.span(), rparen.span()),
//         )
//         .okay(),
//         (true, true, false) => syntax!(
//             UnexpectedExpression,
//             (
//                 lie.span(),
//                 format!("Unexpected {} where List is expected", lie)
//             )
//         )
//         .error(),
//         (true, false, true) => syntax!(
//             UnexpectedExpression,
//             (
//                 truth.span(),
//                 format!("Unexpected {} where List is expected", truth)
//             )
//         )
//         .error(),
//         (true, false, false) => syntax!(
//             UnexpectedExpression,
//             (
//                 truth.span(),
//                 format!("Unexpected {} where List is expected", truth)
//             ),
//             (
//                 lie.span(),
//                 format!("Unexpected {} where List is expected", lie)
//             )
//         )
//         .error(),
//         (false, true, true) => syntax!(
//             UnexpectedExpression,
//             (
//                 cond.span(),
//                 format!("Unexpected {} where List is expected", cond)
//             )
//         )
//         .error(),
//         (false, true, false) => syntax!(
//             UnexpectedExpression,
//             (
//                 cond.span(),
//                 format!("Unexpected {} where List is expected", cond)
//             ),
//             (
//                 lie.span(),
//                 format!("Unexpected {} where List is expected", lie)
//             )
//         )
//         .error(),
//         (false, false, true) => syntax!(
//             UnexpectedExpression,
//             (
//                 cond.span(),
//                 format!("Unexpected {} where List is expected", cond)
//             ),
//             (
//                 truth.span(),
//                 format!("Unexpected {} where List is expected", truth)
//             )
//         )
//         .error(),
//         (false, false, false) => syntax!(
//             UnexpectedExpression,
//             (
//                 cond.span(),
//                 format!("Unexpected {} where List is expected", cond)
//             ),
//             (
//                 truth.span(),
//                 format!("Unexpected {} where List is expected", truth)
//             ),
//             (
//                 lie.span(),
//                 format!("Unexpected {} where List is expected", lie)
//             )
//         )
//         .error(),
//     }
// }

pub fn eval((atom, span): (Atom, Span)) -> Result<Atom> {
    match atom.tag {
        Tag::Symbol => unsafe {
            let s = atom.as_symbol();
            (*lookup_value(s.name.clone()).ok_or_else(|| {
                eprintln!("Undefined in eval translation");
                CranelispError::Eval(EvalError::Undefined(s.name.clone().into(), span))
            })?)
            .okay()
        },
        Tag::Pair => {
            let maybe_symbol = ocar(atom);
            let fun = match maybe_symbol.tag {
                Tag::Error => unreachable!(),
                Tag::Func => atom,
                Tag::Symbol => {
                    let sym = &*maybe_symbol.as_symbol().name;
                    let sym_value = unsafe { *maybe_symbol.as_symbol().val };
                    match sym {
                        "let" => {
                            let val = eval((ocdr(atom), span))?;
                            set_value(Symbol::new_with_atom(sym, val));
                            return val.okay();
                        }
                        "if" => {
                            let cond = eval((ocar(ocdr(atom)), span))?;
                            if cond.as_bool() {
                                let truth = ocar(ocdr(ocdr(atom)));
                                return eval((truth, span));
                            } else {
                                let lie = ocar(ocdr(ocdr(ocdr(atom))));
                                return eval((lie, span));
                            }
                        }
                        _ => match sym_value.tag {
                            Tag::Func => sym_value,
                            _ => {
                                return CranelispError::Eval(EvalError::UnexpectedAtom(
                                    span,
                                    format!("Unexpected atom: {}", sym_value),
                                ))
                                .error();
                            }
                        },
                    }
                }
                _ => {
                    return CranelispError::Eval(EvalError::UnexpectedAtom(
                        span,
                        format!("Unexpected atom: {}", atom),
                    ))
                    .error();
                }
            };
            let values = map_to_vec(ocdr(atom), |a| eval((a, span)).unwrap());
            unsafe { (*fun.as_func()).call(values).okay() }
        }
        _ => atom.okay(),
        // todo: Quoted, return, loop
        // Expr::Return(expr, ..) => {
        //     if let Some(e) = expr {
        //         Atom::new_return(eval(*e, jit)?).okay()
        //     } else {
        //         Atom::new_return(Atom::NULL).okay()
        //     }
        // }
        // Expr::Loop(body, ..) => {
        //     let mut last_val = Atom::NULL;
        //     while !matches!(last_val.tag, value::Tag::Return) {
        //         last_val = eval(*body.clone(), jit)?;
        //     }
        //     unsafe { *last_val.as_ptr::<Atom>() }.okay()
        // }
    }
}
