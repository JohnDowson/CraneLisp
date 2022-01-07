use crate::{
    // jit::Jit,
    lookup_value,
    set_value,
    value::{car, cdr, map_to_vec, scar, scdr, Atom, Symbol, Tag},
    CranelispError,
    EvalError,
    Result,
    Span,
};
use somok::Somok;

pub fn eval((atom, span): (Atom, Span)) -> Result<Atom> {
    match atom.tag {
        Tag::Symbol => {
            let s = atom.as_symbol();
            (lookup_value(s.name.clone()).cloned().ok_or_else(|| {
                eprintln!("Undefined in eval translation");
                CranelispError::Eval(EvalError::Undefined(s.name.clone().into(), span))
            })?)
            .okay()
        }
        Tag::Pair => {
            let maybe_symbol = car(atom.clone());
            let fun = match maybe_symbol.tag {
                Tag::Error => unreachable!(),
                Tag::Func => atom.clone(),
                Tag::Symbol => {
                    let sym = &*maybe_symbol.as_symbol().name;
                    let sym_value: Atom = *maybe_symbol.as_symbol().val.clone();
                    match sym {
                        "let" => {
                            let sym = scar(scdr(&atom)).as_symbol().name.clone();
                            let val = eval((car(cdr(cdr(atom))), span))?;
                            set_value(Symbol::new_with_atom(sym, val.clone()));
                            return val.okay();
                        }
                        "if" => {
                            let cond = eval((car(cdr(atom.clone())), span))?;
                            if cond.as_bool() {
                                let truth = car(cdr(cdr(atom)));
                                return eval((truth, span));
                            } else {
                                let lie = car(cdr(cdr(cdr(atom))));
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
            let values = map_to_vec(cdr(atom), |a| eval((a, span)).unwrap());
            (*fun.as_func()).call(values).okay()
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
