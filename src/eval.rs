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
                            let sym = dbg! {ocar(ocdr(atom))}.as_symbol().name.clone();
                            let val = eval((ocar(ocdr(ocdr(atom))), span))?;
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
