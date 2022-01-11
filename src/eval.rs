use std::ops::Deref;

use somok::{Leaksome, Somok};

use crate::{
    errors,
    function::Func,
    mem,
    value::{map, map_to_vec, Atom, SymId},
    CranelispError, Env, EvalError, Result, Span,
};

pub fn eval<'a, 'b>((atom, span): (Atom, Span), env: &'a mut Env<'b>) -> Result<Atom> {
    match atom {
        Atom::FRef(_) => unreachable!(),
        Atom::Error(_) => todo!(),
        Atom::Pair(p) => {
            let head = p.car.deref();
            let func = match head {
                Atom::Func(_) => todo!(),
                Atom::Symbol(s) => match &**s {
                    "fn" => {
                        return Atom::Func(
                            Func::interp(
                                map_to_vec(p.cdr.as_pair().unwrap().car.clone(), |a| {
                                    a.deref().as_symbol().cloned().ok_or_else(|| {
                                        CranelispError::Eval(EvalError::UnexpectedAtom(
                                            span,
                                            "".into(),
                                        ))
                                    })
                                })
                                .into_iter()
                                .collect::<Result<Vec<SymId>>>()?,
                                p.cdr
                                    .as_pair()
                                    .unwrap()
                                    .cdr
                                    .as_pair()
                                    .unwrap()
                                    .car
                                    .deref()
                                    .clone(),
                            )
                            .boxed()
                            .leak(),
                        )
                        .okay()
                    }
                    "set" => {
                        let sym = p.cdr.as_pair().unwrap().car.as_symbol().unwrap();
                        let atom = p.cdr.as_pair().unwrap().cdr.as_pair().unwrap().car.deref();
                        let atom = eval((atom.clone(), span), env)?;
                        env.insert(*sym, atom.clone());
                        return atom.okay();
                    }
                    _ => env
                        .try_get(*s)
                        .ok_or_else(|| {
                            errors::CranelispError::Eval(EvalError::Undefined(
                                format!("Undefined symbol: {:?}", s),
                                span,
                            ))
                        })?
                        .clone(),
                },
                _ => {
                    return errors::eval(EvalError::UnexpectedAtom(
                        span,
                        format!("Unexpected atom {:?} where func expected", head),
                    ))
                }
            };
            let args = map(p.cdr, |a| eval((a, span), env).unwrap());
            let mut subenv: Env<'_> = env.fork();
            let res = func.as_func().unwrap().call(mem::alloc(args), &mut subenv);
            res.okay()
        }
        Atom::Return(_) => todo!(),
        Atom::Symbol(s) => env
            .try_get(s)
            .ok_or_else(|| {
                errors::CranelispError::Eval(EvalError::Undefined(
                    format!("Undefined symbol: {:?}", s),
                    span,
                ))
            })?
            .clone()
            .okay(),
        Atom::Quoted(i) => i.deref().clone().okay(),
        _ => atom.okay(),
    }
}
