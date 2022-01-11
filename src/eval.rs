use crate::{
    env::{fork_env, get_env, unfork_env, EnvId},
    errors,
    function::Func,
    mem,
    value::{map, map_to_vec, Atom, SymId},
    CranelispError, EvalError, Result, Span,
};
use somok::{Leaksome, Somok};
use std::ops::Deref;

pub fn eval((atom, span): (Atom, Span), env: EnvId) -> Result<Atom> {
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
                        get_env(env).insert(*sym, atom.clone());
                        return atom.okay();
                    }
                    "progn" => {
                        return map_to_vec(p.cdr, |a| eval((a.clone(), span), env))
                            .into_iter()
                            .collect::<Result<Vec<Atom>>>()?
                            .pop()
                            .unwrap_or(Atom::Null)
                            .okay();
                    }
                    "if" => {
                        return if !matches!(
                            eval((p.cdr.as_pair().unwrap().car.deref().clone(), span), env)?,
                            Atom::Null,
                        ) {
                            eval(
                                (
                                    p.cdr
                                        .as_pair()
                                        .unwrap()
                                        .cdr
                                        .as_pair()
                                        .unwrap()
                                        .car
                                        .deref()
                                        .clone(),
                                    span,
                                ),
                                env,
                            )
                        } else {
                            eval(
                                (
                                    p.cdr
                                        .as_pair()
                                        .unwrap()
                                        .cdr
                                        .as_pair()
                                        .unwrap()
                                        .cdr
                                        .as_pair()
                                        .unwrap()
                                        .car
                                        .deref()
                                        .clone(),
                                    span,
                                ),
                                env,
                            )
                        }
                    }
                    _ => get_env(env)
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
            let subenv = fork_env(env);
            let res = func.as_func().unwrap().call(mem::alloc(args), subenv);
            unfork_env(subenv);
            res.okay()
        }
        Atom::Return(_) => todo!(),
        Atom::Symbol(s) => get_env(env)
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
