use std::str::FromStr;

use somok::Somok;
pub mod value;
use crate::{
    eval::value::{list, CLString, Symbol},
    function::Func,
    jit::Jit,
    lookup_value,
    parser::Expr,
    set_value, CranelispError, EvalError, Result, TryRemove,
};
pub use value::Atom;

pub fn eval(expr: Expr, jit: &mut Jit) -> Result<Atom> {
    match expr {
        Expr::String(s, _) => Atom::new_string(CLString::from_string(s)).okay(),
        Expr::Symbol(s, span) => unsafe {
            (*lookup_value(s.clone()).ok_or_else(|| {
                eprintln!("Undefined in eval translation");
                CranelispError::Eval(EvalError::Undefined(s.into(), span))
            })?)
            .okay()
        },
        Expr::Float(val, _) => Atom::new_float(val).okay(),
        Expr::Integer(val, _) => Atom::new_int(val).okay(),
        Expr::List(mut list, _) => {
            let fun = match list.try_remove(0) {
                Some(Expr::Symbol(s, span)) => unsafe {
                    *lookup_value(s.clone()).ok_or_else(|| {
                        eprintln!("Undefined in eval translation");
                        CranelispError::Eval(EvalError::Undefined(s.into(), span))
                    })?
                },
                Some(expr @ Expr::Defun(..)) => eval(expr, jit)?,
                Some(expr @ Expr::If(..)) => return eval(expr, jit),
                Some(Expr::Float(_val, _)) => todo!(),
                Some(expr @ Expr::List(..)) => {
                    let mut last = eval(expr, jit);
                    for expr in list {
                        last = eval(expr, jit);
                    }
                    return last;
                }
                Some(expr @ Expr::Let(_, _, _)) => {
                    return eval(expr, jit);
                }
                Some(expr @ Expr::Return(..)) => return eval(expr, jit),
                Some(expr @ Expr::Loop(..)) => return eval(expr, jit),
                None => todo!("empty list"),
                e => todo!("Error: unquoted list that isn't application\n{:#?}", e),
            };
            let values = list
                .into_iter()
                .map(|e| eval(e, jit))
                .collect::<Result<Vec<_>>>()?;
            unsafe { (*fun.as_func()).call(values) }
        }
        Expr::Quoted(expr, _) => match *expr {
            Expr::Symbol(s, _) => Atom::new_string(CLString::from_str(&s).unwrap()).okay(),
            Expr::Float(_, _) => todo!(),
            Expr::Integer(_, _) => todo!(),
            Expr::List(exprs, _) => {
                let items = exprs
                    .into_iter()
                    .map(|e| eval(e, jit))
                    .collect::<Result<Vec<_>>>()?;
                list(items).okay()
            }
            Expr::Quoted(_, _) => todo!(),
            Expr::Defun(_, _) => todo!(),
            Expr::If(_, _, _, _) => todo!(),
            Expr::Return(_, _) => todo!(),
            Expr::Loop(_, _) => todo!(),
            Expr::Let(_, _, _) => todo!(),
            Expr::String(_, _) => todo!(),
        },
        Expr::Defun(defun_expr, _span) => {
            let name = defun_expr.name.clone();
            // jit
            let func = Func::jit(jit, *defun_expr)?;
            // store
            if &*name != "_" {
                set_value(Symbol::new_with_atom(name, Atom::new_func(func)));
            };
            Atom::new_func(func).okay()
        }
        Expr::Let(sym, expr, _) => {
            let val = eval(*expr, jit)?;
            set_value(Symbol::new_with_atom(sym, val));
            val.okay()
        }
        Expr::If(cond, truth, lie, _) => {
            if eval(*cond, jit)?.as_bool() {
                eval(*truth, jit)
            } else {
                eval(*lie, jit)
            }
        }
        Expr::Return(expr, ..) => {
            if let Some(e) = expr {
                Atom::new_return(eval(*e, jit)?).okay()
            } else {
                Atom::new_return(Atom::NULL).okay()
            }
        }
        Expr::Loop(body, ..) => {
            let mut last_val = Atom::NULL;
            while !matches!(last_val.tag, value::Tag::Return) {
                last_val = eval(*body.clone(), jit)?;
            }
            unsafe { *last_val.as_ptr::<Atom>() }.okay()
        }
    }
}
