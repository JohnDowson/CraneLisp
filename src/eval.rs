use somok::Somok;
pub mod value;
use crate::{function::Func, jit::Jit, parser::Expr, CranelispError, EvalError, Result, TryRemove};
pub use value::Atom;

pub fn eval(expr: Expr, jit: &mut Jit) -> Result<Atom> {
    match expr {
        Expr::String(_s, _) => Atom::new_string(_s).okay(),
        ref expr @ Expr::Symbol(s, _) => jit.env.env.get(&s).cloned().ok_or_else(|| {
            eprintln!("Undefined in eval translation");
            CranelispError::Eval(EvalError::Undefined(
                (&*jit.env.lookup_symbol(s).unwrap()).to_owned(),
                expr.span(),
            ))
        }),
        Expr::Float(val, _) => Atom::new_float(val).okay(),
        Expr::Integer(val, _) => Atom::new_int(val).okay(),
        Expr::List(mut list, _) => {
            let value_store;
            let fun = *match list.try_remove(0) {
                Some(Expr::Symbol(s, _)) => jit.env.env.get(&s).unwrap(),
                Some(expr @ Expr::Defun(..)) => {
                    value_store = eval(expr, jit)?;
                    &value_store
                }
                Some(expr @ Expr::If(..)) => return eval(expr, jit),
                Some(Expr::Float(_val, _)) => todo!(),
                Some(expr @ Expr::List(..)) => {
                    let mut last = eval(expr, jit);
                    for expr in list {
                        last = eval(expr, jit);
                    }
                    return last;
                }
                Some(Expr::Let(sym, expr, _)) => {
                    let v = eval(*expr, jit)?;
                    jit.env.env.insert(sym, v);
                    return Atom::NULL.okay();
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
        Expr::Quoted(_, _) => todo!("Deal vith evaluating things that shouldn't be evaluated"),
        Expr::Defun(defun_expr, _span) => {
            let name = defun_expr.name;
            // jit
            let func = Func::jit(jit, *defun_expr)?;
            // store
            if &*jit.env.lookup_symbol(name).expect("Symbol is not interned") != "_" {
                jit.env.env.insert(name, Atom::new_func(func));
            };
            Atom::new_func(func).okay()
        }
        Expr::Let(sym, expr, _) => {
            let val = eval(*expr, jit)?;
            jit.env.env.insert(sym, val);
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
