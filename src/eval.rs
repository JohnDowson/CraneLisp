use somok::Somok;
pub mod value;
use crate::{function::Func, jit::Jit, parser::Expr, CranelispError, EvalError, Result, TryRemove};
pub use value::Value;

pub fn eval(expr: Expr, jit: &mut Jit) -> Result<Value> {
    match expr {
        Expr::String(_s, _) => todo!(),
        ref expr @ Expr::Symbol(s, _) => jit.env.env.get(&s).cloned().ok_or_else(|| {
            CranelispError::Eval(EvalError::Undefined(
                jit.env.lookup_symbol(s).unwrap().clone(),
                expr.span(),
            ))
        }),
        Expr::Float(val, _) => Value::new_float(val).okay(),
        Expr::Integer(val, _) => Value::new_int(val).okay(),
        Expr::List(mut list, _) => {
            let value_store;
            let fun = *match list.try_remove(0) {
                Some(Expr::Symbol(s, _)) => jit.env.env.get(&s).unwrap(),
                Some(expr @ Expr::Defun(..)) => {
                    value_store = eval(expr, jit)?;
                    &value_store
                }
                Some(Expr::If(_cond, _truth, _lie, _)) => {
                    todo!()
                }
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
                    return Value::NULL.okay();
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
            if jit.env.lookup_symbol(name).expect("Symbol is not interned") != "_" {
                jit.env.env.insert(name, Value::new_func(func.clone()));
            };
            Value::new_func(func).okay()
        }
        Expr::Let(sym, expr, _) => {
            let val = eval(*expr, jit)?;
            jit.env.env.insert(sym, val);
            val.okay()
        }
        Expr::If(_cond, _truth, _lie, _) => {
            todo!()
        }
        Expr::Return(_expr, ..) => {
            todo!()
        }
        Expr::Loop(_body, ..) => {
            todo!()
        }
    }
}
