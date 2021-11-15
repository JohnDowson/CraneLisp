use somok::Somok;
mod value;
use crate::{
    function::{FnArgs, FnBody, Function, Signature},
    jit::Jit,
    parser::Expr,
    CranelispError, Env, EvalError, Result,
};
pub use value::Value;

pub fn eval(expr: Expr, env: &mut Env, jit: &mut Jit) -> Result<Value> {
    match expr {
        Expr::String(s, _) => Value::String(s).okay(),
        ref expr @ Expr::Symbol(ref s, _) => env
            .get(s)
            .cloned()
            .ok_or_else(|| CranelispError::Eval(EvalError::Undefined(s.clone(), expr.span()))),
        Expr::Float(val, _) => Value::Float(val).okay(),
        Expr::Integer(val, _) => Value::Integer(val).okay(),
        Expr::List(mut list, _) => {
            let value_store;
            let fun = match list.remove(0) {
                Expr::Symbol(s, _) => env.get(&s).unwrap(),
                expr @ Expr::Defun(..) => {
                    value_store = eval(expr, env, jit)?;
                    &value_store
                }
                Expr::If(cond, truth, lie, _) => {
                    let cond = eval(*cond, env, jit)?;
                    return if cond > Value::Float(0.0) {
                        eval(*truth, env, jit)
                    } else {
                        eval(*lie, env, jit)
                    };
                }
                Expr::Float(val, _) => return Value::Float(val).okay(),
                expr @ Expr::List(..) => {
                    let mut last = eval(expr, env, jit);
                    for expr in list {
                        last = eval(expr, env, jit);
                    }
                    return last;
                }
                Expr::Let(sym, expr, _) => {
                    let v = eval(*expr, env, jit)?;
                    env.insert(sym, v);
                    return Value::None.okay();
                }
                expr @ Expr::Break(..) => return eval(expr, env, jit),
                expr @ Expr::Loop(..) => return eval(expr, env, jit),
                e => todo!("Error: unquoted list that isn't application\n{:#?}", e),
            }
            .clone();
            let values = list
                .into_iter()
                .map(|e| eval(e, env, jit))
                .collect::<Result<Vec<_>>>()?;
            fun.unwrap_fn().call(values)
        }
        Expr::Quoted(_, _) => todo!("Deal vith evaluating things that shouldn't be evaluated"),
        Expr::Defun(name, args, body, ret, _) => {
            let sig = match args {
                FnArgs::Arglist(args) => Signature::build_from_arglist(args)
                    .set_name(name.clone())
                    .set_ret(ret)
                    .finish()?,
                FnArgs::Foldable(ty) => Signature::build()
                    .set_foldable(ty)
                    .set_name(name.clone())
                    .set_ret(ret)
                    .finish()?,
            };
            let func = Function::new(sig, FnBody::Virtual(*body)).jit(jit)?;
            env.insert(name, Value::Func(func.clone()));
            Value::Func(func).okay()
        }
        Expr::Let(_sym, expr, _) => eval(*expr, env, jit),
        Expr::If(cond, truth, lie, _) => {
            let cond = eval(*cond, env, jit)?;
            if cond > Value::Float(0.0) {
                eval(*truth, env, jit)
            } else {
                eval(*lie, env, jit)
            }
        }
        Expr::Break(expr, ..) => {
            if let Some(expr) = expr {
                Value::Return(Box::new(eval(*expr, env, jit)?)).okay()
            } else {
                Value::Return(Box::new(Value::None)).okay()
            }
        }
        Expr::Loop(body, ..) => {
            let mut previous_value = Value::None;
            loop {
                let value = eval(*body.clone(), env, jit)?;
                if value.is_return() {
                    if value.is_valued_return() {
                        return value.return_inner().okay();
                    }
                    return previous_value.okay();
                }
                previous_value = value;
            }
        }
    }
}
