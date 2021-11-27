use cranelift::codegen::ir::{types, HeapData, HeapStyle, StackSlot}; //, Function};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, ModuleError};
use smol_str::SmolStr;
//DataContext
use somok::{Leaksome, Somok};
use std::collections::HashMap;
use std::ffi::CString;
use std::str::FromStr;
//use std::slice;

use crate::eval::value::{CLString, Symbol, Tag};
use crate::function::Func;
use crate::parser::{Args, DefunExpr, Expr};
use crate::{libcl::*, lookup_value, set_value, Atom, CranelispError, EvalError, Result};

pub struct Jit {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    // data_ctx: DataContext,
    module: JITModule,
    show_clir: bool,
}

macro_rules! defsym {
    // Named
    ($builder:expr; $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        crate::set_value(crate::eval::value::Symbol::new_with_atom(
            $sym,
            crate::Atom::new_func(crate::function::Func::from_fn($fun)),
        ));
    };
    // Autoname
    ($builder:expr; $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        $builder.symbol(stringify!($fun), $fun as *const u8);
        crate::set_value(crate::eval::value::Symbol::new_with_atom(
            stringify!($fun),
            crate::Atom::new_func(crate::function::Func::from_fn($fun)),
        ));
    };
}

impl Jit {
    pub fn new(show_clir: bool) -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());
        builder.hotswap(true);

        defsym!(builder; cl_print);
        defsym!(builder; cl_eprint);
        defsym!(builder; setf);
        defsym!(builder; "+".to_string() => add);
        defsym!(builder; "-".to_string() => sub);
        defsym!(builder; "<".to_string() => less_than);
        defsym!(builder; ">".to_string() => more_than);
        defsym!(builder; cl_alloc_value);
        defsym!(builder; car);
        defsym!(builder; cdr);

        let module = JITModule::new(builder);
        Jit {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            //  data_ctx: DataContext::new(),
            module,
            show_clir,
        }
    }

    pub fn compile(&mut self, fun: DefunExpr) -> Result<*const u8> {
        self.module.clear_context(&mut self.ctx);
        let name = fun.name.clone();

        set_value(Symbol {
            name,
            val: Atom::new_func(Func::from_fn(unsafe {
                #[allow(invalid_value)]
                // This is used as a marker
                std::mem::transmute(0usize)
            }))
            .boxed()
            .leak(),
        });
        let name = fun.name.clone();
        self.translate(fun)?;

        let id = if &name == "_" {
            self.module
                .declare_anonymous_function(&self.ctx.func.signature)?
        } else {
            self.module
                .declare_function(&name, Linkage::Export, &self.ctx.func.signature)?
        };

        match self.module.define_function(
            id,
            &mut self.ctx,
            &mut codegen::binemit::NullTrapSink {},
            &mut codegen::binemit::NullStackMapSink {},
        ) {
            Err(ModuleError::DuplicateDefinition(..)) => {
                self.module.prepare_for_function_redefine(id)?;
                self.module.define_function(
                    id,
                    &mut self.ctx,
                    &mut codegen::binemit::NullTrapSink {},
                    &mut codegen::binemit::NullStackMapSink {},
                )?;
            }
            Err(e) => return CranelispError::from(e).error(),
            _ => (),
        }

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(id);
        set_value(Symbol {
            name,
            val: Atom::new_func(Func::from_fn(unsafe { std::mem::transmute(code) }))
                .boxed()
                .leak(),
        });
        Ok(code)
    }

    fn translate(&mut self, fun: DefunExpr) -> Result<()> {
        self.ctx
            .func
            .signature
            .params
            .push(AbiParam::new(types::R64));

        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::R64));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let variables = declare_variables(&mut builder, &fun, entry_block);

        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
        };
        let mut memflags = MemFlags::new();
        memflags.set_aligned();

        let return_value = trans.translate_expr(fun.body)?;

        trans.builder.ins().return_(&[return_value]);

        trans.builder.finalize();
        if self.show_clir {
            println!("{}", trans.builder.func.display());
        }

        Ok(())
    }
}

fn store_atom(trans: &mut FunctionTranslator, src: Value, target: Value) -> Value {
    let mut memflags = MemFlags::new();
    memflags.set_aligned();

    let tag = trans.builder.ins().load(types::I64, memflags, src, 0);
    let value = trans.builder.ins().load(types::I64, memflags, src, 8);

    trans.builder.ins().store(memflags, tag, target, 0);
    trans.builder.ins().store(memflags, value, target, 8);
    target
}
fn store_new_atom(trans: &mut FunctionTranslator, target: Value, tag: Tag, value: i64) -> Value {
    let tag = tag as i64;

    let mut memflags = MemFlags::new();
    memflags.set_aligned();

    let tag = trans.builder.ins().iconst(types::I64, tag);
    let value = trans.builder.ins().iconst(types::I64, value);

    trans.builder.ins().store(memflags, tag, target, 0);
    trans.builder.ins().store(memflags, value, target, 8);
    target
}
fn alloc_heap_atom(trans: &mut FunctionTranslator) -> Result<Value> {
    let mut sig = trans.module.make_signature();
    sig.returns.push(AbiParam::new(types::R64));

    let callee = trans
        .module
        .declare_function("cl_alloc_value", Linkage::Import, &sig)?;
    let local_callee = trans
        .module
        .declare_func_in_func(callee, trans.builder.func);

    let call = trans.builder.ins().call(local_callee, &[]);
    trans.builder.inst_results(call)[0].okay()
}
fn alloc_stack_atom(trans: &mut FunctionTranslator) -> StackSlot {
    let slot = trans.builder.create_stack_slot(StackSlotData {
        kind: StackSlotKind::ExplicitSlot,
        size: 16,
    });
    let null = trans.builder.ins().iconst(types::I64, 0);

    trans.builder.ins().stack_store(null, slot, 0);
    trans.builder.ins().stack_store(null, slot, 8);
    slot
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<SmolStr, Variable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_expr(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Symbol(s, span) => {
                if &s == "nil" {
                    alloc_heap_atom(self)
                } else {
                    self.variables
                        .get(&s)
                        .map(|var| self.builder.use_var(*var))
                        .or_else(|| {
                            let var = lookup_value(s.clone())? as i64;
                            self.builder.ins().iconst(types::I64, var).some()
                        })
                        .ok_or_else(|| CranelispError::Eval(EvalError::Undefined(s.into(), span)))
                }
            }
            Expr::Float(n, _) => self.builder.ins().f64const(n).okay(),
            Expr::List(mut exprs, _) if matches!(exprs.first(), Some(Expr::Symbol(..))) => {
                if let Expr::Symbol(name, ..) = exprs.remove(0) {
                    self.translate_call(name, exprs)
                } else {
                    // SAFETY: We have just checked that first element exists
                    unreachable!()
                }
            }
            Expr::List(exprs, _) => {
                let mut list_ret = alloc_heap_atom(self)?;
                for expr in exprs {
                    list_ret = self.translate_expr(expr)?;
                }
                list_ret.okay()
            }
            Expr::Quoted(expr, _) => match *expr {
                Expr::Symbol(s, _) => {
                    let s = CLString::from_str(&s).unwrap().boxed().leak() as *mut _ as i64;
                    let a = alloc_heap_atom(self).unwrap();
                    store_new_atom(self, a, Tag::String, s).okay()
                }
                expr @ (Expr::Integer(_, _) | Expr::Float(_, _)) => self.translate_expr(expr),
                Expr::List(exprs, _) => {
                    let head = alloc_heap_atom(self)?;

                    if !exprs.is_empty() {
                        let mut memflags = MemFlags::new();
                        memflags.set_aligned();
                        let tag = self.builder.ins().iconst(types::I64, 4);
                        let pair = alloc_heap_atom(self)?;
                        self.builder.ins().store(memflags, tag, head, 0);
                        self.builder.ins().store(memflags, pair, head, 8);

                        let mut previous = pair;
                        if !exprs.is_empty() {
                            let vals = exprs
                                .into_iter()
                                .map(|e| self.translate_expr(e))
                                .collect::<Result<Vec<_>>>()?;

                            let car = alloc_heap_atom(self)?;
                            store_atom(self, vals[0], car);
                            self.builder.ins().store(memflags, car, pair, 0);

                            for &val in &vals[1..] {
                                let head = alloc_heap_atom(self)?;
                                let mut memflags = MemFlags::new();
                                memflags.set_aligned();
                                let tag = self.builder.ins().iconst(types::I64, 4);
                                let pair = alloc_heap_atom(self)?;
                                self.builder.ins().store(memflags, tag, head, 0);
                                self.builder.ins().store(memflags, pair, head, 8);

                                let car = alloc_heap_atom(self)?;
                                store_atom(self, val, car);
                                self.builder.ins().store(memflags, car, pair, 0);

                                self.builder.ins().store(memflags, head, previous, 8);

                                previous = pair;
                            }

                            // store nil into cdr of the last pair
                            let nil = alloc_heap_atom(self)?;
                            self.builder.ins().store(memflags, nil, previous, 8);
                        }
                    }
                    head.okay()
                }
                Expr::Quoted(_, _) => todo!(),
                Expr::Defun(_, _) => todo!(),
                Expr::If(_, _, _, _) => todo!(),
                Expr::Return(_, _) => todo!(),
                Expr::Loop(_, _) => todo!(),
                Expr::Let(_, _, _) => todo!(),
                Expr::String(_, _) => todo!(),
            },
            Expr::Defun(_d, _) => todo!("Should this be allowed?"),
            Expr::If(cond, truth, lie, _) => self.translate_if(*cond, *truth, *lie),
            Expr::Return(_, _) => {
                let cond_var = *self.variables.get("loop_cond").unwrap();
                let truth = self.builder.ins().iconst(types::I64, 1);
                self.builder.def_var(cond_var, truth);
                alloc_heap_atom(self)
            }
            Expr::Loop(expr, _) => self.translate_loop(*expr),
            Expr::Let(name, expr, _) => self.translate_let(name, *expr),
            Expr::Integer(int, _) => {
                let slot = self.builder.create_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 16,
                });
                let slot_addr = self.builder.ins().stack_addr(types::R64, slot, 0);
                let int = self.builder.ins().iconst(types::I64, int);
                let tag = self.builder.ins().iconst(types::I64, 1);
                let mut memflags = MemFlags::new();
                memflags.set_aligned();

                self.builder.ins().store(memflags, tag, slot_addr, 0);
                self.builder.ins().store(memflags, int, slot_addr, 8);
                slot_addr.okay()
            }
            Expr::String(s, _) => {
                let atom = alloc_stack_atom(self);
                let ptr = {
                    let ptr = CString::new(s).unwrap().into_raw() as i64;
                    self.builder.ins().iconst(types::I64, ptr)
                };
                let string_tag = self.builder.ins().iconst(types::I64, 7);

                self.builder.ins().stack_store(string_tag, atom, 0);
                self.builder.ins().stack_store(ptr, atom, 8);
                self.builder.ins().stack_addr(types::R64, atom, 0).okay()
            }
        }
    }

    fn translate_loop(&mut self, expr: Expr) -> Result<Value> {
        let cond_var = *self.variables.get("loop_cond").unwrap();
        let lie = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(cond_var, lie);

        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.builder.use_var(cond_var);
        self.builder.ins().brnz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        self.translate_expr(expr)?;

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);
        let lie = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(cond_var, lie);

        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);
        alloc_heap_atom(self)
    }

    fn translate_call(&mut self, name: SmolStr, exprs: Vec<Expr>) -> Result<Value> {
        let func = unsafe {
            *(*lookup_value(dbg! {name.clone()}).ok_or_else(|| {
                eprintln!("Undefined in call translation");
                CranelispError::Eval(EvalError::Undefined(
                    (&*name).to_owned(),
                    exprs.first().unwrap().span(),
                ))
            })?)
            .as_func()
        };

        // malloc
        let mut m_sig = self.module.make_signature();
        m_sig.params.push(AbiParam::new(types::I64));
        m_sig.returns.push(AbiParam::new(types::R64));
        let malloc = self
            .module
            .declare_function("malloc", Linkage::Import, &m_sig)?;
        let local_malloc = self.module.declare_func_in_func(malloc, self.builder.func);

        let mut sig = self.module.make_signature();

        sig.returns.push(AbiParam::new(types::R64));

        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::R64));

        #[allow(clippy::fn_to_numeric_cast)]
        if func.body as u64 != 0 {
            let sig = self.builder.func.import_signature(sig);
            let callee = self.builder.ins().iconst(types::I64, func.body as i64);
            let callee = self.builder.ins().raw_bitcast(types::R64, callee);

            let mut arg_values = Vec::new();

            let args = exprs
                .into_iter()
                .map(|a| self.translate_expr(a))
                .collect::<Result<Vec<_>>>()?;
            let count = self.builder.ins().iconst(types::I64, args.len() as i64);

            // call malloc
            let count_bytes = self
                .builder
                .ins()
                .iconst(types::I64, (args.len() * 8) as i64);
            let call = self.builder.ins().call(local_malloc, &[count_bytes]);
            let heap_addr = self.builder.inst_results(call)[0];

            let mut memflags = MemFlags::new();
            memflags.set_aligned();
            for (i, arg) in args.into_iter().enumerate() {
                self.builder
                    .ins()
                    .store(memflags, arg, heap_addr, (i * 16) as i32);
            }

            arg_values.push(count);
            arg_values.push(heap_addr);

            let call = self.builder.ins().call_indirect(sig, callee, &arg_values);
            self.builder.inst_results(call)[0].okay()
        } else {
            let callee = self.module.declare_function(&name, Linkage::Import, &sig)?;
            let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

            let mut arg_values = Vec::new();

            let args = exprs
                .into_iter()
                .map(|a| self.translate_expr(a))
                .collect::<Result<Vec<_>>>()?;
            let count = self.builder.ins().iconst(types::I64, args.len() as i64);

            // call malloc
            let count_bytes = self
                .builder
                .ins()
                .iconst(types::I64, (args.len() * 8) as i64);
            let call = self.builder.ins().call(local_malloc, &[count_bytes]);
            let heap_addr = self.builder.inst_results(call)[0];

            let mut memflags = MemFlags::new();
            memflags.set_aligned();
            for (i, arg) in args.into_iter().enumerate() {
                self.builder
                    .ins()
                    .store(memflags, arg, heap_addr, (i * 16) as i32);
            }

            arg_values.push(count);
            arg_values.push(heap_addr);

            let call = self.builder.ins().call(local_callee, &arg_values);
            self.builder.inst_results(call)[0].okay()
        }
    }

    fn translate_let(&mut self, id: SmolStr, expr: Expr) -> Result<Value> {
        let val = self.translate_expr(expr)?;
        let variable = self.variables.get(&id).expect("Undefined");
        self.builder.def_var(*variable, val);
        val.okay()
    }

    fn translate_if(&mut self, cond: Expr, truth: Expr, lie: Expr) -> Result<Value> {
        let cond_val = self.translate_expr(cond)?;

        let truth_block = self.builder.create_block();
        let lie_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, types::R64);
        let mut memflags = MemFlags::new();
        memflags.set_aligned();
        let cond_val = self.builder.ins().load(types::I64, memflags, cond_val, 8);
        self.builder.ins().brz(cond_val, lie_block, &[]);
        self.builder.ins().jump(truth_block, &[]);

        self.builder.switch_to_block(truth_block);
        self.builder.seal_block(truth_block);
        let truth_return = self.translate_expr(truth)?;
        self.builder.ins().jump(merge_block, &[truth_return]);

        self.builder.switch_to_block(lie_block);
        self.builder.seal_block(lie_block);
        let lie_return = self.translate_expr(lie)?;
        self.builder.ins().jump(merge_block, &[lie_return]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        let phi = self.builder.block_params(merge_block)[0];
        phi.okay()
    }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    fun: &DefunExpr,
    block: Block,
) -> HashMap<SmolStr, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    match &fun.args {
        Args::Foldable => {
            unreachable!()
        }
        Args::Arglist(args) => {
            for (i, name) in args.iter().enumerate() {
                let val = builder.block_params(block)[i];
                let var = declare_variable(
                    types::R64,
                    builder,
                    &mut variables,
                    &mut index,
                    name.clone(),
                );
                builder.def_var(var, val);
            }
        }
    }

    declare_variables_in_expr(fun.body.clone(), builder, &mut variables, &mut index);
    variables
}

fn declare_variables_in_expr(
    expr: Expr,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<SmolStr, Variable>,
    index: &mut usize,
) {
    match expr {
        Expr::Symbol(_, _) => (),
        Expr::Float(_, _) => (),
        Expr::List(exprs, _) => {
            for expr in exprs {
                declare_variables_in_expr(expr, builder, variables, index)
            }
        }
        Expr::Quoted(_, _) => (),
        Expr::Defun(..) => todo!("Not sure what to do"),
        Expr::If(cond, truth, lie, _) => {
            for expr in [*cond, *truth, *lie] {
                declare_variables_in_expr(expr, builder, variables, index)
            }
        }
        Expr::Return(Some(expr), _) => declare_variables_in_expr(*expr, builder, variables, index),
        Expr::Return(None, _) => (),
        Expr::Loop(expr, _) => {
            let zero = builder.ins().iconst(types::I64, 0);
            let return_variable =
                declare_variable(types::I64, builder, variables, index, "loop_cond".into());
            builder.def_var(return_variable, zero);
            declare_variables_in_expr(*expr, builder, variables, index)
        }
        Expr::Let(name, expr, _) => {
            declare_variable(types::R64, builder, variables, index, name);
            declare_variables_in_expr(*expr, builder, variables, index)
        }
        Expr::Integer(_, _) => (),
        Expr::String(_, _) => (),
    }
}

fn declare_variable(
    ty: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<SmolStr, Variable>,
    index: &mut usize,
    symbol: SmolStr,
) -> Variable {
    let var = Variable::new(*index);
    if let std::collections::hash_map::Entry::Vacant(e) = variables.entry(symbol) {
        e.insert(var);
        builder.declare_var(var, ty);
        *index += 1;
    }
    var
}
