use cranelift::codegen::ir::types; //, Function};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, ModuleError}; //DataContext
use somok::Somok;
use std::collections::HashMap;
//use std::slice;

use crate::function::Func;
use crate::parser::{Args, DefunExpr, Expr};
use crate::{libcl::*, CranelispError, Env, EvalError, Result};

pub struct Jit<'e> {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    // data_ctx: DataContext,
    module: JITModule,
    show_clir: bool,
    pub env: &'e mut Env,
}

macro_rules! defsym {
    // Named
    ($builder:expr; $env:expr; $arity:expr; $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        let id = $env.insert_symbol($sym);
        $env.insert_value(
            id,
            crate::Value::new_func(crate::function::Func::from_fn($fun as *const u8, $arity)),
        );
    };
    // Autoname
    ($builder:expr; $env:expr; $arity:expr; $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        let id = $env.insert_symbol(std::stringify!($fun).into());
        $env.insert_value(
            id,
            crate::Value::new_func(crate::function::Func::from_fn($fun as *const u8, $arity)),
        );
    };
}

impl<'e> Jit<'e> {
    pub fn new(env: &'e mut Env, show_clir: bool) -> Jit<'e> {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());
        builder.hotswap(true);

        defsym!(builder; env; 1; cl_print);
        defsym!(builder; env; usize::MAX; "+".to_string() => add);
        defsym!(builder; env; usize::MAX; "-".to_string() => sub);
        defsym!(builder; env; 2; "<".to_string() => less_than);
        defsym!(builder; env; 2; ">".to_string() => more_than);

        let module = JITModule::new(builder);
        Jit {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            //  data_ctx: DataContext::new(),
            module,
            show_clir,
            env,
        }
    }

    pub fn compile(&mut self, fun: DefunExpr) -> Result<*const u8> {
        self.module.clear_context(&mut self.ctx);
        let name = self.env.lookup_symbol(fun.name).unwrap().clone();
        let arity = match &fun.args {
            Args::Foldable => usize::MAX,
            Args::Arglist(a) => a.len(),
        };
        self.env.insert_value(
            fun.name,
            crate::Value::new_func(Func::from_fn(0 as _, arity)),
        );
        self.translate(fun)?;

        let id = if name == "_" {
            self.module
                .declare_anonymous_function(&self.ctx.func.signature)?
        } else {
            self.module
                .declare_function(&name, Linkage::Export, &self.ctx.func.signature)?
        };

        if let Err(ModuleError::DuplicateDefinition(..)) = self.module.define_function(
            id,
            &mut self.ctx,
            &mut codegen::binemit::NullTrapSink {},
            &mut codegen::binemit::NullStackMapSink {},
        ) {
            self.module.prepare_for_function_redefine(id)?;
            self.module.define_function(
                id,
                &mut self.ctx,
                &mut codegen::binemit::NullTrapSink {},
                &mut codegen::binemit::NullStackMapSink {},
            )?;
        }

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(id);
        Ok(code)
    }

    fn translate(&mut self, fun: DefunExpr) -> Result<()> {
        // Return slot
        self.ctx
            .func
            .signature
            .params
            .push(AbiParam::new(types::R64));
        match &fun.args {
            Args::Foldable => {
                self.ctx
                    .func
                    .signature
                    .params
                    .extend([AbiParam::new(types::R64), AbiParam::new(types::R64)]);
            }
            Args::Arglist(args) => {
                for _ in args {
                    self.ctx
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(types::R64))
                }
            }
        }

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let variables = declare_variables(&mut builder, &fun, entry_block, self.env);

        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
            env: self.env,
        };
        let mut memflags = MemFlags::new();
        memflags.set_aligned();

        let return_addr = trans.translate_expr(fun.body)?;
        let return_tag = trans
            .builder
            .ins()
            .load(types::I64, memflags, return_addr, 0);

        let return_value = trans
            .builder
            .ins()
            .load(types::I64, memflags, return_addr, 8);

        let n = trans.env.insert_symbol("ret".into());
        let ret_var = trans.builder.use_var(*trans.variables.get(&n).unwrap());

        // here we should write return_value to the return slot
        trans.builder.ins().store(memflags, return_tag, ret_var, 0);
        trans
            .builder
            .ins()
            .store(memflags, return_value, ret_var, 8);
        trans.builder.ins().return_(&[]);
        trans.builder.finalize();

        if self.show_clir {
            println!("{}", trans.builder.func.display());
        }

        Ok(())
    }
}

struct FunctionTranslator<'a, 'e> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<usize, Variable>,
    module: &'a mut JITModule,
    env: &'e mut Env,
}

impl<'a, 'e> FunctionTranslator<'a, 'e> {
    fn translate_expr(&mut self, expr: Expr) -> Result<Value> {
        match expr {
            Expr::Symbol(n, _) => {
                let var = self.variables.get(&n).expect("Undefined variable");
                self.builder.use_var(*var).okay()
                // let mut memflags = MemFlags::new();
                // memflags.set_aligned();
                // // tag
                // self.builder.ins().load(types::I64, memflags, addr, 0);
                // // value
                // self.builder.ins().load(types::I64, memflags, addr, 8);
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
                let mut list_ret = self.builder.ins().iconst(types::I64, 0);
                for expr in exprs {
                    list_ret = self.translate_expr(expr)?;
                }
                list_ret.okay()
            }
            Expr::Quoted(_, _) => todo!("dunno"),
            Expr::Defun(..) => todo!("dunno"),
            Expr::If(cond, truth, lie, _) => self.translate_if(*cond, *truth, *lie),
            Expr::Return(_, _) => {
                let n = self.env.insert_symbol("loop_cond".into());
                let cond_var = *self.variables.get(&n).unwrap();
                let truth = self.builder.ins().iconst(types::I64, 1);
                self.builder.def_var(cond_var, truth);
                self.builder.ins().f64const(0).okay()
            }
            Expr::Loop(expr, _) => self.translate_loop(*expr),
            Expr::Let(name, expr, _) => {
                self.translate_let(self.env.lookup_symbol(name).unwrap().clone(), *expr)
            }
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
            Expr::String(_, _) => todo!(),
        }
    }

    fn translate_loop(&mut self, expr: Expr) -> Result<Value> {
        let n = self.env.insert_symbol("loop_cond".into());
        let cond_var = *self.variables.get(&n).unwrap();
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
        self.builder.ins().f64const(0).okay()
    }

    fn translate_call(&mut self, name: usize, exprs: Vec<Expr>) -> Result<Value> {
        dbg! {&self.env};
        let signature = self.env.lookup_value(name).ok_or_else(|| {
            eprintln!("Undefined in call translation");
            CranelispError::Eval(EvalError::Undefined(
                self.env.lookup_symbol(name).unwrap().clone(),
                exprs.first().unwrap().span(),
            ))
        })?;

        let mut sig = self.module.make_signature();

        // Return slot
        sig.params.push(AbiParam::new(types::R64));
        let arity = unsafe { (*signature.as_func()).arity };
        for _ in 0..arity {
            sig.params.push(AbiParam::new(types::R64));
        }

        // Yanked straight from JIT demo
        let callee = self
            .module
            .declare_function(self.env.lookup_symbol(name).unwrap(), Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let mut arg_values = Vec::new();
        // allocate return slot
        let ret = self
            .builder
            .create_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 31));
        let ret = self.builder.ins().stack_addr(types::R64, ret, 0);
        arg_values.push(ret);
        for arg in exprs {
            arg_values.push(self.translate_expr(arg)?)
        }
        self.builder.ins().call(local_callee, &arg_values);
        ret.okay()
    }

    fn translate_let(&mut self, name: String, expr: Expr) -> Result<Value> {
        let val = self.translate_expr(expr)?;

        let n = self.env.insert_symbol(name);
        let variable = self.variables.get(&n).expect("Undefined");
        self.builder.def_var(*variable, val);
        val.okay()
    }

    fn translate_if(&mut self, cond: Expr, truth: Expr, lie: Expr) -> Result<Value> {
        let cond_val = self.translate_expr(cond)?;

        let truth_block = self.builder.create_block();
        let lie_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, types::F64);
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
    env: &mut Env,
) -> HashMap<usize, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;
    let val = builder.block_params(block)[0];
    let var = declare_variable(
        types::R64,
        builder,
        &mut variables,
        env,
        &mut index,
        "ret".into(),
    );
    builder.def_var(var, val);

    match &fun.args {
        Args::Foldable => {
            let val = builder.block_params(block)[1];
            let var = declare_variable(
                types::R64,
                builder,
                &mut variables,
                env,
                &mut index,
                "a".into(),
            );
            builder.def_var(var, val);

            let val = builder.block_params(block)[2];
            let var = declare_variable(
                types::R64,
                builder,
                &mut variables,
                env,
                &mut index,
                "b".into(),
            );
            builder.def_var(var, val);
        }
        Args::Arglist(args) => {
            for (i, name) in args.iter().enumerate() {
                let val = builder.block_params(block)[i + 1];
                let var = declare_variable(
                    types::R64,
                    builder,
                    &mut variables,
                    env,
                    &mut index,
                    name.into(),
                );
                builder.def_var(var, val);
            }
        }
    }

    declare_variables_in_expr(fun.body.clone(), builder, &mut variables, &mut index, env);
    variables
}

fn declare_variables_in_expr(
    expr: Expr,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<usize, Variable>,
    index: &mut usize,
    env: &mut Env,
) {
    match expr {
        Expr::Symbol(_, _) => (),
        Expr::Float(_, _) => (),
        Expr::List(exprs, _) => {
            for expr in exprs {
                declare_variables_in_expr(expr, builder, variables, index, env)
            }
        }
        Expr::Quoted(_, _) => (),
        Expr::Defun(..) => todo!("Not sure what to do"),
        Expr::If(cond, truth, lie, _) => {
            for expr in [*cond, *truth, *lie] {
                declare_variables_in_expr(expr, builder, variables, index, env)
            }
        }
        Expr::Return(Some(expr), _) => {
            declare_variables_in_expr(*expr, builder, variables, index, env)
        }
        Expr::Return(None, _) => (),
        Expr::Loop(expr, _) => {
            let zero = builder.ins().iconst(types::I64, 0);
            let return_variable = declare_variable(
                types::I64,
                builder,
                variables,
                env,
                index,
                "loop_cond".into(),
            );
            builder.def_var(return_variable, zero);
            declare_variables_in_expr(*expr, builder, variables, index, env)
        }
        Expr::Let(name, expr, _) => {
            let name = env.lookup_symbol(name).unwrap().clone();
            declare_variable(types::F64, builder, variables, env, index, name);
            declare_variables_in_expr(*expr, builder, variables, index, env)
        }
        Expr::Integer(_, _) => (),
        Expr::String(_, _) => todo!(),
    }
}

fn declare_variable(
    ty: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<usize, Variable>,
    env: &mut Env,
    index: &mut usize,
    name: String,
) -> Variable {
    let var = Variable::new(*index);
    let n = env.insert_symbol(name);
    if let std::collections::hash_map::Entry::Vacant(e) = variables.entry(n) {
        e.insert(var);
        builder.declare_var(var, ty);
        *index += 1;
    }
    var
}
