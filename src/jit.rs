use cranelift::codegen::ir::types; //, Function};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, ModuleError}; //DataContext
use fnv::FnvHashMap;
use std::collections::HashMap;
//use std::slice;

use crate::parser::{Args, DefunExpr, Expr};
use crate::{libcl::*, Env, Result};

pub struct Jit<'e> {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    // data_ctx: DataContext,
    module: JITModule,
    fun_lookup: FnvHashMap<usize, Args>,
    show_clir: bool,
    pub env: &'e mut Env,
}

pub struct JitBuilder {
    fun_lookup: FnvHashMap<usize, Args>,
    pub show_clir: bool,
}

macro_rules! defsym {
    // Named
    ($builder:expr; $env:expr;  $fun_lookup:expr; BINARY $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        let id = $env.insert_symbol($sym);
        $fun_lookup.insert(id, Args::Arglist(vec!["a".into(), "b".into()]));
    };
    ($builder:expr; $env:expr; $fun_lookup:expr; VARARG $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        let id = $env.insert_symbol($sym);
        $fun_lookup.insert(id, Args::Foldable);
    };
    ($builder:expr; $env:expr; $fun_lookup:expr; UNARY $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        let id = $env.insert_symbol($sym);
        $fun_lookup.insert(id, Args::Arglist(vec!["a".into()]));
    };
    // Autoname
    ($builder:expr; $env:expr; $fun_lookup:expr; BINARY $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        let id = $env.insert_symbol(std::stringify!($fun).into());
        $fun_lookup.insert(id, Args::Arglist(vec!["a".into(), "b".into()]));
    };
    ($builder:expr; $env:expr; $fun_lookup:expr; VARARG $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        let id = $env.insert_symbol(std::stringify!($fun).into());
        $fun_lookup.insert(id, Args::Foldable);
    };
    ($builder:expr; $env:expr; $fun_lookup:expr; UNARY $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        let id = $env.insert_symbol(std::stringify!($fun).into());
        $fun_lookup.insert(id, Args::Arglist(vec!["a".into()]));
    };
}

impl<'e> JitBuilder {
    pub fn finish(&self, env: &'e mut Env) -> Jit<'e> {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let mut fun_lookup = self.fun_lookup.clone();
        builder.hotswap(true);

        defsym!(builder; env; fun_lookup; VARARG "+".to_string() => add);
        defsym!(builder; env; fun_lookup; VARARG "-".to_string() => sub);

        let module = JITModule::new(builder);
        Jit {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            //  data_ctx: DataContext::new(),
            module,
            fun_lookup,
            show_clir: self.show_clir,
            env,
        }
    }
}

impl<'e> Jit<'e> {
    pub fn build() -> JitBuilder {
        let fun_lookup = FnvHashMap::default();
        JitBuilder {
            fun_lookup,
            show_clir: false,
        }
    }

    pub fn compile(&mut self, fun: DefunExpr) -> Result<*const u8> {
        self.module.clear_context(&mut self.ctx);
        let name = self.env.lookup_symbol(fun.name).unwrap().clone();
        let sig = fun.args.clone();
        self.fun_lookup.insert(fun.name, sig);
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
            fun_lookup: &self.fun_lookup,
            env: self.env,
        };

        let return_value = trans.translate_expr(fun.body);
        let tag = trans.builder.ins().iconst(types::I64, 1);
        let return_slot = trans
            .builder
            .use_var(*trans.variables.get("ret").expect("Undefined variable"));
        let mut memflags = MemFlags::new();
        memflags.set_aligned();
        // here we should write return_value to the return slot
        trans.builder.ins().store(memflags, return_slot, tag, 0);
        trans
            .builder
            .ins()
            .store(memflags, return_slot, return_value, 8);
        trans.builder.ins().return_(&[]);
        trans.builder.finalize();

        if self.show_clir {
            println!("{}", trans.builder.func.display());
        }

        Ok(())
    }
}

struct FunctionTranslator<'a, 'f, 'e> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
    fun_lookup: &'f FnvHashMap<usize, Args>,
    env: &'e mut Env,
}

impl<'a, 'f, 'e> FunctionTranslator<'a, 'f, 'e> {
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Symbol(n, _) => {
                let var = self
                    .variables
                    .get(self.env.lookup_symbol(n).unwrap())
                    .expect("Undefined variable");
                self.builder.use_var(*var)
            }
            Expr::Float(n, _) => self.builder.ins().f64const(n),
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
                    list_ret = self.translate_expr(expr);
                }
                list_ret
            }
            Expr::Quoted(_, _) => todo!("dunno"),
            Expr::Defun(..) => todo!("dunno"),
            Expr::If(cond, truth, lie, _) => self.translate_if(*cond, *truth, *lie),
            Expr::Return(_, _) => {
                let cond_var = *self.variables.get("loop_cond").unwrap();
                let truth = self.builder.ins().iconst(types::I64, 1);
                self.builder.def_var(cond_var, truth);
                self.builder.ins().f64const(0)
            }
            Expr::Loop(expr, _) => self.translate_loop(*expr),
            Expr::Let(name, expr, _) => {
                self.translate_let(self.env.lookup_symbol(name).unwrap().clone(), *expr)
            }
            Expr::Integer(int, _) => self.builder.ins().iconst(types::I64, int),
            Expr::String(_, _) => todo!(),
        }
    }

    fn translate_loop(&mut self, expr: Expr) -> Value {
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

        self.translate_expr(expr);

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);
        let lie = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(cond_var, lie);

        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);
        self.builder.ins().f64const(0)
    }

    fn translate_call(&mut self, name: usize, exprs: Vec<Expr>) -> Value {
        // TODO this should be an ERROR!
        let signature = self.fun_lookup.get(&name).expect("Undefined function");

        let mut sig = self.module.make_signature();

        // Return slot
        sig.params.push(AbiParam::new(types::R64));
        match signature {
            Args::Foldable => {
                sig.params.push(AbiParam::new(types::R64));
                sig.params.push(AbiParam::new(types::R64));
            }
            Args::Arglist(args) => {
                for _ in args {
                    sig.params.push(AbiParam::new(types::R64));
                }
            }
        }

        // Yanked straight from JIT demo
        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(self.env.lookup_symbol(name).unwrap(), Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let mut arg_values = Vec::new();
        // allocate return slot
        let ret = self.builder.ins().null(types::R64);
        arg_values.push(ret);

        for _arg in exprs {
            let ret = self.builder.ins().null(types::R64);
            arg_values.push(ret);
            //arg_values.push(self.translate_expr(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call);
        self.builder.ins().iconst(types::I64, 0)
    }

    fn translate_let(&mut self, name: String, expr: Expr) -> Value {
        let val = self.translate_expr(expr);
        let variable = self.variables.get(&name).expect("Undefined");
        self.builder.def_var(*variable, val);
        val
    }

    fn translate_if(&mut self, cond: Expr, truth: Expr, lie: Expr) -> Value {
        let cond_val = self.translate_expr(cond);

        let truth_block = self.builder.create_block();
        let lie_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(merge_block, types::F64);
        let cond_val = self.builder.ins().fcvt_to_sint(types::I64, cond_val);
        self.builder.ins().brz(cond_val, lie_block, &[]);
        self.builder.ins().jump(truth_block, &[]);

        self.builder.switch_to_block(truth_block);
        self.builder.seal_block(truth_block);
        let truth_return = self.translate_expr(truth);
        self.builder.ins().jump(merge_block, &[truth_return]);

        self.builder.switch_to_block(lie_block);
        self.builder.seal_block(lie_block);
        let lie_return = self.translate_expr(lie);
        self.builder.ins().jump(merge_block, &[lie_return]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        let phi = self.builder.block_params(merge_block)[0];
        phi
    }
}

fn declare_variables(
    builder: &mut FunctionBuilder,
    fun: &DefunExpr,
    block: Block,
    env: &Env,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;
    let val = builder.block_params(block)[0];
    let var = declare_variable(types::R64, builder, &mut variables, &mut index, "ret");
    builder.def_var(var, val);

    match &fun.args {
        Args::Foldable => {
            let val = builder.block_params(block)[1];
            let var = declare_variable(types::R64, builder, &mut variables, &mut index, "a");
            builder.def_var(var, val);

            let val = builder.block_params(block)[2];
            let var = declare_variable(types::R64, builder, &mut variables, &mut index, "b");
            builder.def_var(var, val);
        }
        Args::Arglist(args) => {
            for (i, name) in args.iter().enumerate() {
                let val = builder.block_params(block)[i];
                let var = declare_variable(types::R64, builder, &mut variables, &mut index, name);
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
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    env: &Env,
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
            let return_variable =
                declare_variable(types::I64, builder, variables, index, "loop_cond");
            builder.def_var(return_variable, zero);
            declare_variables_in_expr(*expr, builder, variables, index, env)
        }
        Expr::Let(name, expr, _) => {
            declare_variable(
                types::F64,
                builder,
                variables,
                index,
                env.lookup_symbol(name).unwrap(),
            );
            declare_variables_in_expr(*expr, builder, variables, index, env)
        }
        Expr::Integer(_, _) => (),
        Expr::String(_, _) => todo!(),
    }
}

fn declare_variable(
    ty: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    let var = Variable::new(*index);
    if !variables.contains_key(name) {
        variables.insert(name.into(), var);
        builder.declare_var(var, ty);
        *index += 1;
    }
    var
}
