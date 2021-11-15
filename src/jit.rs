use cranelift::codegen::ir::types; //, Function};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module}; //DataContext
use fnv::FnvHashMap;
use std::collections::HashMap;
//use std::slice;

use crate::parser::Expr;
use crate::{libcl::*, Result};

pub struct Jit {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    // data_ctx: DataContext,
    module: JITModule,
    fun_lookup: FnvHashMap<String, crate::function::Signature>,
    pub show_clir: bool,
}

macro_rules! defsym {
    // Named
    ($builder:expr; $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
    };
    ($builder:expr; $fun_lookup:expr; BINARY $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        $fun_lookup.insert(
            $sym.into(),
            crate::function::Signature::build()
                .set_name(std::stringify!($sym).into())
                .push_arg(("a".into(), crate::function::Type::Float))
                .push_arg(("b".into(), crate::function::Type::Float))
                .set_ret(crate::function::Type::Float)
                .finish()
                .unwrap(),
        );
    };
    ($builder:expr; $fun_lookup:expr; VARARG $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        $fun_lookup.insert(
            $sym.into(),
            crate::function::Signature::build()
                .set_name(std::stringify!($sym).into())
                .set_foldable(crate::function::Type::Float)
                .set_ret(crate::function::Type::Float)
                .finish()
                .unwrap(),
        );
    };
    ($builder:expr; $fun_lookup:expr; UNARY $sym:expr => $fun:path) => {
        $builder.symbol($sym, $fun as *const u8);
        $fun_lookup.insert(
            $sym.into(),
            crate::function::Signature::build()
                .set_name(std::stringify!($sym).into())
                .push_arg(("a".into(), crate::function::Type::Float))
                .set_ret(crate::function::Type::Number)
                .finish()
                .unwrap(),
        );
    };
    // Autoname
    ($builder:expr; $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
    };
    ($builder:expr; $fun_lookup:expr; BINARY $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        $fun_lookup.insert(
            std::stringify!($fun).into(),
            crate::function::Signature::build()
                .set_name(std::stringify!($fun).into())
                .push_arg(("a".into(), crate::function::Type::Number))
                .push_arg(("b".into(), crate::function::Type::Number))
                .set_ret(crate::function::Type::Number)
                .finish()
                .unwrap(),
        );
    };
    ($builder:expr; $fun_lookup:expr; VARARG $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        $fun_lookup.insert(
            std::stringify!($fun).into(),
            crate::function::Signature::build()
                .set_name(std::stringify!($fun).into())
                .set_foldable(true)
                .set_ret(crate::function::Type::Number)
                .finish()
                .unwrap(),
        );
    };
    ($builder:expr; $fun_lookup:expr; UNARY $fun:path) => {
        $builder.symbol(std::stringify!($fun), $fun as *const u8);
        $fun_lookup.insert(
            std::stringify!($fun).into(),
            crate::function::Signature::build()
                .set_name(std::stringify!($fun).into())
                .push_arg(("a".into(), crate::function::Type::Float))
                .set_ret(crate::function::Type::Float)
                .finish()
                .unwrap(),
        );
    };
}

impl Default for Jit {
    fn default() -> Self {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let mut fun_lookup = FnvHashMap::default();
        defsym!(builder; fun_lookup; UNARY cl_print);
        defsym!(builder; fun_lookup; VARARG "+" => plus);
        defsym!(builder; fun_lookup; VARARG "-" => minus);
        defsym!(builder; fun_lookup; BINARY "<" => less_than);
        defsym!(builder; fun_lookup; BINARY ">" => more_than);
        let module = JITModule::new(builder);
        Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            //  data_ctx: DataContext::new(),
            module,
            fun_lookup,
            show_clir: false,
        }
    }
}

impl Jit {
    pub fn compile(&mut self, fun: crate::function::Function) -> Result<*const u8> {
        self.module.clear_context(&mut self.ctx);
        let name = fun.name();
        let sig = fun.signature();
        self.fun_lookup.insert(name.clone(), sig);
        self.translate(fun)?;

        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)?;

        self.module.define_function(
            id,
            &mut self.ctx,
            &mut codegen::binemit::NullTrapSink {},
            &mut codegen::binemit::NullStackMapSink {},
        )?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(id);
        Ok(code)
    }

    fn translate(&mut self, fun: crate::function::Function) -> Result<()> {
        if fun.foldable() {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(types::F64));
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(types::F64))
        } else {
            for _p in fun.args() {
                self.ctx
                    .func
                    .signature
                    .params
                    .push(AbiParam::new(types::F64))
            }
        }
        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::F64));

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
            fun_lookup: &self.fun_lookup,
        };

        let return_value = trans.translate_expr(fun.body());
        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();

        if self.show_clir {
            println!("{}", trans.builder.func.display());
        }

        Ok(())
    }
}

struct FunctionTranslator<'a, 'e> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
    fun_lookup: &'e FnvHashMap<String, crate::function::Signature>,
}

impl<'a, 'e> FunctionTranslator<'a, 'e> {
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Symbol(n, _) => {
                let var = self.variables.get(&n).expect("Undefined variable");
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
                let mut list_ret = self.builder.ins().f64const(0);
                for expr in exprs {
                    list_ret = self.translate_expr(expr);
                }
                list_ret
            }
            Expr::Quoted(_, _) => todo!("dunno"),
            Expr::Defun(_, _, _, _, _) => todo!("dunno"),
            Expr::If(cond, truth, lie, _) => self.translate_if(*cond, *truth, *lie),
            Expr::Break(_, _) => todo!("Why did i call break return? Its confusing af"),
            Expr::Loop(_, _) => todo!(),
            Expr::Let(name, expr, _) => self.translate_let(name, *expr),
            _ => todo!(),
        }
    }

    fn _translate_loop(&mut self, _name: String, _expr: Expr) -> Value {
        todo!()
    }

    fn translate_call(&mut self, name: String, exprs: Vec<Expr>) -> Value {
        // TODO this should be an ERROR!
        let signature = self.fun_lookup.get(&name).expect("Undefined function");

        let mut sig = self.module.make_signature();
        if signature.foldable() {
            sig.params.push(AbiParam::new(types::F64));
            sig.params.push(AbiParam::new(types::F64));
        } else {
            for _arg in &signature.args() {
                sig.params.push(AbiParam::new(types::F64));
            }
        }
        sig.returns.push(AbiParam::new(types::F64));

        // Yanked straight from JIT demo
        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self
            .module
            .declare_func_in_func(callee, &mut self.builder.func);

        let mut arg_values = Vec::new();
        for arg in exprs {
            arg_values.push(self.translate_expr(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
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
    fun: &crate::function::Function,
    block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    if fun.foldable() {
        let val = builder.block_params(block)[0];
        let var = declare_variable(types::F64, builder, &mut variables, &mut index, "a");
        builder.def_var(var, val);

        let val = builder.block_params(block)[1];
        let var = declare_variable(types::F64, builder, &mut variables, &mut index, "b");
        builder.def_var(var, val);
    } else {
        for (i, (name, _ty)) in fun.args().iter().enumerate() {
            let val = builder.block_params(block)[i];
            let var = declare_variable(types::F64, builder, &mut variables, &mut index, name);
            builder.def_var(var, val);
        }
    }

    declare_variables_in_expr(fun.body(), builder, &mut variables, &mut index);
    variables
}

fn declare_variables_in_expr(
    expr: Expr,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
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
        Expr::Defun(_, _, _, _, _) => todo!("Not sure what to do"),
        Expr::If(cond, truth, lie, _) => {
            for expr in [*cond, *truth, *lie] {
                declare_variables_in_expr(expr, builder, variables, index)
            }
        }
        Expr::Break(Some(expr), _) => declare_variables_in_expr(*expr, builder, variables, index),
        Expr::Break(None, _) => (),
        Expr::Loop(expr, _) => {
            let zero = builder.ins().iconst(types::I64, 0);
            let return_variable =
                declare_variable(types::I64, builder, variables, index, "loop_cond");
            builder.def_var(return_variable, zero);
            declare_variables_in_expr(*expr, builder, variables, index)
        }
        Expr::Let(name, expr, _) => {
            declare_variable(types::F64, builder, variables, index, &name);
            declare_variables_in_expr(*expr, builder, variables, index)
        }
        _ => todo!(),
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
