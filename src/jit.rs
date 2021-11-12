use cranelift::codegen::ir::{types, Function};
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;

use crate::parser::Expr;
use crate::Result;

pub struct Jit {
    builder_ctx: FunctionBuilderContext,
    ctx: codegen::Context,
    data_ctx: DataContext,
    module: JITModule,
    funcounter: usize,
}
impl Default for Jit {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
            funcounter: 0,
        }
    }
}

impl Jit {
    pub fn compile(&mut self, fun: crate::eval::Function) -> Result<*const u8, String> {
        let name = {
            self.funcounter += 1;
            let mut name = "JitFn".to_string();
            name.push_str(&self.funcounter.to_string());
            name
        };

        self.translate(fun)?;

        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module
            .define_function(
                id,
                &mut self.ctx,
                &mut codegen::binemit::NullTrapSink {},
                &mut codegen::binemit::NullStackMapSink {},
            )
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    fn translate(&mut self, fun: crate::eval::Function) -> Result<(), String> {
        for _p in fun.args() {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(types::F64))
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
        };

        let return_value = trans.translate_expr(fun.body());
        trans.builder.ins().return_(&[return_value]);
        trans.builder.finalize();

        println!("{}", trans.builder.func.display());

        Ok(())
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Symbol(n, _) => {
                let var = self.variables.get(&n).expect("Undefined variable");
                self.builder.use_var(*var)
            }
            Expr::Number(n, _) => self.builder.ins().f64const(n),
            Expr::List(exprs, _) => {
                let mut list_ret = self.builder.ins().f64const(0);
                for expr in exprs {
                    list_ret = self.translate_expr(expr);
                }
                list_ret
            }
            Expr::Quoted(_, _) => todo!("dunno"),
            Expr::Defun(_, _, _, _) => todo!("dunno"),
            Expr::If(cond, truth, lie, _) => self.translate_if(*cond, *truth, *lie),
            Expr::Return(_, _) => todo!("Why did i call break return? Its confusing af"),
            Expr::Loop(_, _) => todo!(),
            Expr::Let(name, expr, _) => self.translate_let(name, *expr),
        }
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
    fun: &crate::eval::Function,
    block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, (name, _ty)) in fun.args().iter().enumerate() {
        let val = builder.block_params(block)[i];
        let var = declare_variable(types::F64, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
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
        Expr::Number(_, _) => (),
        Expr::List(exprs, _) => {
            for expr in exprs {
                declare_variables_in_expr(expr, builder, variables, index)
            }
        }
        Expr::Quoted(_, _) => (),
        Expr::Defun(_, _, _, _) => todo!("Not sure what to do"),
        Expr::If(cond, truth, lie, _) => {
            for expr in [*cond, *truth, *lie] {
                declare_variables_in_expr(expr, builder, variables, index)
            }
        }
        Expr::Return(Some(expr), _) => declare_variables_in_expr(*expr, builder, variables, index),
        Expr::Return(None, _) => (),
        Expr::Loop(expr, _) => declare_variables_in_expr(*expr, builder, variables, index),
        Expr::Let(name, expr, _) => {
            declare_variable(types::F64, builder, variables, index, &name);
            declare_variables_in_expr(*expr, builder, variables, index)
        }
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
