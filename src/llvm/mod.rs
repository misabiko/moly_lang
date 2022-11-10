use std::collections::HashMap;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::FloatPredicate;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue};
use crate::ast::InfixOperator;
use crate::type_checker::typed_ast::{TypedExpression, TypedStatement};

pub struct LLVMCompiler<'a, 'ctx> {
	pub context: &'ctx Context,
	pub builder: &'a Builder<'ctx>,
	pub fpm: &'a PassManager<FunctionValue<'ctx>>,
	pub module: &'a Module<'ctx>,

	variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'a, 'ctx> LLVMCompiler<'a, 'ctx> {
	pub fn compile(
		context: &'ctx Context,
		builder: &'a Builder<'ctx>,
		pass_manager: &'a PassManager<FunctionValue<'ctx>>,
		module: &'a Module<'ctx>,
		function: TypedStatement,
	) -> CResult<FunctionValue<'ctx>> {
		let mut compiler = LLVMCompiler {
			context,
			builder,
			fpm: pass_manager,
			module,
			// function,
			// fn_value_opt: None,
			variables: HashMap::new(),
		};

		compiler.compile_statement(function)
	}

	#[inline]
	fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
		self.module.get_function(name)
	}

	fn create_entry_block_alloca(&self, fn_val: FunctionValue, name: &str) -> PointerValue<'ctx> {
		let builder = self.context.create_builder();

		let entry = fn_val.get_first_basic_block().unwrap();

		match entry.get_first_instruction() {
			Some(first_instr) => builder.position_before(&first_instr),
			None => builder.position_at_end(entry),
		}

		builder.build_alloca(self.context.f32_type(), name)
	}

	/*fn compile_block(&mut self, block: TypedStatementBlock) -> CResult<FloatValue<'ctx>> {
		for stmt in block.statements {
			self.compile_statement(stmt);
		}
	}*/

	fn compile_statement(&mut self, stmt: TypedStatement) -> CResult<FunctionValue<'ctx>> {
		match stmt {
			TypedStatement::Function(func) => {
				let return_type = self.context.f32_type();
				let args_types = std::iter::repeat(return_type)
					.take(func.parameters.len())
					.map(|f| f.into())
					.collect::<Vec<BasicMetadataTypeEnum>>();
				let args_types = args_types.as_slice();

				let fn_type = self.context.f32_type().fn_type(args_types, false);
				let fn_val = self.module.add_function(func.name.unwrap().as_str(), fn_type, None);

				for (i, param) in fn_val.get_param_iter().enumerate() {
					param.into_float_value().set_name(func.parameters[i].0.as_str());
				}

				let entry = self.context.append_basic_block(fn_val, "entry");

				self.builder.position_at_end(entry);

				self.variables.reserve(func.parameters.len());

				for (i, param) in fn_val.get_param_iter().enumerate() {
					let param_name = func.parameters[i].0.as_str();
					let alloca = self.create_entry_block_alloca(fn_val, param_name);

					self.builder.build_store(alloca, param);

					self.variables.insert(func.parameters[i].0.clone(), alloca);
				}

				//TODO compile_block
				let return_expr = if let TypedStatement::Expression { expr, .. } = func.body.statements.last().unwrap() {
					self.compile_expression(expr.clone())?
				} else {
					panic!("Function doesn't end with expression")
				};

				self.builder.build_return(Some(&return_expr));

				if fn_val.verify(true) {
					self.fpm.run_on(&fn_val);

					Ok(fn_val)
				}else {
					unsafe {
						//TODO Confirm that we can't just panic
						fn_val.delete();
					}

					Err("Invalid generated function.".into())
				}
			}
			stmt => panic!("compile_statement({:?})", stmt)//TODO compile_statement rest
		}
	}

	fn compile_expression(&mut self, expr: TypedExpression) -> CResult<FloatValue<'ctx>> {
		match expr {
			TypedExpression::Float(n) => Ok(self.context.f32_type().const_float(n as f64)),
			TypedExpression::Identifier { name, .. } => {
				match self.variables.get(name.as_str()) {
					Some(var) => Ok(
						self.builder
						.build_load(*var, name.as_str())
						.into_float_value()
					),
					None => Err(format!("Could not find a matching variable for {}.", name)),
				}
			}
			TypedExpression::Infix { left, operator, right, .. } => {
				let left = self.compile_expression(*left)?;
				let right = self.compile_expression(*right)?;

				match operator {
					InfixOperator::Plus => Ok(self.builder.build_float_add(left, right, "tmpadd")),
					InfixOperator::Minus => Ok(self.builder.build_float_sub(left, right, "tmpsub")),
					InfixOperator::Mul => Ok(self.builder.build_float_mul(left, right, "tmpmul")),
					// InfixOperator::Div => Ok(self.builder.build_float_div(left, right, "tmpdiv")),
					InfixOperator::LessThan => Ok({
						let cmp = self
							.builder
							.build_float_compare(FloatPredicate::ULT, left, right, "tmpcmp");

						self.builder
							.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
					}),
					_ => todo!()
				}
			}
			TypedExpression::Call { function, arguments, .. } => {
				let func_name = if let TypedExpression::Identifier { name, .. } = *function {
					name
				}else {
					todo!()
				};

				match self.get_function(func_name.as_str()) {
					Some(func) => {
						let mut compiled_args = Vec::with_capacity(arguments.len());

						for arg in arguments {
							compiled_args.push(self.compile_expression(arg)?);
						}

						let argsv: Vec<BasicMetadataValueEnum> =
							compiled_args.iter().by_ref().map(|&val| val.into()).collect();

						match self
							.builder
							.build_call(func, argsv.as_slice(), "calltmp")
							.try_as_basic_value()
							.left()
						{
							Some(value) => Ok(value.into_float_value()),
							None => Err("Invalid call produced.".into()),
						}
					}
					None => Err(format!("Unknown function {}.", func_name)),
				}
			}
			expr => panic!("compile_expression({:?})", expr) //TODO compile_expression rest
		}
	}
}

type CResult<T> = Result<T, String>;