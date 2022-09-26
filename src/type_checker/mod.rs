use crate::ast::{Expression, Program, Statement};
use crate::object::builtins::BUILTINS;
use crate::type_checker::type_env::TypeEnv;
use crate::type_checker::typed_ast::{TypedExpression, TypedProgram, TypedStatement};

pub mod typed_ast;
pub mod type_env;

pub struct TypeChecker {
	type_env: TypeEnv,
}

impl TypeChecker {
	pub fn new() -> Self {
		let mut type_env = TypeEnv::new();
		for v in BUILTINS.iter() {
			type_env.define_identifier(v.name, v.type_expr.clone());
		}

		Self {
			type_env,
		}
	}

	pub fn check(&mut self, program: Program) -> TCResult<TypedProgram> {
		Ok(TypedProgram {
			statements: program.statements.into_iter()
				.map(|stmt| self.check_statement(stmt))
				.collect::<TCResult<Vec<TypedStatement>>>()?
		})
	}

	pub fn check_statement(&mut self, stmt: Statement) -> TCResult<TypedStatement> {
		match stmt {
			Statement::Expression(expr) => self.check_expression(expr)
				.map(|expr| TypedStatement::Expression(expr)),
			Statement::Let { name, value } => Ok(TypedStatement::Let {
				name,
				value: self.check_expression(value)?,
			}),
			Statement::Return(None) => Ok(TypedStatement::Return(None)),
			Statement::Return(Some(value)) => Ok(TypedStatement::Return(Some(self.check_expression(value)?))),
		}
	}

	pub fn check_expression(&mut self, expr: Expression) -> TCResult<TypedExpression> {
		match expr {
			Expression::Boolean(value) => Ok(TypedExpression::Boolean(value)),
			//TODO Add different integer sizes
			Expression::Integer(value) => Ok(TypedExpression::Integer(value)),
			Expression::String(value) => Ok(TypedExpression::String(value)),
			Expression::Identifier(name) => {
				let type_expr = self.type_env.get_identifier_type(name.as_str())
					.cloned()
					.ok_or(format!("cannot find value `{}` in this scope", name))?;

				Ok(TypedExpression::Identifier {
					name,
					type_expr,
				})
			}
			Expression::Prefix { operator, right } => {
				let right = self.check_expression(*right)?;
				Ok(TypedExpression::Prefix {
					operator,
					right: Box::new(right),
				})
			}
			Expression::Infix { left, operator, right } => {
				let left = self.check_expression(*left)?;
				let right = self.check_expression(*right)?;
				Ok(TypedExpression::Infix {
					left: Box::new(left),
					operator,
					right: Box::new(right),
				})
			}
			Expression::If { condition, consequence, alternative } => {
				//Ok(TypedExpression::If { condition, consequence, alternative })
				Err("if todo".into())
			}
			Expression::Function { name, parameters, body } => {
				let body = self.check(body)?;
				Ok(TypedExpression::Function { name, parameters, body })
			}
			Expression::Call { function, arguments } => {
				let function = self.check_expression(*function)?;
				let arguments = arguments.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<TypedExpression>>>()?;

				Ok(TypedExpression::Call {
					function: Box::new(function),
					arguments,
				})
			}
			Expression::Array(elements) => {
				let elements = elements.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<TypedExpression>>>()?;

				Ok(TypedExpression::Array(elements))
			}
			Expression::Index { left, index } => {
				let left = self.check_expression(*left)?;
				let index = self.check_expression(*index)?;

				Ok(TypedExpression::Index {
					left: Box::new(left),
					index: Box::new(index),
				})
			}
			Expression::Hash(pairs) => {
				let mut typed_pairs: Vec<(TypedExpression, TypedExpression)> = Vec::with_capacity(pairs.len());

				for (key, value) in pairs {
					typed_pairs.push((self.check_expression(key)?, self.check_expression(value)?))
				}

				Ok(TypedExpression::Hash(typed_pairs))
			}
		}
	}
}

type TCResult<T> = Result<T, String>;