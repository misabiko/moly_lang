use crate::ast::{Expression, Program, Statement};
use crate::object::builtins::get_builtins;
use crate::type_checker::type_env::{IntegerSize, TypeEnv, TypeExpr};
use crate::type_checker::typed_ast::{TypedExpression, TypedProgram, TypedStatement};

pub mod typed_ast;
pub mod type_env;

pub struct TypeChecker {
	type_env: TypeEnv,
}

impl TypeChecker {
	pub fn new() -> Self {
		let mut type_env = TypeEnv::new();
		for v in get_builtins().iter() {
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
			Statement::Expression { expr, has_semicolon } => self.check_expression(expr)
				.map(|expr| TypedStatement::Expression { expr, has_semicolon }),
			Statement::Let { name, value } => {
				let value = self.check_expression(value)?;

				let value_type = match self.get_type_from_expression(&value) {
					None => return Err(format!("cannot assign void type to a variable")),
					Some(type_expr) => type_expr,
				};
				self.type_env.define_identifier(&name, value_type);

				Ok(TypedStatement::Let {
					name,
					value,
				})
			},
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
				let condition = self.check_expression(*condition)?;
				let condition_type = self.get_type_from_expression(&condition);
				if !matches!(condition_type, Some(TypeExpr::Bool)) {
					return Err(format!("expected `bool`, found {:?}", condition_type))
				}

				let consequence = self.check(consequence)?;
				let consequence_type: Option<TypeExpr> = match consequence.statements.last() {
					Some(TypedStatement::Expression { expr, has_semicolon: false }) => self.get_type_from_expression(&expr),
					_ => None,
				};

				let alternative = match alternative {
					Some(alternative) => Some(self.check(alternative)?),
					None => None
				};

				match alternative.as_ref().map(|a| a.statements.last()) {
					Some(Some(TypedStatement::Expression { expr, has_semicolon: false })) => {
						let alternative_type = self.get_type_from_expression(&expr);
						if consequence_type != alternative_type {
							return Err(format!("mismatched if types {:?} vs {:?}", consequence_type, alternative_type))
						}
					},
					_ => if let Some(consequence_type) = consequence_type {
						//TODO expected `()`, found `{:?}`
						return Err(format!("mismatched if types {:?} vs None", consequence_type))
					}
				};

				Ok(TypedExpression::If {
					condition: Box::new(condition),
					type_expr: consequence_type,
					consequence,
					alternative,
				})
			}
			Expression::Function { name, parameters, body } => {
				for (param, param_type) in parameters.iter() {
					self.type_env.define_identifier(&param, param_type.clone());
				}

				if let Some(name) = &name {
					self.type_env.define_identifier(name, TypeExpr::Call {
						//TODO Parse fn return type
						return_type: None
					});
				}

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

	fn get_type_from_expression(&self, expr: &TypedExpression) -> Option<TypeExpr> {
		match expr {
			TypedExpression::Identifier { type_expr, .. } => Some(type_expr.clone()),
			//TODO Get proper int type
			TypedExpression::Integer(_) => Some(TypeExpr::Int {
				unsigned: false,
				size: IntegerSize::S64,
			}),
			TypedExpression::Boolean(_) => Some(TypeExpr::Bool),
			TypedExpression::String(_) => Some(TypeExpr::String),
			TypedExpression::Function { .. } => Some(TypeExpr::FnLiteral),
			expr => panic!("todo get_type_from_expression {:?}", expr)//TODO Remove rest
			/*TypedExpression::Prefix { operator, right } => {

			}
			TypedExpression::Infix { .. } => {}
			TypedExpression::If { .. } => {}
			TypedExpression::Function { .. } => {}
			TypedExpression::Call { .. } => {}
			TypedExpression::Array(_) => {}
			TypedExpression::Index { .. } => {}
			TypedExpression::Hash(_) => {}*/
		}
	}
}

type TCResult<T> = Result<T, String>;