use crate::ast::{Expression, InfixOperator, IntExpr, PrefixOperator, Program, Statement};
use crate::object::builtins::get_builtins;
use crate::token::IntType;
use crate::type_checker::type_env::{TypeEnv, TypeExpr};
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
		let statements = program.statements.into_iter()
			.map(|stmt| self.check_statement(stmt))
			.collect::<TCResult<Vec<TypedStatement>>>()?;
		let return_type = statements.last().map(|stmt| match stmt {
			TypedStatement::Let { .. } => TypeExpr::Void,
			TypedStatement::Return(None) => TypeExpr::Void,
			TypedStatement::Expression { has_semicolon: true, .. } => TypeExpr::Void,
			TypedStatement::Return(Some(e)) => get_type(e),
			TypedStatement::Expression { expr, .. } => get_type(expr),
		}).unwrap_or(TypeExpr::Void);

		Ok(TypedProgram {
			statements,
			return_type,
		})
	}

	pub fn check_statement(&mut self, stmt: Statement) -> TCResult<TypedStatement> {
		match stmt {
			Statement::Expression { expr, has_semicolon } => self.check_expression(expr)
				.map(|expr| TypedStatement::Expression { expr, has_semicolon }),
			Statement::Let { name, value } => {
				let value = self.check_expression(value)?;

				let value_type = match get_type(&value) {
					TypeExpr::Void => return Err(TypeCheckError::Generic(format!("cannot assign void type to a variable"))),
					type_expr => type_expr,
				};
				self.type_env.define_identifier(&name, value_type);

				Ok(TypedStatement::Let {
					name,
					value,
				})
			}
			Statement::Return(None) => Ok(TypedStatement::Return(None)),
			Statement::Return(Some(value)) => Ok(TypedStatement::Return(Some(self.check_expression(value)?))),
		}
	}

	//Maybe return the TypeExpr too?
	pub fn check_expression(&mut self, expr: Expression) -> TCResult<TypedExpression> {
		match expr {
			Expression::Boolean(value) => Ok(TypedExpression::Boolean(value)),
			Expression::Integer(value) => Ok(TypedExpression::Integer(value)),
			Expression::String(value) => Ok(TypedExpression::String(value)),
			Expression::Identifier(name) => {
				let type_expr = self.type_env.get_identifier_type(name.as_str())
					.cloned()
					.ok_or(TypeCheckError::Generic(format!("cannot find value `{}` in this scope", name)))?;

				Ok(TypedExpression::Identifier {
					name,
					type_expr,
				})
			}
			Expression::Prefix { operator, right } => {
				let right = self.check_expression(*right)?;
				let right_type = get_type(&right);

				match operator {
					PrefixOperator::Minus => if !matches!(right_type, TypeExpr::Int { .. }) {
						return Err(TypeCheckError::PrefixTypeMismatch {
							operator,
							right_type,
						});
					},
					PrefixOperator::Bang => if right_type != TypeExpr::Bool {
						return Err(TypeCheckError::PrefixTypeMismatch {
							operator,
							right_type,
						});
					},
				}

				Ok(TypedExpression::Prefix {
					operator,
					right: Box::new(right),
				})
			}
			Expression::Infix { left, operator, right } => {
				let left = self.check_expression(*left)?;
				let right = self.check_expression(*right)?;

				let type_expr = check_infix(&operator, &left, &right)?;

				Ok(TypedExpression::Infix {
					left: Box::new(left),
					operator,
					right: Box::new(right),
					type_expr,
				})
			}
			Expression::If { condition, consequence, alternative } => {
				let condition = self.check_expression(*condition)?;
				let condition_type = get_type(&condition);
				if !matches!(condition_type, TypeExpr::Bool) {
					return Err(TypeCheckError::Generic(format!("expected `bool`, found {:?}", condition_type)));
				}

				let consequence = self.check(consequence)?;
				let consequence_type = consequence.return_type.clone();

				let alternative = match alternative {
					Some(alternative) => Some(self.check(alternative)?),
					None => None
				};

				match alternative.as_ref().map(|a| &a.return_type) {
					None | Some(TypeExpr::Void) => if !matches!(consequence_type, TypeExpr::Void) {
						//TODO expected `()`, found `{:?}`
						return Err(TypeCheckError::Generic(format!("mismatched if types {:?} vs None", consequence_type)));
					}
					Some(t) => {
						if &consequence_type != t {
							return Err(TypeCheckError::Generic(format!("mismatched if types {:?} vs {:?}", consequence_type, t)));
						}
					}
				};

				Ok(TypedExpression::If {
					condition: Box::new(condition),
					type_expr: consequence_type,
					consequence,
					alternative,
				})
			}
			Expression::Function { name, parameters, body, return_type } => {
				for (param, param_type) in parameters.iter() {
					self.type_env.define_identifier(&param, param_type.clone());
				}

				if let Some(name) = &name {
					self.type_env.define_identifier(name, TypeExpr::FnLiteral {
						parameter_types: parameters.iter().map(|p| p.1.clone()).collect(),
						return_type: Box::new(return_type.clone()),
					});
				}

				let body = self.check(body)?;

				let body_return_type = &body.return_type;
				if &return_type != body_return_type {
					return Err(TypeCheckError::Generic(format!(
						"function body return type {:?} doesn't match declared return type {:?}",
						body_return_type,
						return_type
					)));
				}

				Ok(TypedExpression::Function { name, parameters, body, return_type })
			}
			Expression::Call { function, arguments } => {
				let function = self.check_expression(*function)?;

				let arguments = arguments.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<TypedExpression>>>()?;

				let return_type = match get_type(&function) {
					TypeExpr::FnLiteral { parameter_types ,return_type } => {
						if arguments.len() != parameter_types.len() {
							return Err(TypeCheckError::CallArgCount {
								parameter_count: parameter_types.len() as u8,
								argument_count: arguments.len() as u8,
							})
						}

						let argument_types: Vec<TypeExpr> = arguments.iter().map(|a| get_type(a)).collect();
						for (arg, param) in argument_types.iter().zip(parameter_types.iter()) {
							match param {
								TypeExpr::Array(elements) => if let TypeExpr::Any = elements.as_ref() {
									continue
								}
								TypeExpr::Any => continue,
								_ => {}
							}
							if arg != param {
								return Err(TypeCheckError::CallArgTypeMismatch {
									parameter_types,
									argument_types,
								})
							}
						}

						*return_type
					},
					TypeExpr::Void => return Err(TypeCheckError::Generic("cannot call void type".into())),
					t => return Err(TypeCheckError::Generic(format!("type {:?} not callable", t)))
				};


				Ok(TypedExpression::Call {
					function: Box::new(function),
					arguments,
					return_type,
				})
			}
			Expression::Array(elements) => {
				let elements = elements.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<TypedExpression>>>()?;
				let element_types: Vec<TypeExpr> = elements.iter().map(|e| get_type(e)).collect();

				let type_expr = match element_types.first() {
					None | Some(TypeExpr::Void) => return Err(TypeCheckError::EmptyArray),
					Some(t) => t.clone(),
				};

				for typ in element_types.iter() {
					match typ {
						TypeExpr::Void => return Err(TypeCheckError::VoidArrayElem(element_types)),
						t => if t != &type_expr {
							return Err(TypeCheckError::ArrayTypeMismatch(element_types));
						}
					}
				}

				Ok(TypedExpression::Array { elements, type_expr })
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

pub fn get_type(expr: &TypedExpression) -> TypeExpr {
	match expr {
		TypedExpression::Identifier { type_expr, .. } => type_expr.clone(),
		TypedExpression::Integer(v) => match v {
			IntExpr::U8(_) => TypeExpr::Int(IntType::U8),
			IntExpr::U16(_) => TypeExpr::Int(IntType::U16),
			IntExpr::U32(_) => TypeExpr::Int(IntType::U32),
			IntExpr::U64(_) => TypeExpr::Int(IntType::U64),
			IntExpr::I8(_) => TypeExpr::Int(IntType::I8),
			IntExpr::I16(_) => TypeExpr::Int(IntType::I16),
			IntExpr::I32(_) => TypeExpr::Int(IntType::I32),
			IntExpr::I64(_) => TypeExpr::Int(IntType::I64),
		},
		TypedExpression::Boolean(_) => TypeExpr::Bool,
		TypedExpression::String(_) => TypeExpr::String,
		TypedExpression::Function { parameters, return_type, .. } => TypeExpr::FnLiteral {
			parameter_types: parameters.iter().map(|p| p.1.clone()).collect(),
			return_type: Box::new(return_type.clone())
		},
		TypedExpression::Prefix {
			operator: PrefixOperator::Minus,
			right
		} => match get_type(right.as_ref()) {
			TypeExpr::Int(IntType::U8) => TypeExpr::Int(IntType::I8),
			TypeExpr::Int(IntType::U16) => TypeExpr::Int(IntType::I16),
			TypeExpr::Int(IntType::U32) => TypeExpr::Int(IntType::I32),
			TypeExpr::Int(IntType::U64) => TypeExpr::Int(IntType::I64),
			t => t,
		},
		TypedExpression::Prefix {
			operator: PrefixOperator::Bang,
			right
		} => get_type(right.as_ref()),
		TypedExpression::Infix { type_expr, .. } => type_expr.clone(),
		TypedExpression::If { type_expr, .. } => type_expr.clone(),
		TypedExpression::Call { return_type, .. } => return_type.clone(),
		TypedExpression::Array { type_expr, .. } => TypeExpr::Array(Box::new(type_expr.clone())),
		TypedExpression::Index { left, .. } => {
			let left_type = get_type(left.as_ref());
			match left_type {
				TypeExpr::Array(elements_type) => {
					/*if !matches!(index_type, Some(TypeExpr::Int { .. })) {
						return Err(TypeCheckError::IndexTypeMismatch {
							index_type,
							indexed_type: left_type,
						})
					}*/
					*elements_type.clone()
				}
				//Temporary
				TypeExpr::Hash => TypeExpr::Void,
				_ => panic!("{:?} isn't indexable (should be handled in check_expression())", left)
			}
		}
		TypedExpression::Hash { .. } => TypeExpr::Hash,
	}
}

fn check_infix(operator: &InfixOperator, left: &TypedExpression, right: &TypedExpression) -> TCResult<TypeExpr> {
	let left_type = get_type(left);
	let right_type = get_type(right);
	match (&left_type, right_type) {
		(TypeExpr::Void, _) | (_, TypeExpr::Void) => return Err(TypeCheckError::Generic(format!("cannot include void type in infix operator ({:?} and {:?})", left, right))),
		//(Some(TypeExpr::Int { .. }), Some(TypeExpr::Int { .. })) => {}
		(left, right) => if left != &right {
			return Err(TypeCheckError::Generic(format!("incompatible types ({:?} and {:?})", left, right)));
		}
	}

	match operator {
		InfixOperator::LessThan |
		InfixOperator::GreaterThan |
		InfixOperator::Equal |
		InfixOperator::Unequal => Ok(TypeExpr::Bool),
		_ => Ok(left_type)
	}
}

type TCResult<T> = Result<T, TypeCheckError>;

#[derive(Debug, PartialEq)]
pub enum TypeCheckError {
	PrefixTypeMismatch {
		operator: PrefixOperator,
		right_type: TypeExpr,
	},
	IndexTypeMismatch {
		indexed_type: TypeExpr,
		index_type: TypeExpr,
	},
	IndexedTypeMismatch {
		indexed_type: TypeExpr,
		index_type: TypeExpr,
	},
	EmptyArray,
	VoidArrayElem(Vec<TypeExpr>),
	ArrayTypeMismatch(Vec<TypeExpr>),
	CallArgCount {
		parameter_count: u8,
		argument_count: u8,
	},
	CallArgTypeMismatch {
		parameter_types: Vec<TypeExpr>,
		argument_types: Vec<TypeExpr>,
	},
	Generic(String),
}