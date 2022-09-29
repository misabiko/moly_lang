use crate::ast::{Expression, InfixOperator, IntExpr, PrefixOperator, Program, Statement, StatementBlock};
use crate::object::builtins::get_builtins;
use crate::token::IntType;
use crate::type_checker::type_env::{TypeEnv, TypeExpr};
use crate::type_checker::typed_ast::{TypedExpression, TypedProgram, TypedStatement, TypedStatementBlock};

pub mod typed_ast;
pub mod type_env;

pub struct TypeChecker {
	type_env: TypeEnv,
	scope_return_types: Vec<Option<TypeExpr>>,
}

impl TypeChecker {
	pub fn new() -> Self {
		let mut type_env = TypeEnv::new();
		for v in get_builtins().iter() {
			type_env.define_identifier(v.name, v.type_expr.clone());
		}

		Self {
			type_env,
			scope_return_types: vec![],
		}
	}

	pub fn check(&mut self, program: Program, new_scope: bool) -> TCResult<TypedProgram> {
		let block = self.check_block(program, new_scope, false)?;

		Ok(TypedProgram {
			statements: block.statements,
			return_type: block.return_type,
		})
	}

	pub fn check_block(&mut self, block: StatementBlock, new_scope: bool, already_type_scoped: bool) -> TCResult<TypedStatementBlock> {
		if new_scope {
			self.scope_return_types.push(None);

			//Kind of hard-coded bool to include function's parameters in the scope
			if !already_type_scoped {
				self.type_env.push_scope();
			}
		}

		let statements = block.0.into_iter()
			.map(|stmt| self.check_statement(stmt))
			.collect::<TCResult<Vec<TypedStatement>>>()?;

		let return_type = statements.last().map(|stmt| match stmt {
			TypedStatement::Let { .. } => TypeExpr::Void,
			TypedStatement::Return(None) => TypeExpr::Void,
			TypedStatement::Expression { has_semicolon: true, .. } => TypeExpr::Void,
			TypedStatement::Return(Some(e)) => if new_scope {
				get_type(e)
			} else {
				TypeExpr::Return(Box::new(get_type(e)))
			},
			TypedStatement::Expression { expr, .. } => get_type(expr),
		}).unwrap_or(TypeExpr::Void);

		if new_scope {
			self.check_scope_return_type(&return_type)?;
			self.scope_return_types.pop();
			self.type_env.pop_scope();
		}

		Ok(TypedStatementBlock {
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
			Statement::Return(None) => {
				self.check_scope_return_type(&TypeExpr::Void)?;

				Ok(TypedStatement::Return(None))
			},
			Statement::Return(Some(value)) => {
				let returned = self.check_expression(value)?;

				self.check_scope_return_type(&get_type(&returned))?;

				Ok(TypedStatement::Return(Some(returned)))
			},
		}
	}

	//TODO Maybe return the TypeExpr too?
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

				let consequence = self.check(consequence, false)?;
				let cons_type = consequence.return_type.clone();

				let alternative = match alternative {
					Some(alternative) => Some(self.check(alternative, false)?),
					None => None
				};
				let alt_type = alternative.as_ref().map(|a| &a.return_type);

				//Checking if branches match
				match (&cons_type, alt_type) {
					(TypeExpr::Return(_), _) |
					(_, Some(TypeExpr::Return(_))) |
					(TypeExpr::Void, None) => {}
					(cons_type, None | Some(TypeExpr::Void)) => {
						//TODO expected `()`, found `{:?}`
						return Err(TypeCheckError::Generic(format!("mismatched if types {:?} vs None", cons_type)));
					}
					(cons_type, Some(t)) => {
						if cons_type != t {
							return Err(TypeCheckError::Generic(format!("mismatched if types {:?} vs {:?}", cons_type, t)));
						}
					}
				}

				let type_expr = match (&cons_type, alt_type) {
					(TypeExpr::Return(cons_type), Some(TypeExpr::Return(_))) => cons_type,
					(TypeExpr::Return(_), Some(alt_type)) => alt_type,
					(cons_type, _) => cons_type,
				}.clone();

				Ok(TypedExpression::If {
					condition: Box::new(condition),
					type_expr,
					consequence,
					alternative,
				})
			}
			Expression::Function { name, parameters, body, return_type } => {
				self.type_env.push_scope();

				for (param, param_type) in parameters.iter() {
					self.type_env.define_identifier(&param, param_type.clone());
				}

				if let Some(name) = &name {
					self.type_env.define_identifier(name, TypeExpr::FnLiteral {
						parameter_types: parameters.iter().map(|p| p.1.clone()).collect(),
						return_type: Box::new(return_type.clone()),
					});
				}

				let body = self.check_block(body, true, true)?;

				let body_return_type = match &body.return_type {
					TypeExpr::Return(returned_type) => returned_type.as_ref(),
					t => t,
				};
				if &return_type != body_return_type {
					return Err(TypeCheckError::Generic(format!(
						"function body return type {:?} doesn't match declared return type {:?}",
						body_return_type,
						return_type
					)));
				}

				Ok(TypedExpression::Function { name, parameters, body })
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
			Expression::Block { statements, return_transparent } => {
				let block = self.check_block(statements, !return_transparent, false)?;

				Ok(TypedExpression::Block {
					block,
					return_transparent
				})
			}
		}
	}

	fn check_scope_return_type(&mut self, new_return_type: &TypeExpr) -> TCResult<()> {
		let current = self.scope_return_types.last_mut().unwrap();
		let new_return_type = match new_return_type {
			TypeExpr::Return(returned) => &returned,
			t => t,
		};

		match current {
			None => *current = Some(new_return_type.clone()),
			Some(current) => if current != new_return_type {
				return Err(TypeCheckError::ReturnTypeMismatch {
					scope_return_type: current.clone(),
					mismatched_type: new_return_type.clone(),
				})
			}
		}

		Ok(())
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
		TypedExpression::Function { parameters, body, .. } => TypeExpr::FnLiteral {
			parameter_types: parameters.iter().map(|p| p.1.clone()).collect(),
			return_type: Box::new(body.return_type.clone())
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
				_ => panic!("{:?} isn't indexable (should be handled in check_expression())", left)
			}
		}
		TypedExpression::Block { block, .. } => block.return_type.clone(),
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
	ReturnTypeMismatch {
		scope_return_type: TypeExpr,
		mismatched_type: TypeExpr,
	},
	Generic(String),
}