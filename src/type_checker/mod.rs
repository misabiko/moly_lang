use std::convert::identity;
use crate::ast::{Expression, Function, InfixOperator, IntExpr, ParsedType, PrefixOperator, Program, Statement, StatementBlock, StructConstructor, StructDecl};
use crate::object::builtins::get_builtins;
use crate::token::IntType;
use crate::type_checker::type_env::{TypeBinding, TypeEnv, TypeExpr};
use crate::type_checker::typed_ast::{TypedExpression, TypedFunction, TypedProgram, TypedStatement, TypedStatementBlock};

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

		Ok(TypedProgram(block.statements))
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
			.collect::<TCResult<Vec<Option<TypedStatement>>>>()?
			.into_iter().filter_map(identity)
			.collect::<Vec<TypedStatement>>();

		let return_type = statements.last().map(|stmt| match stmt {
			TypedStatement::Let { .. } |
			TypedStatement::Function(_) |
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

	pub fn check_statement(&mut self, stmt: Statement) -> TCResult<Option<TypedStatement>> {
		match stmt {
			Statement::Expression { expr, has_semicolon } => self.check_expression(expr)
				.map(|(expr, _)| Some(TypedStatement::Expression { expr, has_semicolon })),
			Statement::Let { name, value } => {
				let (value, value_type) = self.check_expression(value)?;

				let value_type = match value_type {
					TypeExpr::Void => return Err(TypeCheckError::Generic(format!("cannot assign void type to a variable"))),
					type_expr => type_expr,
				};
				self.type_env.define_identifier(&name, value_type.clone());

				Ok(Some(TypedStatement::Let {
					name,
					value,
					type_expr: value_type,
				}))
			}
			Statement::Return(None) => {
				self.check_scope_return_type(&TypeExpr::Void)?;

				Ok(Some(TypedStatement::Return(None)))
			}
			Statement::Return(Some(value)) => {
				let (returned, returned_type) = self.check_expression(value)?;

				self.check_scope_return_type(&returned_type)?;

				Ok(Some(TypedStatement::Return(Some(returned))))
			}
			Statement::Function(func) => Ok(Some(TypedStatement::Function(self.check_function(func, true)?))),
			Statement::Struct { name, decl } => {
				self.check_struct_decl(name, decl)?;
				Ok(None)
			}
		}
	}

	pub fn check_expression(&mut self, expr: Expression) -> TCResult<(TypedExpression, TypeExpr)> {
		match expr {
			Expression::Boolean(value) => Ok((TypedExpression::Boolean(value), TypeExpr::Bool)),
			Expression::Integer(IntExpr::U8(value)) => Ok((TypedExpression::Integer(IntExpr::U8(value)), TypeExpr::Int(IntType::U8))),
			Expression::Integer(IntExpr::U16(value)) => Ok((TypedExpression::Integer(IntExpr::U16(value)), TypeExpr::Int(IntType::U16))),
			Expression::Integer(IntExpr::U32(value)) => Ok((TypedExpression::Integer(IntExpr::U32(value)), TypeExpr::Int(IntType::U32))),
			Expression::Integer(IntExpr::U64(value)) => Ok((TypedExpression::Integer(IntExpr::U64(value)), TypeExpr::Int(IntType::U64))),
			Expression::Integer(IntExpr::I8(value)) => Ok((TypedExpression::Integer(IntExpr::I8(value)), TypeExpr::Int(IntType::I8))),
			Expression::Integer(IntExpr::I16(value)) => Ok((TypedExpression::Integer(IntExpr::I16(value)), TypeExpr::Int(IntType::I16))),
			Expression::Integer(IntExpr::I32(value)) => Ok((TypedExpression::Integer(IntExpr::I32(value)), TypeExpr::Int(IntType::I32))),
			Expression::Integer(IntExpr::I64(value)) => Ok((TypedExpression::Integer(IntExpr::I64(value)), TypeExpr::Int(IntType::I64))),
			Expression::Float(value) => Ok((TypedExpression::Float(value), TypeExpr::Float)),
			Expression::String(value) => Ok((TypedExpression::String(value), TypeExpr::String)),
			Expression::Identifier(name) => {
				let type_expr = self.type_env.get_identifier_type(name.as_str())
					.or_else(|| self.type_env.get_custom_type(name.as_str()))
					.cloned()
					.ok_or(TypeCheckError::Generic(format!("cannot find value `{}` in this scope", name)))?;

				Ok((TypedExpression::Identifier {
					name,
					type_expr: type_expr.clone(),
				}, type_expr))
			}
			Expression::Prefix { operator, right } => {
				let (right, right_type) = self.check_expression(*right)?;

				let type_expr = match operator {
					PrefixOperator::Minus => match right_type {
						TypeExpr::Int(IntType::U8) => TypeExpr::Int(IntType::I8),
						TypeExpr::Int(IntType::U16) => TypeExpr::Int(IntType::I16),
						TypeExpr::Int(IntType::U32) => TypeExpr::Int(IntType::I32),
						TypeExpr::Int(IntType::U64) => TypeExpr::Int(IntType::I64),
						t @ TypeExpr::Int { .. } => t,
						_ => {
							return Err(TypeCheckError::PrefixTypeMismatch {
								operator,
								right_type,
							})
						}
					},
					PrefixOperator::Bang => if right_type != TypeExpr::Bool {
						return Err(TypeCheckError::PrefixTypeMismatch {
							operator,
							right_type,
						});
					}else {
						right_type
					},
				};

				Ok((TypedExpression::Prefix {
					operator,
					right: Box::new(right),
				}, type_expr))
			}
			Expression::Infix { left, operator, right } => {
				let (left, left_type) = self.check_expression(*left)?;
				let (right, right_type) = self.check_expression(*right)?;

				let type_expr = check_infix(&operator, &left, left_type, &right, right_type)?;

				Ok((TypedExpression::Infix {
					left: Box::new(left),
					operator,
					right: Box::new(right),
					type_expr: type_expr.clone(),
				}, type_expr))
			}
			Expression::If { condition, consequence, alternative } => {
				let (condition, condition_type) = self.check_expression(*condition)?;
				if !matches!(condition_type, TypeExpr::Bool) {
					return Err(TypeCheckError::Generic(format!("expected `bool`, found {:?}", condition_type)));
				}

				let consequence = self.check_block(consequence, false, false)?;
				let cons_type = consequence.return_type.clone();

				let alternative = match alternative {
					Some(alternative) => Some(self.check_block(alternative, false, false)?),
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

				Ok((TypedExpression::If {
					condition: Box::new(condition),
					type_expr: type_expr.clone(),
					consequence,
					alternative,
				}, type_expr))
			}
			Expression::Function(func) => {
				let func = self.check_function(func, false)?;
				let type_expr = TypeExpr::FnLiteral {
					parameter_types: func.parameters.iter().map(|p| p.1.clone()).collect(),
					return_type: Box::new(func.body.return_type.clone()),
				};

				Ok((TypedExpression::Function(func), type_expr))
			}
			Expression::Call { function, arguments } => {
				let (function, function_type) = self.check_expression(*function)?;

				let (arguments, argument_types): (Vec<TypedExpression>, Vec<TypeExpr>) = arguments.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<(TypedExpression, TypeExpr)>>>()?
					.into_iter()
					.unzip();

				let return_type = match function_type {
					TypeExpr::FnLiteral { parameter_types, return_type } => {
						if arguments.len() != parameter_types.len() {
							return Err(TypeCheckError::CallArgCount {
								parameter_count: parameter_types.len() as u8,
								argument_count: arguments.len() as u8,
							});
						}

						for (arg, param) in argument_types.iter().zip(parameter_types.iter()) {
							match param {
								TypeExpr::Array(elements) => if let TypeExpr::Any = elements.as_ref() {
									continue;
								}
								TypeExpr::Any => continue,
								_ => {}
							}
							if arg != param {
								return Err(TypeCheckError::CallArgTypeMismatch {
									parameter_types,
									argument_types,
								});
							}
						}

						*return_type
					}
					TypeExpr::Struct { name, bindings } => {
						if arguments.len() != bindings.len() {
							return Err(TypeCheckError::CallArgCount {
								parameter_count: bindings.len() as u8,
								argument_count: arguments.len() as u8,
							});
						}

						for (arg, binding) in argument_types.iter().zip(bindings.iter()) {
							match &binding.type_expr {
								TypeExpr::Array(elements) => if let TypeExpr::Any = elements.as_ref() {
									continue;
								}
								TypeExpr::Any => continue,
								_ => {}
							}
							if arg != &binding.type_expr {
								return Err(TypeCheckError::CallArgTypeMismatch {
									parameter_types: bindings.iter().map(|b| b.type_expr.clone()).collect(),
									argument_types,
								});
							}
						}

						TypeExpr::Struct { name, bindings }
					}
					TypeExpr::Void => return Err(TypeCheckError::Generic("cannot call void type".into())),
					t => return Err(TypeCheckError::Generic(format!("type {:?} not callable", t)))
				};


				Ok((TypedExpression::Call {
					function: Box::new(function),
					arguments,
					return_type: return_type.clone(),
				}, return_type))
			}
			Expression::Field { left, field } => {
				let (left, left_type) = self.check_expression(*left)?;

				//TODO Allow fields on primitive too (for methods)
				if let TypeExpr::Struct { bindings, .. } = left_type.clone() {
					if let Some(binding_index) = bindings.iter().position(|t| t.ident == field) {
						let type_expr = bindings[binding_index].type_expr.clone();

						Ok((TypedExpression::Field {
							left: Box::new(left),
							left_type,
							binding_index,
							field,
							field_type: type_expr.clone(),
						}, type_expr))
					}else {
						Err(TypeCheckError::UnknownField {left, field})
					}
				}else {
					Err(TypeCheckError::Generic(format!("For now can't use dot notation on non struct. left={:?}", left)))
				}

			}
			Expression::Array(elements) => {
				let (elements, element_types): (Vec<TypedExpression>, Vec<TypeExpr>) = elements.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<(TypedExpression, TypeExpr)>>>()?
					.into_iter()
					.unzip();

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

				Ok((
					TypedExpression::Array { elements, type_expr: type_expr.clone() },
					TypeExpr::Array(Box::new(type_expr))
				))
			}
			Expression::Index { left, index } => {
				let (left, left_type) = self.check_expression(*left)?;
				let (index, _) = self.check_expression(*index)?;
				let type_expr = match left_type {
					TypeExpr::Array(elements_type) => {
						*elements_type.clone()
					}
					_ => panic!("{:?} isn't indexable (should be handled in check_expression())", left)
				};

				Ok((TypedExpression::Index {
					left: Box::new(left),
					index: Box::new(index),
				}, type_expr))
			}
			Expression::Block { statements, return_transparent } => {
				let block = self.check_block(statements, !return_transparent, false)?;
				let type_expr = block.return_type.clone();

				Ok((TypedExpression::Block {
					block,
					return_transparent,
				}, type_expr))
			}
			Expression::Struct { name, constructor } => {
				match self.type_env.get_custom_type(&name).cloned() {
					Some(type_expr) => {
						let parsed_fields = match constructor {
							StructConstructor::Block(fields) => fields,
							StructConstructor::Tuple(fields) => fields.into_iter().enumerate()
								.map(|(i, t)| (i.to_string(), t))
								.collect(),
						};

						let mut fields = vec![];
						for (name, value) in parsed_fields {
							fields.push((name, self.check_expression(value)?.0));
						}

						Ok((TypedExpression::Struct { name, fields, type_expr: type_expr.clone() }, type_expr))
					}
					None => Err(TypeCheckError::UnknownType(name))
				}
			}
		}
	}

	fn check_function(&mut self, function: Function, global: bool) -> TCResult<TypedFunction> {
		let return_type = self.check_parsed_type(function.return_type)?;
		let parameters = function.parameters.iter()
			.map(|p| self.check_parsed_type(p.1.clone()))
			.collect::<TCResult<Vec<TypeExpr>>>()?;

		if global {
			self.register_function_type(function.name.as_ref(), parameters.clone(), return_type.clone());
		}

		self.type_env.push_scope();

		for (param, param_type) in function.parameters.iter() {
			let type_expr = self.check_parsed_type(param_type.clone())?;
			self.type_env.define_identifier(&param, type_expr);
		}

		if !global {
			self.register_function_type(function.name.as_ref(), parameters.clone(), return_type.clone());
		}

		let body = self.check_block(function.body, true, true)?;

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

		Ok(TypedFunction {
			name: function.name,
			parameters: function.parameters.iter().zip(parameters.into_iter())
				.map(|((param, _), param_type)| (param.clone(), param_type))
				.collect(),
			body,
		})
	}

	fn register_function_type(&mut self, name: Option<&String>, parameter_types: Vec<TypeExpr>, return_type: TypeExpr) {
		if let Some(name) = name {
			self.type_env.define_identifier(name, TypeExpr::FnLiteral {
				parameter_types,
				return_type: Box::new(return_type.clone()),
			});
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
				});
			}
		}

		Ok(())
	}

	fn check_struct_decl(&mut self, name: String, decl: StructDecl) -> TCResult<()> {
		let type_expr = TypeExpr::Struct {
			name: name.clone(),
			bindings: match &decl {
				StructDecl::Block(fields) => {
					let types = fields.iter()
						.map(|(_, t)| self.check_parsed_type(t.clone()))
						.collect::<TCResult<Vec<TypeExpr>>>()?;
					fields.iter().zip(types.into_iter())
						.map(|(f, type_expr)|
							TypeBinding {
								ident: f.0.clone(),
								type_expr,
							}).collect()
				},
				StructDecl::Tuple(fields) => fields.iter()
					.map(|t| self.check_parsed_type(t.clone())).collect::<TCResult<Vec<TypeExpr>>>()?
					.into_iter().enumerate()
					.map(|(i, type_expr)|
						TypeBinding {
							ident: i.to_string(),
							type_expr,
						}
					).collect(),
			}
		};
		self.type_env.define_type(&name, type_expr);

		Ok(())
	}

	fn check_parsed_type(&mut self, parsed_type: ParsedType) -> TCResult<TypeExpr> {
		match parsed_type {
			ParsedType::Primitive(t) => Ok(t),
			ParsedType::FnLiteral { parameter_types, return_type } => Ok(TypeExpr::FnLiteral {
				parameter_types: parameter_types.into_iter()
					.map(|t| self.check_parsed_type(t))
					.collect::<TCResult<Vec<TypeExpr>>>()?,
				return_type: Box::new(self.check_parsed_type(*return_type)?)
			}),
			ParsedType::Custom(name) => match self.type_env.get_custom_type(&name) {
				Some(t) => Ok(t.clone()),
				None => Err(TypeCheckError::UnknownType(name)),
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
		TypedExpression::Float(_) => TypeExpr::Float,
		TypedExpression::Boolean(_) => TypeExpr::Bool,
		TypedExpression::String(_) => TypeExpr::String,
		TypedExpression::Function(TypedFunction { parameters, body, .. }) => TypeExpr::FnLiteral {
			parameter_types: parameters.iter().map(|p| p.1.clone()).collect(),
			return_type: Box::new(body.return_type.clone()),
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
		TypedExpression::Field { field_type, .. } => field_type.clone(),
		TypedExpression::Array { type_expr, .. } => TypeExpr::Array(Box::new(type_expr.clone())),
		TypedExpression::Index { left, .. } => {
			let left_type = get_type(left.as_ref());
			match left_type {
				TypeExpr::Array(elements_type) => {
					*elements_type.clone()
				}
				_ => panic!("{:?} isn't indexable (should be handled in check_expression())", left)
			}
		}
		TypedExpression::Block { block, .. } => block.return_type.clone(),
		TypedExpression::Struct { type_expr, .. } => type_expr.clone(),
	}
}

fn check_infix(operator: &InfixOperator, left: &TypedExpression, left_type: TypeExpr, right: &TypedExpression, right_type: TypeExpr) -> TCResult<TypeExpr> {
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
	UnknownType(String),
	UnknownField {
		left: TypedExpression,
		field: String,
	},
	Generic(String),
}