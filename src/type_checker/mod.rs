use std::convert::identity;
use std::fmt::Formatter;
use crate::ast::{Expression, Function, InfixOperator, IntExpr, ParsedType, PrefixOperator, Program, Statement, StatementBlock, StructConstructor, StructDecl};
use crate::object::builtins::{get_builtin_functions, get_builtin_traits};
use crate::type_checker::type_env::{type_id_eq, TypeBinding, TypeEnv, TypeExpr, TypeId};
use crate::type_checker::typed_ast::{TypedExpression, TypedFunction, TypedProgram, TypedStatement, TypedStatementBlock};

pub mod typed_ast;
pub mod type_env;

pub struct TypeChecker {
	pub type_env: TypeEnv,
	pub scope_return_types: Vec<Option<TypeId>>,
}

impl TypeChecker {
	pub fn new() -> Self {
		let mut type_env = TypeEnv::new();

		for v in get_builtin_traits() {
			type_env.define_type(v.name.into(), v.type_expr).unwrap();
		}

		for v in get_builtin_functions() {
			let id = type_env.get_id(&v.type_expr).unwrap();
			type_env.define_identifier(v.name, id);
		}

		Self {
			type_env,
			scope_return_types: vec![],
		}
    }

    pub fn new_without_builtins() -> Self {
        Self {
            type_env: TypeEnv::new(),
            scope_return_types: vec![],
        }
    }

	///Setting `new_scope` to false and manually pushing scope avoids the top types being popped
	pub fn check(&mut self, program: Program, new_scope: bool) -> TCResult<TypedProgram> {
		let block = self.check_block(program, new_scope, false)?;

		Ok(TypedProgram(block.statements))
	}

	pub fn check_block(&mut self, block: StatementBlock, new_scope: bool, already_type_scoped: bool) -> TCResult<TypedStatementBlock> {
		if new_scope {
			self.push_scope(already_type_scoped);
		}

		let statements = block.0.into_iter()
			.map(|stmt| self.check_statement(stmt))
			.collect::<TCResult<Vec<Option<TypedStatement>>>>()?
			.into_iter().filter_map(identity)
			.collect::<Vec<TypedStatement>>();

		let return_type = statements.last().map(|stmt| match stmt {
			TypedStatement::Let { .. } |
			TypedStatement::Function(_) |
			TypedStatement::Return(None) |
			TypedStatement::While { .. } => TypeId::Void,
			TypedStatement::Expression { has_semicolon: true, .. } => TypeId::Void,
			TypedStatement::Return(Some(e)) => if new_scope {
				get_type(e)
			} else {
				TypeId::Return(Box::new(get_type(e)))
			},
			TypedStatement::Expression { expr, .. } => get_type(expr),
		}).unwrap_or(TypeId::Void);

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
					TypeId::Void |
					TypeId::Return(_) |
					TypeId::Any => return Err(TypeCheckError::Generic(format!("cannot assign void or never type to a variable"))),
					type_id => type_id,
				};
				self.type_env.define_identifier(&name, value_type.clone());

				Ok(Some(TypedStatement::Let {
					name,
					value,
					type_id: value_type,
				}))
			}
			Statement::Return(None) => {
				self.check_scope_return_type(&TypeId::Void)?;

				Ok(Some(TypedStatement::Return(None)))
			}
			Statement::Return(Some(value)) => {
				let (returned, returned_type) = self.check_expression(value)?;

				self.check_scope_return_type(&returned_type)?;

				Ok(Some(TypedStatement::Return(Some(returned))))
			}
			Statement::Function(func) => {
				let (func, func_type) = self.check_function(func, true)?;

				if func.is_method {
					self.type_env.define_identifier(
						func.name.as_ref().unwrap(),
						func_type,
					);
				}

				Ok(Some(TypedStatement::Function(func)))
			}
			Statement::Struct { name, decl } => {
				self.check_struct_decl(name, decl)?;
				Ok(None)
			}
			Statement::While { condition, block } => {
				let (condition, condition_type) = self.check_expression(condition)?;
				if !matches!(condition_type, TypeId::Bool) {
					return Err(TypeCheckError::Generic(format!("expected `bool`, found {:?}", condition_type)));
				}

				let block = self.check_block(block, false, false)?;

				Ok(Some(TypedStatement::While {
					condition,
					block,
				}))
			}
		}
	}

	pub fn check_expression(&mut self, expr: Expression) -> TCResult<(TypedExpression, TypeId)> {
		match expr {
			Expression::Boolean(value) => Ok((TypedExpression::Boolean(value), TypeId::Bool)),
			Expression::Integer(IntExpr::U8(value)) => Ok((TypedExpression::Integer(IntExpr::U8(value)), TypeId::U8)),
			Expression::Integer(IntExpr::U16(value)) => Ok((TypedExpression::Integer(IntExpr::U16(value)), TypeId::U16)),
			Expression::Integer(IntExpr::U32(value)) => Ok((TypedExpression::Integer(IntExpr::U32(value)), TypeId::U32)),
			Expression::Integer(IntExpr::U64(value)) => Ok((TypedExpression::Integer(IntExpr::U64(value)), TypeId::U64)),
			Expression::Integer(IntExpr::I8(value)) => Ok((TypedExpression::Integer(IntExpr::I8(value)), TypeId::I8)),
			Expression::Integer(IntExpr::I16(value)) => Ok((TypedExpression::Integer(IntExpr::I16(value)), TypeId::I16)),
			Expression::Integer(IntExpr::I32(value)) => Ok((TypedExpression::Integer(IntExpr::I32(value)), TypeId::I32)),
			Expression::Integer(IntExpr::I64(value)) => Ok((TypedExpression::Integer(IntExpr::I64(value)), TypeId::I64)),
			Expression::Float(value) => Ok((TypedExpression::Float(value), TypeId::Float)),
			Expression::String(value) => Ok((TypedExpression::String(value), TypeId::String)),
			Expression::Identifier(name) => {
				let type_id = self.type_env.get_identifier_type(name.as_str())
					.or_else(|| self.type_env.get_custom_type(&name))
					.cloned()
					.ok_or_else(|| TypeCheckError::UnknownVariable(name.clone()))?;

				Ok((TypedExpression::Identifier {
					name,
					type_id: type_id.clone(),
				}, type_id))
			}
			Expression::Prefix { operator, right } => {
				let (right, right_type) = self.check_expression(*right)?;

				let type_id = match operator {
					PrefixOperator::Minus => match right_type {
						TypeId::U8 |
						TypeId::I8 => TypeId::I8,
						TypeId::U16 |
						TypeId::I16 => TypeId::I16,
						TypeId::U32 |
						TypeId::I32 => TypeId::I32,
						TypeId::U64 |
						TypeId::I64 => TypeId::I64,
						_ => {
							return Err(TypeCheckError::PrefixTypeMismatch {
								operator,
								right_type,
							});
						}
					},
					PrefixOperator::Bang => if right_type != TypeId::Bool {
						return Err(TypeCheckError::PrefixTypeMismatch {
							operator,
							right_type,
						});
					} else {
						right_type
					},
				};

				Ok((TypedExpression::Prefix {
					operator,
					right: Box::new(right),
				}, type_id))
			}
			Expression::Infix { left, operator, right } => {
				let (left, left_type) = self.check_expression(*left)?;
				let (right, right_type) = self.check_expression(*right)?;

				let type_id = check_infix(&operator, &left, left_type.clone(), &right, right_type)?;

				Ok((TypedExpression::Infix {
					left: Box::new(left),
					operator,
					right: Box::new(right),
					operand_type: left_type,
					type_id: type_id.clone(),
				}, type_id))
			}
			Expression::If { condition, consequence, alternative } => {
				let (condition, condition_type) = self.check_expression(*condition)?;
				if !matches!(condition_type, TypeId::Bool) {
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
					(TypeId::Return(_), _) |
					(_, Some(TypeId::Return(_))) |
					(TypeId::Void, None) => {}
					(cons_type, None | Some(TypeId::Void)) => {
						//TODO expected `()`, found `{:?}`
						return Err(TypeCheckError::Generic(format!("mismatched if types {:?} vs None", cons_type)));
					}
					(cons_type, Some(t)) => {
						if cons_type != t {
							return Err(TypeCheckError::Generic(format!("mismatched if types {:?} vs {:?}", cons_type, t)));
						}
					}
				}

				let type_id: TypeId = match (&cons_type, alt_type) {
					(TypeId::Return(cons_type), Some(TypeId::Return(_))) => cons_type,
					(TypeId::Return(_), Some(alt_type)) => alt_type,
					(cons_type, _) => cons_type,
				}.clone();

				Ok((TypedExpression::If {
					condition: Box::new(condition),
					type_id: type_id.clone(),
					consequence,
					alternative,
				}, type_id))
			}
			Expression::Function(func) => {
				let (func, type_id) = self.check_function(func, false)?;

				Ok((TypedExpression::Function(func), type_id))
			}
			Expression::Call { function, arguments } => {
				let (function, function_type) = self.check_expression(*function)?;

				let (arguments, argument_types): (Vec<TypedExpression>, Vec<TypeId>) = arguments.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<(TypedExpression, TypeId)>>>()?
					.into_iter()
					.unzip();

				let return_type = match function_type {
					TypeId::Function { parameters: parameter_types, return_type, is_method } => {
						if let TypedExpression::Field { .. } = &function {
							if !is_method {
								return Err(TypeCheckError::Generic(format!("{} isn't a method", function)))
							}
						}else {
							if arguments.len() != parameter_types.len() {
								return Err(TypeCheckError::CallArgCount {
									parameter_count: parameter_types.len() as u8,
									argument_count: arguments.len() as u8,
								});
							}

							let mut concrete_params = vec![];
							for (arg, param) in argument_types.iter().zip(parameter_types.iter()) {
								match param {
									TypeId::Array(elements) => if let TypeId::Any = elements.as_ref() {
										continue;
									}
									TypeId::Any => continue,
									_ => {}
								}
								if !type_id_eq(arg, param, &mut concrete_params) {
									return Err(TypeCheckError::CallArgTypeMismatch {
										parameter_types,
										argument_types,
									});
								}
							}
						}

						*return_type
					}
					TypeId::Struct(index) => {
						let info = &self.type_env.get_type_info(index).unwrap();
						let type_expr = self.type_env.get_type(&info.id)
							.cloned()
							.ok_or_else(|| TypeCheckError::UnknownType(info.custom_name.as_ref().unwrap().clone()))?;

						if let TypeExpr::Struct { fields, .. } = type_expr {
							if arguments.len() != fields.len() {
								return Err(TypeCheckError::CallArgCount {
									parameter_count: fields.len() as u8,
									argument_count: arguments.len() as u8,
								});
							}

							for (arg, binding) in argument_types.iter().zip(fields.iter()) {
								match &binding.type_id {
									TypeId::Array(elements) => if let TypeId::Any = elements.as_ref() {
										continue;
									}
									TypeId::Any => continue,
									_ => {}
								}
								if arg != &binding.type_id {
									return Err(TypeCheckError::CallArgTypeMismatch {
										parameter_types: fields.iter().map(|b| b.type_id.clone()).collect(),
										argument_types,
									});
								}
							}

							TypeId::Struct(index)
						} else {
							return Err(TypeCheckError::Generic(format!("type {:?} not callable", type_expr)));
						}
					}
					TypeId::Void => return Err(TypeCheckError::Generic("cannot call void type".into())),
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

				match &left_type {
					TypeId::Struct(index) => {
						let info = &self.type_env.get_type_info(*index).unwrap();
						let type_expr = self.type_env.get_type(&info.id)
							.cloned()
							.ok_or_else(|| TypeCheckError::UnknownType(info.custom_name.as_ref().unwrap().clone()))?;

						if let TypeExpr::Struct { fields, .. } = type_expr {
							if let Some(binding_index) = fields.iter().position(|t| t.ident == field) {
								let type_id = fields[binding_index].type_id.clone();

								Ok((TypedExpression::Field {
									left: Box::new(left),
									left_type,
									binding_index: Some(binding_index),
									field,
									field_type: type_id.clone(),
								}, type_id))
							} else if let Some(method_id) = self.type_env.get_method(&field, &left_type) {
								Ok((TypedExpression::Field {
									left: Box::new(left),
									left_type,
									binding_index: None,
									field,
									field_type: method_id.clone(),
								}, method_id))
							} else {
								Err(TypeCheckError::UnknownField { left, field })
							}
						} else {
							return Err(TypeCheckError::Generic(format!("For now can't use dot notation on non struct. left={:?}", left)))
						}
					}
					type_id => {
						if let Some(method_id) = self.type_env.get_method(&field, type_id) {
							Ok((TypedExpression::Field {
								left: Box::new(left),
								left_type,
								binding_index: None,
								field,
								field_type: method_id.clone(),
							}, method_id))
						}else {
							return Err(TypeCheckError::UnknownField {
								left,
								field
							})
						}
					}
				}
			}
			Expression::Array(elements) => {
				let (elements, element_types): (Vec<TypedExpression>, Vec<TypeId>) = elements.into_iter()
					.map(|arg| self.check_expression(arg))
					.collect::<TCResult<Vec<(TypedExpression, TypeId)>>>()?
					.into_iter()
					.unzip();

				let type_id = match element_types.first() {
					None | Some(TypeId::Void) => return Err(TypeCheckError::EmptyArray),
					Some(t) => t.clone(),
				};

				for typ in element_types.iter() {
					match typ {
						TypeId::Void => return Err(TypeCheckError::VoidArrayElem(element_types)),
						t => if t != &type_id {
							return Err(TypeCheckError::ArrayTypeMismatch(element_types));
						}
					}
				}

				Ok((
					TypedExpression::Array { elements, type_id: type_id.clone() },
					TypeId::Array(Box::new(type_id))
				))
			}
			Expression::Index { left, index } => {
				let (left, left_type) = self.check_expression(*left)?;
				let (index, _) = self.check_expression(*index)?;
				let type_id = match left_type {
					TypeId::Array(elements_type) => {
						*elements_type.clone()
					}
					_ => panic!("{:?} isn't indexable (should be handled in check_expression())", left)
				};

				Ok((TypedExpression::Index {
					left: Box::new(left),
					index: Box::new(index),
				}, type_id))
			}
			Expression::Block { statements, return_transparent } => {
				let block = self.check_block(statements, !return_transparent, false)?;
				let type_id = block.return_type.clone();

				Ok((TypedExpression::Block {
					block,
					return_transparent,
				}, type_id))
			}
			Expression::Struct { name, constructor } => {
				match self.type_env.get_custom_type(&name).cloned() {
					Some(type_id) => {
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

						Ok((TypedExpression::Struct { name, fields, type_id: type_id.clone() }, type_id))
					}
					None => Err(TypeCheckError::UnknownType(name))
				}
			}
			Expression::Assignment { ident, new_value } => {
				let type_id = self.type_env.get_identifier_type(ident.as_str())
					.or_else(|| self.type_env.get_custom_type(&ident))
					.cloned()
					.ok_or_else(|| TypeCheckError::UnknownVariable(ident.clone()))?;
				//TODO Check mutability

				let (new_value, new_type_id) = self.check_expression(*new_value)?;
				if type_id != new_type_id {
					return Err(TypeCheckError::AssignMismatch {
						variable: type_id,
						new_value: new_type_id,
					});
				}

				Ok((TypedExpression::Assignment {
					ident,
					new_value: Box::new(new_value),
					type_id: type_id.clone(),
				}, type_id))
			}
		}
	}

	fn check_function(&mut self, function: Function, global: bool) -> TCResult<(TypedFunction, TypeId)> {
		let return_type = self.check_parsed_type(function.return_type)?;
		let parameters = function.parameters.iter()
			.map(|p| self.check_parsed_type(p.1.clone()))
			.collect::<TCResult<Vec<TypeId>>>()?;

		if global {
			self.register_function_type(
				function.name.as_ref(),
				parameters.clone(),
				return_type.clone(),
				function.is_method,
			);
		}

		self.type_env.push_scope();

		for (param, param_type) in function.parameters.iter() {
			let type_id = self.check_parsed_type(param_type.clone())?;
			self.type_env.define_identifier(&param, type_id);
		}

		if !global {
			self.register_function_type(
				function.name.as_ref(),
				parameters.clone(),
				return_type.clone(),
				function.is_method,
			);
		}

		let body = self.check_block(function.body, true, true)?;

		let body_return_type = match &body.return_type {
			TypeId::Return(returned_type) => returned_type.as_ref(),
			t => t,
		};
		if &return_type != body_return_type {
			return Err(TypeCheckError::Generic(format!(
				"function body return type {:?} doesn't match declared return type {:?}",
				body_return_type,
				return_type
			)));
		}

		let type_id = TypeId::Function {
			parameters: parameters.clone(),
			return_type: Box::new(return_type.clone()),
			is_method: function.is_method,
		};

		Ok((TypedFunction {
			name: function.name,
			parameters: function.parameters.iter().zip(parameters.into_iter())
				.map(|((param, _), param_type)| (param.clone(), param_type))
				.collect(),
			body,
			return_type,
			is_method: function.is_method,
		},
			type_id))
	}

	fn register_function_type(&mut self, name: Option<&String>, parameter_types: Vec<TypeId>, return_type: TypeId, is_method: bool) {
		if let Some(name) = name {
			self.type_env.define_identifier(name, TypeId::Function {
				parameters: parameter_types,
				return_type: Box::new(return_type.clone()),
				is_method,
			});
		}
	}

	fn check_scope_return_type(&mut self, new_return_type: &TypeId) -> TCResult<()> {
		let current = self.scope_return_types.last_mut().unwrap();
		let new_return_type = match new_return_type {
			TypeId::Return(returned) => returned.as_ref(),
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
			fields: match &decl {
				StructDecl::Block(fields) => {
					let types = fields.iter()
						.map(|(_, t)| self.check_parsed_type(t.clone()))
						.collect::<TCResult<Vec<TypeId>>>()?;
					fields.iter().zip(types.into_iter())
						.map(|(f, type_id)|
							TypeBinding {
								ident: f.0.clone(),
								type_id,
							}).collect()
				}
				StructDecl::Tuple(fields) => fields.iter()
					.map(|t| self.check_parsed_type(t.clone())).collect::<TCResult<Vec<TypeId>>>()?
					.into_iter().enumerate()
					.map(|(i, type_id)|
						TypeBinding {
							ident: i.to_string(),
							type_id,
						}
					).collect(),
			},
		};
		self.type_env.define_type(name, type_expr)?;

		Ok(())
	}

	fn check_parsed_type(&mut self, parsed_type: ParsedType) -> TCResult<TypeId> {
		match parsed_type {
			ParsedType::Primitive(t) => Ok(self.type_env.get_id(&t).unwrap()),
			ParsedType::FnLiteral { parameter_types, return_type } => Ok(TypeId::Function {
				parameters: parameter_types.into_iter()
					.map(|t| self.check_parsed_type(t))
					.collect::<TCResult<Vec<TypeId>>>()?,
				return_type: Box::new(self.check_parsed_type(*return_type)?),
				is_method: false,
			}),
			ParsedType::Custom(name) => match self.type_env.get_custom_type(&name) {
				Some(t) => Ok(t.clone()),
				None => Err(TypeCheckError::UnknownType(name)),
			}
		}
	}

	pub fn push_scope(&mut self, already_type_scoped: bool) {
		self.scope_return_types.push(None);

		//Kind of hard-coded bool to include function's parameters in the scope
		if !already_type_scoped {
			self.type_env.push_scope();
		}
	}
}

pub fn get_type(expr: &TypedExpression) -> TypeId {
	match expr {
		TypedExpression::Identifier { type_id, .. } => type_id.clone(),
		TypedExpression::Integer(v) => match v {
			IntExpr::U8(_) => TypeId::U8,
			IntExpr::U16(_) => TypeId::U16,
			IntExpr::U32(_) => TypeId::U32,
			IntExpr::U64(_) => TypeId::U64,
			IntExpr::I8(_) => TypeId::I8,
			IntExpr::I16(_) => TypeId::I16,
			IntExpr::I32(_) => TypeId::I32,
			IntExpr::I64(_) => TypeId::I64,
		},
		TypedExpression::Float(_) => TypeId::Float,
		TypedExpression::Boolean(_) => TypeId::Bool,
		TypedExpression::String(_) => TypeId::String,
		TypedExpression::Function(TypedFunction { parameters, body, is_method, .. }) => TypeId::Function {
			parameters: parameters.iter().map(|p| p.1.clone()).collect(),
			return_type: Box::new(body.return_type.clone()),
			is_method: *is_method,
		},
		TypedExpression::Prefix {
			operator: PrefixOperator::Minus,
			right
		} => match get_type(right.as_ref()) {
			TypeId::U8 => TypeId::I8,
			TypeId::U16 => TypeId::I16,
			TypeId::U32 => TypeId::I32,
			TypeId::U64 => TypeId::I64,
			t => t,
		},
		TypedExpression::Prefix {
			operator: PrefixOperator::Bang,
			right
		} => get_type(right.as_ref()),
		TypedExpression::Infix { type_id, .. } => type_id.clone(),
		TypedExpression::If { type_id, .. } => type_id.clone(),
		TypedExpression::Call { return_type, .. } => return_type.clone(),
		TypedExpression::Field { field_type, .. } => field_type.clone(),
		TypedExpression::Array { type_id, .. } => TypeId::Array(Box::new(type_id.clone())),
		TypedExpression::Index { left, .. } => {
			let left_type = get_type(left.as_ref());
			match left_type {
				TypeId::Array(elements_type) => {
					*elements_type.clone()
				}
				_ => panic!("{:?} isn't indexable (should be handled in check_expression())", left)
			}
		}
		TypedExpression::Block { block, .. } => block.return_type.clone(),
		TypedExpression::Struct { type_id, .. } => type_id.clone(),
		TypedExpression::Assignment { type_id, .. } => type_id.clone(),
	}
}

fn check_infix(operator: &InfixOperator, left: &TypedExpression, left_type: TypeId, right: &TypedExpression, right_type: TypeId) -> TCResult<TypeId> {
	match (&left_type, right_type) {
		(TypeId::Void, _) | (_, TypeId::Void) => return Err(TypeCheckError::Generic(format!("cannot include void type in infix operator ({:?} and {:?})", left, right))),
		//(Some(TypeId::Int { .. }), Some(TypeId::Int { .. })) => {}
		(left, right) => if left != &right {
			return Err(TypeCheckError::Generic(format!("incompatible types ({:?} and {:?})", left, right)));
		}
	}

	match operator {
		InfixOperator::LessThan |
		InfixOperator::GreaterThan |
		InfixOperator::Equal |
		InfixOperator::Unequal => Ok(TypeId::Bool),
		_ => Ok(left_type)
	}
}

type TCResult<T> = Result<T, TypeCheckError>;

#[derive(Debug, PartialEq, Clone)]
pub enum TypeCheckError {
	PrefixTypeMismatch {
		operator: PrefixOperator,
		right_type: TypeId,
	},
	IndexTypeMismatch {
		indexed_type: TypeId,
		index_type: TypeId,
	},
	IndexedTypeMismatch {
		indexed_type: TypeId,
		index_type: TypeId,
	},
	AssignMismatch {
		variable: TypeId,
		new_value: TypeId,
	},
	EmptyArray,
	VoidArrayElem(Vec<TypeId>),
	ArrayTypeMismatch(Vec<TypeId>),
	CallArgCount {
		parameter_count: u8,
		argument_count: u8,
	},
	CallArgTypeMismatch {
		parameter_types: Vec<TypeId>,
		argument_types: Vec<TypeId>,
	},
	ReturnTypeMismatch {
		scope_return_type: TypeId,
		mismatched_type: TypeId,
	},
	UnknownType(String),
	UnknownField {
		left: TypedExpression,
		field: String,
	},
	UnknownVariable(String),
	Generic(String),
}

impl std::fmt::Display for TypeCheckError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			TypeCheckError::PrefixTypeMismatch { .. } => write!(f, "type check PrefixTypeMismatch error"),
			TypeCheckError::IndexTypeMismatch { .. } => write!(f, "type check IndexTypeMismatch error"),
			TypeCheckError::IndexedTypeMismatch { .. } => write!(f, "type check IndexedTypeMismatch error"),
			TypeCheckError::AssignMismatch { .. } => write!(f, "type check AssignMismatch error"),
			TypeCheckError::EmptyArray => write!(f, "type check EmptyArray error"),
			TypeCheckError::VoidArrayElem(_) => write!(f, "type check VoidArrayElem error"),
			TypeCheckError::ArrayTypeMismatch(_) => write!(f, "type check ArrayTypeMismatch error"),
			TypeCheckError::CallArgCount { .. } => write!(f, "type check CallArgCount error"),
			TypeCheckError::CallArgTypeMismatch { .. } => write!(f, "type check CallArgTypeMismatch error"),
			TypeCheckError::ReturnTypeMismatch { .. } => write!(f, "type check ReturnTypeMismatch error"),
			TypeCheckError::UnknownType(_) => write!(f, "type check UnknownType error"),
			TypeCheckError::UnknownField { .. } => write!(f, "type check UnknownField error"),
			TypeCheckError::UnknownVariable { .. } => write!(f, "type check UnknownVariable error"),
			TypeCheckError::Generic(_) => write!(f, "type check Generic error"),
		}
	}
}