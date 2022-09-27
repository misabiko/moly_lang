use std::fmt;
use crate::ast::{InfixOperator, IntExpr, PrefixOperator};
use super::type_env::TypeExpr;

pub type TypedProgram = TypedBlockStatement;

#[derive(Debug)]
pub struct TypedBlockStatement {
	pub statements: Vec<TypedStatement>
}

impl fmt::Display for TypedBlockStatement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}

impl PartialEq for TypedBlockStatement {
	fn eq(&self, other: &Self) -> bool {
		self.to_string() == other.to_string()
	}
}

#[derive(Debug, PartialEq)]
pub enum TypedStatement {
	Let {
		name: String,
		value: TypedExpression,
	},
	Return(Option<TypedExpression>),
	Expression {
		expr: TypedExpression,
		has_semicolon: bool,
	},
}

impl fmt::Display for TypedStatement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			TypedStatement::Let { name, value } => write!(f, "let {} = {};", name, value),
			TypedStatement::Return(Some(value)) => write!(f, "return {};", value),
			TypedStatement::Return(None) => write!(f, "return;"),
			TypedStatement::Expression { expr, has_semicolon }
				=> write!(f, "{}{}", expr, if *has_semicolon { ";" } else { "" }),
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum TypedExpression {
	Identifier {
		name: String,
		type_expr: TypeExpr,
	},
	Integer(IntExpr),
	Boolean(bool),
	String(String),
	Prefix {
		operator: PrefixOperator,
		right: Box<TypedExpression>,
	},
	Infix {
		left: Box<TypedExpression>,
		operator: InfixOperator,
		right: Box<TypedExpression>,
		type_expr: TypeExpr,
	},
	If {
		condition: Box<TypedExpression>,
		type_expr: Option<TypeExpr>,
		consequence: TypedBlockStatement,
		alternative: Option<TypedBlockStatement>,
	},
	Function {
		parameters: Vec<(String, TypeExpr)>,
		body: TypedBlockStatement,
		name: Option<String>,
		return_type: Option<TypeExpr>,
	},
	Call {
		function: Box<TypedExpression>,
		arguments: Vec<TypedExpression>,
		return_type: Option<TypeExpr>,
	},
	Array(Vec<TypedExpression>),
	Index {
		left: Box<TypedExpression>,
		index: Box<TypedExpression>,
	},
	Hash(Vec<(TypedExpression, TypedExpression)>),
}

impl fmt::Display for TypedExpression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			TypedExpression::Identifier {name, .. } => write!(f, "{}", name),
			TypedExpression::Integer(value) => write!(f, "{}", value),
			TypedExpression::Boolean(value) => write!(f, "{}", value),
			TypedExpression::String(value) => write!(f, "{}", value),
			TypedExpression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
			TypedExpression::Infix { left, operator, right, .. } => write!(f, "({} {} {})", left, operator, right),
			TypedExpression::If { condition, consequence, alternative, .. } => {
				let alt_str = if let Some(alternative) = alternative {
					format!(" else {}", alternative)
				}else {
					"".into()
				};

				write!(f, "if {} {}{})", condition, consequence, alt_str)
			},
			TypedExpression::Function { parameters, body, name, return_type } => {
				let name = if let Some(name) = name {
					format!("<{}>", name)
				}else {
					"".into()
				};
				let parameters = parameters.iter()
					.map(|(i, t)| format!("{} {}", i, t))
					.collect::<Vec<String>>()
					.join(", ");
				let return_type = if let Some(r) = return_type {
					format!(" {}", r)
				}else {
					"".into()
				};

				write!(f, "fn{}({}) {} {}", name, parameters, return_type, body)
			},
			TypedExpression::Call { function, arguments, .. } => write!(f, "{}({})", function, join_expression_vec(arguments)),
			TypedExpression::Array(elements) => write!(f, "[{}]", join_expression_vec(elements)),
			TypedExpression::Index { left, index } => write!(f, "({}[{}])", left, index),
			TypedExpression::Hash(pairs) => {
				let pairs = pairs.iter()
					.map(|(k, v)| format!("{}:{}", k, v))
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "{{{}}}", pairs)
			},
		}
	}
}

fn join_expression_vec(expressions: &[TypedExpression]) -> String {
	expressions.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}