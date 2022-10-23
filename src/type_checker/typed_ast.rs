use std::fmt;
use crate::ast::{InfixOperator, IntExpr, PrefixOperator};
use super::type_env::TypeExpr;

#[derive(Debug, PartialEq)]
pub struct TypedProgram(pub Vec<TypedStatement>);

impl fmt::Display for TypedProgram {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.0 {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}

#[derive(Debug, PartialEq)]
pub struct TypedStatementBlock {
	pub statements: Vec<TypedStatement>,
	pub return_type: TypeExpr,
}

impl fmt::Display for TypedStatementBlock {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt)?;
		}
		Ok(())
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
	/// TypedFunction::name should be Some
	Function(TypedFunction),
}

impl fmt::Display for TypedStatement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			TypedStatement::Let { name, value } => write!(f, "let {} = {};", name, value),
			TypedStatement::Return(Some(value)) => write!(f, "return {};", value),
			TypedStatement::Return(None) => write!(f, "return;"),
			TypedStatement::Expression { expr, has_semicolon }
				=> write!(f, "{}{}", expr, if *has_semicolon { ";" } else { "" }),
			TypedStatement::Function(func) => write!(f, "{}", func),
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
		type_expr: TypeExpr,
		consequence: TypedStatementBlock,
		alternative: Option<TypedStatementBlock>,
	},
	Function(TypedFunction),
	Call {
		function: Box<TypedExpression>,
		arguments: Vec<TypedExpression>,
		return_type: TypeExpr,
	},
	Field {
		left: Box<TypedExpression>,
		field: String,
		left_type: TypeExpr,
		field_type: TypeExpr,
		binding_index: usize,
	},
	Array {
		elements: Vec<TypedExpression>,
		type_expr: TypeExpr,
	},
	Index {
		left: Box<TypedExpression>,
		index: Box<TypedExpression>,
	},
	Block {
		block: TypedStatementBlock,
		return_transparent: bool,
	},
	Struct {
		name: String,
		fields: Vec<(String, TypedExpression)>,
		type_expr: TypeExpr,
	}
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
			TypedExpression::Function(func) => write!(f, "{}", func),
			TypedExpression::Call { function, arguments, .. } => write!(f, "{}({})", function, join_expression_vec(arguments)),
			TypedExpression::Field { left, field, .. } => write!(f, "{}.{}", left, field),
			TypedExpression::Array { elements, .. } => write!(f, "[{}]", join_expression_vec(elements)),
			TypedExpression::Index { left, index } => write!(f, "({}[{}])", left, index),
			TypedExpression::Block { block, .. } => write!(f, "{{{}}}", block),
			TypedExpression::Struct { name, fields, .. } => {
				//Could take note of block vs tuple
				let fields = fields.iter()
					.map(|(name, typ)| format!("{}: {}", name, typ))
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "{} {{ {} }}", name, fields)
			}
		}
	}
}

fn join_expression_vec(expressions: &[TypedExpression]) -> String {
	expressions.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}

#[derive(Debug, PartialEq)]
pub struct TypedFunction {
	pub parameters: Vec<(String, TypeExpr)>,
	pub body: TypedStatementBlock,
	pub name: Option<String>,
}

impl fmt::Display for TypedFunction {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let name = if let Some(name) = &self.name {
			format!("<{}>", name)
		}else {
			"".into()
		};
		let parameters = &self.parameters.iter()
			.map(|(i, t)| format!("{} {}", i, t))
			.collect::<Vec<String>>()
			.join(", ");
		let return_type = match &self.body.return_type {
			TypeExpr::Void => "".into(),
			r => format!(" {}", r),
		};

		write!(f, "fn{}({}) {} {}", name, parameters, return_type, self.body)
	}
}