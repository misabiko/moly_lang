use std::fmt;
use crate::type_checker::type_env::TypeExpr;

pub type Program = BlockStatement;

#[derive(Debug)]
pub struct BlockStatement {
	pub statements: Vec<Statement>
}

impl fmt::Display for BlockStatement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}

impl PartialEq for BlockStatement {
	fn eq(&self, other: &Self) -> bool {
		self.to_string() == other.to_string()
	}
}

#[derive(Debug, PartialEq)]
pub enum Statement {
	Let {
		name: String,
		value: Expression,
	},
	Return(Option<Expression>),
	Expression {
		expr: Expression,
		has_semicolon: bool
	},
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
			Statement::Return(Some(value)) => write!(f, "return {};", value),
			Statement::Return(None) => write!(f, "return;"),
			Statement::Expression { expr, has_semicolon }
				=> write!(f, "{}{}", expr, if *has_semicolon { ";" } else { "" }),
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum Expression {
	Identifier(String),
	Integer(IntExpr),
	Boolean(bool),
	String(String),
	Prefix {
		operator: PrefixOperator,
		right: Box<Expression>,
	},
	Infix {
		left: Box<Expression>,
		operator: InfixOperator,
		right: Box<Expression>,
	},
	If {
		condition: Box<Expression>,
		consequence: BlockStatement,
		alternative: Option<BlockStatement>,
	},
	Function {
		/// name in Expression::Identifier
		parameters: Vec<(String, TypeExpr)>,
		body: BlockStatement,
		name: Option<String>,
		return_type: TypeExpr,
	},
	Call {
		function: Box<Expression>,
		arguments: Vec<Expression>,
	},
	Array(Vec<Expression>),
	Index {
		left: Box<Expression>,
		index: Box<Expression>,
	},
	Hash(Vec<(Expression, Expression)>),
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::Identifier(value) => write!(f, "{}", value),
			Expression::Integer(IntExpr::U8(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::U16(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::U32(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::U64(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::I8(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::I16(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::I32(value)) => write!(f, "{}", value),
			Expression::Integer(IntExpr::I64(value)) => write!(f, "{}", value),
			Expression::Boolean(value) => write!(f, "{}", value),
			Expression::String(value) => write!(f, "{}", value),
			Expression::Prefix { operator, right } => write!(f, "({}{})", operator, right),
			Expression::Infix { left, operator, right } => write!(f, "({} {} {})", left, operator, right),
			Expression::If { condition, consequence, alternative } => {
				let alt_str = if let Some(alternative) = alternative {
					format!(" else {}", alternative)
				}else {
					"".into()
				};

				write!(f, "if {} {}{})", condition, consequence, alt_str)
			},
			Expression::Function { parameters, body, name, return_type } => {
				let name = if let Some(name) = name {
					format!("<{}>", name)
				}else {
					"".into()
				};
				let parameters = parameters.iter()
					.map(|(i, t)| format!("{} {}", i, t))
					.collect::<Vec<String>>()
					.join(", ");
				let return_type = match return_type {
					TypeExpr::Void => "".into(),
					r => format!(" {}", r),
				};

				write!(f, "fn{}({}) {} {}", name, parameters, return_type, body)
			},
			Expression::Call { function, arguments } => write!(f, "{}({})", function, join_expression_vec(arguments)),
			Expression::Array(elements) => write!(f, "[{}]", join_expression_vec(elements)),
			Expression::Index { left, index } => write!(f, "({}[{}])", left, index),
			Expression::Hash(pairs) => {
				let pairs = pairs.iter()
					.map(|(k, v)| format!("{}:{}", k, v))
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "{{{}}}", pairs)
			},
		}
	}
}

fn join_expression_vec(expressions: &[Expression]) -> String {
	expressions.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
	Minus,
	Bang,
}

impl fmt::Display for PrefixOperator {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			PrefixOperator::Minus => write!(f, "-"),
			PrefixOperator::Bang => write!(f, "!"),
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum InfixOperator {
	Plus,
	Minus,
	Mul,
	Div,
	//TODO Modulo,

	Equal,
	Unequal,
	LessThan,
	GreaterThan,
	//TODO LesserEqual,
	//TODO GreaterEqual,
}

impl fmt::Display for InfixOperator {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			InfixOperator::Plus => write!(f, "+"),
			InfixOperator::Minus => write!(f, "-"),
			InfixOperator::Mul => write!(f, "*"),
			InfixOperator::Div => write!(f, "/"),
			//InfixOperator::Modulo => write!(f, "%"),

			InfixOperator::Equal => write!(f, "=="),
			InfixOperator::Unequal => write!(f, "!="),
			InfixOperator::LessThan => write!(f, "<"),
			InfixOperator::GreaterThan => write!(f, ">"),
			//InfixOperator::LesserEqual => write!(f, "<="),
			//InfixOperator::GreaterEqual => write!(f, ">="),
		}
	}
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum IntExpr {
	U8(u8),
	U16(u16),
	U32(u32),
	U64(u64),
	I8(i8),
	I16(i16),
	I32(i32),
	I64(i64),
}

impl fmt::Display for IntExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			IntExpr::U8(v) => write!(f, "{}", v),
			IntExpr::U16(v) => write!(f, "{}", v),
			IntExpr::U32(v) => write!(f, "{}", v),
			IntExpr::U64(v) => write!(f, "{}", v),
			IntExpr::I8(v) => write!(f, "{}", v),
			IntExpr::I16(v) => write!(f, "{}", v),
			IntExpr::I32(v) => write!(f, "{}", v),
			IntExpr::I64(v) => write!(f, "{}", v),
		}
	}
}