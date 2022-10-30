use std::fmt;
use crate::token::Token;
use crate::type_checker::type_env::TypeExpr;

pub type Program = StatementBlock;

#[derive(Debug, PartialEq, Clone)]
pub struct StatementBlock(pub Vec<Statement>);

impl fmt::Display for StatementBlock {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.0 {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}

#[derive(Debug, PartialEq, Clone)]
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
	/// Function::name should be Some, might split expr and stmt function later
	Function(Function),
	Struct {
		name: String,
		decl: StructDecl,
	},
	While {
		condition: Expression,
		block: StatementBlock,
	}
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
			Statement::Return(Some(value)) => write!(f, "return {};", value),
			Statement::Return(None) => write!(f, "return;"),
			Statement::Expression { expr, has_semicolon }
				=> write!(f, "{}{}", expr, if *has_semicolon { ";" } else { "" }),
			Statement::Function(func) => write!(f, "{}", func),
			Statement::Struct { name, decl } => match decl {
				StructDecl::Block(fields) => {
					let fields = fields.iter()
						.map(|(name, typ)| format!("{} {}", name, typ))
						.collect::<Vec<String>>()
						.join(", ");

					write!(f, "struct {} {{ {} }}", name, fields)
				},
				StructDecl::Tuple(fields) => write!(f, "struct {}({})", name, join_type_vec(fields))
			}
			Statement::While { condition, block} => write!(f, "while {} {})", condition, block),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
	Identifier(String),
	Integer(IntExpr),
	Float(f32),
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
		consequence: StatementBlock,
		alternative: Option<StatementBlock>,
	},
	Function(Function),
	Call {
		function: Box<Expression>,
		arguments: Vec<Expression>,
	},
	Field {
		left: Box<Expression>,
		field: String,
	},
	Array(Vec<Expression>),
	Index {
		left: Box<Expression>,
		index: Box<Expression>,
	},
	Block {
		statements: StatementBlock,
		/// true if return statement go affect the outer block instead (e.g. if)
		return_transparent: bool,
	},
	Struct {
		name: String,
		constructor: StructConstructor,
	},
	//Would like to make it a statement
	Assignment {
		ident: String,
		new_value: Box<Expression>,
	}
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
			Expression::Float(value) => write!(f, "{}", value),
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
			Expression::Function(func) => write!(f, "{}", func),
			Expression::Call { function, arguments } => write!(f, "{}({})", function, join_expression_vec(arguments)),
			Expression::Field { left, field } => write!(f, "{}.{}", left, field),
			Expression::Array(elements) => write!(f, "[{}]", join_expression_vec(elements)),
			Expression::Index { left, index } => write!(f, "({}[{}])", left, index),
			Expression::Block { statements, .. } => write!(f, "{{{}}}", statements),
			Expression::Struct { name, constructor: StructConstructor::Block(fields) } => {
				let fields = fields.iter()
					.map(|(name, typ)| format!("{}: {}", name, typ))
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "{} {{ {} }}", name, fields)
			}
			Expression::Struct { name, constructor: StructConstructor::Tuple(fields) }
				=> write!(f, "{} ({})", name, join_expression_vec(fields)),
			Expression::Assignment { ident, new_value } => write!(f, "{} = {}", ident, new_value),
		}
	}
}

fn join_expression_vec(expressions: &[Expression]) -> String {
	expressions.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}

fn join_type_vec(expressions: &[ParsedType]) -> String {
	expressions.iter()
		.map(|p| p.to_string())
		.collect::<Vec<String>>()
		.join(", ")
}

#[derive(Debug, PartialEq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Copy, Clone)]
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
	And,
	Or,
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
			InfixOperator::And => write!(f, "&&"),
			InfixOperator::Or => write!(f, "||"),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
	pub parameters: Vec<(String, ParsedType)>,
	pub parameters_token: Token,
	pub body: StatementBlock,
	pub name: Option<String>,
	pub return_type: ParsedType,
	pub is_method: bool,
}

impl fmt::Display for Function {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let name = if let Some(name) = &self.name {
			format!("<{}>", name)
		}else {
			"".into()
		};
		let parameters = self.parameters.iter()
			.map(|(i, t)| format!("{} {}", i, t))
			.collect::<Vec<String>>()
			.join(", ");
		let return_type = match &self.return_type {
			ParsedType::Primitive(TypeExpr::Void) => "".into(),
			r => format!(" {}", r),
		};

		write!(f, "fn{}({}) {} {}", name, parameters, return_type, self.body)
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructDecl {
	Block (Vec<(String, ParsedType)>),
	Tuple(Vec<ParsedType>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructConstructor {
	Block (Vec<(String, Expression)>),
	Tuple(Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParsedType {
	Primitive(TypeExpr),
	FnLiteral {
		parameter_types: Vec<ParsedType>,
		return_type: Box<ParsedType>,
	},
	Custom(String),
}

impl fmt::Display for ParsedType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParsedType::Primitive(t) => t.fmt(f),
			ParsedType::FnLiteral { parameter_types, return_type } => {
				let parameter_types = parameter_types.iter()
					.map(|t| t.to_string())
					.collect::<Vec<String>>()
					.join(", ");

				write!(f, "fn({}) {} {{}}", parameter_types, return_type)
			}
			ParsedType::Custom(name) => write!(f, "{}", name),
		}
	}
}

impl From<TypeExpr> for ParsedType {
	fn from(t: TypeExpr) -> Self {
		ParsedType::Primitive(t)
	}
}