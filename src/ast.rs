use std::fmt;

pub type Program = BlockStatement;
/*pub struct Program {
	pub statements: Vec<Statement>
}

impl fmt::Display for Program {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}*/

#[derive(Debug)]
pub struct BlockStatement {
	pub statements: Vec<Statement>
}

impl fmt::Display for Program {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for stmt in &self.statements {
			write!(f, "{}", stmt)?;
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum Statement {
	Let {
		name: Expression,
		value: Expression,
	},
	Return(Option<Expression>),
	Expression(Expression),
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
			Statement::Return(Some(value)) => write!(f, "return {};", value),
			Statement::Return(None) => write!(f, "return;"),
			Statement::Expression(e) => write!(f, "{}", e),
		}
	}
}

#[derive(Debug)]
pub enum Expression {
	Identifier(String),
	Integer(i64),
	Boolean(bool),
	Prefix {
		operator: String,
		right: Box<Expression>,
	},
	Infix {
		left: Box<Expression>,
		operator: String,
		right: Box<Expression>,
	},
	If {
		condition: Box<Expression>,
		consequence: BlockStatement,
		alternative: Option<BlockStatement>,
	},
	Function {
		parameters: Vec<Expression>,	//Identifiers
		body: BlockStatement,
	},
	Call {
		function: Box<Expression>,
		arguments: Vec<Expression>,
	}
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::Identifier(name) => write!(f, "{}", name),
			Expression::Integer(value) => write!(f, "{}", value),
			Expression::Boolean(value) => write!(f, "{}", value),
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
			Expression::Function { parameters, body } => {
				let params_str = parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");

				write!(f, "fn({}) {}", params_str, body)
			},
			Expression::Call { function, arguments } => {
				let args_str = arguments.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", ");

				write!(f, "{}({})", function, args_str)
			}
		}
	}
}