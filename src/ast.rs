use std::fmt;

pub struct Program {
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
}

impl fmt::Display for Statement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Statement::Let { name, value } => write!(f, "let {} = {}", name, value),
			Statement::Return(Some(value)) => write!(f, "return {}", value),
			Statement::Return(None) => write!(f, "return"),
		}
	}
}

#[derive(Debug)]
pub enum Expression {
	Identifier(String)
}

impl fmt::Display for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Expression::Identifier(name) => write!(f, "{}", name),
		}
	}
}