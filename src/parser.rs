use std::fmt;
use std::fmt::Formatter;
use crate::ast::{BlockStatement, Expression, InfixOperator, PrefixOperator, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenLiteral, TokenType};

pub struct Parser {
	pub lexer: Lexer,

	pub cur_token: Token,
	pub peek_token: Token,
}

impl Parser {
	pub fn new(lexer: Lexer) -> Self {
		let default_token = Token { token_type: TokenType::Illegal, literal: TokenLiteral::Static("") };
		let mut parser = Parser {
			lexer,

			cur_token: default_token.clone(),
			peek_token: default_token,
		};

		parser.next_token();
		parser.next_token();

		parser
	}

	fn no_prefix_parse_fn_error(&mut self) -> PResult<()> {
		Err(ParserError::Generic(format!(
			"no prefix parse function for {:?} found",
			self.cur_token
		)))
	}

	pub fn next_token(&mut self) {
		self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
	}

	pub fn parse_program(&mut self) -> Result<Program, ParserError> {
		let mut statements = vec![];

		while self.cur_token.token_type != TokenType::EOF {
			statements.push(self.parse_statement()?);

			self.next_token();
		}

		Ok(Program { statements })
	}

	fn parse_statement(&mut self) -> PResult<Statement> {
		match self.cur_token.token_type {
			TokenType::Let => self.parse_let_statement(),
			TokenType::Return => self.parse_return_statement(),
			_ => self.parse_expression_statement(),
		}
	}

	fn parse_let_statement(&mut self) -> PResult<Statement> {
		self.expect_peek(TokenType::Ident)?;

		let stmt_name = self.cur_token.literal
			.get_string().cloned()
			.expect("literal isn't string");

		self.expect_peek(TokenType::Assign)?;

		self.next_token();

		let mut value = self.parse_expression(Precedence::Lowest)?;

		if let Expression::Function { name, .. } = &mut value {
			*name = Some(stmt_name.clone());
		}

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Ok(Statement::Let {
			name: stmt_name,
			value,
		})
	}

	fn parse_return_statement(&mut self) -> PResult<Statement> {
		self.next_token();

		let return_value = if let TokenType::Semicolon | TokenType::RBrace | TokenType::Comma = self.cur_token.token_type {
			return Ok(Statement::Return(None));
		} else {
			self.parse_expression(Precedence::Lowest)?
		};

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Ok(Statement::Return(Some(return_value)))
	}

	fn parse_expression_statement(&mut self) -> PResult<Statement> {
		let expression = self.parse_expression(Precedence::Lowest);

		if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
		}

		Ok(Statement::Expression(expression?))
	}

	fn parse_expression(&mut self, precedence: Precedence) -> PResult<Expression> {
		let mut left_exp = match &self.cur_token {
			//TODO Consume Token instead of cloning
			Token { token_type: TokenType::Ident, literal: TokenLiteral::String(ident) }
			=> Ok(Expression::Identifier(ident.clone())),
			Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(value) }
			=> Ok(Expression::Integer(*value as isize)),
			Token { token_type: TokenType::True, .. } => Ok(Expression::Boolean(true)),
			Token { token_type: TokenType::False, .. } => Ok(Expression::Boolean(false)),
			Token { token_type: TokenType::Bang, .. } |
			Token { token_type: TokenType::Minus, .. } => self.parse_prefix_expression(),
			Token { token_type: TokenType::LParen, .. } => self.parse_grouped_expression(),
			Token { token_type: TokenType::If, .. } => self.parse_if_expression(),
			Token { token_type: TokenType::Function, .. } => self.parse_function_literal(),
			Token { token_type: TokenType::String, .. } => Ok(self.parse_string_literal()),
			Token { token_type: TokenType::LBracket, .. } => self.parse_array_literal(),
			Token { token_type: TokenType::LBrace, .. } => self.parse_hash_literal(),
			_ => Err(self.no_prefix_parse_fn_error().unwrap_err()),
		}?;

		while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
			let infix = match self.peek_token {
				Token { token_type: TokenType::Plus, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::Minus, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::Slash, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::Asterisk, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::Eq, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::NotEq, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::LT, literal: TokenLiteral::Static(_) } |
				Token { token_type: TokenType::GT, literal: TokenLiteral::Static(_) } => Parser::parse_infix_expression,
				Token { token_type: TokenType::LParen, literal: TokenLiteral::Static(_) } => Parser::parse_call_expression,
				Token { token_type: TokenType::LBracket, literal: TokenLiteral::Static(_) } => Parser::parse_index_expression,
				_ => return Ok(left_exp)
			};

			self.next_token();

			left_exp = infix(self, left_exp)?;
		}

		Ok(left_exp)
	}

	fn parse_prefix_expression(&mut self) -> PResult<Expression> {
		let operator = match self.cur_token.literal {
			TokenLiteral::Static(operator) => match operator {
				"-" => Ok(PrefixOperator::Minus),
				"!" => Ok(PrefixOperator::Bang),
				_ => Err(ParserError::Generic(format!("{:?} isn't a prefix operator token", self.cur_token))),
			}
			_ => Err(ParserError::Generic(format!("{:?} isn't a operator token", self.cur_token))),
		}?;

		self.next_token();

		let right = self.parse_expression(Precedence::Prefix)?;

		Ok(Expression::Prefix {
			operator,
			right: Box::new(right),
		})
	}

	fn parse_infix_expression(&mut self, left: Expression) -> PResult<Expression> {
		let operator = match self.cur_token.literal {
			TokenLiteral::Static(operator) => match operator {
				"+" => Ok(InfixOperator::Plus),
				"-" => Ok(InfixOperator::Minus),
				"*" => Ok(InfixOperator::Mul),
				"/" => Ok(InfixOperator::Div),
				//TODO "%" => Ok(InfixOperator::Modulo),

				"==" => Ok(InfixOperator::Equal),
				"!=" => Ok(InfixOperator::Unequal),
				"<" => Ok(InfixOperator::LessThan),
				">" => Ok(InfixOperator::GreaterThan),
				//TODO "<=" => Ok(InfixOperator::LesserEqual),
				//TODO ">=" => Ok(InfixOperator::GreaterEqual),
				_ => Err(ParserError::Generic(format!("{:?} isn't a infix operator token", self.cur_token))),
			}
			_ => Err(ParserError::Generic(format!("{:?} isn't operator token", self.cur_token))),
		}?;

		let precedence = self.cur_precedence();
		self.next_token();

		let right = self.parse_expression(precedence)?;

		Ok(Expression::Infix {
			left: Box::new(left),
			operator,
			right: Box::new(right),
		})
	}

	fn parse_grouped_expression(&mut self) -> PResult<Expression> {
		self.next_token();

		let exp = self.parse_expression(Precedence::Lowest);

		self.expect_peek(TokenType::RParen)?;

		return exp;
	}

	fn parse_if_expression(&mut self) -> PResult<Expression> {
		self.next_token();

		//TODO Should be able to check which expression can return bool (not minus, not string)
		let condition = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::LBrace)?;

		let consequence = self.parse_block_statement()?;

		let alternative = if self.peek_token_is(TokenType::Else) {
			self.next_token();

			self.expect_peek(TokenType::LBrace)?;

			Some(self.parse_block_statement()?)
		} else {
			None
		};

		Ok(Expression::If {
			condition: Box::new(condition),
			consequence,
			alternative,
		})
	}

	fn parse_block_statement(&mut self) -> PResult<BlockStatement> {
		self.next_token();

		let mut statements = vec![];

		//TODO Parse error on EOF
		while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::EOF) {
			statements.push(self.parse_statement()?);

			self.next_token();
		}

		Ok(BlockStatement { statements })
	}

	fn parse_function_literal(&mut self) -> PResult<Expression> {
		self.expect_peek(TokenType::LParen)?;

		let parameters = self.parse_function_parameters()?;

		self.expect_peek(TokenType::LBrace)?;

		let body = self.parse_block_statement()?;

		Ok(Expression::Function {
			parameters,
			body,
			name: None,
		})
	}

	fn parse_function_parameters(&mut self) -> PResult<Vec<String>> {
		let mut identifiers = vec![];

		if self.peek_token_is(TokenType::RParen) {
			self.next_token();
			return Ok(identifiers);
		}

		self.next_token();

		let expect_str = "identifier literal isn't string";
		identifiers.push(if let TokenLiteral::String(ident) = &self.cur_token.literal {
			ident.clone()
		} else {
			return Err(ParserError::Generic(expect_str.into()));
		});

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();

			identifiers.push(if let TokenLiteral::String(ident) = &self.cur_token.literal {
				ident.clone()
			} else {
				return Err(ParserError::Generic(expect_str.into()));
			});
		}

		self.expect_peek(TokenType::RParen)?;

		Ok(identifiers)
	}

	fn parse_call_expression(&mut self, function: Expression) -> PResult<Expression> {
		Ok(Expression::Call {
			function: Box::new(function),
			arguments: self.parse_expression_list(TokenType::RParen)?,
		})
	}

	fn parse_string_literal(&self) -> Expression {
		Expression::String(
			self.cur_token.literal
				.get_string().cloned()
				.expect("literal isn't string")
		)
	}

	fn parse_array_literal(&mut self) -> PResult<Expression> {
		Ok(Expression::Array(self.parse_expression_list(TokenType::RBracket)?))
	}

	fn parse_expression_list(&mut self, end: TokenType) -> PResult<Vec<Expression>> {
		let mut list = vec![];

		if self.peek_token_is(end) {
			self.next_token();
			return Ok(list);
		}

		self.next_token();
		list.push(self.parse_expression(Precedence::Lowest)?);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();
			list.push(self.parse_expression(Precedence::Lowest)?);
		}

		self.expect_peek(end)?;

		Ok(list)
	}

	fn parse_index_expression(&mut self, left: Expression) -> PResult<Expression> {
		self.next_token();
		let index = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::RBracket)?;

		Ok(Expression::Index {
			left: Box::new(left),
			index: Box::new(index),
		})
	}

	fn parse_hash_literal(&mut self) -> PResult<Expression> {
		let mut pairs = vec![];

		//We could skip a peek_token_is with a manual loop, but probably not worth it
		while !self.peek_token_is(TokenType::RBrace) {
			self.next_token();

			let key = self.parse_expression(Precedence::Lowest)?;

			self.expect_peek(TokenType::Colon)?;

			self.next_token();
			let value = self.parse_expression(Precedence::Lowest)?;

			pairs.push((key, value));

			if !self.peek_token_is(TokenType::RBrace) {
				self.expect_peek(TokenType::Comma)?;
			}
		}

		self.expect_peek(TokenType::RBrace)?;

		Ok(Expression::Hash(pairs))
	}

	fn cur_token_is(&self, t: TokenType) -> bool {
		self.cur_token.token_type == t
	}

	fn peek_token_is(&self, t: TokenType) -> bool {
		self.peek_token.token_type == t
	}

	fn expect_peek(&mut self, t: TokenType) -> PResult<()> {
		if self.peek_token_is(t) {
			self.next_token();
			Ok(())
		} else {
			Err(ParserError::Generic(format!(
				"expected next token to be {}, got {} instead",
				t, self.peek_token.token_type
			)))
		}
	}

	fn peek_precedence(&self) -> Precedence {
		precedences(self.peek_token.token_type).unwrap_or(Precedence::Lowest)
	}

	fn cur_precedence(&self) -> Precedence {
		precedences(self.cur_token.token_type).unwrap_or(Precedence::Lowest)
	}
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
enum Precedence {
	Lowest,
	Equals,
	LessGreater,
	Sum,
	Product,
	Prefix,
	Call,
	Index,
}

const fn precedences(token_type: TokenType) -> Option<Precedence> {
	match token_type {
		TokenType::Eq |
		TokenType::NotEq => Some(Precedence::Equals),
		TokenType::LT |
		TokenType::GT => Some(Precedence::LessGreater),
		TokenType::Plus |
		TokenType::Minus => Some(Precedence::Sum),
		TokenType::Slash |
		TokenType::Asterisk => Some(Precedence::Product),
		TokenType::LParen => Some(Precedence::Call),
		TokenType::LBracket => Some(Precedence::Index),
		_ => None,
	}
}

type PResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub enum ParserError {
	ExpectedType {
		expected: String,
		found: String,
	},
	//TODO Classify generic errors
	Generic(String),
}

impl fmt::Display for ParserError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			ParserError::ExpectedType { expected, found } => write!(f, "expected {}, found {}", expected, found),
			ParserError::Generic(err) => write!(f, "{}", err),
		}
	}
}