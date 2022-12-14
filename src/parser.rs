use std::error::Error;
use std::fmt;
use crate::ast::{Expression, Function, InfixOperator, IntExpr, ParsedType, PrefixOperator, Program, Statement, StatementBlock, StructConstructor, StructDecl};
use crate::lexer::Lexer;
use crate::token::{IntType, Token, TokenLiteral, TokenType};
use crate::type_checker::type_env::TypeExpr;

pub struct Parser {
	pub lexer: Lexer,

	pub cur_token: Token,
	pub cur_token_index: usize,
	cur_token_line: usize,
	pub peek_token: Token,
}

impl Parser {
	pub fn new(lexer: Lexer) -> Self {
		let temp_token = Token {
			token_type: TokenType::Illegal,
			literal: TokenLiteral::Static(""),
			position: 0,
			line: 0,
			after_whitespace: false,
		};
		let mut parser = Parser {
			lexer,

			cur_token: temp_token.clone(),
			cur_token_index: 0,
			cur_token_line: 0,
			peek_token: temp_token,
		};

		parser.next_token();
		parser.next_token();

		parser.cur_token_index = 0;
		parser.cur_token_line = 0;

		parser
	}

	pub fn next_token(&mut self) {
		loop {
			self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
			self.cur_token_index += 1;

			if !matches!(self.cur_token.token_type, TokenType::LineComment | TokenType::MultilineComment) {
				break;
			}
		}
	}

	pub fn parse_program(&mut self) -> Result<Program, ParserError> {
		let mut statements = vec![];

		while self.cur_token.token_type != TokenType::EOF {
			statements.push(self.parse_global_statement()?);

			self.next_token();
		}

		Ok(StatementBlock(statements))
	}

	pub fn parse_block_statement(&mut self, end: TokenType) -> PResult<StatementBlock> {
		let mut statements = vec![];

		while !self.cur_token_is(end) {
			statements.push(self.parse_statement()?);

			self.next_token();
		}

		Ok(StatementBlock(statements))
	}

	fn parse_global_statement(&mut self) -> PResult<Statement> {
		match self.cur_token.token_type {
			TokenType::Function => {
				let func = self.parse_function_literal()?;
				if func.name.is_none() {
					return Err(ParserError {
						token: Some(func.parameters_token),
						cause: ParserErrorCause::MissingGlobalFunctionName,
					});
				}

				Ok(Statement::Function(func))
			}
			TokenType::Struct => self.parse_struct_decl(),
			_ => return Err(ParserError {
				token: None,
				cause: ParserErrorCause::InvalidGlobalToken(self.cur_token.clone()),
			}),
		}
	}

	fn parse_statement(&mut self) -> PResult<Statement> {
		match self.cur_token.token_type {
			TokenType::Let => self.parse_let_statement(),
			TokenType::Return => self.parse_return_statement(),
			TokenType::While => self.parse_while_statement(),
			TokenType::Struct => self.parse_struct_decl(),
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

		if let Expression::Function(Function { name, .. }) = &mut value {
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

	fn parse_while_statement(&mut self) -> PResult<Statement> {
		self.next_token();
		let condition = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::LBrace)?;
		self.next_token();

		let block = self.parse_block_statement(TokenType::RBrace)?;

		Ok(Statement::While {
			condition,
			block,
		})
	}

	fn parse_struct_decl(&mut self) -> PResult<Statement> {
		self.expect_peek(TokenType::Ident)?;

		let name = self.cur_token.literal
			.get_string().cloned()
			.expect("literal isn't string");

		let decl = if self.peek_token_is(TokenType::LBrace) {
			self.next_token();
			self.parse_struct_block_decl()?
		} else if self.peek_token_is(TokenType::LParen) {
			self.next_token();
			StructDecl::Tuple(self.parse_type_list(TokenType::RParen)?)
		} else {
			return Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!(
					"expected next token to be ( or {{, got {} instead",
					self.peek_token.token_type
				)),
			});
		};

		Ok(Statement::Struct { name, decl })
	}

	fn parse_struct_block_decl(&mut self) -> PResult<StructDecl> {
		let mut fields = vec![];

		//We could skip a peek_token_is with a manual loop, but probably not worth it
		while !self.peek_token_is(TokenType::RBrace) {
			fields.push(self.parse_parameter()?);

			if !self.peek_token_is(TokenType::RBrace) {
				self.expect_peek(TokenType::Comma)?;
			}
		}

		self.expect_peek(TokenType::RBrace)?;

		Ok(StructDecl::Block(fields))
	}

	fn parse_expression_statement(&mut self) -> PResult<Statement> {
		let expression = self.parse_expression(Precedence::Lowest);

		let has_semicolon = if self.peek_token_is(TokenType::Semicolon) {
			self.next_token();
			true
		} else {
			false
		};

		Ok(Statement::Expression {
			expr: expression?,
			has_semicolon,
		})
	}

	fn parse_expression(&mut self, precedence: Precedence) -> PResult<Expression> {
		let mut left_exp = match &self.cur_token {
			//TODO Consume Token instead of cloning
			Token { token_type: TokenType::Ident, literal: TokenLiteral::String(ident), .. }
			=> Ok(Expression::Identifier(ident.clone())),
			Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(value), .. } => {
				let value = *value;
				if let TokenType::IntegerType(int_type) = self.peek_token.token_type {
					self.next_token();
					match int_type {
						IntType::U8 => Ok(Expression::Integer(IntExpr::U8(value as u8))),
						IntType::U16 => Ok(Expression::Integer(IntExpr::U16(value as u16))),
						IntType::U32 => Ok(Expression::Integer(IntExpr::U32(value as u32))),
						IntType::U64 => Ok(Expression::Integer(IntExpr::U64(value as u64))),
						IntType::I8 => Ok(Expression::Integer(IntExpr::I8(value as i8))),
						IntType::I16 => Ok(Expression::Integer(IntExpr::I16(value as i16))),
						IntType::I32 => Ok(Expression::Integer(IntExpr::I32(value as i32))),
						IntType::I64 => Ok(Expression::Integer(IntExpr::I64(value as i64))),
					}
				} else {
					match value {
						value if value < u8::MAX as usize => Ok(Expression::Integer(IntExpr::U8(value as u8))),
						value if value < u16::MAX as usize => Ok(Expression::Integer(IntExpr::U16(value as u16))),
						value if value < u32::MAX as usize => Ok(Expression::Integer(IntExpr::U32(value as u32))),
						value if value < u64::MAX as usize => Ok(Expression::Integer(IntExpr::U64(value as u64))),
						_ => Err(ParserError {
							token: None,
							cause: ParserErrorCause::Generic(format!("unsupported integer size {}", value)),
						}),
					}
				}
			}
			Token { token_type: TokenType::Float, literal: TokenLiteral::Float(int, decimals), .. } => {
				//Probably is a smarter way to do this
				let mut decimals = *decimals as f32;
				while decimals > 1.0 {
					decimals /= 10.0;
				}
				Ok(Expression::Float(*int as f32 + decimals))
			}
			Token { token_type: TokenType::True, .. } => Ok(Expression::Boolean(true)),
			Token { token_type: TokenType::False, .. } => Ok(Expression::Boolean(false)),
			Token { token_type: TokenType::Bang, .. } |
			Token { token_type: TokenType::Minus, .. } => self.parse_prefix_expression(),
			Token { token_type: TokenType::LParen, .. } => self.parse_grouped_expression(),
			Token { token_type: TokenType::If, .. } |
			Token { token_type: TokenType::Unless, .. } => self.parse_if_expression(),
			Token { token_type: TokenType::Function, .. } => Ok(Expression::Function(self.parse_function_literal()?)),
			Token { token_type: TokenType::String, .. } => Ok(self.parse_string_literal()),
			Token { token_type: TokenType::LBracket, .. } => self.parse_array_literal(),
			Token { token_type: TokenType::LBrace, .. } => {
				self.next_token();
				Ok(Expression::Block {
					statements: self.parse_block_statement(TokenType::RBrace)?,
					return_transparent: false,
				})
			}
			_ => Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!(
					"no prefix parse function for {:?} found",
					self.cur_token
				)),
			}),
		}?;

		while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
			let infix = match self.peek_token {
				Token { token_type: TokenType::Plus, .. } |
				Token { token_type: TokenType::Minus, .. } |
				Token { token_type: TokenType::Slash, .. } |
				Token { token_type: TokenType::Asterisk, .. } |
				Token { token_type: TokenType::Eq, .. } |
				Token { token_type: TokenType::NotEq, .. } |
				Token { token_type: TokenType::LT, .. } |
				Token { token_type: TokenType::GT, .. } |
				Token { token_type: TokenType::And, .. } |
				Token { token_type: TokenType::Or, .. } => Parser::parse_infix_expression,
				Token { token_type: TokenType::LParen, .. } => Parser::parse_call_expression,
				Token { token_type: TokenType::Dot, .. } => Parser::parse_field_expression,
				Token { token_type: TokenType::LBracket, .. } => Parser::parse_index_expression,
				Token { token_type: TokenType::Assign, .. } => Parser::parse_assignment,
				Token { token_type: TokenType::LBrace, .. } => if let Expression::Identifier(_) = left_exp {
					Parser::parse_brace_infix
				} else {
					return Ok(left_exp);
				},
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
				_ => Err(ParserError {
					token: None,
					cause: ParserErrorCause::Generic(format!("{:?} isn't a prefix operator token", self.cur_token)),
				}),
			}
			_ => Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!("{:?} isn't a operator token", self.cur_token)),
			}),
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
				"&&" => Ok(InfixOperator::And),
				"||" => Ok(InfixOperator::Or),
				_ => Err(ParserError {
					token: None,
					cause: ParserErrorCause::Generic(format!("{:?} isn't a infix operator token", self.cur_token)),
				}),
			}
			_ => Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!("{:?} isn't operator token", self.cur_token)),
			}),
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

		let condition = self.parse_expression(Precedence::Lowest)?;

		self.expect_peek(TokenType::LBrace)?;
		self.next_token();

		let consequence = self.parse_block_statement(TokenType::RBrace)?;

		let alternative = if self.peek_token_is(TokenType::Else) {
			self.next_token();

			self.expect_peek(TokenType::LBrace)?;
			self.next_token();

			Some(self.parse_block_statement(TokenType::RBrace)?)
		} else {
			None
		};

		Ok(Expression::If {
			condition: Box::new(condition),
			consequence,
			alternative,
		})
	}

	fn parse_function_literal(&mut self) -> PResult<Function> {
		//Temporarily using brackets instead of parens for method notation
		let method_receiver = if self.peek_token_is(TokenType::LBracket) {
			self.next_token();
			let receiver = self.parse_parameter()?;
			self.expect_peek(TokenType::RBracket)?;

			Some(receiver)
		} else {
			None
		};
		let is_method = method_receiver.is_some();

		let name = if self.peek_token_is(TokenType::Ident) {
			self.next_token();
			Some(self.cur_token.literal.get_string().unwrap().clone())
		} else {
			None
		};

		let parameters_token = self.peek_token.clone();
		self.expect_peek(TokenType::LParen)?;

		let mut parameters = self.parse_function_parameters()?;
		if let Some(r) = method_receiver {
			parameters.insert(0, r);
		}

		let return_type = if !self.peek_token_is(TokenType::LBrace) {
			self.next_token();
			self.parse_type_identifier()?
		} else {
			TypeExpr::Void.into()
		};

		self.expect_peek(TokenType::LBrace)?;
		self.next_token();

		let body = self.parse_block_statement(TokenType::RBrace)?;

		Ok(Function {
			parameters,
			parameters_token,
			body,
			name,
			return_type,
			is_method,
		})
	}

	//Could have a better name, like parse_ident_and_type_pair
	fn parse_parameter(&mut self) -> PResult<(String, ParsedType)> {
		self.expect_peek(TokenType::Ident)?;

		let param = self.cur_token.literal.get_string().cloned().unwrap();

		self.next_token();
		let param_type = self.parse_type_identifier()?;

		Ok((param, param_type))
	}

	fn parse_function_parameters(&mut self) -> PResult<Vec<(String, ParsedType)>> {
		let mut identifiers = vec![];

		if self.peek_token_is(TokenType::RParen) {
			self.next_token();
			return Ok(identifiers);
		}

		identifiers.push(self.parse_parameter()?);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();

			identifiers.push(self.parse_parameter()?);
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

	fn parse_field_expression(&mut self, left: Expression) -> PResult<Expression> {
		self.next_token();
		let field = match self.cur_token.token_type {
			TokenType::Ident => self.cur_token.literal.get_string().cloned().unwrap(),
			TokenType::Int => {
				self.cur_token.literal
					.get_integer().cloned().unwrap()
					.to_string()
			}
			_ => return Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!("Unexpected field token {:?}", self.peek_token)),
			})
		};

		Ok(Expression::Field {
			left: Box::new(left),
			field,
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

		loop {
			self.next_token();
			list.push(self.parse_expression(Precedence::Lowest)?);

			if self.peek_token_is(TokenType::Comma) {
				self.next_token();

				//In case of trailing comma
				if self.peek_token_is(end) {
					self.next_token();
					break;
				}
			} else {
				self.expect_peek(end)?;
				break;
			}
		}

		if list.len() > u16::MAX as usize {
			return Err(ParserError {
				token: None,
				cause: ParserErrorCause::ArrayTooLong(list.len()),
			});
		}

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

	fn parse_brace_infix(&mut self, left: Expression) -> PResult<Expression> {
		if let Expression::Identifier(left) = left {
			let mut fields = vec![];

			//We could skip a peek_token_is with a manual loop, but probably not worth it
			while !self.peek_token_is(TokenType::RBrace) {
				self.expect_peek(TokenType::Ident)?;

				let field = self.cur_token.literal
					.get_string().cloned()
					.expect("literal isn't string");

				if self.peek_token_is(TokenType::Colon) {
					self.next_token();
					self.next_token();
					let field_value = self.parse_expression(Precedence::Lowest)?;

					fields.push((field, field_value));
				} else {
					fields.push((field.clone(), Expression::Identifier(field)));
				}

				if !self.peek_token_is(TokenType::RBrace) {
					self.expect_peek(TokenType::Comma)?;
				}
			}

			self.expect_peek(TokenType::RBrace)?;

			Ok(Expression::Struct {
				name: left,
				constructor: StructConstructor::Block(fields),
			})
		} else {
			Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!("unexpected {{ token after {:?}", left)),
			})
		}
	}

	fn parse_assignment(&mut self, left: Expression) -> PResult<Expression> {
		//Temporary check, will need to accept fields and maybe calls
		let ident = if let Expression::Identifier(ident) = left {
			ident
		} else {
			return Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!("expected identifier, got {:?}", left)),
			});
		};

		self.next_token();
		let new_value = self.parse_expression(Precedence::Lowest)?;

		Ok(Expression::Assignment {
			ident,
			new_value: Box::new(new_value),
		})
	}

	fn parse_type_identifier(&mut self) -> PResult<ParsedType> {
		match &self.cur_token.token_type {
			TokenType::Function => {
				self.expect_peek(TokenType::LParen)?;

				let parameter_types = self.parse_type_list(TokenType::RParen)?;

				let return_type = if !self.peek_token_is(TokenType::LBrace) {
					self.next_token();
					self.parse_type_identifier()?
				} else {
					TypeExpr::Void.into()
				};

				Ok(ParsedType::FnLiteral {
					parameter_types,
					return_type: Box::new(return_type),
				})
			}
			TokenType::IntegerType(IntType::U8) => Ok(TypeExpr::Int(IntType::U8).into()),
			TokenType::IntegerType(IntType::U16) => Ok(TypeExpr::Int(IntType::U16).into()),
			TokenType::IntegerType(IntType::U32) => Ok(TypeExpr::Int(IntType::U32).into()),
			TokenType::IntegerType(IntType::U64) => Ok(TypeExpr::Int(IntType::U64).into()),
			TokenType::IntegerType(IntType::I8) => Ok(TypeExpr::Int(IntType::I8).into()),
			TokenType::IntegerType(IntType::I16) => Ok(TypeExpr::Int(IntType::I16).into()),
			TokenType::IntegerType(IntType::I32) => Ok(TypeExpr::Int(IntType::I32).into()),
			TokenType::IntegerType(IntType::I64) => Ok(TypeExpr::Int(IntType::I64).into()),
			TokenType::FloatType => Ok(TypeExpr::Float.into()),
			TokenType::Bool => Ok(TypeExpr::Bool.into()),
			TokenType::Str => Ok(TypeExpr::String.into()),
			TokenType::Ident => Ok(ParsedType::Custom(self.cur_token.literal.get_string().unwrap().clone())),
			_ => Err(ParserError {
				token: None,
				cause: ParserErrorCause::Generic(format!("unrecognized type: {:?}", self.cur_token)),
			}),
		}
	}

	fn parse_type_list(&mut self, end: TokenType) -> PResult<Vec<ParsedType>> {
		let mut list = vec![];

		if self.peek_token_is(end) {
			self.next_token();
			return Ok(list);
		}

		self.next_token();
		list.push(self.parse_type_identifier()?);

		while self.peek_token_is(TokenType::Comma) {
			self.next_token();
			self.next_token();
			list.push(self.parse_type_identifier()?);
		}

		self.expect_peek(end)?;

		Ok(list)
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
			Err(ParserError {
				token: Some(self.peek_token.clone()),
				cause: ParserErrorCause::UnexpectedTokenType {
					expected: t,
					actual: self.peek_token.token_type,
				},
			})
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
	BlockDef,
	Assign,
	Equals,
	Comparison,
	Sum,
	Product,
	Prefix,
	Call,
	Index,
}

const fn precedences(token_type: TokenType) -> Option<Precedence> {
	match token_type {
		TokenType::LBrace => Some(Precedence::BlockDef),
		TokenType::Eq |
		TokenType::NotEq => Some(Precedence::Equals),
		TokenType::LT |
		TokenType::GT |
		TokenType::And |
		TokenType::Or => Some(Precedence::Comparison),
		TokenType::Plus |
		TokenType::Minus => Some(Precedence::Sum),
		TokenType::Slash |
		TokenType::Asterisk => Some(Precedence::Product),
		TokenType::LParen |
		TokenType::Dot => Some(Precedence::Call),
		TokenType::LBracket => Some(Precedence::Index),
		TokenType::Assign => Some(Precedence::Assign),
		_ => None,
	}
}

type PResult<T> = Result<T, ParserError>;

#[derive(Debug, PartialEq, Clone)]
pub struct ParserError {
	//TODO Dissolve optional token
	pub token: Option<Token>,
	pub cause: ParserErrorCause,
}

impl fmt::Display for ParserError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "parsing error")
	}
}

impl Error for ParserError {}

#[derive(Debug, PartialEq, Clone)]
pub enum ParserErrorCause {
	ExpectedType {
		expected: String,
		found: String,
	},
	InvalidGlobalToken(Token),
	MissingGlobalFunctionName,
	ArrayTooLong(usize),
	UnexpectedTokenType {
		expected: TokenType,
		actual: TokenType,
	},
	//TODO Classify generic errors
	Generic(String),
}

impl fmt::Display for ParserErrorCause {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParserErrorCause::ExpectedType { expected, found } => write!(f, "expected {}, found {}", expected, found),
			_ => write!(f, "{:?}", self),
		}
	}
}