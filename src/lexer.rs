use crate::token::{lookup_ident, Token, TokenLiteral, TokenType};

pub struct Lexer {
	//TODO Could be Chars
	input: String,
	/// current position in input (points to current char)
	position: usize,
	/// current reading position in input (after current char)
	read_position: usize,
	/// current char under examination
	ch: Option<char>,
}

impl Lexer {
	pub fn new(input: &str) -> Self {
		let mut lexer = Self {
			input: input.to_owned(),
			position: 0,
			read_position: 0,
			ch: None,
		};

		lexer.read_char();

		return lexer
	}

	//TODO read runes instead of ascii chars
	pub fn read_char(&mut self) {
		if self.read_position >= self.input.len() {
			self.ch = None;
		}else {
			self.ch = self.input.chars().nth(self.read_position);
		}

		self.position = self.read_position;
		self.read_position += 1;
	}

	pub fn peek_char(&self) -> Option<char> {
		self.input.chars().nth(self.read_position)
	}

	pub fn next_token(&mut self) -> Token {
		self.skip_whitespace();

		let token = match self.ch {
			Some(ch) => match ch {
				'=' => if self.peek_char() == Some('=') {
					self.read_char();
					Token { token_type: TokenType::Eq, literal: TokenLiteral::Static("==") }
				} else {
					Token { token_type: TokenType::Assign, literal: TokenLiteral::Static("=") }
				},
				//TODO += -= /= *= %=
				'+' => Token { token_type: TokenType::Plus, literal: TokenLiteral::Static("+") },
				'-' => Token { token_type: TokenType::Minus, literal: TokenLiteral::Static("-") },
				'!' => if self.peek_char() == Some('=') {
					self.read_char();
					Token { token_type: TokenType::NotEq, literal: TokenLiteral::Static("!=")}
				} else {
					Token { token_type: TokenType::Bang, literal: TokenLiteral::Static("!") }
				},
				'*' => Token { token_type: TokenType::Asterisk, literal: TokenLiteral::Static("*") },
				'/' => Token { token_type: TokenType::Slash, literal: TokenLiteral::Static("/") },
				//TODO %
				//TODO <= >=
				'<' => Token { token_type: TokenType::LT, literal: TokenLiteral::Static("<") },
				'>' => Token { token_type: TokenType::GT, literal: TokenLiteral::Static(">") },
				',' => Token { token_type: TokenType::Comma, literal: TokenLiteral::Static(",") },
				';' => Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";") },
				':' => Token { token_type: TokenType::Colon, literal: TokenLiteral::Static(":") },
				'(' => Token { token_type: TokenType::LParen, literal: TokenLiteral::Static("(") },
				')' => Token { token_type: TokenType::RParen, literal: TokenLiteral::Static(")") },
				'{' => Token { token_type: TokenType::LBrace, literal: TokenLiteral::Static("{") },
				'}' => Token { token_type: TokenType::RBrace, literal: TokenLiteral::Static("}") },
				'[' => Token { token_type: TokenType::LBracket, literal: TokenLiteral::Static("[") },
				']' => Token { token_type: TokenType::RBracket, literal: TokenLiteral::Static("]") },
				'"' => Token { token_type: TokenType::String, literal: TokenLiteral::String(self.read_string())},
				_ => if is_letter(self.ch) {
					//Returning early to skip the read_char() at the end
					return lookup_ident(self.read_identifier())
				} else if is_digit(self.ch) {
					return Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(self.read_number())}
				} else {
					Token { token_type: TokenType::Illegal, literal: TokenLiteral::Static("") }
				}
			}
			None => Token { token_type: TokenType::EOF, literal: TokenLiteral::Static("") },
		};

		self.read_char();

		token
	}

	fn skip_whitespace(&mut self) {
		while matches!(self.ch, Some(' ' | '\t' | '\n' | '\r')) {
			self.read_char()
		}
	}

	fn read_identifier(&mut self) -> String {
		let position = self.position;
		while is_letter(self.ch) {
			self.read_char();
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect()
	}

	fn read_number(&mut self) -> usize {
		let position = self.position;
		while is_digit(self.ch) {
			self.read_char();
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect::<String>()
			.parse()
			.expect("failed to parse integer")
	}

	fn read_string(&mut self) -> String {
		let position = self.position + 1;
		loop {
			self.read_char();
			if let Some('"') | None = self.ch {
				break;
			}
		}
		self.input.chars()
			.skip(position)
			.take(self.position - position)
			.collect()
	}
}

//TODO Add more cases for emojis and other stuff in identifiers
fn is_letter(ch: Option<char>) -> bool {
	matches!(ch, Some(
		'a' ..= 'z' |
		'A' ..= 'Z' |
		'_'
	))
}

//TODO Support _
fn is_digit(ch: Option<char>) -> bool {
	matches!(ch, Some('0' ..= '9'))
}