use core::fmt;
use core::fmt::Formatter;

#[derive(Debug)]
pub struct Token {
	pub token_type: TokenType,
	//TODO Replace keywords and symbols with Literal::Default or something
	pub literal: Option<String>,
}

#[derive(Eq, PartialEq, Debug)]
pub enum TokenType {
	Illegal,
	EOF,

	// Identifiers + literals
	Ident, // add, foobar, x, y, ...
	Int,  // 1343456

	// Operators
	Assing,
	Plus,
	Minus,
	Bang,
	Asterisk,
	Slash,

	LT,
	GT,
	Eq,
	NotEq,

	// Delimiters
	Comma,
	Semicolon,

	LParen,
	RParen,
	LBrace,
	RBrace,

	// Keywords
	Function,
	Let,
	True,
	False,
	If,
	Else,
	Return,
}

impl fmt::Display for TokenType {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			TokenType::Illegal => "ILLEGAL",
			TokenType::EOF => "EOF",

			TokenType::Ident => "IDENT",
			TokenType::Int => "INT",

			TokenType::Assing => "=",
			TokenType::Plus => "+",
			TokenType::Minus => "-",
			TokenType::Bang => "!",
			TokenType::Asterisk => "*",
			TokenType::Slash => "/",

			TokenType::LT => "<",
			TokenType::GT => ">",
			TokenType::Eq => "==",
			TokenType::NotEq => "!=",

			TokenType::Comma => ",",
			TokenType::Semicolon => ";",

			TokenType::LParen => "(",
			TokenType::RParen => ")",
			TokenType::LBrace => "{",
			TokenType::RBrace => "}",

			TokenType::Function => "FUNCTION",
			TokenType::Let => "LET",
			TokenType::True => "TRUE",
			TokenType::False => "FALSE",
			TokenType::If => "IF",
			TokenType::Else => "ELSE",
			TokenType::Return => "RETURN",
		})
	}
}

pub fn lookup_ident(keyword: &str) -> TokenType {
	match keyword {
		"fn" => TokenType::Function,
		"let" => TokenType::Let,
		"true" => TokenType::True,
		"false" => TokenType::False,
		"if" => TokenType::If,
		"else" => TokenType::Else,
		"return" => TokenType::Return,
		_ => TokenType::Ident,
	}
}