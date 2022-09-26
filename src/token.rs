use core::fmt;
use core::fmt::Formatter;

#[derive(Debug, Clone)]
pub struct Token {
	pub token_type: TokenType,
	pub literal: TokenLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenLiteral {
	Static(&'static str),
	String(String),
	//Minus is parsed as a separate token, so we store positive only
	Integer(usize),
}

impl TokenLiteral {
	pub fn get_static(&self) -> Option<&'static str> {
		if let TokenLiteral::Static(literal) = self {
			Some(literal)
		}else {
			None
		}
	}

	pub fn get_string(&self) -> Option<&String> {
		if let TokenLiteral::String(literal) = self {
			Some(literal)
		}else {
			None
		}
	}

	pub fn get_integer(&self) -> Option<&usize> {
		if let TokenLiteral::Integer(literal) = self {
			Some(literal)
		}else {
			None
		}
	}
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
	Illegal,
	EOF,

	// Identifiers + literals
	Ident, // add, foobar, x, y, ...
	Int,  // 1343456

	// Operators
	Assign,
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
	Colon,

	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,

	// Keywords
	Function,
	Let,
	True,
	False,
	If,
	Else,
	Return,
	U8,
	U16,
	U32,
	U64,
	I8,
	I16,
	I32,
	I64,
	Bool,
	Str,

	String,
}

impl fmt::Display for TokenType {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			TokenType::Illegal => "ILLEGAL",
			TokenType::EOF => "EOF",

			TokenType::Ident => "IDENT",
			TokenType::Int => "INT",

			TokenType::Assign => "=",
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
			TokenType::Colon => ":",

			TokenType::LParen => "(",
			TokenType::RParen => ")",
			TokenType::LBrace => "{",
			TokenType::RBrace => "}",
			TokenType::LBracket => "[",
			TokenType::RBracket => "]",

			TokenType::Function => "FUNCTION",
			TokenType::Let => "LET",
			TokenType::True => "TRUE",
			TokenType::False => "FALSE",
			TokenType::If => "IF",
			TokenType::Else => "ELSE",
			TokenType::Return => "RETURN",
			TokenType::U8 => "U8",
			TokenType::U16 => "U16",
			TokenType::U32 => "U32",
			TokenType::U64 => "U64",
			TokenType::I8 => "I8",
			TokenType::I16 => "I16",
			TokenType::I32 => "I32",
			TokenType::I64 => "I64",
			TokenType::Bool => "BOOL",
			TokenType::Str => "STR",

			TokenType::String => "STRING",
		})
	}
}

pub fn lookup_ident(keyword: String) -> Token {
	match keyword.as_str() {
		"fn" => Token {
			token_type: TokenType::Function,
			literal: TokenLiteral::Static("fn")
		},
		"let" => Token {
			token_type: TokenType::Let,
			literal: TokenLiteral::Static("let")
		},
		"true" => Token {
			token_type: TokenType::True,
			literal: TokenLiteral::Static("true")
		},
		"false" => Token {
			token_type: TokenType::False,
			literal: TokenLiteral::Static("false")
		},
		"if" => Token {
			token_type: TokenType::If,
			literal: TokenLiteral::Static("if")
		},
		"else" => Token {
			token_type: TokenType::Else,
			literal: TokenLiteral::Static("else")
		},
		"return" => Token {
			token_type: TokenType::Return,
			literal: TokenLiteral::Static("return")
		},
		"u8" => Token {
			token_type: TokenType::U8,
			literal: TokenLiteral::Static("u8")
		},
		"u16" => Token {
			token_type: TokenType::U16,
			literal: TokenLiteral::Static("u16")
		},
		"u32" => Token {
			token_type: TokenType::U32,
			literal: TokenLiteral::Static("u32")
		},
		"u64" => Token {
			token_type: TokenType::U64,
			literal: TokenLiteral::Static("u64")
		},
		"i8" => Token {
			token_type: TokenType::I8,
			literal: TokenLiteral::Static("i8")
		},
		"i16" => Token {
			token_type: TokenType::I16,
			literal: TokenLiteral::Static("i16")
		},
		"i32" => Token {
			token_type: TokenType::I32,
			literal: TokenLiteral::Static("i32")
		},
		"i64" => Token {
			token_type: TokenType::I64,
			literal: TokenLiteral::Static("i64")
		},
		"bool" => Token {
			token_type: TokenType::Bool,
			literal: TokenLiteral::Static("bool")
		},
		"str" => Token {
			token_type: TokenType::Str,
			literal: TokenLiteral::Static("str")
		},
		_ => Token {
			token_type: TokenType::Ident,
			literal: TokenLiteral::String(keyword)
		},
	}
}