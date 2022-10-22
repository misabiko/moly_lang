use moly::{
	lexer::Lexer,
	token::TokenType,
};
use moly::token::{IntType, Token, TokenLiteral};

#[test]
fn test_next_token() {
	const INPUT: &str = r#"let five = 5;
let ten10 = 10;

u8 u16 u32 u64
i8 i16 i32 i64
bool str

let add = fn(x u8, y u8) u8 {
  x + y
};

let result = add(five, ten);
!-*/5;
5 < 10 > 5;

if 5 < 10 {
    return true;
} else {
    return false;
}

//
// my comment
/**/
/*
*/
/* my multi
line
comment*/

10 == 10;
10 != 9;
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
"#;
//TODO Replace hash with struct

	let tests = vec![
		Token { token_type: TokenType::Let, literal: TokenLiteral::Static("let"), after_whitespace: false},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("five".into()), after_whitespace: true},
		Token { token_type: TokenType::Assign, literal: TokenLiteral::Static("="), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(5), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::Let, literal: TokenLiteral::Static("let"), after_whitespace: true},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("ten10".into()), after_whitespace: true},
		Token { token_type: TokenType::Assign, literal: TokenLiteral::Static("="), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(10), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::IntegerType(IntType::U8), literal: TokenLiteral::Static("u8"), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::U16), literal: TokenLiteral::Static("u16"), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::U32), literal: TokenLiteral::Static("u32"), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::U64), literal: TokenLiteral::Static("u64"), after_whitespace: true},

		Token { token_type: TokenType::IntegerType(IntType::I8), literal: TokenLiteral::Static("i8"), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::I16), literal: TokenLiteral::Static("i16"), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::I32), literal: TokenLiteral::Static("i32"), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::I64), literal: TokenLiteral::Static("i64"), after_whitespace: true},

		Token { token_type: TokenType::Bool, literal: TokenLiteral::Static("bool"), after_whitespace: true},
		Token { token_type: TokenType::Str, literal: TokenLiteral::Static("str"), after_whitespace: true},

		Token { token_type: TokenType::Let, literal: TokenLiteral::Static("let"), after_whitespace: true},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("add".into()), after_whitespace: true},
		Token { token_type: TokenType::Assign, literal: TokenLiteral::Static("="), after_whitespace: true},
		Token { token_type: TokenType::Function, literal: TokenLiteral::Static("fn"), after_whitespace: true},
		Token { token_type: TokenType::LParen, literal: TokenLiteral::Static("("), after_whitespace: false},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("x".into()), after_whitespace: false},
		Token { token_type: TokenType::IntegerType(IntType::U8), literal: TokenLiteral::Static("u8"), after_whitespace: true},
		Token { token_type: TokenType::Comma, literal: TokenLiteral::Static(","), after_whitespace: false},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("y".into()), after_whitespace: true},
		Token { token_type: TokenType::IntegerType(IntType::U8), literal: TokenLiteral::Static("u8"), after_whitespace: true},
		Token { token_type: TokenType::RParen, literal: TokenLiteral::Static(")"), after_whitespace: false},
		Token { token_type: TokenType::IntegerType(IntType::U8), literal: TokenLiteral::Static("u8"), after_whitespace: true},
		Token { token_type: TokenType::LBrace, literal: TokenLiteral::Static("{"), after_whitespace: true},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("x".into()), after_whitespace: true},
		Token { token_type: TokenType::Plus, literal: TokenLiteral::Static("+"), after_whitespace: true},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("y".into()), after_whitespace: true},
		Token { token_type: TokenType::RBrace, literal: TokenLiteral::Static("}"), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::Let, literal: TokenLiteral::Static("let"), after_whitespace: true},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("result".into()), after_whitespace: true},
		Token { token_type: TokenType::Assign, literal: TokenLiteral::Static("="), after_whitespace: true},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("add".into()), after_whitespace: true},
		Token { token_type: TokenType::LParen, literal: TokenLiteral::Static("("), after_whitespace: false},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("five".into()), after_whitespace: false},
		Token { token_type: TokenType::Comma, literal: TokenLiteral::Static(","), after_whitespace: false},
		Token { token_type: TokenType::Ident, literal: TokenLiteral::String("ten".into()), after_whitespace: true},
		Token { token_type: TokenType::RParen, literal: TokenLiteral::Static(")"), after_whitespace: false},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::Bang, literal: TokenLiteral::Static("!"), after_whitespace: true},
		Token { token_type: TokenType::Minus, literal: TokenLiteral::Static("-"), after_whitespace: false},
		Token { token_type: TokenType::Asterisk, literal: TokenLiteral::Static("*"), after_whitespace: false},
		Token { token_type: TokenType::Slash, literal: TokenLiteral::Static("/"), after_whitespace: false},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(5), after_whitespace: false},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(5), after_whitespace: true},
		Token { token_type: TokenType::LT, literal: TokenLiteral::Static("<"), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(10), after_whitespace: true},
		Token { token_type: TokenType::GT, literal: TokenLiteral::Static(">"), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(5), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::If, literal: TokenLiteral::Static("if"), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(5), after_whitespace: true},
		Token { token_type: TokenType::LT, literal: TokenLiteral::Static("<"), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(10), after_whitespace: true},
		Token { token_type: TokenType::LBrace, literal: TokenLiteral::Static("{"), after_whitespace: true},
		Token { token_type: TokenType::Return, literal: TokenLiteral::Static("return"), after_whitespace: true},
		Token { token_type: TokenType::True, literal: TokenLiteral::Static("true"), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},
		Token { token_type: TokenType::RBrace, literal: TokenLiteral::Static("}"), after_whitespace: true},
		Token { token_type: TokenType::Else, literal: TokenLiteral::Static("else"), after_whitespace: true},
		Token { token_type: TokenType::LBrace, literal: TokenLiteral::Static("{"), after_whitespace: true},
		Token { token_type: TokenType::Return, literal: TokenLiteral::Static("return"), after_whitespace: true},
		Token { token_type: TokenType::False, literal: TokenLiteral::Static("false"), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},
		Token { token_type: TokenType::RBrace, literal: TokenLiteral::Static("}"), after_whitespace: true},

		Token { token_type: TokenType::LineComment, literal: TokenLiteral::String("".into()), after_whitespace: true},
		Token { token_type: TokenType::LineComment, literal: TokenLiteral::String(" my comment".into()), after_whitespace: true},
		Token { token_type: TokenType::MultilineComment, literal: TokenLiteral::String("".into()), after_whitespace: true},
		Token { token_type: TokenType::MultilineComment, literal: TokenLiteral::String("\n".into()), after_whitespace: true},
		Token { token_type: TokenType::MultilineComment, literal: TokenLiteral::String(" my multi
line
comment".into()), after_whitespace: true},

		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(10), after_whitespace: true},
		Token { token_type: TokenType::Eq, literal: TokenLiteral::Static("=="), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(10), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(10), after_whitespace: true},
		Token { token_type: TokenType::NotEq, literal: TokenLiteral::Static("!="), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(9), after_whitespace: true},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::String, literal: TokenLiteral::String("foobar".into()), after_whitespace: true},

		Token { token_type: TokenType::String, literal: TokenLiteral::String("foo bar".into()), after_whitespace: true},

		Token { token_type: TokenType::LBracket, literal: TokenLiteral::Static("["), after_whitespace: true},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(1), after_whitespace: false},
		Token { token_type: TokenType::Comma, literal: TokenLiteral::Static(","), after_whitespace: false},
		Token { token_type: TokenType::Int, literal: TokenLiteral::Integer(2), after_whitespace: true},
		Token { token_type: TokenType::RBracket, literal: TokenLiteral::Static("]"), after_whitespace: false},
		Token { token_type: TokenType::Semicolon, literal: TokenLiteral::Static(";"), after_whitespace: false},

		Token { token_type: TokenType::LBrace, literal: TokenLiteral::Static("{"), after_whitespace: true},
		Token { token_type: TokenType::String, literal: TokenLiteral::String("foo".into()), after_whitespace: false},
		Token { token_type: TokenType::Colon, literal: TokenLiteral::Static(":"), after_whitespace: false},
		Token { token_type: TokenType::String, literal: TokenLiteral::String("bar".into()), after_whitespace: true},
		Token { token_type: TokenType::RBrace, literal: TokenLiteral::Static("}"), after_whitespace: false},

		Token { token_type: TokenType::EOF, literal: TokenLiteral::Static(""), after_whitespace: true},
	];

	let mut lexer = Lexer::new(INPUT);

	for expected in tests {
		assert_eq!(lexer.next_token_with_comments(), expected);
	}
	let result = 2 + 2;
	assert_eq!(result, 4);
}