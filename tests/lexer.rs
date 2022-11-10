use moly_lib::{
	lexer::Lexer,
	token::TokenType,
};
use moly_lib::token::{IntType, TokenLiteral};

#[test]
fn test_next_token() {
	const INPUT: &str = r#"let five = 5;
let ten10 = 10;
float43.field = 43.0;

u8 u16 u32 u64
i8 i16 i32 i64
f32 bool str

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

unless while until for

//
// my comment
/**/
/*
*/
/* my multi
line
comment*/
/* /**/ /* /**/ */*/

10 == 10;
10 != 9;
true && false;
false || false;
"foobar"
"foo bar"
[1, 2];

struct MyStruct {
	foo str,
	bar u8,
}
MyStruct {foo: "bar", bar: 8}
s.foo

enum Fruits {
	Apple,
	Banana,
	Cherry,
}

enum Fruits => Apple | Banana | Cherry;
"#;

	let tests = vec![
		(TokenType::Let, TokenLiteral::Static("let"), false, 0),
		(TokenType::Ident, TokenLiteral::String("five".into()), true, 0),
		(TokenType::Assign, TokenLiteral::Static("="), true, 0),
		(TokenType::Int, TokenLiteral::Integer(5), true, 0),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 0),
		(TokenType::Let, TokenLiteral::Static("let"), true, 1),
		(TokenType::Ident, TokenLiteral::String("ten10".into()), true, 1),
		(TokenType::Assign, TokenLiteral::Static("="), true, 1),
		(TokenType::Int, TokenLiteral::Integer(10), true, 1),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 1),
		(TokenType::Ident, TokenLiteral::String("float43".into()), true, 2),
		(TokenType::Dot, TokenLiteral::Static("."), false, 2),
		(TokenType::Ident, TokenLiteral::String("field".into()), false, 2),
		(TokenType::Assign, TokenLiteral::Static("="), true, 2),
		(TokenType::Float, TokenLiteral::Float(43, 0), true, 2),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 2),
		(TokenType::IntegerType(IntType::U8), TokenLiteral::Static("u8"), true, 4),
		(TokenType::IntegerType(IntType::U16), TokenLiteral::Static("u16"), true, 4),
		(TokenType::IntegerType(IntType::U32), TokenLiteral::Static("u32"), true, 4),
		(TokenType::IntegerType(IntType::U64), TokenLiteral::Static("u64"), true, 4),
		(TokenType::IntegerType(IntType::I8), TokenLiteral::Static("i8"), true, 5),
		(TokenType::IntegerType(IntType::I16), TokenLiteral::Static("i16"), true, 5),
		(TokenType::IntegerType(IntType::I32), TokenLiteral::Static("i32"), true, 5),
		(TokenType::IntegerType(IntType::I64), TokenLiteral::Static("i64"), true, 5),
		(TokenType::FloatType, TokenLiteral::Static("f32"), true, 6),
		(TokenType::Bool, TokenLiteral::Static("bool"), true, 6),
		(TokenType::Str, TokenLiteral::Static("str"), true, 6),
		(TokenType::Let, TokenLiteral::Static("let"), true, 8),
		(TokenType::Ident, TokenLiteral::String("add".into()), true, 8),
		(TokenType::Assign, TokenLiteral::Static("="), true, 8),
		(TokenType::Function, TokenLiteral::Static("fn"), true, 8),
		(TokenType::LParen, TokenLiteral::Static("("), false, 8),
		(TokenType::Ident, TokenLiteral::String("x".into()), false, 8),
		(TokenType::IntegerType(IntType::U8), TokenLiteral::Static("u8"), true, 8),
		(TokenType::Comma, TokenLiteral::Static(","), false, 8),
		(TokenType::Ident, TokenLiteral::String("y".into()), true, 8),
		(TokenType::IntegerType(IntType::U8), TokenLiteral::Static("u8"), true, 8),
		(TokenType::RParen, TokenLiteral::Static(")"), false, 8),
		(TokenType::IntegerType(IntType::U8), TokenLiteral::Static("u8"), true, 8),
		(TokenType::LBrace, TokenLiteral::Static("{"), true, 8),
		(TokenType::Ident, TokenLiteral::String("x".into()), true, 9),
		(TokenType::Plus, TokenLiteral::Static("+"), true, 9),
		(TokenType::Ident, TokenLiteral::String("y".into()), true, 9),
		(TokenType::RBrace, TokenLiteral::Static("}"), true, 10),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 10),
		(TokenType::Let, TokenLiteral::Static("let"), true, 12),
		(TokenType::Ident, TokenLiteral::String("result".into()), true, 12),
		(TokenType::Assign, TokenLiteral::Static("="), true, 12),
		(TokenType::Ident, TokenLiteral::String("add".into()), true, 12),
		(TokenType::LParen, TokenLiteral::Static("("), false, 12),
		(TokenType::Ident, TokenLiteral::String("five".into()), false, 12),
		(TokenType::Comma, TokenLiteral::Static(","), false, 12),
		(TokenType::Ident, TokenLiteral::String("ten".into()), true, 12),
		(TokenType::RParen, TokenLiteral::Static(")"), false, 12),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 12),
		(TokenType::Bang, TokenLiteral::Static("!"), true, 13),
		(TokenType::Minus, TokenLiteral::Static("-"), false, 13),
		(TokenType::Asterisk, TokenLiteral::Static("*"), false, 13),
		(TokenType::Slash, TokenLiteral::Static("/"), false, 13),
		(TokenType::Int, TokenLiteral::Integer(5), false, 13),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 13),
		(TokenType::Int, TokenLiteral::Integer(5), true, 14),
		(TokenType::LT, TokenLiteral::Static("<"), true, 14),
		(TokenType::Int, TokenLiteral::Integer(10), true, 14),
		(TokenType::GT, TokenLiteral::Static(">"), true, 14),
		(TokenType::Int, TokenLiteral::Integer(5), true, 14),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 14),
		(TokenType::If, TokenLiteral::Static("if"), true, 16),
		(TokenType::Int, TokenLiteral::Integer(5), true, 16),
		(TokenType::LT, TokenLiteral::Static("<"), true, 16),
		(TokenType::Int, TokenLiteral::Integer(10), true, 16),
		(TokenType::LBrace, TokenLiteral::Static("{"), true, 16),
		(TokenType::Return, TokenLiteral::Static("return"), true, 17),
		(TokenType::True, TokenLiteral::Static("true"), true, 17),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 17),
		(TokenType::RBrace, TokenLiteral::Static("}"), true, 18),
		(TokenType::Else, TokenLiteral::Static("else"), true, 18),
		(TokenType::LBrace, TokenLiteral::Static("{"), true, 18),
		(TokenType::Return, TokenLiteral::Static("return"), true, 19),
		(TokenType::False, TokenLiteral::Static("false"), true, 19),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 19),
		(TokenType::RBrace, TokenLiteral::Static("}"), true, 20),
		(TokenType::Unless, TokenLiteral::Static("unless"), true, 22),
		(TokenType::While, TokenLiteral::Static("while"), true, 22),
		(TokenType::Until, TokenLiteral::Static("until"), true, 22),
		(TokenType::For, TokenLiteral::Static("for"), true, 22),
		(TokenType::LineComment, TokenLiteral::String("".into()), true, 24),
		(TokenType::LineComment, TokenLiteral::String(" my comment".into()), true, 25),
		(TokenType::MultilineComment, TokenLiteral::String("".into()), true, 26),
		(TokenType::MultilineComment, TokenLiteral::String("\n".into()), true, 28),
		(TokenType::MultilineComment, TokenLiteral::String(" my multi
line
comment".into()), true, 31),
		(TokenType::MultilineComment, TokenLiteral::String(" /**/ /* /**/ */".into()), true, 32),
		(TokenType::Int, TokenLiteral::Integer(10), true, 34),
		(TokenType::Eq, TokenLiteral::Static("=="), true, 34),
		(TokenType::Int, TokenLiteral::Integer(10), true, 34),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 34),
		(TokenType::Int, TokenLiteral::Integer(10), true, 35),
		(TokenType::NotEq, TokenLiteral::Static("!="), true, 35),
		(TokenType::Int, TokenLiteral::Integer(9), true, 35),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 35),
		(TokenType::True, TokenLiteral::Static("true"), true, 36),
		(TokenType::And, TokenLiteral::Static("&&"), true, 36),
		(TokenType::False, TokenLiteral::Static("false"), true, 36),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 36),
		(TokenType::False, TokenLiteral::Static("false"), true, 37),
		(TokenType::Or, TokenLiteral::Static("||"), true, 37),
		(TokenType::False, TokenLiteral::Static("false"), true, 37),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 37),
		(TokenType::String, TokenLiteral::String("foobar".into()), true, 38),
		(TokenType::String, TokenLiteral::String("foo bar".into()), true, 39),
		(TokenType::LBracket, TokenLiteral::Static("["), true, 40),
		(TokenType::Int, TokenLiteral::Integer(1), false, 40),
		(TokenType::Comma, TokenLiteral::Static(","), false, 40),
		(TokenType::Int, TokenLiteral::Integer(2), true, 40),
		(TokenType::RBracket, TokenLiteral::Static("]"), false, 40),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 40),
		(TokenType::Struct, TokenLiteral::Static("struct"), true, 42),
		(TokenType::Ident, TokenLiteral::String("MyStruct".into()), true, 42),
		(TokenType::LBrace, TokenLiteral::Static("{"), true, 42),
		(TokenType::Ident, TokenLiteral::String("foo".into()), true, 43),
		(TokenType::Str, TokenLiteral::Static("str"), true, 43),
		(TokenType::Comma, TokenLiteral::Static(","), false, 43),
		(TokenType::Ident, TokenLiteral::String("bar".into()), true, 44),
		(TokenType::IntegerType(IntType::U8), TokenLiteral::Static("u8"), true, 44),
		(TokenType::Comma, TokenLiteral::Static(","), false, 44),
		(TokenType::RBrace, TokenLiteral::Static("}"), true, 45),
		(TokenType::Ident, TokenLiteral::String("MyStruct".into()), true, 46),
		(TokenType::LBrace, TokenLiteral::Static("{"), true, 46),
		(TokenType::Ident, TokenLiteral::String("foo".into()), false, 46),
		(TokenType::Colon, TokenLiteral::Static(":"), false, 46),
		(TokenType::String, TokenLiteral::String("bar".into()), true, 46),
		(TokenType::Comma, TokenLiteral::Static(","), false, 46),
		(TokenType::Ident, TokenLiteral::String("bar".into()), true, 46),
		(TokenType::Colon, TokenLiteral::Static(":"), false, 46),
		(TokenType::Int, TokenLiteral::Integer(8), true, 46),
		(TokenType::RBrace, TokenLiteral::Static("}"), false, 46),
		(TokenType::Ident, TokenLiteral::String("s".into()), true, 47),
		(TokenType::Dot, TokenLiteral::Static("."), false, 47),
		(TokenType::Ident, TokenLiteral::String("foo".into()), false, 47),
		(TokenType::Enum, TokenLiteral::Static("enum"), true, 49),
		(TokenType::Ident, TokenLiteral::String("Fruits".into()), true, 49),
		(TokenType::LBrace, TokenLiteral::Static("{"), true, 49),
		(TokenType::Ident, TokenLiteral::String("Apple".into()), true, 50),
		(TokenType::Comma, TokenLiteral::Static(","), false, 50),
		(TokenType::Ident, TokenLiteral::String("Banana".into()), true, 51),
		(TokenType::Comma, TokenLiteral::Static(","), false, 51),
		(TokenType::Ident, TokenLiteral::String("Cherry".into()), true, 52),
		(TokenType::Comma, TokenLiteral::Static(","), false, 52),
		(TokenType::RBrace, TokenLiteral::Static("}"), true, 53),
		(TokenType::Enum, TokenLiteral::Static("enum"), true, 55),
		(TokenType::Ident, TokenLiteral::String("Fruits".into()), true, 55),
		(TokenType::BigRightArrow, TokenLiteral::Static("=>"), true, 55),
		(TokenType::Ident, TokenLiteral::String("Apple".into()), true, 55),
		(TokenType::VBar, TokenLiteral::Static("|"), true, 55),
		(TokenType::Ident, TokenLiteral::String("Banana".into()), true, 55),
		(TokenType::VBar, TokenLiteral::Static("|"), true, 55),
		(TokenType::Ident, TokenLiteral::String("Cherry".into()), true, 55),
		(TokenType::Semicolon, TokenLiteral::Static(";"), false, 55),
		(TokenType::EOF, TokenLiteral::Static(""), true, 56),
	];

	let mut lexer = Lexer::new(INPUT);

	for (i, (token_type, literal, after_whitespace, line)) in tests.into_iter().enumerate() {
		let actual = lexer.next_token_with_comments();
		assert_eq!(token_type, actual.token_type, "{}: expected {:?}", i, (token_type, literal.clone(), after_whitespace, line));
		assert_eq!(literal, actual.literal, "{}: expected {:?}", i, (token_type, literal.clone(), after_whitespace, line));
		assert_eq!(after_whitespace, actual.after_whitespace, "{}: expected {:?}", i, (token_type, literal.clone(), after_whitespace, line));

		assert_eq!(lexer.line, line, "wrong line for {:?}", actual);
	}
}

#[test]
fn test_position_and_line() {
	let mut lexer = Lexer::new("fn (x bool, y str) {
	x + y;
}");

	let expected_tokens = vec![
		(TokenType::Function, 0, 0),
		(TokenType::LParen, 0, 3),
		(TokenType::Ident, 0, 4),
		(TokenType::Bool, 0, 6),
		(TokenType::Comma, 0, 10),
		(TokenType::Ident, 0, 12),
		(TokenType::Str, 0, 14),
		(TokenType::RParen, 0, 17),
		(TokenType::LBrace, 0, 19),
		(TokenType::Ident, 1, 22),
		(TokenType::Plus, 1, 24),
		(TokenType::Ident, 1, 26),
		(TokenType::Semicolon, 1, 27),
		(TokenType::RBrace, 2, 29),
	];

	for (token_type, line, position) in expected_tokens {
		let actual = lexer.next_token_with_comments();
		assert_eq!(token_type, actual.token_type);
		assert_eq!(line, actual.line, "wrong line for {}", token_type);
		assert_eq!(position, actual.position, "wrong position for {}", token_type);
	}
}