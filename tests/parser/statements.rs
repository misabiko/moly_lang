use moly::ast::{Expression, IntExpr, ParsedType, Statement, StructDecl};
use moly::token::IntType;
use moly::type_checker::type_env::TypeExpr;

use crate::parse_single_statement;

#[test]
fn test_let_statements() {
	let tests = vec![
		("let x = 5;", "x", Expression::Integer(IntExpr::U8(5))),
		("let y = true;", "y", Expression::Boolean(true)),
		("let foobar = y;", "foobar", Expression::Identifier("y".into())),
	];

	for (input, expected_identifier, expected_value) in tests {
		match parse_single_statement(input) {
			Err(err) => panic!("parse error: {}", err),
			Ok(Statement::Let { name, value }) => {
				assert_eq!(&name, expected_identifier, "wrong let statement identifier");

				assert_eq!(value, expected_value, "wrong let statement value");
			}
			Ok(stmt) => panic!("{:?} not Let", stmt),
		}
	}
}

#[test]
fn test_return_statements() {
	let tests = vec![
		("return 5;", Expression::Integer(IntExpr::U8(5))),
		("return true;", Expression::Boolean(true)),
		("return foobar;", Expression::Identifier("foobar".into())),
	];

	for (input, expected_value) in tests {
		match parse_single_statement(input) {
			Err(err) => panic!("parse error: {}", err),
			Ok(Statement::Return(Some(value))) => assert_eq!(value, expected_value, "wrong returned value"),
			Ok(Statement::Return(None)) => panic!("missing returned value"),
			Ok(stmt) => panic!("{:?} not Return", stmt),
		}
	}
}

#[test]
fn test_struct_declaration() {
	let tests = vec![
		("struct Person {
			name str,
			age u8,
		}", Statement::Struct {
			name: "Person".into(),
			decl: StructDecl::Block(vec![
				("name".into(), ParsedType::Primitive(TypeExpr::String)),
				("age".into(), ParsedType::Primitive(TypeExpr::Int(IntType::U8))),
			]),
		}),
		("struct Person(str, u8)", Statement::Struct {
			name: "Person".into(),
			decl: StructDecl::Tuple(vec![
				ParsedType::Primitive(TypeExpr::String),
				ParsedType::Primitive(TypeExpr::Int(IntType::U8))
			]),
		}),
	];

	for (input, expected_value) in tests {
		let stmt = parse_single_statement(input).unwrap();

		assert_eq!(stmt, expected_value);
	}
}
