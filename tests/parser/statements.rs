use moly::ast::{Expression, IntExpr, Statement};

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
			Statement::Let { name, value } => {
				assert_eq!(&name, expected_identifier, "wrong let statement identifier");

				assert_eq!(value, expected_value, "wrong let statement value");
			}
			stmt => panic!("{:?} not Let", stmt),
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
			Statement::Return(Some(value)) => assert_eq!(value, expected_value, "wrong returned value"),
			Statement::Return(None) => panic!("missing returned value"),
			stmt => panic!("{:?} not Return", stmt),
		}
	}
}
