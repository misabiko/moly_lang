use moly::lexer::Lexer;
use moly::parser::{Parser};
use moly::type_checker::TypeChecker;
use moly::type_checker::typed_ast::{TypedExpression, TypedProgram, TypedStatement};

#[test]
fn test_boolean_expression() {
	let tests = vec![
		("true", true),
		("false", false),
	];

	for (input, expected_value) in tests {
		let stmt = type_check_single_statement(input);

		if let TypedStatement::Expression(TypedExpression::Boolean(value)) = stmt {
			assert_eq!(value, expected_value);
		} else {
			panic!("{:?} not TypedExpression(Boolean)", stmt)
		}
	}
}

struct TestCase {
	input: &'static str,
}

fn type_check(input: &str) -> TypedProgram {
	let program = match Parser::new(Lexer::new(input)).parse_program() {
		Ok(p) => p,
		Err(err) => panic!("parse error: {}", err),
	};

	let mut type_checker = TypeChecker::new();
	match type_checker.check(program) {
		Ok(program) => program,
		Err(err) => {
			panic!("type check error: {:?}", err)
		}
	}
}

fn type_check_single_statement(input: &str) -> TypedStatement {
	let program = type_check(input);

	assert_eq!(program.statements.len(), 1, "program.statements does not contain 1 statement");

	program.statements.into_iter().next().unwrap()
}