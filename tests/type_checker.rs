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

		if let TypedStatement::Expression { expr: TypedExpression::Boolean(value), has_semicolon: false } = stmt {
			assert_eq!(value, expected_value);
		} else {
			panic!("{:?} not TypedExpression(Boolean)", stmt)
		}
	}
}

/*#[test]
fn test_function_parameter_parsing() {
	let tests = vec![
		("fn() {};", vec![]),
		("fn(x u8) {};", vec![
			("x", "u8")
		]),
		("fn(x u8, y str, z i16) {};", vec![
			("x", "u8"),
			("y", "str"),
			("z", "i16"),
		]),
	];

	for (input, expected_params) in tests {
		let stmt = parse_single_statement(input);
		if let Statement::Expression { expr: Expression::Function { parameters, .. }, has_semicolon: _ } = stmt {
			assert_eq!(parameters.len(), expected_params.len());

			for (param, expected_param) in parameters.iter().zip(expected_params.iter()) {
				assert_eq!(param, expected_param)
			}
		} else {
			panic!("{:?} is not Statement::Expression(Function)", stmt);
		}
	}
}*/

//TODO if
//TODO prefix
//TODO infix

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