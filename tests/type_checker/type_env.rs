use moly_lib::lexer::Lexer;
use moly_lib::MolyError;
use moly_lib::parser::Parser;
use moly_lib::reporting::show_error;
use moly_lib::token::TokenType;
use moly_lib::type_checker::type_env::TypeId;
use moly_lib::type_checker::TypeChecker;

#[test]
fn test_struct_is_registered() {
	let tests = vec![
		"struct Apple {}",
	];

	for input in tests {
        let mut type_checker = TypeChecker::new_without_builtins();

		let stmt = (|| {
			let program = match Parser::new(Lexer::new(input)).parse_block_statement(TokenType::EOF) {
				Ok(p) => p,
				Err(err) => return Err(MolyError::Parse(err)),
			};

			type_checker.push_scope(false);

			match type_checker.check_block(program, false, false) {
				Ok(program) => Ok(program),
				Err(err) => return Err(MolyError::TypeCheck(err)),
			}
		})();

		match stmt {
			Err(err) => {
				eprintln!("{}", show_error(err.clone(), input.to_string()));
				panic!("{}", err);
			}
			Ok(_) => {
				println!("{:#?}", type_checker.type_env);
				let apple_id = type_checker.type_env.get_custom_type(&"Apple".to_string());
				assert_eq!(apple_id, Some(&TypeId::Struct(0)));
			}
		}
	}
}

#[test]
fn test_get_method() {
	// let input = "[1].push(2)";

	let type_checker = TypeChecker::new();
	let method = type_checker.type_env.get_method(
		&"push".to_string(),
		&TypeId::Array(Box::new(TypeId::U8))
	);

	let concrete_array = TypeId::Array(Box::new(TypeId::U8));
	assert_eq!(method, Some(TypeId::Function {
		parameters: vec![
			concrete_array.clone(),
			TypeId::U8
		],
		return_type: Box::new(concrete_array),
		is_method: true
	}));
}
