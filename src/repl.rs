use std::io::Write;
use ctrlc;
use crate::{
	lexer::Lexer,
};
use crate::parser::Parser;

const PROMPT: &'static str = ">> ";

pub fn start() {
	ctrlc::set_handler(|| std::process::exit(0))
		.expect("Error setting Ctrl-C handler");

	loop {
		print!("{} ", PROMPT);
		std::io::stdout().flush()
			.expect("Failed to flush stdout");

		let mut buffer = String::new();
		std::io::stdin().read_line(&mut buffer)
			.expect("Failed to read line from stdin");

		let mut parser = Parser::new(Lexer::new(&buffer));

		let program = parser.parse_program();
		if !parser.errors.is_empty() {
			print_parser_errors(&parser.errors);
			continue;
		}

		println!("{}", program);
	}
}

fn print_parser_errors(errors: &Vec<String>) {
	for msg in errors {
		println!("\t{}", msg);
	}
}