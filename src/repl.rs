use std::io::Write;
use std::process::ExitCode;
use ctrlc;
use crate::{
	lexer::Lexer,
	token::TokenType
};

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

		let mut lexer = Lexer::new(&buffer);

		loop {
			let token = lexer.next_token();
			if let TokenType::EOF = token.token_type {
				break;
			}

			println!("{:?}", token);
		}
	}
}