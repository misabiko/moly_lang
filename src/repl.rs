use std::io::Write;
use ctrlc;
use crate::{
	lexer::Lexer,
};
use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::vm::VM;

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

		let mut compiler = Compiler::new();
		if let Err(err) = compiler.compile(program) {
			eprintln!("Compilation failed:\n{}", err);
			continue
		}

		let mut machine = VM::new(compiler.bytecode());
		if let Err(err) = machine.run() {
			eprintln!("Executing bytecode failed:\n{}", err);
			continue
		}

		if let Some(obj) = machine.stack_top() {
			println!("{}", obj)
		}else {
			println!()
		}
	}
}

fn print_parser_errors(errors: &Vec<String>) {
	for msg in errors {
		eprintln!("\t{}", msg);
	}
}