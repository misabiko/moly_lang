use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use ctrlc;
use crate::{
	lexer::Lexer,
};
use crate::compiler::Compiler;
use crate::compiler::symbol_table::SymbolTable;
use crate::object::builtins::get_builtins;
use crate::parser::Parser;
use crate::type_checker::TypeChecker;
use crate::vm::{GLOBALS_SIZE, VM};

const PROMPT: &'static str = ">> ";

pub fn start() {
	ctrlc::set_handler(|| std::process::exit(0))
		.expect("Error setting Ctrl-C handler");

	let mut constants = Vec::new();
	let mut globals = Vec::with_capacity(GLOBALS_SIZE);

	let mut table = SymbolTable::new(None);
	for (i, v) in get_builtins().iter().enumerate() {
		table.define_builtin(i as u8, v.name);
	}
	let mut symbol_table = Rc::new(RefCell::new(table));

	//TODO Show how to exit
	println!("Enter commands");

	loop {
		print!("{} ", PROMPT);
		std::io::stdout().flush()
			.expect("Failed to flush stdout");

		let mut buffer = String::new();
		std::io::stdin().read_line(&mut buffer)
			.expect("Failed to read line from stdin");

		let mut parser = Parser::new(Lexer::new(&buffer));

		let program = match parser.parse_program() {
			Ok(program) => program,
			Err(err) => {
				eprintln!("Parsing error: {}", err);
				continue;
			}
		};

		let mut type_checker = TypeChecker::new();
		let program = match type_checker.check(program) {
			Ok(program) => program,
			Err(err) => {
				eprintln!("Type checking error: {}", err);
				continue;
			}
		};

		//Might be able to not clone, with some std::mem::take
		let mut compiler = Compiler::new_with_state(symbol_table.clone(), constants.clone());
		if let Err(err) = compiler.compile(program) {
			eprintln!("Compilation failed:\n{}", err);
			continue
		}

		symbol_table = compiler.symbol_table.clone();
		constants = compiler.constants.clone();

		let mut machine = VM::new_with_global_store(compiler.bytecode(), globals.clone());
		if let Err(err) = machine.run() {
			eprintln!("Executing bytecode failed:\n{}", err);
			continue
		}

		globals = machine.globals.clone();

		if let Some(obj) = machine.last_popped_stack_elem {
			println!("{}", obj)
		}else {
			println!()
		}
	}
}