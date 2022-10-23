use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc;
use ctrlc;
use crate::lexer::Lexer;
use crate::compiler::Compiler;
use crate::compiler::symbol_table::SymbolTable;
use crate::object::builtins::get_builtins;
use crate::object::Object;
use crate::parser::Parser;
use crate::token::TokenType;
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

	println!("Enter commands, use Ctrl-C or enter `exit` to exit:");

	loop {
		match prompt(std::io::stdin().lock(), std::io::stdout(), &mut constants, &mut globals, &mut symbol_table) {
			PromptResult::Continue => {}
			PromptResult::Exit => std::process::exit(0),
			PromptResult::Err(err) => eprintln!("{}", err)
		}
	}
}

fn prompt<R: BufRead, W: Write>(
	mut reader: R,
	mut writer: W,
	constants: &mut Vec<Object>,
	globals: &mut Vec<Object>,
	symbol_table: &mut Rc<RefCell<SymbolTable>>
) -> PromptResult {
	write!(writer, "{}", PROMPT).expect("failed to write");
	writer.flush().expect("failed to flush stdout");

	let mut buffer = String::new();
	reader.read_line(&mut buffer).expect("failed to read line from stdin");

	if buffer.trim() == "exit" {
		return PromptResult::Exit;
	}

	let mut parser = Parser::new(Lexer::new(&buffer));

	let program = match parser.parse_block_statement(TokenType::EOF) {
		Ok(program) => program,
		Err(err) => return PromptResult::Err(format!("Parsing error: {}", err)),
	};

	let mut type_checker = TypeChecker::new();
	let program = match type_checker.check_block(program, true, false) {
		Ok(program) => program,
		Err(err) => return PromptResult::Err(format!("Type checking error: {:?}", err)),
	};

	//Might be able to not clone, with some std::mem::take
	let mut compiler = Compiler::new_with_state(symbol_table.clone(), constants.clone());
	if let Err(err) = compiler.compile_block(program) {
		return PromptResult::Err(format!("Compilation failed:\n{}", err));
	}

	*symbol_table = compiler.symbol_table.clone();
	*constants = compiler.constants.clone();

	let mut machine = VM::new_with_global_store(compiler.bytecode(), globals.clone());
	if let Err(err) = machine.run() {
		return PromptResult::Err(format!("Executing bytecode failed:\n{}", err));
	}

	*globals = machine.globals.clone();

	if let Some(obj) = machine.stack_top() {
		writeln!(writer, "{}", obj).expect("failed to write")
	}else {
		writeln!(writer).expect("failed to write")
	}

	PromptResult::Continue
}

#[derive(Debug, PartialEq)]
enum PromptResult {
	Continue,
	Exit,
	//Could return MolyError instead
	Err(String),
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_repl() {
		let input = b"5 + 5";
		let mut output = Vec::new();

		let result = single_prompt(input, &mut output);

		assert_eq!(result, PromptResult::Continue);
		assert_eq!(std::str::from_utf8(&output), Ok(">> 10\n"));
	}

	#[test]
	fn test_exit() {
		let input = b"exit";
		let mut output = Vec::new();

		let result = single_prompt(input, &mut output);

		assert_eq!(result, PromptResult::Exit);
		assert_eq!(std::str::from_utf8(&output), Ok(">> "));
	}

	fn single_prompt(input: &[u8], output: &mut Vec<u8>) -> PromptResult {
		let mut constants = Vec::new();
		let mut globals = Vec::with_capacity(GLOBALS_SIZE);
		let mut symbol_table = Rc::new(RefCell::new(SymbolTable::new(None)));

		prompt(input, output, &mut constants, &mut globals, &mut symbol_table)
	}
}