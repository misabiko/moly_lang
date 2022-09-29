extern crate core;

use std::path::PathBuf;
use crate::compiler::{Bytecode, Compiler};
use crate::lexer::Lexer;
use crate::parser::{Parser, ParserError};
use crate::type_checker::{TypeChecker, TypeCheckError};
use crate::vm::VM;

pub mod token;
pub mod lexer;
pub mod repl;
pub mod parser;
pub mod ast;
pub mod object;
pub mod code;
pub mod compiler;
pub mod vm;
pub mod type_checker;

//TODO Error if returned value is not handled
//TODO (Go-style) Skip struct initializing field names and depend on order
//TODO Explore how Elm did List with "1 or more" length
//TODO Static string in sum type to pattern match with
	/* type BooleanOperator = "<" | ">" | "<=" | ">=" | "==" | "!="; */
//TODO Store line number in Result type for better stack trace
//Destructure struct directly fn parameters

pub fn build(input: &str) -> Result<Bytecode, String> {
	let mut parser = Parser::new(Lexer::new(input));

	let program = match parser.parse_program() {
		Ok(program) => program,
		Err(err) => {
			return Err(format!("Parsing error: {}", err));
		}
	};

	let mut type_checker = TypeChecker::new();
	let program = match type_checker.check(program, true) {
		Ok(program) => program,
		Err(err) => {
			return Err(format!("Type checking error: {:?}", err));
		}
	};

	let mut compiler = Compiler::new();
	if let Err(err) = compiler.compile(program) {
		Err(format!("Compilation failed:\n{}", err))
	}else {
		Ok(compiler.bytecode())
	}
}

pub fn run_string(input: &str) {
	let bytecode = match build(input) {
		Ok(b) => b,
		Err(err) => {
			eprintln!("{}", err);
			return;
		}
	};

	let mut machine = VM::new(bytecode);
	if let Err(err) = machine.run() {
		eprintln!("Executing bytecode failed:\n{}", err);
		return
	}

	if let Some(obj) = machine.last_popped_stack_elem {
		println!("{}", obj)
	}else {
		println!()
	}
}

pub fn run_file(file: PathBuf) {
	let input = std::fs::read_to_string(file).expect("failed to read file");

	run_string(&input);
}

#[derive(Debug, PartialEq)]
pub enum MolyError {
	Parse(ParserError),
	TypeCheck(TypeCheckError),
}