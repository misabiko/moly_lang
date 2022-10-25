extern crate core;

use std::path::PathBuf;

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
pub mod wasm;

use crate::compiler::{Bytecode, Compiler};
use crate::lexer::Lexer;
use crate::parser::{Parser, ParserError};
use crate::token::TokenType;
use crate::type_checker::{TypeChecker, TypeCheckError};
use crate::vm::VM;

//TODO Error if returned value is not handled
//TODO (Go-style) Skip struct initializing field names and depend on order
//TODO Explore how Elm did List with "1 or more" length
//TODO Static string in sum type to pattern match with
	/* type BooleanOperator = "<" | ">" | "<=" | ">=" | "==" | "!="; */
//TODO Store line number in Result type for better stack trace
//Destructure struct directly fn parameters

pub fn build(input: &str, full_program: bool) -> Result<Bytecode, String> {
	let mut parser = Parser::new(Lexer::new(input));

	let program = if full_program {
		parser.parse_program()
	}else {
		parser.parse_block_statement(TokenType::EOF)
	};
	let program = match program {
		Ok(program) => program,
		Err(err) => {
			return Err(format!("Parsing error: {}", err));
		}
	};

	let mut type_checker = TypeChecker::new();

	let mut compiler = Compiler::new();
	let compiled = if full_program {
		let program = match type_checker.check(program, true) {
			Ok(program) => program,
			Err(err) => {
				return Err(format!("Type checking error: {:?}", err));
			}
		};
		compiler.compile(program)
	}else {
		let program = match type_checker.check_block(program, true, false) {
			Ok(program) => program,
			Err(err) => {
				return Err(format!("Type checking error: {:?}", err));
			}
		};
		compiler.compile_block(program)
	};
	if let Err(err) = compiled {
		Err(format!("Compilation failed:\n{}", err))
	}else {
		Ok(compiler.bytecode())
	}
}

pub fn run_string(input: &str, full_program: bool) {
	let bytecode = match build(input, full_program) {
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

	if let Some(obj) = machine.stack_top() {
		println!("{}", obj)
	}else {
		println!()
	}
}

pub fn run_file(file: PathBuf) {
	let input = match std::fs::read_to_string(file.clone()) {
		Ok(ok) => ok,
		Err(err) => panic!("failed to read {:?}:{}", std::env::current_dir().unwrap().join(file), err),
	};

	run_string(&input, true);
}

#[derive(Debug, PartialEq)]
pub enum MolyError {
	Parse(ParserError),
	TypeCheck(TypeCheckError),
}