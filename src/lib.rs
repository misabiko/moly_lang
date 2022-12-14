extern crate core;

use std::fmt::{Debug, Formatter};
use std::path::PathBuf;
#[cfg(feature = "llvm")]
use inkwell::context::Context;
use crate::ast::Program;

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
pub mod server;
pub mod reporting;
#[cfg(feature = "llvm")]
pub mod llvm;

use compiler::{Bytecode, Compiler};
use lexer::Lexer;
use parser::Parser;
use token::TokenType;
use type_checker::{TypeChecker, TypeCheckError};
use vm::VM;
#[cfg(feature = "llvm")]
use crate::llvm::LLVMCompiler;
use crate::parser::ParserError;

//TODO Error if returned value is not handled
//TODO (Go-style) Skip struct initializing field names and depend on order
//TODO Explore how Elm did List with "1 or more" length
//TODO Static string in sum type to pattern match with
/* type BooleanOperator = "<" | ">" | "<=" | ">=" | "==" | "!="; */
//TODO Store line number in Result type for better stack trace
//Destructure struct directly fn parameters

pub fn parse(input: &str, full_program: bool) -> Result<Program, ParserError> {
	let mut parser = Parser::new(Lexer::new(input));

	if full_program {
		parser.parse_program()
	} else {
		parser.parse_block_statement(TokenType::EOF)
	}
}

pub fn build(input: &str, full_program: bool) -> Result<Bytecode, String> {
	let program = match parse(input, full_program) {
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
	} else {
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
	} else {
		Ok(compiler.bytecode())
	}
}

pub fn build_wasm(input: &str, full_program: bool) -> Result<Vec<u8>, String> {
	let program = match parse(input, full_program) {
		Ok(program) => program,
		Err(err) => {
			return Err(format!("Parsing error: {}", err));
		}
	};

	let mut type_checker = TypeChecker::new();

	let bytecode = /*if full_program {
		let program = match type_checker.check(program, true) {
			Ok(program) => program,
			Err(err) => {
				return Err(format!("Type checking error: {:?}", err));
			}
		};
		compiler.compile(program)
	}else */{
		let program = match type_checker.check_block(program, true, false) {
			Ok(program) => program,
			Err(err) => {
				return Err(format!("Type checking error: {:?}", err));
			}
		};
		wasm::compile_block_with_header(program)
	};
	match bytecode {
		Ok(b) => Ok(b),
		Err(err) => {
			Err(format!("Compilation failed:\n{}", err))
		}
	}
}

#[cfg(feature = "llvm")]
pub fn build_llvm(input: &str) -> Result<String, String> {
	let context = Context::create();
	let module = context.create_module("built");
	let builder = context.create_builder();

	let program = match parse(input, true) {
		Ok(program) => program,
		Err(err) => {
			return Err(format!("Parsing error: {}", err));
		}
	};

	let mut type_checker = TypeChecker::new();
	let program = match type_checker.check_block(program, true, false) {
		Ok(program) => program,
		Err(err) => {
			return Err(format!("Type checking error: {:?}", err));
		}
	};
	let compiled = LLVMCompiler::compile(
		&context, &builder, &module,
		program.statements[0].clone(),
	);
	match compiled {
		Ok(func) => Ok(func.to_string()),
		Err(err) => Err(format!("Compilation failed:\n{}", err)),
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
		return;
	}

	if let Some(obj) = machine.stack_top() {
		println!("{}", obj)
	} else {
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

pub fn build_to_wat(input: &str, full_program: bool) -> Result<String, String> {
	let bytecode = match build_wasm(input, full_program) {
		Ok(b) => b,
		Err(err) => return Err(err)
	};

	let wat = wasmprinter::print_bytes(bytecode);
	match wat {
		Ok(w) => Ok(w),
		Err(err) => Err(err.to_string()),
	}
}

pub fn print_wat(input: &str, full_program: bool) {
	match build_to_wat(input, full_program) {
		Ok(b) => println!("{}", b),
		Err(err) => eprintln!("{}", err),
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum MolyError {
	Parse(ParserError),
	TypeCheck(TypeCheckError),
}

impl std::fmt::Display for MolyError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			MolyError::Parse(err) => std::fmt::Display::fmt(err, f),
			MolyError::TypeCheck(err) => std::fmt::Display::fmt(err, f),
		}
	}
}