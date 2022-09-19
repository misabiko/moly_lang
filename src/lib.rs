pub mod token;
pub mod lexer;
pub mod repl;
pub mod parser;
pub mod ast;
pub mod object;
pub mod code;
pub mod compiler;
pub mod vm;

//TODO Error if returned value is not handled
//TODO (Go-style) Skip struct initializing field names and depend on order
//TODO Explore how Elm did List with "1 or more" length
//TODO Static string in sum type to pattern match with
	/* type BooleanOperator = "<" | ">" | "<=" | ">=" | "==" | "!="; */