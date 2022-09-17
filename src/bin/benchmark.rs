use std::time::{Duration, SystemTime};
use moly_lang::compiler::Compiler;
use moly_lang::lexer::Lexer;
use moly_lang::object::Object;
use moly_lang::parser::Parser;
use moly_lang::vm::VM;

const INPUT: &str = "
let fibonacci = fn(x) {
  if (x == 0) {
    0
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
fibonacci(35);
";

fn main() {
	let duration: Duration;
	let result: Object;

	let lexer = Lexer::new(INPUT);
	let mut parser = Parser::new(lexer);
	let program = parser.parse_program();

	let mut compiler = Compiler::new();
	if let Err(err) = compiler.compile(program) {
		panic!("compiler error: {}", err)
	}

	let mut machine = VM::new(compiler.bytecode());

	let start = SystemTime::now();
	if let Err(err) = machine.run() {
		panic!("vm error: {}", err)
	}

	duration = SystemTime::now().duration_since(start).unwrap();
	result = machine.last_popped_stack_elem.expect("missing last popped stack element");

	println!("engine=vm, result={}, duration={:.2}s", result, duration.as_secs_f32())
}