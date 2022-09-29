use std::time::SystemTime;
use moly::build;
use moly::vm::VM;

const INPUT: &str = "
fn main() {
	let fibonacci = fn(x u64) u64 {
	  if x == 0u64 {
		0u64
	  } else {
		if x == 1u64 {
		  1u64
		} else {
		  fibonacci(x - 1u64) + fibonacci(x - 2u64)
		}
	  }
	};
	fibonacci(35u64);
}
";

fn main() {
	let bytecode = match build(INPUT, true) {
		Ok(b) => b,
		Err(err) => {
			eprintln!("{}", err);
			return;
		}
	};

	let mut machine = VM::new(bytecode);

	let start = SystemTime::now();
	if let Err(err) = machine.run() {
		panic!("vm error: {}", err)
	}

	let duration = SystemTime::now().duration_since(start).unwrap();
	let result = machine.last_popped_stack_elem.expect("missing last popped stack element");

	println!("engine=vm, result={}, duration={:.2}s", result, duration.as_secs_f32())
}