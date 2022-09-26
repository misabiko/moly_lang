use std::time::SystemTime;
use moly::build;
use moly::vm::VM;

const INPUT: &str = "
let fibonacci = fn(x u8) {
  if x == 0 {
    0
  } else {
    if x == 1 {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
fibonacci(35);
";

fn main() {
	let bytecode = match build(INPUT) {
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