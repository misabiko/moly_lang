use std::process::{Command};
use moly::wasm::emitter::compile_block_with_header;

#[test]
fn test_empty_program() {
	let wasm = compile_block_with_header().unwrap();
	assert_eq!(wasm, [
		//header
		0, 97, 115, 109,
		//module version
		1, 0, 0, 0,
		//types
		1, 8, 2, 96, 0, 0, 96, 1, 125, 0,
		//imports
		2, 13, 1, 3, 101, 110, 118, 5, 112, 114, 105, 110, 116, 0, 1,
		//functions
		3, 2, 1, 0,
		//exports
		7, 7, 1, 3, 114, 117, 110, 0, 1,
		//code
		10, 4, 1, 2, 0, 11
	]);
}

#[test]
fn test_print() {
	let tests = vec![
		(
			"",
			"an empty program"
		),
		// ("print(8)", vec![8]),
		// ("print(8); print(24);", vec![8, 24]),
	];

	for (input, test_name) in tests {
		let bytecode = compile_block_with_header().unwrap();
		let bytecode = bytecode.into_iter()
			.map(|b| format!("{:#x}", b))
			.collect::<Vec<String>>()
			.join(" ");

		let cmd_output = Command::new("deno")
			.args(["test", "wasmTest.ts", "--filter", test_name, "--", &bytecode])
			.output()
			.expect("failed to execute command");

		//println!("{:#?}", cmd_output);
		println!("{}", std::str::from_utf8(cmd_output.stdout.as_slice()).unwrap());

		assert!(cmd_output.status.success(), "deno tests failed:\n{}", std::str::from_utf8(cmd_output.stderr.as_slice()).unwrap());
	}
}