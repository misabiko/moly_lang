use moly_lang::code::{concat_instructions, instruction_to_string, lookup, make, Opcode, read_operands};

#[test]
fn test_make() {
	let tests: Vec<(Opcode, Vec<usize>, Vec<u8>)> = vec![
		(Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant as u8, 255, 254]),
		(Opcode::OpAdd, vec![], vec![Opcode::OpAdd as u8]),
		(Opcode::OpGetLocal, vec![255], vec![Opcode::OpGetLocal as u8, 255]),
	];

	for (op, operands, expected) in tests {
		let instruction = make(op, &operands);

		assert_eq!(instruction.len(), expected.len(), "wrong instruction length");

		for (i, (ins, expected)) in instruction.into_iter().zip(expected.into_iter()).enumerate() {
			assert_eq!(ins, expected, "wrong u8 at pos {}, want {}, got {}", i, expected, ins)
		}
	}
}

#[test]
fn test_instruction_string() {
	let instructions = vec![
		make(Opcode::OpAdd, &vec![]),
		make(Opcode::OpGetLocal, &vec![1]),
		make(Opcode::OpConstant, &vec![2]),
		make(Opcode::OpConstant, &vec![65535]),
	];

	let expected = "0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
";

	let concatted = concat_instructions(instructions);

	assert_eq!(instruction_to_string(&concatted), expected, "instructions wrongly formatted");
}

#[test]
fn test_read_operands() {
	let tests = vec![
		(Opcode::OpConstant, vec![65535], 2),
		(Opcode::OpGetLocal, vec![255], 1),
	];

	for (op, operands, bytes_read) in tests {
		let instruction = make(op, &operands);

		let def = lookup(op as u8).expect("definition not found");

		let (operands_read, n) = read_operands(&def, &instruction[1..]);
		assert_eq!(n, bytes_read);

		for (read, want) in operands_read.into_iter().zip(operands.into_iter()) {
			assert_eq!(read, want, "operand wrong")
		}
	}
}