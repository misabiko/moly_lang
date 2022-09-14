use std::fmt::Write;
use byteorder::{BigEndian, ByteOrder};

pub type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Opcode {
	OpConstant = 0,
	OpPop,

	OpAdd,
	OpSub,
	OpMul,
	OpDiv,

	OpTrue,
	OpFalse,

	OpEqual,
	OpNotEqual,
	OpGreaterThan,

	OpMinus,
	OpBang,

	OpJumpIfFalse,
	OpJump,

	OpGetGlobal,
	OpSetGlobal,
}

//TODO Replace with macro
impl TryFrom<u8> for Opcode {
	type Error = ();

	fn try_from(value: u8) -> Result<Self, Self::Error> {
		match value {
			0 => Ok(Opcode::OpConstant),
			1 => Ok(Opcode::OpPop),

			2 => Ok(Opcode::OpAdd),
			3 => Ok(Opcode::OpSub),
			4 => Ok(Opcode::OpMul),
			5 => Ok(Opcode::OpDiv),

			6 => Ok(Opcode::OpTrue),
			7 => Ok(Opcode::OpFalse),

			8 => Ok(Opcode::OpEqual),
			9 => Ok(Opcode::OpNotEqual),
			10 => Ok(Opcode::OpGreaterThan),

			11 => Ok(Opcode::OpMinus),
			12 => Ok(Opcode::OpBang),

			13 => Ok(Opcode::OpJumpIfFalse),
			14 => Ok(Opcode::OpJump),

			15 => Ok(Opcode::OpGetGlobal),
			16 => Ok(Opcode::OpSetGlobal),
			_ => Err(())
		}
	}
}

pub struct Definition {
	pub name: &'static str,
	pub operand_widths: Vec<u8>,
}

pub type Operand = usize;

pub fn lookup(op: u8) -> Result<Definition, String> {
	match op.try_into() {
		Ok(Opcode::OpConstant) => Ok(Definition {name: "OpConstant", operand_widths: vec![2]}),
		Ok(Opcode::OpPop) => Ok(Definition {name: "OpPop", operand_widths: vec![]}),

		Ok(Opcode::OpAdd) => Ok(Definition {name: "OpAdd", operand_widths: vec![]}),
		Ok(Opcode::OpSub) => Ok(Definition {name: "OpSub", operand_widths: vec![]}),
		Ok(Opcode::OpMul) => Ok(Definition {name: "OpMul", operand_widths: vec![]}),
		Ok(Opcode::OpDiv) => Ok(Definition {name: "OpDiv", operand_widths: vec![]}),

		Ok(Opcode::OpTrue) => Ok(Definition {name: "OpTrue", operand_widths: vec![]}),
		Ok(Opcode::OpFalse) => Ok(Definition {name: "OpFalse", operand_widths: vec![]}),

		Ok(Opcode::OpEqual) => Ok(Definition {name: "OpEqual", operand_widths: vec![]}),
		Ok(Opcode::OpNotEqual) => Ok(Definition {name: "OpNotEqual", operand_widths: vec![]}),
		Ok(Opcode::OpGreaterThan) => Ok(Definition {name: "OpGreaterThan", operand_widths: vec![]}),

		Ok(Opcode::OpMinus) => Ok(Definition {name: "OpMinus", operand_widths: vec![]}),
		Ok(Opcode::OpBang) => Ok(Definition {name: "OpBang", operand_widths: vec![]}),

		Ok(Opcode::OpJumpIfFalse) => Ok(Definition {name: "OpJumpIfFalse", operand_widths: vec![2]}),
		Ok(Opcode::OpJump) => Ok(Definition {name: "OpJump", operand_widths: vec![2]}),

		Ok(Opcode::OpGetGlobal) => Ok(Definition {name: "OpGetGlobal", operand_widths: vec![2]}),
		Ok(Opcode::OpSetGlobal) => Ok(Definition {name: "OpSetGlobal", operand_widths: vec![2]}),
		Err(_) => Err(format!("opcode {} undefined", op))
	}
}

pub fn make(op: Opcode, operands: &Vec<Operand>) -> Instructions {
	let def = if let Ok(def) = lookup(op as u8) {
		def
	}else {
		return vec![].into()
	};

	let mut instruction_len: usize = 1;
	for w in &def.operand_widths {
		instruction_len += *w as usize
	}

	let mut instruction = vec![0; instruction_len];
	instruction[0] = op as u8;

	let mut offset = 1;
	for (i, operand) in operands.iter().enumerate() {
		let width = def.operand_widths[i] as usize;

		match width {
			2 => BigEndian::write_u16(&mut instruction[offset..], *operand as u16),
			_ => {}
		};

		offset += width;
	}

	instruction.into()
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<Operand>, usize) {
	let mut operands = vec![0; def.operand_widths.len()];
	let mut offset = 0;

	for (i, width) in def.operand_widths.iter().enumerate() {
		match width {
			2 => operands[i] = read_u16(&ins[offset..]) as Operand,
			_ => {}
		}

		offset += *width as usize;
	}

	(operands, offset)
}

pub fn read_u16(ins: &[u8]) -> u16 {
	BigEndian::read_u16(ins)
}

pub fn instruction_to_string(ins: &Instructions) -> String {
	let mut out = String::new();

	let mut i = 0;
	while i < ins.len() {
		let def = match lookup(ins[i]) {
			Ok(def) => def,
			Err(err) => {
				writeln!(&mut out, "ERROR: {}", err)
					.expect("Failed to write to buffer");
				continue
			}
		};

		let (operands, read) = read_operands(&def, &ins[i+1..]);

		writeln!(&mut out, "{:04} {}", i, fmt_instruction(def, operands))
			.expect("Failed to write to buffer");

		i += 1 + read;
	}

	out
}

fn fmt_instruction(def: Definition, operands: Vec<Operand>) -> String {
	let operand_count = def.operand_widths.len();

	if operands.len() != operand_count {
		return format!("ERROR: operand len {} does not match defined {}\n", operands.len(), operand_count)
	}

	match operand_count {
		0 => def.name.into(),
		1 => format!("{} {}", def.name, operands[0]),
		_ => format!("ERROR: unhandled operand_count for {}\n", def.name),
	}
}