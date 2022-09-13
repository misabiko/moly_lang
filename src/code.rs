use std::fmt::Write;
use byteorder::{BigEndian, ByteOrder};

pub type Instructions = Vec<u8>;

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

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone)]
pub enum Opcode {
	OpConstant = 0,
	OpAdd,
}

impl TryFrom<u8> for Opcode {
	type Error = ();

	fn try_from(value: u8) -> Result<Self, Self::Error> {
		match value {
			0 => Ok(Opcode::OpConstant),
			1 => Ok(Opcode::OpAdd),
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
		Ok(Opcode::OpAdd) => Ok(Definition {name: "OpAdd", operand_widths: vec![]}),
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