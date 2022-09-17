use std::fmt::Write;
use byteorder::{BigEndian, ByteOrder};
use enum_primitive::{enum_from_primitive, enum_from_primitive_impl, enum_from_primitive_impl_ty, FromPrimitive};

pub type Instructions = Vec<u8>;

//TODO Document calling convention
enum_from_primitive! {
#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Opcode {
	Constant = 0,
	Pop,

	Add,
	Sub,
	Mul,
	Div,

	True,
	False,

	Equal,
	NotEqual,
	GreaterThan,

	Minus,
	Bang,

	JumpIfFalse,
	Jump,

	GetGlobal,
	SetGlobal,
	GetLocal,
	SetLocal,
	GetBuiltin,
	GetFree,

	Array,
	Hash,
	Index,

	Call,
	ReturnValue,
	Return,
	Closure,
	CurrentClosure,
}
}

pub struct Definition {
	pub name: &'static str,
	pub operand_widths: Vec<u8>,
}

pub type OperandIndex = usize;

pub fn lookup(op: u8) -> Result<Definition, String> {
	match Opcode::from_u8(op) {
		Some(op) => match op {
			Opcode::Constant => Ok(Definition { name: "OpConstant", operand_widths: vec![2] }),
			Opcode::Pop => Ok(Definition { name: "OpPop", operand_widths: vec![] }),

			Opcode::Add => Ok(Definition { name: "OpAdd", operand_widths: vec![] }),
			Opcode::Sub => Ok(Definition { name: "OpSub", operand_widths: vec![] }),
			Opcode::Mul => Ok(Definition { name: "OpMul", operand_widths: vec![] }),
			Opcode::Div => Ok(Definition { name: "OpDiv", operand_widths: vec![] }),

			Opcode::True => Ok(Definition { name: "OpTrue", operand_widths: vec![] }),
			Opcode::False => Ok(Definition { name: "OpFalse", operand_widths: vec![] }),

			Opcode::Equal => Ok(Definition { name: "OpEqual", operand_widths: vec![] }),
			Opcode::NotEqual => Ok(Definition { name: "OpNotEqual", operand_widths: vec![] }),
			Opcode::GreaterThan => Ok(Definition { name: "OpGreaterThan", operand_widths: vec![] }),

			Opcode::Minus => Ok(Definition { name: "OpMinus", operand_widths: vec![] }),
			Opcode::Bang => Ok(Definition { name: "OpBang", operand_widths: vec![] }),

			Opcode::JumpIfFalse => Ok(Definition { name: "OpJumpIfFalse", operand_widths: vec![2] }),
			Opcode::Jump => Ok(Definition { name: "OpJump", operand_widths: vec![2] }),

			Opcode::GetGlobal => Ok(Definition { name: "OpGetGlobal", operand_widths: vec![2] }),
			Opcode::SetGlobal => Ok(Definition { name: "OpSetGlobal", operand_widths: vec![2] }),
			Opcode::GetLocal => Ok(Definition { name: "OpGetLocal", operand_widths: vec![1] }),
			Opcode::SetLocal => Ok(Definition { name: "OpSetLocal", operand_widths: vec![1] }),
			Opcode::GetBuiltin => Ok(Definition { name: "OpGetBuiltin", operand_widths: vec![1] }),
			Opcode::GetFree => Ok(Definition { name: "OpGetFree", operand_widths: vec![1] }),

			//TODO Test error message for too many elements in an array
			//Operand width: Number of elements
			Opcode::Array => Ok(Definition { name: "OpArray", operand_widths: vec![2] }),
			//Operand width: Number of keys + Number of values
			Opcode::Hash => Ok(Definition { name: "OpHash", operand_widths: vec![2] }),
			Opcode::Index => Ok(Definition { name: "OpIndex", operand_widths: vec![] }),

			Opcode::Call => Ok(Definition { name: "OpCall", operand_widths: vec![1] }),
			Opcode::ReturnValue => Ok(Definition { name: "OpReturnValue", operand_widths: vec![] }),
			Opcode::Return => Ok(Definition { name: "OpReturn", operand_widths: vec![] }),
			Opcode::Closure => Ok(Definition { name: "OpClosure", operand_widths: vec![2, 1] }),
			Opcode::CurrentClosure => Ok(Definition { name: "OpCurrentClosure", operand_widths: vec![] }),
		},
		None => Err(format!("undefined opcode {}", op))
	}
}

pub fn make(op: Opcode, operands: &Vec<OperandIndex>) -> Instructions {
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
			1 => instruction[offset] = *operand as u8,
			2 => BigEndian::write_u16(&mut instruction[offset..], *operand as u16),
			_ => {}
		};

		offset += width;
	}

	instruction.into()
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<OperandIndex>, usize) {
	let mut operands = vec![0; def.operand_widths.len()];
	let mut offset = 0;

	for (i, width) in def.operand_widths.iter().enumerate() {
		match width {
			1 => operands[i] = read_u8(&ins[offset..]) as OperandIndex,
			2 => operands[i] = read_u16(&ins[offset..]) as OperandIndex,
			_ => {}
		}

		offset += *width as usize;
	}

	(operands, offset)
}

pub fn read_u8(ins: &[u8]) -> u8 {
	ins[0]
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

fn fmt_instruction(def: Definition, operands: Vec<OperandIndex>) -> String {
	let operand_count = def.operand_widths.len();

	if operands.len() != operand_count {
		return format!("ERROR: operand len {} does not match defined {}\n", operands.len(), operand_count)
	}

	match operand_count {
		0 => def.name.into(),
		1 => format!("{} {}", def.name, operands[0]),
		2 => format!("{} {} {}", def.name, operands[0], operands[1]),
		_ => format!("ERROR: unhandled operand_count for {}\n", def.name),
	}
}

pub

fn concat_instructions(instructions: Vec<Instructions>) -> Instructions {
	instructions.into_iter().flatten().collect()
}