use std::fmt::Write;
use byteorder::{BigEndian, ByteOrder};
use enum_primitive::{enum_from_primitive, enum_from_primitive_impl, enum_from_primitive_impl_ty, FromPrimitive};

pub type Instructions = Vec<u8>;

enum_from_primitive! {
#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Opcode {
	/// Copies the constant to the stack
	/// Operands:
	/// constant_index: u16
	Constant = 0,
	/// Removes the last object off the stack
	Pop,
	/// Pops the last two elements adds them
	Add,
	/// Pops the last two elements subtracts them
	Sub,
	/// Pops the last two elements multiplies them
	Mul,
	/// Pops the last two elements divides them
	Div,
	/// Adds true to the stack
	True,
	/// Adds false to the stack
	False,
	/// Pops the last two elements and adds true or false if equal
	Equal,
	/// Pops the last two elements and adds true or false if not equal
	NotEqual,
	/// Pops the last two elements and adds true or false if greater than
	GreaterThan,
	/// Pops the last element and inverts it
	Minus,
	/// Pops the last element and negates it
	Bang,
	/// Pops the last element and jumps to the given instruction if false
	/// Operands:
	/// instruction_index: u16
	JumpIfFalse,
	/// Jumps to the given instruction
	/// Operands:
	/// instruction_index: u16
	Jump,
	/// Copies the global symbol to the stack
	/// Operands:
	/// symbol_index: u16
	GetGlobal,
	/// Copies the symbol to the global array
	/// Operands:
	/// index: u16
	SetGlobal,
	/// Copies the local symbol to the stack
	/// Operands:
	/// symbol_index: u8
	GetLocal,
	/// Copies the symbol to the stack, indexed after the frame
	/// Operands:
	/// index: u8
	SetLocal,
	/// Copies the builtin symbol to the stack
	/// Operands:
	/// symbol_index: u8
	GetBuiltin,
	/// Copies the free symbol to the stack
	/// Operands:
	/// symbol_index: u8
	GetFree,
	/// Pops the elements off the stack and make an array with them
	/// Operands:
	/// element_count: u16
	Array,
	/// Pops the index and container off the stack, and push the indexed value on it
	Index,
	/// Pop the called closure and its arguments from the stack
	/// Operands:
	/// arg_count: u8
	Call,
	/// Pop the last value off the stack, pops the frame, and pushes to the next
	ReturnValue,
	/// Pops the current frame and sets the last popped element to None
	Return,
	/// Copies the closure from the constants, then pops the free symbols from the stack, and pushes the closure back
	/// Operands:
	/// closure_index: u16
	/// free_symbol_count: u8
	Closure,
	/// Copies the closure of the current frame, and pushes it on the stack
	CurrentClosure,
}}

pub struct Definition {
	pub name: &'static str,
	pub operand_widths: &'static [u8],
}

pub type OperandIndex = usize;

pub fn lookup(op: u8) -> Result<Definition, String> {
	match Opcode::from_u8(op) {
		Some(op) => match op {
			Opcode::Constant => Ok(Definition { name: "OpConstant", operand_widths: &[2] }),
			Opcode::Pop => Ok(Definition { name: "OpPop", operand_widths: &[] }),

			Opcode::Add => Ok(Definition { name: "OpAdd", operand_widths: &[] }),
			Opcode::Sub => Ok(Definition { name: "OpSub", operand_widths: &[] }),
			Opcode::Mul => Ok(Definition { name: "OpMul", operand_widths: &[] }),
			Opcode::Div => Ok(Definition { name: "OpDiv", operand_widths: &[] }),

			Opcode::True => Ok(Definition { name: "OpTrue", operand_widths: &[] }),
			Opcode::False => Ok(Definition { name: "OpFalse", operand_widths: &[] }),

			Opcode::Equal => Ok(Definition { name: "OpEqual", operand_widths: &[] }),
			Opcode::NotEqual => Ok(Definition { name: "OpNotEqual", operand_widths: &[] }),
			Opcode::GreaterThan => Ok(Definition { name: "OpGreaterThan", operand_widths: &[] }),

			Opcode::Minus => Ok(Definition { name: "OpMinus", operand_widths: &[] }),
			Opcode::Bang => Ok(Definition { name: "OpBang", operand_widths: &[] }),

			Opcode::JumpIfFalse => Ok(Definition { name: "OpJumpIfFalse", operand_widths: &[2] }),
			Opcode::Jump => Ok(Definition { name: "OpJump", operand_widths: &[2] }),

			Opcode::GetGlobal => Ok(Definition { name: "OpGetGlobal", operand_widths: &[2] }),
			Opcode::SetGlobal => Ok(Definition { name: "OpSetGlobal", operand_widths: &[2] }),
			Opcode::GetLocal => Ok(Definition { name: "OpGetLocal", operand_widths: &[1] }),
			Opcode::SetLocal => Ok(Definition { name: "OpSetLocal", operand_widths: &[1] }),
			Opcode::GetBuiltin => Ok(Definition { name: "OpGetBuiltin", operand_widths: &[1] }),
			Opcode::GetFree => Ok(Definition { name: "OpGetFree", operand_widths: &[1] }),

			//Operand width: Number of elements
			Opcode::Array => Ok(Definition { name: "OpArray", operand_widths: &[2] }),
			Opcode::Index => Ok(Definition { name: "OpIndex", operand_widths: &[] }),

			Opcode::Call => Ok(Definition { name: "OpCall", operand_widths: &[1] }),
			Opcode::ReturnValue => Ok(Definition { name: "OpReturnValue", operand_widths: &[] }),
			Opcode::Return => Ok(Definition { name: "OpReturn", operand_widths: &[] }),
			Opcode::Closure => Ok(Definition { name: "OpClosure", operand_widths: &[2, 1] }),
			Opcode::CurrentClosure => Ok(Definition { name: "OpCurrentClosure", operand_widths: &[] }),
		},
		None => Err(format!("undefined opcode {}", op))
	}
}

pub fn make(op: Opcode, operands: &[usize]) -> Instructions {
	let def = if let Ok(def) = lookup(op as u8) {
		def
	}else {
		return [].into()
	};

	let mut instruction_len: usize = 1;
	for w in def.operand_widths {
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