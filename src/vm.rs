use crate::code::{Instructions, Opcode, read_u16};
use crate::compiler::Bytecode;
use crate::object::Object;

const STACK_SIZE: usize = 2048;

pub struct VM {
	constants: Vec<Object>,
	instructions: Instructions,

	//TODO Try fixed size pre allocated array
	stack: Vec<Object>,
	//sp: usize,
}

impl VM {
	pub fn new(bytecode: Bytecode) -> Self {
		VM {
			instructions: bytecode.instructions,
			constants: bytecode.constants,

			stack: Vec::with_capacity(STACK_SIZE),
			//sp: 0,
		}
	}

	pub fn stack_top(&self) -> Option<&Object> {
		self.stack.last()
	}

	pub fn run(&mut self) -> Result<(), String> {
		let mut ip = 0;
		while ip < self.instructions.len() {
			let op = self.instructions[ip];

			match op.try_into() {
				Ok(Opcode::OpConstant) => {
					let const_index = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					self.push(self.constants[const_index].clone())?;
				}
				Ok(Opcode::OpAdd) => {
					let right = self.pop();
					let left = self.pop();

					match (&left, &right) {
						(Some(Object::Integer(left)), Some(Object::Integer(right))) => {
							let result = left + right;
							self.push(Object::Integer(result))?;
						}
						_ => return Err(format!("undefined add operands {:?} and {:?}", left, right))
					}
				}
				_ /*TODO Err(_) */=> panic!("{} undefined opcode", op)
			}

			ip += 1;
		}

		Ok(())
	}

	fn push(&mut self, obj: Object) -> Result<(), String> {
		if self.stack.len() >= STACK_SIZE {
			return Err("stack overflow".into())
		}

		self.stack.push(obj);
		//self.sp += 1;

		Ok(())
	}

	//TODO Dissolve once we decided not to go for preallocated array stack
	fn pop(&mut self) -> Option<Object> {
		self.stack.pop()
	}
}