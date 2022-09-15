use std::collections::HashMap;
use crate::code::{Instructions, Opcode, read_u16};
use crate::compiler::Bytecode;
use crate::object::{HashingObject, Object};

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;

const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);

pub struct VM {
	instructions: Instructions,
	constants: Vec<Object>,
	pub globals: Vec<Object>,

	//TODO Try fixed size pre allocated array
	stack: Vec<Object>,
	//sp: usize,
	pub last_popped_stack_elem: Option<Object>,
}

impl VM {
	pub fn new(bytecode: Bytecode) -> Self {
		Self {
			instructions: bytecode.instructions,
			constants: bytecode.constants,
			globals: Vec::with_capacity(GLOBALS_SIZE),

			stack: Vec::with_capacity(STACK_SIZE),
			//sp: 0,
			last_popped_stack_elem: None,
		}
	}

	pub fn new_with_global_store(bytecode: Bytecode, globals: Vec<Object>) -> Self {
		Self {
			instructions: bytecode.instructions,
			constants: bytecode.constants,
			globals,

			stack: Vec::with_capacity(STACK_SIZE),
			//sp: 0,
			last_popped_stack_elem: None,
		}
	}

	pub fn stack_top(&self) -> Option<&Object> {
		self.stack.last()
	}

	pub fn run(&mut self) -> VMResult {
		let mut ip = 0;
		while ip < self.instructions.len() {
			let op = self.instructions[ip];

			match op.try_into() {
				Ok(Opcode::OpConstant) => {
					let const_index = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					self.push(self.constants[const_index].clone())?;
				}
				Ok(Opcode::OpPop) => { self.pop(); },

				Ok(
					op @ Opcode::OpAdd |
				    op @ Opcode::OpSub |
					op @ Opcode::OpMul |
					op @ Opcode::OpDiv
				) => {
					self.execute_binary_operation(op)?
				}

				Ok(Opcode::OpTrue) => self.push(TRUE_OBJ)?,
				Ok(Opcode::OpFalse) => self.push(FALSE_OBJ)?,

				Ok(Opcode::OpBang) => self.execute_bang_operator()?,
				Ok(Opcode::OpMinus) => self.execute_minus_operator()?,

				Ok(
					op @ Opcode::OpEqual |
					op @ Opcode::OpNotEqual |
					op @ Opcode::OpGreaterThan
				) => {
					self.execute_comparison(op)?
				}

				Ok(Opcode::OpJump) => {
					let pos = read_u16(&self.instructions[ip+1..]) as usize;
					ip = pos - 1;
				}
				Ok(Opcode::OpJumpIfFalse) => {
					let pos = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					let condition = self.pop().unwrap();
					if condition == FALSE_OBJ {
						ip = pos - 1;
					}
				}

				Ok(Opcode::OpSetGlobal) => {
					let global_index = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					let len = self.globals.len();
					let value = self.pop().unwrap();

					match global_index {
						i if i == len => self.globals.push(value),
						i if i < len => self.globals[global_index] = value,
						_ => panic!("Global index higher than length (...which might a thing)")
					}
				}
				Ok(Opcode::OpGetGlobal) => {
					let global_index = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					self.push(self.globals[global_index].clone())?;
				}

				Ok(Opcode::OpArray) => {
					let num_elements = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					let sp = self.stack.len();
					let array = self.build_array(sp - num_elements, sp);
					self.stack.truncate(sp - num_elements);

					self.push(array)?;
				}
				Ok(Opcode::OpHash) => {
					let num_elements = read_u16(&self.instructions[ip+1..]) as usize;
					ip += 2;

					let sp = self.stack.len();
					let hash = self.build_hash(sp - num_elements, sp)?;
					self.stack.truncate(sp - num_elements);

					self.push(hash)?;
				}
				Ok(Opcode::OpIndex) => {
					let index = self.pop().ok_or("index not on stack")?;
					let left = self.pop().ok_or("left expression not on stack")?;

					self.execute_index_expression(left, index)?;
				}

				_ => panic!("{} undefined opcode", op)
				//TODO Err(_) => panic!("{} undefined opcode", op)
			}

			ip += 1;
		}

		Ok(())
	}

	fn execute_binary_operation(&mut self, op: Opcode) -> VMResult {
		let right = self.pop();
		let left = self.pop();

		match (&left, &right) {
			(Some(Object::Integer(left)), Some(Object::Integer(right)))
				=> self.execute_binary_integer_operation(op, *left, *right)?,
			(Some(Object::String(left)), Some(Object::String(right)))
			=> self.execute_binary_string_operation(op, left, right)?,
			_ => return Err(format!("unsupported types for binary operation: {:?} and {:?}", left, right))
		}

		Ok(())
	}

	fn execute_binary_integer_operation(&mut self, op: Opcode, left: i64, right: i64) -> VMResult {
		let result = match op {
			Opcode::OpAdd => left + right,
			Opcode::OpSub => left - right,
			Opcode::OpMul => left * right,
			Opcode::OpDiv => left / right,
			_ => return Err(format!("unknown integer operator: {:?}", op))
		};

		self.push(Object::Integer(result))
	}

	fn execute_binary_string_operation(&mut self, op: Opcode, left: &str, right: &str) -> VMResult {
		if !matches!(op, Opcode::OpAdd) {
			return Err(format!("unknown string operator: {:?}", op))
		}

		self.push(Object::String(format!("{}{}", left, right)))
	}

	fn execute_comparison(&mut self, op: Opcode) -> VMResult {
		let left = self.pop().unwrap();
		let right = self.pop().unwrap();

		if let (Object::Integer(left), Object::Integer(right)) = (&left, &right) {
			return self.execute_integer_comparison(op, *left, *right)
		}

		match op {
			Opcode::OpEqual => self.push(Object::Boolean(right == left)),
			Opcode::OpNotEqual => self.push(Object::Boolean(right != left)),
			_ => return Err(format!("unknown operator: {:?} ({:?} {:?})", op, left, right))
		}
	}

	fn execute_integer_comparison(&mut self, op: Opcode, left: i64, right: i64) -> VMResult {
		match op {
			Opcode::OpEqual => self.push(Object::Boolean(right == left)),
			Opcode::OpNotEqual => self.push(Object::Boolean(right != left)),
			Opcode::OpGreaterThan => self.push(Object::Boolean(right > left)),
			_ => Err(format!("unknown operator: {:?}", op))
		}
	}

	fn execute_bang_operator(&mut self) -> VMResult {
		let operand = self.pop().unwrap();

		match operand {
			TRUE_OBJ => self.push(FALSE_OBJ),
			FALSE_OBJ => self.push(TRUE_OBJ),
			_ => return Err(format!("unsupported type for bang operator: {:?}", operand)),
		}
	}

	fn execute_minus_operator(&mut self) -> VMResult {
		let operand = self.pop().unwrap();

		if let Object::Integer(value) = operand {
			self.push(Object::Integer(-value))
		}else {
			Err(format!("unsupported type for negation: {:?}", operand))
		}
	}

	fn build_array(&self, start_index: usize, end_index: usize) -> Object {
		Object::Array(
			self.stack[start_index..end_index]
				.iter().cloned().collect()
		)
	}

	fn build_hash(&self, start_index: usize, end_index: usize) -> Result<Object, String> {
		let mut pairs = HashMap::new();

		for i in (start_index..end_index).step_by(2) {
			let key = &self.stack[i];
			let value = &self.stack[i+1];

			let hash_key = HashingObject::try_from(key.clone())?;

			pairs.insert(hash_key.clone(), (hash_key, value.clone()));
		}

		Ok(Object::Hash(pairs))
	}

	fn execute_index_expression(&mut self, left: Object, index: Object) -> VMResult {
		match (left, index) {
			(Object::Array(elements), Object::Integer(index))
				=> self.execute_array_index(elements, index),
			(Object::Hash(pairs), index)
				=> self.execute_hash_index(pairs, index),
			(left, index) => Err(format!("index operator not supported: {:?}[{:?}]", left, index))
		}
	}

	fn execute_array_index(&mut self, array: Vec<Object>, index: i64) -> VMResult {
		let index = if index >= 0 && index as usize <= array.len() {
			index as usize
		}else {
			return Err(format!("index {} out of bound [0..{}]", index, array.len() - 1))
		};

		self.push(array.get(index).cloned().unwrap())
	}

	fn execute_hash_index(&mut self, hash: HashMap<HashingObject, (HashingObject, Object)>, index: Object) -> VMResult {
		let hash_key = HashingObject::try_from(index)?;

		let pair = hash.get(&hash_key).ok_or_else(|| format!("value not found for {:?}", hash_key))?;

		self.push(pair.1.clone())
	}

	fn push(&mut self, obj: Object) -> VMResult {
		if self.stack.len() >= STACK_SIZE {
			return Err("stack overflow".into())
		}

		self.stack.push(obj);
		//self.sp += 1;

		Ok(())
	}

	fn pop(&mut self) -> Option<Object> {
		self.last_popped_stack_elem = self.stack.pop();

		self.last_popped_stack_elem.clone()
	}
}

type VMResult = Result<(), String>;