use std::collections::HashMap;
use std::convert::identity;
use enum_primitive::FromPrimitive;
use crate::code::{Instructions, Opcode, read_u16, read_u8};
use crate::compiler::Bytecode;
use crate::object::{Builtin, Closure, Function, HashingObject, Object};
use crate::object::builtins::BUILTINS;
use crate::vm::frame::Frame;

pub mod frame;

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);

pub struct VM {
	constants: Vec<Object>,
	pub globals: Vec<Object>,

	//TODO Try fixed size pre allocated array
	stack: Vec<Option<Object>>,
	pub last_popped_stack_elem: Option<Object>,

	//TODO Try fixed size pre allocated array
	pub frames: Vec<Frame>,
}

impl VM {
	pub fn new(bytecode: Bytecode) -> Self {
		let main_closure = Closure {
			func: Function {
				instructions: bytecode.instructions,
				num_locals: 0,
				num_parameters: 0,
			},
			free: vec![],
		};

		let mut frames = Vec::with_capacity(MAX_FRAMES);
		frames.push(Frame::new(main_closure, 0));

		Self {
			constants: bytecode.constants,
			globals: Vec::with_capacity(GLOBALS_SIZE),

			stack: Vec::with_capacity(STACK_SIZE),
			last_popped_stack_elem: None,

			frames,
		}
	}

	pub fn new_with_global_store(bytecode: Bytecode, globals: Vec<Object>) -> Self {
		let main_closure = Closure {
			func: Function {
				instructions: bytecode.instructions,
				num_locals: 0,
				num_parameters: 0,
			},
			free: vec![],
		};

		let mut frames = Vec::with_capacity(MAX_FRAMES);
		frames.push(Frame::new(main_closure, 0));

		Self {
			constants: bytecode.constants,
			globals,

			stack: Vec::with_capacity(STACK_SIZE),
			last_popped_stack_elem: None,

			frames,
		}
	}

	pub fn stack_top(&self) -> Option<&Object> {
		self.stack.last().and_then(|e| e.as_ref())
	}

	pub fn run(&mut self) -> VMResult {
		let mut ip;
		let mut ins: &Instructions;

		while self.current_frame().ip < self.current_frame().instructions().len() {
			self.current_frame().ip += 1;

			ip = self.current_frame().ip;
			ins = self.current_frame().instructions();

			match Opcode::from_u8(ins[ip - 1]) {
				Some(op) => match op {
					Opcode::Constant => {
						let const_index = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						self.push(self.constants[const_index].clone())?;
					}
					Opcode::Pop => { self.pop(); }

					Opcode::Add |
					Opcode::Sub |
					Opcode::Mul |
					Opcode::Div => self.execute_binary_operation(op)?,

					Opcode::True => self.push(TRUE_OBJ)?,
					Opcode::False => self.push(FALSE_OBJ)?,

					Opcode::Bang => self.execute_bang_operator()?,
					Opcode::Minus => self.execute_minus_operator()?,

					Opcode::Equal |
					Opcode::NotEqual |
					Opcode::GreaterThan => self.execute_comparison(op)?,

					Opcode::Jump => {
						let pos = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip = pos;
					}
					Opcode::JumpIfFalse => {
						let pos = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						let condition = self.pop().unwrap();
						if condition == FALSE_OBJ {
							self.current_frame().ip = pos;
						}
					}

					Opcode::SetGlobal => {
						let global_index = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						let len = self.globals.len();
						let value = self.pop().unwrap();

						match global_index {
							i if i == len => self.globals.push(value),
							i if i < len => self.globals[global_index] = value,
							_ => panic!("Global index higher than length (...which might a thing)")
						}
					}
					Opcode::GetGlobal => {
						let global_index = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						self.push(self.globals[global_index].clone())?;
					}
					Opcode::SetLocal => {
						let local_index = read_u8(&ins[ip..]) as usize;
						self.current_frame().ip += 1;

						let base_pointer = self.current_frame().base_pointer;
						self.stack[base_pointer + local_index] = self.pop();
					}
					Opcode::GetLocal => {
						let local_index = read_u8(&ins[ip..]) as usize;
						self.current_frame().ip += 1;

						let base_pointer = self.current_frame().base_pointer;
						self.push(self.stack[base_pointer + local_index].clone().unwrap())?;
					}
					Opcode::GetBuiltin => {
						let builtin_index = read_u8(&ins[ip..]) as usize;
						self.current_frame().ip += 1;

						let definition = &BUILTINS[builtin_index];

						self.push(Object::Builtin(definition.builtin))?;
					}
					Opcode::GetFree => {
						let free_index = read_u8(&ins[ip..]) as usize;
						self.current_frame().ip += 1;

						let obj = self.current_frame().closure.free[free_index].clone();
						self.push(obj)?;
					}

					Opcode::Array => {
						let num_elements = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						let sp = self.stack.len();
						let array = self.build_array(sp - num_elements, sp);
						self.stack.truncate(sp - num_elements);

						self.push(array)?;
					}
					Opcode::Hash => {
						let num_elements = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						let sp = self.stack.len();
						let hash = self.build_hash(sp - num_elements, sp)?;
						self.stack.truncate(sp - num_elements);

						self.push(hash)?;
					}
					Opcode::Index => {
						let index = self.pop().ok_or("index not on stack")?;
						let left = self.pop().ok_or("left expression not on stack")?;

						self.execute_index_expression(left, index)?;
					}

					Opcode::Call => {
						let num_args = read_u8(&ins[ip..]);
						self.current_frame().ip += 1;

						self.execute_call(num_args)?;
					}
					Opcode::ReturnValue => {
						let return_value = self.pop().unwrap();

						let frame = self.pop_frame().unwrap();
						self.stack.truncate(frame.base_pointer - 1);

						self.push(return_value)?;
					}
					Opcode::Return => {
						let frame = self.pop_frame().unwrap();
						self.stack.truncate(frame.base_pointer - 1);
					}
					Opcode::Closure => {
						let const_index = read_u16(&ins[ip..]);
						let num_free = read_u8(&ins[ip + 2..]);
						self.current_frame().ip += 3;

						self.push_closure(const_index, num_free)?;
					}
					Opcode::CurrentClosure => {
						let current_closure = self.current_frame().closure.clone();
						self.push(Object::Closure(current_closure))?;
					}
				},
				None => panic!("undefined opcode {}", ins[ip - 1]),
			}
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
			Opcode::Add => left + right,
			Opcode::Sub => left - right,
			Opcode::Mul => left * right,
			Opcode::Div => left / right,
			_ => return Err(format!("unknown integer operator: {:?}", op))
		};

		self.push(Object::Integer(result))
	}

	fn execute_binary_string_operation(&mut self, op: Opcode, left: &str, right: &str) -> VMResult {
		if !matches!(op, Opcode::Add) {
			return Err(format!("unknown string operator: {:?}", op));
		}

		self.push(Object::String(format!("{}{}", left, right)))
	}

	fn execute_comparison(&mut self, op: Opcode) -> VMResult {
		let left = self.pop().unwrap();
		let right = self.pop().unwrap();

		if let (Object::Integer(left), Object::Integer(right)) = (&left, &right) {
			return self.execute_integer_comparison(op, *left, *right);
		}

		match op {
			Opcode::Equal => self.push(Object::Boolean(right == left)),
			Opcode::NotEqual => self.push(Object::Boolean(right != left)),
			_ => return Err(format!("unknown operator: {:?} ({:?} {:?})", op, left, right))
		}
	}

	fn execute_integer_comparison(&mut self, op: Opcode, left: i64, right: i64) -> VMResult {
		match op {
			Opcode::Equal => self.push(Object::Boolean(right == left)),
			Opcode::NotEqual => self.push(Object::Boolean(right != left)),
			Opcode::GreaterThan => self.push(Object::Boolean(right > left)),
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
		} else {
			Err(format!("unsupported type for negation: {:?}", operand))
		}
	}

	fn build_array(&self, start_index: usize, end_index: usize) -> Object {
		Object::Array(
			self.stack[start_index..end_index]
				.iter().map(|o| o.clone().unwrap()).collect()
		)
	}

	fn build_hash(&self, start_index: usize, end_index: usize) -> Result<Object, String> {
		let mut pairs = HashMap::new();

		for i in (start_index..end_index).step_by(2) {
			let key = self.stack[i].as_ref().unwrap();
			let value = self.stack[i + 1].as_ref().unwrap();

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
		} else {
			return Err(format!("index {} out of bound [0..{}]", index, array.len() - 1));
		};

		self.push(array.get(index).cloned().unwrap())
	}

	fn execute_hash_index(&mut self, hash: HashMap<HashingObject, (HashingObject, Object)>, index: Object) -> VMResult {
		let hash_key = HashingObject::try_from(index)?;

		let pair = hash.get(&hash_key).ok_or_else(|| format!("value not found for {:?}", hash_key))?;

		self.push(pair.1.clone())
	}

	fn execute_call(&mut self, num_args: u8) -> VMResult {
		let num_args_usize = num_args as usize;
		let callee = self.stack[self.stack.len() - 1 - num_args_usize].clone().unwrap();
		match callee {
			Object::Closure(closure) => self.call_closure(closure, num_args),
			Object::Builtin(builtin) => self.call_builtin(builtin, num_args),
			callee => Err(format!("calling non-closure and non-builtin {:?}", callee))
		}
	}

	fn call_closure(&mut self, closure: Closure, num_args: u8) -> VMResult {
		if closure.func.num_parameters != num_args {
			//TODO Standardize assert_eq errors
			return Err(format!("wrong number of arguments: want={}, got={}", closure.func.num_parameters, num_args));
		}

		let num_args = num_args as usize;
		let num_locals = closure.func.num_locals;
		self.push_frame(Frame::new(closure, self.stack.len() - num_args));

		self.stack.resize(self.stack.len() - num_args + num_locals as usize, None);

		Ok(())
	}

	fn call_builtin(&mut self, builtin: Builtin, num_args: u8) -> VMResult {
		let num_args = num_args as usize;
		let args = &self.stack[self.stack.len() - num_args..];

		let result = builtin(args.iter().cloned().map(|a| a.unwrap()).collect());
		self.stack.truncate(self.stack.len() - num_args - 1);

		if let Some(result) = result {
			self.push(result)
		} else {
			Ok(())
		}
	}

	fn push(&mut self, obj: Object) -> VMResult {
		if self.stack.len() >= STACK_SIZE {
			return Err("stack overflow".into());
		}

		self.stack.push(Some(obj));
		//self.sp += 1;

		Ok(())
	}

	fn pop(&mut self) -> Option<Object> {
		self.last_popped_stack_elem = self.stack.pop().and_then(identity);

		self.last_popped_stack_elem.clone()
	}

	//TODO Dissolve if still not using index iterator
	fn current_frame(&mut self) -> &mut Frame {
		self.frames.last_mut().unwrap()
	}

	fn push_frame(&mut self, frame: Frame) {
		self.frames.push(frame)
	}

	fn pop_frame(&mut self) -> Option<Frame> {
		self.frames.pop()
	}

	fn push_closure(&mut self, const_index: u16, num_free: u8) -> VMResult {
		let num_free = num_free as usize;

		let constant = self.constants[const_index as usize].clone();
		if let Object::Function(func) = constant {
			//TODO Can we take from stack instead of copy and truncate?
			let free = self.stack[self.stack.len() - num_free..].iter().cloned().map(|o| o.unwrap()).collect();
			self.stack.truncate(self.stack.len() - num_free);

			self.push(Object::Closure(Closure { func, free }))
		} else {
			Err(format!("not a function: {}", constant))
		}
	}
}

type VMResult = Result<(), String>;