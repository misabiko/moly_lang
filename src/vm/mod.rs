use enum_primitive::FromPrimitive;
use crate::code::{Instructions, Opcode, read_u16, read_u8};
use crate::compiler::Bytecode;
use crate::object::{Builtin, Closure, Function, Object};
use crate::object::builtins::get_builtin_functions;
use crate::vm::frame::Frame;

pub mod frame;

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);

//https://github.com/rust-lang/rust/issues/44796
const STACK_INIT: Option<Object> = None;
const FRAME_INIT: Option<Frame> = None;

pub struct VM {
	constants: Vec<Object>,
	pub globals: Vec<Object>,

	stack: [Option<Object>; STACK_SIZE],
	sp: usize,
	pub last_popped_stack_elem: Option<Object>,

	pub frames: [Option<Frame>; MAX_FRAMES],
	frame_index: usize,
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

		let mut frames: [Option<Frame>; MAX_FRAMES] = [FRAME_INIT; MAX_FRAMES];
		frames[0] = Some(Frame::new(main_closure, 0));

		Self {
			constants: bytecode.constants,
			globals: Vec::with_capacity(GLOBALS_SIZE),

			stack: [STACK_INIT; STACK_SIZE],
			sp: 0,
			last_popped_stack_elem: None,

			frames,
			frame_index: 1,
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

		let mut frames: [Option<Frame>; MAX_FRAMES] = [FRAME_INIT; MAX_FRAMES];
		frames[0] = Some(Frame::new(main_closure, 0));

		Self {
			constants: bytecode.constants,
			globals,

			stack: [STACK_INIT; STACK_SIZE],
			sp: 0,
			last_popped_stack_elem: None,

			frames,
			frame_index: 1,
		}
	}

	pub fn stack_top(&self) -> Option<&Object> {
		if self.sp == 0 {
			None
		} else {
			self.stack[self.sp - 1].as_ref()
		}
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
						if condition == &FALSE_OBJ {
							self.current_frame().ip = pos;
						}

						self.last_popped_stack_elem = None;
					}

					Opcode::SetGlobal => {
						let global_index = read_u16(&ins[ip..]) as usize;
						self.current_frame().ip += 2;

						let len = self.globals.len();
						let value = self.pop().cloned().unwrap();

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
						self.stack[base_pointer + local_index] = self.pop().cloned();
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

						let definition = &get_builtin_functions()[builtin_index];

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

						let array = self.build_array(self.sp - num_elements, self.sp);
						self.sp -= num_elements;

						self.push(array)?;
					}
					Opcode::Index => {
						let index = self.pop().cloned().ok_or("index not on stack")?;
						let left = self.pop().cloned().ok_or("left expression not on stack")?;

						self.execute_index_expression(left, index)?;
					}

					Opcode::Call => {
						let num_args = read_u8(&ins[ip..]);
						self.current_frame().ip += 1;

						self.execute_call(num_args)?;
					}
					Opcode::ReturnValue => {
						let return_value = self.pop().cloned().unwrap();

						let base_pointer = self.pop_frame().as_ref().unwrap().base_pointer;
						self.sp = base_pointer - 1;

						self.push(return_value)?;
					}
					Opcode::Return => {
						let base_pointer = self.pop_frame().as_ref().unwrap().base_pointer;
						self.sp = base_pointer - 1;

						self.last_popped_stack_elem = None;
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
		let right = self.pop().cloned();
		let left = self.pop().cloned();

		match (&left, &right) {
			(Some(Object::U8(left)), Some(Object::U8(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::U8(result))
			}
			(Some(Object::U16(left)), Some(Object::U16(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::U16(result))
			}
			(Some(Object::U32(left)), Some(Object::U32(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::U32(result))
			}
			(Some(Object::U64(left)), Some(Object::U64(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::U64(result))
			}
			(Some(Object::I8(left)), Some(Object::I8(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::I8(result))
			}
			(Some(Object::I16(left)), Some(Object::I16(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::I16(result))
			}
			(Some(Object::I32(left)), Some(Object::I32(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::I32(result))
			}
			(Some(Object::I64(left)), Some(Object::I64(right))) => {
				let result = match op {
					Opcode::Add => left + right,
					Opcode::Sub => left - right,
					Opcode::Mul => left * right,
					Opcode::Div => left / right,
					_ => return Err(format!("unknown integer operator: {:?}", op))
				};

				self.push(Object::I64(result))
			}
			(Some(Object::String(left)), Some(Object::String(right)))
			=> self.execute_binary_string_operation(op, left, right),
			_ => Err(format!("unsupported types for binary operation: {:?} and {:?}", left, right))
		}
	}

	fn execute_binary_string_operation(&mut self, op: Opcode, left: &str, right: &str) -> VMResult {
		if !matches!(op, Opcode::Add) {
			return Err(format!("unknown string operator: {:?}", op));
		}

		self.push(Object::String(format!("{}{}", left, right)))
	}

	fn execute_comparison(&mut self, op: Opcode) -> VMResult {
		let left = self.pop().cloned().unwrap();
		let right = self.pop().cloned().unwrap();

		match (&left, &right) {
			(Object::U8(left), Object::U8(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::U16(left), Object::U16(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::U32(left), Object::U32(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::U64(left), Object::U64(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::I8(left), Object::I8(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::I16(left), Object::I16(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::I32(left), Object::I32(right))
			=> return self.execute_integer_comparison(op, *left as i64, *right as i64),
			(Object::I64(left), Object::I64(right))
			=> return self.execute_integer_comparison(op, *left, *right),
			_ => {}
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

		if *operand == TRUE_OBJ {
			self.push(FALSE_OBJ)?;
		}else if *operand == FALSE_OBJ {
			self.push(TRUE_OBJ)?;
		}else {
			return Err(format!("unsupported type for bang operator: {:?}", operand));
		}

		Ok(())
	}

	fn execute_minus_operator(&mut self) -> VMResult {
		let operand = self.pop().unwrap();

		match operand {
			Object::U8(value) => {
				let value = *value;
				self.push(Object::I8(-(value as i8)))
			}
			Object::U16(value) => {
				let value = *value;
				self.push(Object::I16(-(value as i16)))
			}
			Object::U32(value) => {
				let value = *value;
				self.push(Object::I32(-(value as i32)))
			}
			Object::U64(value) => {
				let value = *value;
				self.push(Object::I64(-(value as i64)))
			}
			Object::I8(value) => {
				let value = *value;
				self.push(Object::I8(-value))
			}
			Object::I16(value) => {
				let value = *value;
				self.push(Object::I16(-value))
			}
			Object::I32(value) => {
				let value = *value;
				self.push(Object::I32(-value))
			}
			Object::I64(value) => {
				let value = *value;
				self.push(Object::I64(-value))
			}
			_ => {
				Err(format!("unsupported type for negation: {:?}", operand))
			}
		}
	}

	fn build_array(&self, start_index: usize, end_index: usize) -> Object {
		Object::Array(
			self.stack[start_index..end_index]
				.iter().map(|o| o.clone().unwrap()).collect()
		)
	}

	fn execute_index_expression(&mut self, left: Object, index: Object) -> VMResult {
		match (left, index) {
			(Object::Array(elements), Object::U8(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::U16(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::U32(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::U64(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::I8(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::I16(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::I32(index))
			=> self.execute_array_index(elements, index as i64),
			(Object::Array(elements), Object::I64(index))
			=> self.execute_array_index(elements, index),
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

	fn execute_call(&mut self, num_args: u8) -> VMResult {
		let num_args_usize = num_args as usize;
		let callee = self.stack[self.sp - 1 - num_args_usize].clone().unwrap();
		match callee {
			Object::Closure(closure) => self.call_closure(closure, num_args),
			Object::Builtin(builtin) => self.call_builtin(builtin, num_args),
			callee => Err(format!("calling non-closure and non-builtin {:?}", callee))
		}
	}

	fn call_closure(&mut self, closure: Closure, num_args: u8) -> VMResult {
		assert_eq!(closure.func.num_parameters, num_args,
				   "wrong number of arguments: want={}, got={}",
				   closure.func.num_parameters,
				   num_args
		);

		let num_args = num_args as usize;
		let num_locals = closure.func.num_locals;
		let base_pointer = self.sp - num_args;
		self.push_frame(Frame::new(closure, base_pointer));

		self.sp = base_pointer + num_locals as usize;

		Ok(())
	}

	fn call_builtin(&mut self, builtin: Builtin, num_args: u8) -> VMResult {
		let num_args = num_args as usize;
		let args = &self.stack[self.sp - num_args..self.sp];

		let result = builtin(args.iter().cloned().map(|a| a.unwrap()).collect());
		self.sp = self.sp - num_args - 1;

		if let Some(result) = result {
			self.push(result)
		} else {
			self.last_popped_stack_elem = None;
			Ok(())
		}
	}

	fn push(&mut self, obj: Object) -> VMResult {
		if self.sp >= STACK_SIZE {
			return Err("stack overflow".into());
		}

		self.stack[self.sp] = Some(obj);
		self.sp += 1;

		Ok(())
	}

	fn pop(&mut self) -> Option<&Object> {
		self.last_popped_stack_elem = self.stack[self.sp - 1].take();
		assert!(self.last_popped_stack_elem.is_some(), "missing popped value");
		self.sp -= 1;

		self.last_popped_stack_elem.as_ref()
	}

	fn current_frame(&mut self) -> &mut Frame {
		self.frames[self.frame_index - 1].as_mut().unwrap()
	}

	fn push_frame(&mut self, frame: Frame) {
		self.frames[self.frame_index] = Some(frame);
		self.frame_index += 1;
	}

	fn pop_frame(&mut self) -> &Option<Frame> {
		self.frame_index -= 1;
		&self.frames[self.frame_index]
	}

	fn push_closure(&mut self, const_index: u16, num_free: u8) -> VMResult {
		let num_free = num_free as usize;

		let constant = self.constants[const_index as usize].clone();
		if let Object::Function(func) = constant {
			let free = self.stack[self.sp - num_free..self.sp].iter_mut().map(|o| o.take().unwrap()).collect();
			self.sp -= num_free;

			self.push(Object::Closure(Closure { func, free }))
		} else {
			Err(format!("not a function: {}", constant))
		}
	}
}

type VMResult = Result<(), String>;