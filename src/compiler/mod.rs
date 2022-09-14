use crate::ast::{Expression, Program, Statement};
use crate::code::{Instructions, make, Opcode, Operand};
use crate::compiler::symbol_table::SymbolTable;
use crate::object::Object;

pub mod symbol_table;

pub struct Compiler {
	instructions: Instructions,
	pub constants: Vec<Object>,
	pub symbol_table: SymbolTable,

	last_instruction: Option<EmittedInstruction>,
	previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
	pub fn new() -> Self {
		Self {
			instructions: Default::default(),
			constants: vec![],
			symbol_table: SymbolTable::new(),
			last_instruction: None,
			previous_instruction: None,
		}
	}

	pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
		Self {
			instructions: Default::default(),
			constants,
			symbol_table,
			last_instruction: None,
			previous_instruction: None,
		}
	}

	//TODO Check difference between Program and BlockStatement
	pub fn compile(&mut self, program: Program) -> CompilerResult {
		for stmt in program.statements {
			self.compile_statement(stmt)?
		}

		Ok(())
	}

	fn compile_statement(&mut self, stmt: Statement) -> CompilerResult {
		match stmt {
			Statement::Expression(exp) => {
				self.compile_expression(exp)?;

				self.emit(Opcode::OpPop, vec![]);
			},
			Statement::Let { name, value } => {
				self.compile_expression(value)?;

				//TODO Either dissolve Expression in Let statement, or add nested struct
				let name = if let Expression::Identifier(name) = name {
					name
				} else {
					panic!("{:?} is not Identifier", name)
				};

				let symbol = self.symbol_table.define(name.as_str());
				let index = symbol.index;
				self.emit(Opcode::OpSetGlobal, vec![index]);
			}
			_ => return Err(format!("{:?} not handled", stmt))
		}

		Ok(())
	}

	fn compile_expression(&mut self, exp: Expression) -> CompilerResult {
		match exp {
			Expression::Integer(value) => {
				let operand = self.add_constant(Object::Integer(value));
				self.emit(Opcode::OpConstant, vec![operand]);
			}
			Expression::Boolean(value) => if value {
				self.emit(Opcode::OpTrue, vec![]);
			}else {
				self.emit(Opcode::OpFalse, vec![]);
			}
			Expression::String(value) => {
				let operand = self.add_constant(Object::String(value));
				self.emit(Opcode::OpConstant, vec![operand]);
			}
			Expression::Prefix { operator, right } => {
				self.compile_expression(*right)?;

				match operator.as_str() {
					"!" => self.emit(Opcode::OpBang, vec![]),
					"-" => self.emit(Opcode::OpMinus, vec![]),
					_ => return Err(format!("unknown operator {:?}", operator))
				};
			}
			Expression::Infix { left, operator, right } => {
				if operator.as_str() == "<" {
					self.compile_expression(*right)?;

					self.compile_expression(*left)?;

					self.emit(Opcode::OpGreaterThan, vec![]);

					return Ok(())
				}

				self.compile_expression(*left)?;

				self.compile_expression(*right)?;

				match operator.as_str() {
					"+" => self.emit(Opcode::OpAdd, vec![]),
					"-" => self.emit(Opcode::OpSub, vec![]),
					"*" => self.emit(Opcode::OpMul, vec![]),
					"/" => self.emit(Opcode::OpDiv, vec![]),
					">" => self.emit(Opcode::OpGreaterThan, vec![]),
					"==" => self.emit(Opcode::OpEqual, vec![]),
					"!=" => self.emit(Opcode::OpNotEqual, vec![]),
					_ => return Err(format!("unknown operator {}", operator))
				};
			}
			Expression::Identifier(name) => {
				let symbol = if let Some(s) = self.symbol_table.resolve(&name) {
					s
				}else {
					return Err(format!("undefined variable {}", name))
				};

				let index = symbol.index;
				self.emit(Opcode::OpGetGlobal, vec![index]);
			}
			Expression::If { condition, consequence, alternative } => {
				self.compile_expression(*condition)?;

				//Emit an OpJumpIfFalse with temp value
				let jump_if_false_pos = self.emit(Opcode::OpJumpIfFalse, vec![0]);

				self.compile(consequence)?;

				if self.last_instruction_is_pop() {
					self.remove_last_pop();
				}

				if let Some(alternative) = alternative {
					//Emit an OpJump with temp value
					let jump_pos = self.emit(Opcode::OpJump, vec![0]);

					let after_consequence_pos = self.instructions.len();
					self.change_operand(jump_if_false_pos, after_consequence_pos);

					self.compile(alternative)?;

					if self.last_instruction_is_pop() {
						self.remove_last_pop();
					}

					let after_alternative_pos = self.instructions.len();
					self.change_operand(jump_pos, after_alternative_pos);
				} else {
					let after_consequence_pos = self.instructions.len();
					self.change_operand(jump_if_false_pos, after_consequence_pos);
				}
			},
			Expression::Array(elements) => {
				let length = elements.len();
				for el in elements {
					self.compile_expression(el)?;
				}

				self.emit(Opcode::OpArray, vec![length]);
			}
			_ => return Err(format!("{:?} not handled", exp))
		}

		Ok(())
	}

	pub fn bytecode(self) -> Bytecode {
		Bytecode {
			instructions: self.instructions,
			constants: self.constants,
		}
	}

	fn add_constant(&mut self, obj: Object) -> Operand {
		self.constants.push(obj);

		self.constants.len() - 1
	}

	fn emit(&mut self, op: Opcode, operands: Vec<Operand>) -> usize {
		let pos = self.add_instruction(make(op, &operands));
		self.set_last_instruction(op, pos);

		pos
	}

	fn add_instruction(&mut self, ins: Instructions) -> usize {
		let pos_new_instruction = self.instructions.len();
		self.instructions.extend(ins);

		pos_new_instruction
	}

	fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
		let previous = std::mem::take(&mut self.last_instruction);
		let last = EmittedInstruction { opcode: op, position: pos };

		self.previous_instruction = previous;
		self.last_instruction = Some(last);
	}

	fn last_instruction_is_pop(&self) -> bool {
		self.last_instruction.as_ref().map(|ins| ins.opcode) == Some(Opcode::OpPop)
	}

	fn remove_last_pop(&mut self) {
		self.instructions.truncate(self.last_instruction.as_ref().unwrap().position);
		//Will cause issues if previous_instruction is relied on
		self.last_instruction = std::mem::take(&mut self.previous_instruction);
	}

	fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
		//Maybe could be done with self.instructions[pos+1..] = new_instruction[..]?
		for (i, ins) in new_instruction.into_iter().enumerate() {
			self.instructions[pos+i] = ins;
		}
	}

	fn change_operand(&mut self, op_pos: usize, operand: usize) {
		let op: Opcode = self.instructions[op_pos].try_into().unwrap();
		let new_instruction = make(op, &vec![operand]);

		self.replace_instruction(op_pos, new_instruction)
	}
}

pub struct Bytecode {
	pub instructions: Instructions,
	pub constants: Vec<Object>,
}

struct EmittedInstruction {
	opcode: Opcode,
	position: usize,
}

type CompilerResult = Result<(), String>;