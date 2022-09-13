use crate::ast::{Expression, Program, Statement};
use crate::code::{Instructions, make, Opcode, Operand};
use crate::object::Object;

pub struct Compiler {
	instructions: Instructions,
	constants: Vec<Object>,
}

impl Compiler {
	pub fn new() -> Self {
		Self {
			instructions: Default::default(),
			constants: vec![],
		}
	}

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
			_ => return Err(format!("{:?} not handled", stmt))
		}

		Ok(())
	}

	fn compile_expression(&mut self, exp: Expression) -> CompilerResult {
		match exp {
			Expression::Integer(value) => {
				let operands = vec![self.add_constant(Object::Integer(value))];
				self.emit(Opcode::OpConstant, operands);
			}
			Expression::Boolean(value) => if value {
				self.emit(Opcode::OpTrue, vec![]);
			}else {
				self.emit(Opcode::OpFalse, vec![]);
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
		self.add_instruction(make(op, &operands))
	}

	fn add_instruction(&mut self, ins: Instructions) -> usize {
		let pos_new_instruction = self.instructions.len();
		self.instructions.extend(ins);

		pos_new_instruction
	}
}

pub struct Bytecode {
	pub instructions: Instructions,
	pub constants: Vec<Object>,
}

type CompilerResult = Result<(), String>;