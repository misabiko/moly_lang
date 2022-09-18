use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use enum_primitive::FromPrimitive;
use crate::ast::{Expression, Program, Statement};
use crate::code::{Instructions, make, Opcode, OperandIndex};
use crate::compiler::symbol_table::{SymbolScope, Symbol, SymbolTable};
use crate::object::{Function, Object};
use crate::object::builtins::BUILTINS;

pub mod symbol_table;

pub struct Compiler {
	pub constants: Vec<Object>,
	pub symbol_table: Rc<RefCell<SymbolTable>>,
	pub scopes: Vec<CompilationScope>,
	pub scope_index: u8,
}

pub struct CompilationScope {
	pub instructions: Instructions,
	pub last_instruction: Option<EmittedInstruction>,
	pub previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
	pub fn new() -> Self {
		let mut table = SymbolTable::new(None);
		for (i, v) in BUILTINS.iter().enumerate() {
			table.define_builtin(i as u8, v.name);
		}

		Self {
			constants: vec![],
			symbol_table: Rc::new(RefCell::new(table)),
			scopes: vec![
				CompilationScope {
					instructions: vec![],
					last_instruction: None,
					previous_instruction: None,
				}
			],
			scope_index: 0,
		}
	}

	pub fn new_with_state(symbol_table: Rc<RefCell<SymbolTable>>, constants: Vec<Object>) -> Self {
		Self {
			constants,
			symbol_table,
			scopes: vec![
				CompilationScope {
					instructions: vec![],
					last_instruction: None,
					previous_instruction: None,
				}
			],
			scope_index: 0,
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

				self.emit(Opcode::Pop, vec![]);
			}
			Statement::Let { name, value } => {
				let (index, scope) = {
					let mut table = self.symbol_table.borrow_mut();
					let symbol = table.define(name.as_str());
					(symbol.index as OperandIndex, symbol.scope)
				};

				self.compile_expression(value)?;

				if let SymbolScope::Global = scope {
					self.emit(Opcode::SetGlobal, vec![index]);
				} else {
					self.emit(Opcode::SetLocal, vec![index]);
				}
			}
			Statement::Return(Some(exp)) => {
				self.compile_expression(exp)?;

				self.emit(Opcode::ReturnValue, vec![]);
			}
			_ => return Err(format!("{:?} not handled", stmt))
		}

		Ok(())
	}

	fn compile_expression(&mut self, exp: Expression) -> CompilerResult {
		match exp {
			Expression::Integer(value) => {
				let operand = self.add_constant(Object::Integer(value as i64));
				self.emit(Opcode::Constant, vec![operand]);
			}
			Expression::Boolean(value) => if value {
				self.emit(Opcode::True, vec![]);
			} else {
				self.emit(Opcode::False, vec![]);
			}
			Expression::String(value) => {
				let operand = self.add_constant(Object::String(value));
				self.emit(Opcode::Constant, vec![operand]);
			}
			Expression::Prefix { operator, right } => {
				self.compile_expression(*right)?;

				match operator.as_str() {
					"!" => self.emit(Opcode::Bang, vec![]),
					"-" => self.emit(Opcode::Minus, vec![]),
					_ => return Err(format!("unknown operator {:?}", operator))
				};
			}
			Expression::Infix { left, operator, right } => {
				if operator == "<" {
					self.compile_expression(*right)?;

					self.compile_expression(*left)?;

					self.emit(Opcode::GreaterThan, vec![]);

					return Ok(());
				}

				self.compile_expression(*left)?;

				self.compile_expression(*right)?;

				match operator {
					"+" => self.emit(Opcode::Add, vec![]),
					"-" => self.emit(Opcode::Sub, vec![]),
					"*" => self.emit(Opcode::Mul, vec![]),
					"/" => self.emit(Opcode::Div, vec![]),
					">" => self.emit(Opcode::GreaterThan, vec![]),
					"==" => self.emit(Opcode::Equal, vec![]),
					"!=" => self.emit(Opcode::NotEqual, vec![]),
					_ => return Err(format!("unknown operator {}", operator))
				};
			}
			Expression::Identifier(name) => {
				let symbol = if let Some(s) = self.symbol_table.borrow_mut().resolve(&name) {
					s
				} else {
					return Err(format!("undefined variable {}", name));
				};

				self.load_symbol(symbol);
			}
			Expression::If { condition, consequence, alternative } => {
				self.compile_expression(*condition)?;

				//Emit an OpJumpIfFalse with temp value
				let jump_if_false_pos = self.emit(Opcode::JumpIfFalse, vec![0]);

				self.compile(consequence)?;

				if self.last_instruction_is(Opcode::Pop) {
					self.remove_last_pop();
				}

				//Emit an OpJump with temp value
				let jump_pos = self.emit(Opcode::Jump, vec![0]);

				let after_consequence_pos = self.current_instructions().len();
				self.change_operand(jump_if_false_pos, after_consequence_pos);

				if let Some(alternative) = alternative {
					self.compile(alternative)?;

					if self.last_instruction_is(Opcode::Pop) {
						self.remove_last_pop();
					}
				}

				let after_alternative_pos = self.current_instructions().len();
				self.change_operand(jump_pos, after_alternative_pos);
			}
			Expression::Array(elements) => {
				let length = elements.len();
				for el in elements {
					self.compile_expression(el)?;
				}

				self.emit(Opcode::Array, vec![length]);
			}
			Expression::Hash(mut pairs) => {
				let length = pairs.len();
				pairs.sort_by_key(|(k, _)| k.to_string());

				for (k, v) in pairs {
					self.compile_expression(k)?;
					self.compile_expression(v)?;
				}

				self.emit(Opcode::Hash, vec![length * 2]);
			}
			Expression::Index { left, index } => {
				self.compile_expression(*left)?;
				self.compile_expression(*index)?;

				self.emit(Opcode::Index, vec![]);
			}
			Expression::Function { parameters, body, name } => {
				self.enter_scope();

				if let Some(name) = name {
					self.symbol_table.borrow_mut().define_function_name(&name);
				}

				let num_parameters = parameters.len() as u8;
				for param in parameters.into_iter() {
					self.symbol_table.borrow_mut().define(&param);
				}

				self.compile(body)?;

				if self.last_instruction_is(Opcode::Pop) {
					self.replace_last_pop_with_return();
				}
				if !self.last_instruction_is(Opcode::ReturnValue) {
					self.emit(Opcode::Return, vec![]);
				}

				let free_symbols = self.symbol_table.borrow().free_symbols.clone();
				let num_locals = self.symbol_table.borrow().num_definitions as u8;
				let instructions = self.leave_scope().unwrap();

				let num_free_symbols = free_symbols.len();
				for sym in free_symbols {
					self.load_symbol(sym);
				}

				let fn_index = self.add_constant(Object::Function(Function {
					instructions,
					num_locals,
					num_parameters,
				}));
				self.emit(Opcode::Closure, vec![fn_index, num_free_symbols]);
			}
			Expression::Call { function, arguments } => {
				self.compile_expression(*function)?;

				let length = arguments.len();
				for arg in arguments.into_iter() {
					self.compile_expression(arg)?;
				}

				self.emit(Opcode::Call, vec![length]);
			}
		}

		Ok(())
	}

	pub fn bytecode(mut self) -> Bytecode {
		Bytecode {
			instructions: std::mem::take(self.current_instructions()),
			constants: self.constants,
		}
	}

	fn add_constant(&mut self, obj: Object) -> OperandIndex {
		self.constants.push(obj);

		self.constants.len() - 1
	}

	pub fn emit(&mut self, op: Opcode, operands: Vec<OperandIndex>) -> usize {
		let pos = self.add_instruction(make(op, &operands));
		self.set_last_instruction(op, pos);

		pos
	}

	fn current_instructions(&mut self) -> &mut Instructions {
		&mut self.current_scope_mut().instructions
	}

	fn add_instruction(&mut self, ins: Instructions) -> usize {
		let pos_new_instruction = self.current_instructions().len();
		self.current_instructions().extend(ins);

		pos_new_instruction
	}

	fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
		let scope = self.current_scope_mut();
		let previous = std::mem::take(&mut scope.last_instruction);
		let last = EmittedInstruction { opcode: op, position: pos };

		scope.previous_instruction = previous;
		scope.last_instruction = Some(last);
	}

	fn last_instruction_is(&self, op: Opcode) -> bool {
		self.current_scope().last_instruction.as_ref().map(|ins| ins.opcode) == Some(op)
	}

	fn remove_last_pop(&mut self) {
		let scope = self.current_scope_mut();
		let last = scope.last_instruction.as_ref().unwrap();
		let previous = &mut scope.previous_instruction;

		let old = &scope.instructions;
		let new = old[..last.position].into();

		scope.instructions = new;
		//Will cause issues if previous_instruction is relied on
		scope.last_instruction = std::mem::take(previous);
	}

	fn replace_last_pop_with_return(&mut self) {
		let last_pos = self.current_scope().last_instruction.as_ref().unwrap().position;
		self.replace_instruction(last_pos, make(Opcode::ReturnValue, &vec![]));

		self.current_scope_mut().last_instruction.as_mut().unwrap().opcode = Opcode::ReturnValue;
	}

	fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
		self.current_instructions().splice(
			pos..pos+new_instruction.len(),
			new_instruction
		);
	}

	fn change_operand(&mut self, op_pos: usize, operand: usize) {
		let op = Opcode::from_u8(self.current_instructions()[op_pos]).unwrap();
		let new_instruction = make(op, &vec![operand]);

		self.replace_instruction(op_pos, new_instruction)
	}

	pub fn enter_scope(&mut self) {
		self.scopes.push(CompilationScope {
			instructions: vec![],
			last_instruction: None,
			previous_instruction: None,
		});
		self.scope_index += 1;

		let table = std::mem::replace(&mut self.symbol_table, Rc::new(RefCell::new(
			SymbolTable {
				outer: None,
				store: HashMap::new(),
				num_definitions: 0,
				free_symbols: vec![],
			}
		)));
		self.symbol_table = Rc::new(RefCell::new(SymbolTable::new(Some(table))));
	}

	pub fn leave_scope(&mut self) -> Option<Instructions> {
		let ins = self.scopes.pop().map(|s| s.instructions);
		self.scope_index -= 1;

		let outer: Option<Rc<RefCell<SymbolTable>>> = std::mem::take(&mut self.symbol_table.borrow_mut().outer);
		self.symbol_table = outer.unwrap();
		ins
	}

	pub fn current_scope(&self) -> &CompilationScope {
		&self.scopes[self.scope_index as usize]
	}

	pub fn current_scope_mut(&mut self) -> &mut CompilationScope {
		&mut self.scopes[self.scope_index as usize]
	}

	fn load_symbol(&mut self, symbol: Symbol) {
		let operands = vec![symbol.index as OperandIndex];
		match symbol.scope {
			SymbolScope::Global => self.emit(Opcode::GetGlobal, operands),
			SymbolScope::Local => self.emit(Opcode::GetLocal, operands),
			SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, operands),
			SymbolScope::Free => self.emit(Opcode::GetFree, operands),
			SymbolScope::Function => self.emit(Opcode::CurrentClosure, vec![]),
		};
	}
}

#[derive(Debug)]
pub struct Bytecode {
	pub instructions: Instructions,
	pub constants: Vec<Object>,
}

pub struct EmittedInstruction {
	pub opcode: Opcode,
	pub position: usize,
}

type CompilerResult = Result<(), String>;