use std::collections::hash_map::Entry;
use std::collections::HashMap;
use byteorder::{ByteOrder, LittleEndian};
use ieee754::Ieee754;
use crate::ast::{InfixOperator, IntExpr, PrefixOperator};
use crate::token::IntType;
use crate::type_checker::type_env::TypeExpr;
use crate::type_checker::typed_ast::{TypedExpression, TypedStatement, TypedStatementBlock};
use crate::wasm::encoding::*;

pub fn compile_block_with_header(block: TypedStatementBlock) -> Result<Vec<u8>, String> {
	let mut code = vec![];
	code.extend(MAGIC_MODULE_HEADER);
	code.extend(MODULE_VERSION);

	let float_void_type = flatten(vec![
		vec![FUNCTION_TYPE],
		encode_vector(&[ValType::F32 as u8]),
		vec![EMPTY_ARRAY]
	]);

	let int_void_type = flatten(vec![
		vec![FUNCTION_TYPE],
		encode_vector(&[ValType::I32 as u8]),
		vec![EMPTY_ARRAY]
	]);

	//TODO Only add compiled types
	// the type section is a vector of function types
	code.extend(create_section(
		Section::Type,
		&encode_vectors(vec![
			VOID_VOID_TYPE.to_vec(),
			float_void_type,
			int_void_type,
		])
	));

	// the import section is a vector of imported functions
	code.extend(create_section(
		Section::Import,
		&encode_vectors(vec![
			flatten(vec![
				encode_string("env"),
				encode_string("print"),
				vec![
					ExportType::Func as u8,
					0x02, // type index
				],
			]),
			flatten(vec![
				encode_string("env"),
				encode_string("printf"),
				vec![
					ExportType::Func as u8,
					0x01, // type index
				],
			]),
		])
	));

	// the function section is a vector of type indices that indicate the type of each function
	// in the code section
	code.extend(create_section(
		Section::Func,
		&encode_vector(&[
			0x00 /* type index */,
		])
	));

	// the export section is a vector of exported functions
	code.extend(create_section(
		Section::Export,
		&encode_vectors(vec![
			flatten(vec![
				encode_string("run"),
				vec![
					ExportType::Func as u8,
					//Needs to count from import index, for some reason
					0x02,	// function index
				]
			])
		])
	));

	let (block_code, locals) = compile_block(block)?;

	let mut local_section = vec![];
	for (val_type, count) in locals {
		let mut e = vec![];
		leb128::write::unsigned(&mut e, count as u64).unwrap();
		e.push(val_type);
		local_section.push(e);
	}

	let code_section = create_section(
		Section::Code,
		&encode_vectors(vec![
			encode_vector(&flatten(vec![
				encode_vectors(local_section),
				block_code,
				vec![Opcodes::End as u8],
			]))
		]),
	);

	code.extend(code_section);

	Ok(code)
}

fn compile_block(block: TypedStatementBlock) -> Result<(Vec<u8>, HashMap<u8, usize>), String> {
	let mut emitter = WasmEmitter::new();

	emitter.compile_block(block)?;

	let mut locals = HashMap::new();
	for (val_type, _) in emitter.symbols.values() {
		match locals.entry(*val_type as u8) {
			Entry::Occupied(mut e) => *e.get_mut() += 1,
			Entry::Vacant(e) => {
				e.insert(1);
			}
		}
	}

	Ok((emitter.code, locals))
}

struct WasmEmitter {
	code: Vec<u8>,
	symbols: HashMap<String, (ValType, usize)>,
}

impl WasmEmitter {
	fn new() -> Self {
		Self {
			code: Vec::new(),
			symbols: HashMap::new(),
		}
	}

	fn create_local_index(&mut self, name: String, valtype: ValType) -> usize {
		let size = self.symbols.len();
		self.symbols.insert(name, (valtype, size));
		size
	}

	fn get_local(&mut self, name: String) -> usize {
		self.symbols.get(&name).unwrap().1
	}

	fn compile_block(&mut self, block: TypedStatementBlock) -> CompilerResult {
		for stmt in block.statements {
			self.compile_statement(stmt)?;
		}

		Ok(())
	}

	fn compile_statement(&mut self, stmt: TypedStatement) -> CompilerResult {
		match stmt {
			TypedStatement::Expression { expr, .. } => {
				self.compile_expression(expr)?;
			}
			TypedStatement::Let { name, value, type_expr } => {
				let val_type = match val_type_from_type(&type_expr) {
					Ok(t) => t,
					Err(err) => return Err(err),
				};
				self.compile_expression(value)?;
				self.emit_opcode(Opcodes::SetLocal);
				let index = self.create_local_index(name, val_type);
				self.emit_u64(index as u64);
			}
			TypedStatement::While { condition, block } => {
				// outer block
				self.emit_opcode(Opcodes::Block);
				self.code.push(BlockType::Void as u8);
				// inner loop
				self.emit_opcode(Opcodes::Loop);
				self.code.push(BlockType::Void as u8);
				// compute the while expression
				self.compile_expression(condition)?;
				self.emit_opcode(Opcodes::I32EqZero);
				// br_if $label0
				self.emit_opcode(Opcodes::BrIf);
				self.emit_i64(1);
				// the nested logic
				self.compile_block(block)?;
				// br $label1
				self.emit_opcode(Opcodes::Br);
				self.emit_i64(0);
				// end loop
				self.emit_opcode(Opcodes::End);
				// end block
				self.emit_opcode(Opcodes::End);
			}
			_ => eprintln!("compile statement:{:?}", stmt)	//TODO stmt rest
		}

		Ok(())
	}

	fn compile_expression(&mut self, expr: TypedExpression) -> CompilerResult {
		match expr {
			TypedExpression::Integer(int_expr) => {
				let (value, opcode) = match int_expr {
					IntExpr::U8(value) => (value as i64, Opcodes::I32Const),
					IntExpr::U16(value) => (value as i64, Opcodes::I32Const),
					IntExpr::U32(value) => (value as i64, Opcodes::I32Const),
					IntExpr::U64(value) => (value as i64, Opcodes::I64Const),
					IntExpr::I8(value) => (value as i64, Opcodes::I32Const),
					IntExpr::I16(value) => (value as i64, Opcodes::I32Const),
					IntExpr::I32(value) => (value as i64, Opcodes::I32Const),
					IntExpr::I64(value) => (value as i64, Opcodes::I64Const),
				};

				self.emit_opcode(opcode);
				self.emit_i64(value);
			}
			TypedExpression::Float(value) => {
				self.emit_opcode(Opcodes::F32Const);
				let mut buf = [0; 4];
				LittleEndian::write_u32(&mut buf, value.bits());
				self.code.extend(buf);
			}
			TypedExpression::Identifier { name, .. } => {
				self.emit_opcode(Opcodes::GetLocal);
				let index = self.get_local(name);
				self.emit_u64(index as u64);
			}
			TypedExpression::Prefix { operator, right } => {
				match operator {
					PrefixOperator::Minus => {
						//https://github.com/WebAssembly/design/issues/379
						self.emit_opcode(Opcodes::I32Const);
						self.code.push(0);
						self.compile_expression(*right)?;
						self.emit_opcode(Opcodes::I32Sub);
					}
					_ => eprintln!("prefix {:?}", operator)	//TODO prefix rest
				};
			}
			TypedExpression::Infix { left, right, operator, operand_type, .. } => {
				self.compile_expression(*left)?;
				self.compile_expression(*right)?;

				match operand_type {
					TypeExpr::Int(_) => match operator {
						InfixOperator::Plus => self.emit_opcode(Opcodes::I32Add),
						InfixOperator::Minus => self.emit_opcode(Opcodes::I32Sub),
						InfixOperator::Mul => self.emit_opcode(Opcodes::I32Mul),
						InfixOperator::Div => self.emit_opcode(Opcodes::I32DivSigned),
						InfixOperator::Equal => self.emit_opcode(Opcodes::I32Eq),
						InfixOperator::Unequal => self.emit_opcode(Opcodes::I32NotEq),
						InfixOperator::GreaterThan => self.emit_opcode(Opcodes::I32GTSigned),
						InfixOperator::LessThan => self.emit_opcode(Opcodes::I32LTSigned),
					},
					TypeExpr::Float => match operator {
						InfixOperator::Plus => self.emit_opcode(Opcodes::F32Add),
						InfixOperator::Minus => self.emit_opcode(Opcodes::F32Sub),
						InfixOperator::Mul => self.emit_opcode(Opcodes::F32Mul),
						InfixOperator::Div => self.emit_opcode(Opcodes::F32Div),
						InfixOperator::Equal => self.emit_opcode(Opcodes::F32Eq),
						InfixOperator::Unequal => self.emit_opcode(Opcodes::F32NotEq),
						InfixOperator::GreaterThan => self.emit_opcode(Opcodes::F32GT),
						InfixOperator::LessThan => self.emit_opcode(Opcodes::F32LT),
					}
					_ => return Err(format!("{:?} not implemented for {:?}", operator, operand_type))
				}
			}
			TypedExpression::Call { function, mut arguments, .. } => {
				if let TypedExpression::Identifier { name, .. } = *function {
					if name == "print" {
						return self.compile_builtin(&mut arguments, 0);
					} else if name == "printf" {
						return self.compile_builtin(&mut arguments, 1);
					} else {
						panic!("function {} not supported", name);
					}
				}
			}
			TypedExpression::Assignment { ident, new_value, .. } => {
				self.compile_expression(*new_value)?;
				self.emit_opcode(Opcodes::SetLocal);

				let index = self.get_local(ident);
				self.emit_u64(index as u64);
			}
			_ => eprintln!("compile expression:{:?}", expr)	//TODO expr rest
		}

		Ok(())
	}

	fn compile_builtin(&mut self, arguments: &mut Vec<TypedExpression>, index: u8) -> CompilerResult {
		assert_eq!(arguments.len(), 1, "wrong arg count for builtin function");

		let arg = arguments.remove(0);
		self.compile_expression(arg)?;

		self.emit_opcode(Opcodes::Call);
		self.emit_u64(index as u64);

		return Ok(())
	}

	fn emit_i64(&mut self, value: i64) {
		let mut buf = vec![];
		//TODO Try writing directly on code slice
		leb128::write::signed(&mut buf, value).unwrap();
		self.code.extend(buf);
	}

	fn emit_u64(&mut self, value: u64) {
		let mut buf = vec![];
		//TODO Try writing directly on code slice
		leb128::write::unsigned(&mut buf, value).unwrap();
		self.code.extend(buf);
	}

	fn emit_opcode(&mut self, op: Opcodes) {
		self.code.push(op as u8);
	}
}

fn val_type_from_type(type_expr: &TypeExpr) -> Result<ValType, String> {
	match type_expr {
		TypeExpr::Int(IntType::U64) |
		TypeExpr::Int(IntType::I64) => Ok(ValType::I64),
		TypeExpr::Int(_) => Ok(ValType::I32),
		TypeExpr::Float => Ok(ValType::F32),
		_ => Err(format!("local {:?} not implemented", type_expr)),
	}
}

type CompilerResult = Result<(), String>;

// http://webassembly.github.io/spec/core/binary/types.html#function-types
const FUNCTION_TYPE: u8 = 0x60;
const EMPTY_ARRAY: u8 = 0x0;

const MAGIC_MODULE_HEADER: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];
const MODULE_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

const VOID_VOID_TYPE: [u8; 3] = [FUNCTION_TYPE, EMPTY_ARRAY, EMPTY_ARRAY];

// https://webassembly.github.io/spec/core/binary/modules.html#sections
// sections are encoded by their type followed by their vector contents
fn create_section(section: Section, data: &[u8]) -> Vec<u8> {
	let mut output = vec![section as u8];
	output.extend(encode_vector(data));
	output
}

// https://webassembly.github.io/spec/core/binary/modules.html#sections
#[repr(u8)]
enum Section {
	// Custom = 0,
	Type = 1,
	Import = 2,
	Func = 3,
	// Table = 4,
	// Memory = 5,
	// Global = 6,
	Export = 7,
	// Start = 8,
	// Element = 9,
	Code = 10,
	// Data = 11,
}

// https://webassembly.github.io/spec/core/binary/types.html
#[repr(u8)]
#[derive(Copy, Clone)]
pub enum ValType {
	I32 = 0x7F,
	I64 = 0x7E,
	F32 = 0x7D,
	// F64 = 0x7C,
}

// https://webassembly.github.io/spec/core/binary/types.html#binary-blocktype
#[repr(u8)]
enum BlockType {
	Void = 0x40,
}

// http://webassembly.github.io/spec/core/binary/modules.html#export-section
#[repr(u8)]
enum ExportType {
	Func = 0x00,
	// Table = 0x01,
	// Mem = 0x02,
	// Global = 0x03,
}

// https://webassembly.github.io/spec/core/binary/instructions.html
#[repr(u8)]
enum Opcodes {
	Block = 0x02,
	Loop = 0x03,
	If = 0x04,
	Else = 0x05,

	End = 0x0B,
	Br = 0x0C,
	BrIf = 0x0D,
	Call = 0x10,
	GetLocal = 0x20,
	SetLocal = 0x21,

	I32Const = 0x41,
	I64Const = 0x42,
	F32Const = 0x43,
	// F64Const = 0x44,

	I32EqZero = 0x45,
	I32Eq = 0x46,
	I32NotEq = 0x47,
	I32LTSigned = 0x48,
	// I32LTUnsigned = 0x49,
	I32GTSigned = 0x4A,
	// I32GTUnsigned = 0x4B,
	// I32LESigned = 0x4C,
	// I32LEUnsigned = 0x4D,
	// I32GESigned = 0x4E,
	// I32GEUnsigned = 0x4F,

	//I64EqZero = 0x50,
	// I64Eq = 0x51,
	// I64NotEq = 0x52,
	// I64LTSigned = 0x53,
	// I64LTUnsigned = 0x54,
	// I64GTSigned = 0x55,
	// I64GTUnsigned = 0x56,
	// I64LESigned = 0x57,
	// I64LEUnsigned = 0x58,
	// I64GESigned = 0x59,
	// I64GEUnsigned = 0x5A,

	F32Eq = 0x5B,
	F32NotEq = 0x5C,
	F32LT = 0x5D,
	F32GT = 0x5E,
	// F32LE = 0x5F,
	// F32GE = 0x60,

	// F64Eq = 0x61,
	// F64NotEq = 0x62,
	// F64LT = 0x63,
	// F64GT = 0x64,
	// F64LE = 0x65,
	// F64GE = 0x66,

	I32Add = 0x6A,
	I32Sub = 0x6B,
	I32Mul = 0x6C,
	I32DivSigned = 0x6D,
	// I32DivUnsigned = 0x6E,

	F32Add = 0x92,
	F32Sub = 0x93,
	F32Mul = 0x94,
	F32Div = 0x95,
}