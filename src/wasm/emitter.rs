use crate::ast::{IntExpr, PrefixOperator};
use crate::type_checker::typed_ast::{TypedExpression, TypedStatement, TypedStatementBlock};
use crate::wasm::encoding::*;

pub fn compile_block_with_header(block: TypedStatementBlock) -> Result<Vec<u8>, String> {
	let mut code = vec![];
	code.extend(MAGIC_MODULE_HEADER);
	code.extend(MODULE_VERSION);

	let float_void_type = flatten(vec![
		vec![FUNCTION_TYPE],
		encode_vector(&[Valtype::F32 as u8]),
		vec![EMPTY_ARRAY]
	]);

	let int_void_type = flatten(vec![
		vec![FUNCTION_TYPE],
		encode_vector(&[Valtype::I32 as u8]),
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
		&encode_vectors(vec![flatten(vec![
			encode_string("env"),
			encode_string("print"),
			vec![
				ExportType::Func as u8,
				0x02, // type index
			],
		])])
	));

	// the function section is a vector of type indices that indicate the type of each function
	// in the code section
	code.extend(create_section(
		Section::Func,
		&encode_vector(&[0x00 /* type index */])
	));

	// the export section is a vector of exported functions
	code.extend(create_section(
		Section::Export,
		&encode_vectors(vec![
			flatten(vec![
				encode_string("run"),
				vec![
					ExportType::Func as u8,
					0x01,	// function index
				]
			])
		])
	));

	code.extend(create_section(
		Section::Code,
		&encode_vectors(vec![
			encode_vector(&flatten(vec![
				vec![EMPTY_ARRAY],	//locals
				compile_block(block)?,
				vec![Opcodes::End as u8],
			]))
		]),
	));

	Ok(code)
}

fn compile_block(block: TypedStatementBlock) -> Result<Vec<u8>, String> {
	let mut code = vec![];

	for stmt in block.statements {
		compile_statement(&mut code, stmt)?
	}

	Ok(code)
}

fn compile_statement(mut code: &mut Vec<u8>, stmt: TypedStatement) -> CompilerResult {
	match stmt {
		TypedStatement::Expression { expr, .. } => {
			compile_expression(&mut code, expr)?;
		}
		_ => eprintln!("compile statement:{:?}", stmt)
	}

	Ok(())
}

fn compile_expression(mut code: &mut Vec<u8>, expr: TypedExpression) -> CompilerResult {
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

			code.push(opcode as u8);

			let mut buf = vec![];
			//TODO Try using write_u16_into with code slice
			leb128::write::signed(&mut buf, value as i64).unwrap();
			code.extend(buf);
		}
		//TODO Add floats
		/*TypedExpression::Float(value) => {
			code.push(Opcodes::F32Const as u8);
			let mut buf = [0; 4];
			LittleEndian::write_u32(&mut buf, (value as f32).bits());
			code.extend(buf);
		}*/
		TypedExpression::Prefix { operator, right } => {

			match operator {
				PrefixOperator::Minus => {
					code.push(Opcodes::I32Const as u8);
					code.push(0);
					compile_expression(&mut code, *right)?;
					code.push(Opcodes::I32Sub as u8);
				}
				_ => eprintln!("prefix {:?}", operator)
			};
		}
		TypedExpression::Call { function, mut arguments, .. } => {
			if let TypedExpression::Identifier { name, .. } = *function {
				if name == "print" {
					return compile_print(&mut code, &mut arguments);
				}
			}
		}
		_ => eprintln!("compile expression:{:?}", expr)
	}

	Ok(())
}

fn compile_print(mut code: &mut Vec<u8>, arguments: &mut Vec<TypedExpression>) -> CompilerResult {
	assert_eq!(arguments.len(), 1, "wrong arg count for print()");

	let arg = arguments.remove(0);
	compile_expression(&mut code, arg)?;

	code.push(Opcodes::Call as u8);
	code.push(0);

	return Ok(())
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
enum Valtype {
	I32 = 0x7F,
	// I64 = 0x7E,
	F32 = 0x7D,
	// F64 = 0x7C,
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
	End = 0x0b,
	Call = 0x10,
	GetLocal = 0x20,
	I32Const = 0x41,
	I64Const = 0x42,
	F32Const = 0x43,
	F64Const = 0x44,
	I32Sub = 0x6B,
	F32Add = 0x92,
}