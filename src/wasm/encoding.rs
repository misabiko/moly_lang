use crate::wasm::emitter::Valtype;

// https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
// Vectors are encoded with their length followed by their element sequence
pub fn encode_vector(data: &[u8]) -> Vec<u8> {
	let mut len = vec![];
	leb128::write::unsigned(&mut len, data.len() as u64).unwrap();
	let mut output = vec![len[0]];
	output.extend(data);
	output
}

pub fn encode_vectors(data: Vec<Vec<u8>>) -> Vec<u8> {
	let mut len = vec![];
	leb128::write::unsigned(&mut len, data.len() as u64).unwrap();
	//TODO Try keeping rest of len
	let mut output = vec![len[0]];
	output.extend(flatten(data));
	output
}

pub fn encode_string(s: &str) -> Vec<u8> {
	let mut e = vec![s.len() as u8];
	e.extend(s.bytes().collect::<Vec<u8>>());
	e
}

pub fn encode_local(count: usize, local_type: Valtype) -> Vec<u8> {
	let mut len = vec![];
	leb128::write::unsigned(&mut len, count as u64).unwrap();
	let mut e = vec![len[0]];
	e.push(local_type as u8);
	e
}

pub fn flatten(data: Vec<Vec<u8>>) -> Vec<u8> {
	data.into_iter().flatten().collect()
}