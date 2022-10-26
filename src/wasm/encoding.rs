// https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
// Vectors are encoded with their length followed by their element sequence
pub fn encode_vector(data: &[u8]) -> Vec<u8> {
	let mut output = vec![];
	leb128::write::unsigned(&mut output, data.len() as u64).unwrap();
	output.extend(data);
	output
}

pub fn encode_vectors(data: Vec<Vec<u8>>) -> Vec<u8> {
	let mut output = vec![];
	leb128::write::unsigned(&mut output, data.len() as u64).unwrap();
	output.extend(flatten(data));
	output
}

pub fn encode_string(s: &str) -> Vec<u8> {
	let mut e = vec![s.len() as u8];
	e.extend(s.bytes().collect::<Vec<u8>>());
	e
}

pub fn flatten(data: Vec<Vec<u8>>) -> Vec<u8> {
	data.into_iter().flatten().collect()
}