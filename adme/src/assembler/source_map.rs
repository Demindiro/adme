use std::collections::HashMap;
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct SourceMap {
	// Could probably be done more efficiently with a Vec but w/e
	ip_to_line: HashMap<u32, u32>,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl SourceMap {
	pub fn new() -> Self {
		Self { ip_to_line: HashMap::default() }
	}

	pub fn insert(&mut self, ip: u32, line: u32) {
		self.ip_to_line.insert(ip, line);
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen)]
	pub fn get_line(&mut self, ip: u32) -> Option<u32> {
		self.ip_to_line.get(&ip).copied()
	}
}
