use crate::*;
use wasm_bindgen::prelude::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen]
	fn serial_out(value: u8);

	#[wasm_bindgen]
	fn error(err: &str);
}

#[wasm_bindgen]
pub struct Mem {
	mem: [u32; 1024],
}

#[wasm_bindgen]
impl Mem {
	#[wasm_bindgen(constructor)]
	pub fn new() -> Mem {
		Self { mem: [0; 1024] }
	}

	#[wasm_bindgen]
	pub fn get_u8(&self, addr: u32) -> Option<u8> {
		let addr = usize::try_from(addr).unwrap();
		util::as_u8(&self.mem).get(addr).copied()
	}

	#[wasm_bindgen]
	pub fn get_u32(&self, addr: u32) -> Option<u32> {
		let addr = usize::try_from(addr).unwrap();
		self.mem.get(addr / 4).copied()
	}
}

impl Memory for Mem {
	fn load_u8(&mut self, addr: u32) -> Result<u8, LoadError> {
		let addr = usize::try_from(addr).unwrap();
		Ok(util::as_u8_mut(&mut self.mem)[addr])
	}

	fn store_u8(&mut self, addr: u32, value: u8) -> Result<(), StoreError> {
		if addr == 0x2000 {
			serial_out(value);
		} else {
			let addr = usize::try_from(addr).unwrap();
			util::as_u8_mut(&mut self.mem)[addr] = value;
		}
		Ok(())
	}

	fn load_u16(&mut self, addr: u32) -> Result<u16, LoadError> {
		let addr = usize::try_from(addr).unwrap();
		Ok(util::as_u16_mut(&mut self.mem)[addr / 2])
	}

	fn store_u16(&mut self, addr: u32, value: u16) -> Result<(), StoreError> {
		let addr = usize::try_from(addr).unwrap();
		util::as_u16_mut(&mut self.mem)[addr / 2] = value;
		Ok(())
	}

	fn load_u32(&mut self, addr: u32) -> Result<u32, LoadError> {
		let addr = usize::try_from(addr).unwrap();
		self.mem
			.get(addr / 4)
			.copied()
			.ok_or(LoadError::OutOfBounds)
	}

	fn store_u32(&mut self, addr: u32, value: u32) -> Result<(), StoreError> {
		let addr = usize::try_from(addr).unwrap();
		self.mem
			.get_mut(addr / 4)
			.map(|v| *v = value)
			.ok_or(StoreError::OutOfBounds)
	}
}

#[wasm_bindgen]
pub fn assemble(source: &str, memory: &mut Mem) -> SourceMap {
	Assembler::assemble(source, &mut memory.mem)
}
