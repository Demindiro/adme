use wasm_bindgen::prelude::*;
use crate::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);

    // The `console.log` is quite polymorphic, so we can bind it with multiple
    // signatures. Note that we need to use `js_name` to ensure we always call
    // `log` in JS.
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    pub fn log_u32(a: u32);

    // Multiple arguments too!
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_many(a: &str, b: &str);
}

#[wasm_bindgen]
pub struct Mem {
	mem: [u32; 1024],
}

#[wasm_bindgen]
impl Mem {
	#[wasm_bindgen(constructor)]
	pub fn new() -> Mem {
		Self {
			mem: [0; 1024],
		}
	}
}

impl Memory for Mem {
	fn load_u8(&mut self, addr: u32) -> Result<u8, LoadError> {
		let addr = usize::try_from(addr).unwrap();
		Ok(util::as_u8(&mut self.mem)[addr])
	}

	fn store_u8(&mut self, addr: u32, value: u8) -> Result<(), StoreError> {
		if addr == 0x2000 {
			log(core::str::from_utf8(&[value]).unwrap_or("\u{fffd}"));
		} else {
			let addr = usize::try_from(addr).unwrap();
			util::as_u8(&mut self.mem)[addr] = value;
		}
		Ok(())
	}

	fn load_u32(&mut self, addr: u32) -> Result<u32, LoadError> {
		let addr = usize::try_from(addr).unwrap();
		Ok(self.mem[addr / 4])
	}

	fn store_u32(&mut self, addr: u32, value: u32) -> Result<(), StoreError> {
		let addr = usize::try_from(addr).unwrap();
		self.mem[addr / 4] = value;
		Ok(())
	}
}

#[wasm_bindgen]
pub fn assemble(source: &str, memory: &mut Mem) {
	Assembler::assemble(source, &mut memory.mem);
}
