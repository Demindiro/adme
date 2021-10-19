use std::env;
use std::fs::File;
use std::io;
use std::process;

fn main() {
	let mut args = env::args();
	args.next(); // bin_name
	let asm_file = args.next().unwrap_or_else(|| err_usage());
	// Ensure remaining args are empty
	args.next().is_some().then(err_usage);

	let mut asm = String::new();

	use std::io::Read;
	File::open(&asm_file)
		.unwrap_or_else(|e| err_open(&asm_file, e))
		.read_to_string(&mut asm)
		.unwrap_or_else(|e| err_read(&asm_file, e));

	let mut cpu = adme::Cpu::new();

	struct Mem {
		mem: [u32; 1024 * 4],
	}

	impl Mem {
		fn as_u8(mem: &mut [u32]) -> &mut [u8] {
			unsafe { core::slice::from_raw_parts_mut(mem.as_mut_ptr().cast::<u8>(), mem.len() * 4) }
		}

		fn as_u16(mem: &mut [u32]) -> &mut [u16] {
			unsafe {
				core::slice::from_raw_parts_mut(mem.as_mut_ptr().cast::<u16>(), mem.len() * 2)
			}
		}
	}

	impl adme::Memory for Mem {
		fn load_u8(&mut self, addr: u32) -> Result<u8, adme::LoadError> {
			let addr = usize::try_from(addr).unwrap();
			Ok(Self::as_u8(&mut self.mem)[addr])
		}

		fn store_u8(&mut self, addr: u32, value: u8) -> Result<(), adme::StoreError> {
			if addr == 0x2000 {
				print!("{}", char::try_from(value).unwrap());
			} else {
				let addr = usize::try_from(addr).unwrap();
				Self::as_u8(&mut self.mem)[addr] = value;
			}
			Ok(())
		}

		fn load_u16(&mut self, addr: u32) -> Result<u16, adme::LoadError> {
			let addr = usize::try_from(addr).unwrap();
			Ok(Self::as_u16(&mut self.mem)[addr / 2])
		}

		fn store_u16(&mut self, addr: u32, value: u16) -> Result<(), adme::StoreError> {
			let addr = usize::try_from(addr).unwrap();
			Self::as_u16(&mut self.mem)[addr / 2] = value;
			Ok(())
		}

		fn load_u32(&mut self, addr: u32) -> Result<u32, adme::LoadError> {
			let addr = usize::try_from(addr).unwrap();
			Ok(self.mem[addr / 4])
		}

		fn store_u32(&mut self, addr: u32, value: u32) -> Result<(), adme::StoreError> {
			let addr = usize::try_from(addr).unwrap();
			self.mem[addr / 4] = value;
			Ok(())
		}
	}

	let mut mem = Mem { mem: [0; 1024 * 4] };

	adme::Assembler::assemble(&asm, &mut mem.mem).unwrap();

	// JIT test
	{
		let mut jit = adme::Jit::new(0);
		mem.mem.iter().take_while(|c| **c != 0).for_each(|c| jit.push(*c));
		let exec = jit.finish();
		let mut regs = adme::Registers::new();
		unsafe {
			exec.run(&mut regs, &mut mem.mem);
		}
	}

	for _ in 0..85 {
		cpu.step(&mut mem).unwrap();
	}
}

fn err_usage() -> ! {
	let mut args = env::args();
	let bin_name = args.next();
	eprintln!("Usage: {} <FILE>", bin_name.as_deref().unwrap_or("adme"));
	process::exit(1);
}

fn err_open(path: &str, err: io::Error) -> ! {
	eprintln!("Failed to open {:?}: {}", path, err);
	process::exit(1);
}

fn err_read(path: &str, err: io::Error) -> ! {
	eprintln!("Failed to read {:?}: {}", path, err);
	process::exit(1);
}
