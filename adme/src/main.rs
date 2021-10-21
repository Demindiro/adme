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

	fn real_syscall(code: u32, a: [u32; 4], mem: &mut [u8]) -> [u32; 2] {
		use std::io::Write;
		match code {
			1 => {
				// print_int
				std::io::stdout()
					.write_all((a[0] as i32).to_string().as_bytes())
					.unwrap();
				std::io::stdout().flush().unwrap();
				[0, 0]
			}
			4 => {
				// print_string
				let s = &mem[usize::try_from(a[0]).unwrap()..];
				let e = s.iter().position(|b| *b == 0).unwrap();
				std::io::stdout().write_all(&s[..e]).unwrap();
				std::io::stdout().flush().unwrap();
				[0, 0]
			}
			5 => {
				// read_int
				let mut s = String::new();
				std::io::stdin().read_line(&mut s).unwrap();
				[
					s.as_str().trim().parse::<i32>().expect("invalid integer") as u32,
					0,
				]
			}
			10 => std::process::exit(0), // exit
			11 => {
				// print_char
				std::io::stdout().write_all(&[a[0] as u8]).unwrap();
				std::io::stdout().flush().unwrap();
				[0, 0]
			}
			s => {
				// invalid
				eprintln!("invalid syscall: {}", s);
				[u32::MAX, 0]
			}
		}
	}

	// JIT test
	#[cfg(feature = "jit")]
	{
		extern "C" fn syscall(regs: &mut adme::Registers, mem: &mut [u8; 0x4000]) {
			let v0 = regs.gp[2];
			let a = regs.gp[4..8].try_into().unwrap();
			// FIXME apparently unsafe bt idgaf
			let mem = mem as *mut _;
			let ret = std::panic::catch_unwind(|| real_syscall(v0, a, unsafe { &mut *mem }));
			let _ = ret.map(|ret| regs.gp[2..4].copy_from_slice(&ret));
		}

		let mut jit = adme::Jit::new(0, syscall);
		mem.mem
			.iter()
			.take_while(|c| **c != 0)
			.for_each(|c| jit.push(*c));
		let exec = jit.finish();
		{
			exec.dump(&mut File::create("/tmp/adme_jit.out").unwrap())
				.unwrap();
		}
		let mut regs = adme::Registers::new();
		unsafe {
			exec.run(&mut regs, &mut mem.mem);
		}
	}

	fn syscall(regs: &mut adme::Cpu, mem: &mut Mem) {
		let v0 = regs.gp()[2];
		let a = regs.gp()[4..8].try_into().unwrap();
		let ret = real_syscall(v0, a, Mem::as_u8(&mut mem.mem));
		regs.gp()[2..4].copy_from_slice(&ret);
	}

	loop {
		cpu.step(&mut mem, syscall).unwrap();
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
