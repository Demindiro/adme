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

	let mut mem = [0; 128];

	adme::assemble(&asm, &mut mem);

	for _ in 0..6 {
		cpu.step(&mut mem).unwrap();
		dbg!(&cpu);
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
