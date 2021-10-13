use super::interpreter::{Op, Function};

fn new_r(op: Op, s: u32, t: u32, d: u32, sa: u32, function: Function) -> u32 {
	(op as u32) << 26 | s << 21 | t << 16 | d << 11 | sa << 6 | function as u32
}

fn new_i(op: Op, s: u32, t: u32, imm: u32) -> u32 {
	(op as u32) << 26 | s << 21 | t << 16 | imm
}

fn decode_3_regs(args: &str) -> [u32; 3] {
	let mut s = [None; 3];
	for (i, a) in args.split(',').enumerate() {
		s[i] = Some(a.trim_start());
	}
	let mut r = [0; 3];
	for (r, s) in r.iter_mut().zip(s) {
		*r = u32::from_str_radix(&s.unwrap()[1..], 10).unwrap();
	}
	r
}

fn decode_2_regs_1_imm(args: &str) -> [u32; 3] {
	let mut s = [None; 3];
	for (i, a) in args.split(',').enumerate() {
		s[i] = Some(a.trim_start());
	}
	let mut r = [0; 3];
	for (r, s) in r[..2].iter_mut().zip(s) {
		*r = u32::from_str_radix(&s.unwrap()[1..], 10).unwrap();
	}
	r[2] = u32::from_str_radix(s[2].unwrap(), 10).unwrap();
	r
}

fn parse_r(op: Op, function: Function, args: &str) -> u32 {
	let [d, t, s] = decode_3_regs(args);
	new_r(op, s, t, d, 0, function)
}

fn parse_i(op: Op, args: &str) -> u32 {
	let [t, s, imm] = decode_2_regs_1_imm(args);
	new_i(op, s, t, imm)
}

pub fn assemble(source: &str, destination: &mut [u32]) {
	let mut ip = 0;

	for line in source.lines() {
		// Remove any comments
		let line = line.split(';').next().unwrap();
		let line = line.split('#').next().unwrap();
		let line = line.split("//").next().unwrap();

		// Get the mnemonic
		// If this fails, the line is empty
		if let Some((mnem, args)) = line.trim().split_once(char::is_whitespace) {
			dbg!((mnem, args));
			destination[ip / 4] = match mnem {
				"add" => parse_r(Op::Function, Function::Add, args),
				"addi" => parse_i(Op::Addi, args),
				op => todo!("{}", op),
			};
			ip = ip.wrapping_add(4);
		}
	}
}
