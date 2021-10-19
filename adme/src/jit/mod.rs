//! # JIT compilers

mod x86_64;

use crate::interpreter::{Function, Op, I, R};

/// Pseudo instructions to ease translation
#[derive(Clone, Copy, Debug)]
enum IrOp {
	Add {
		dst: u8,
		a: u8,
		b: u8,
	},
	Addi {
		dst: u8,
		a: u8,
		imm: isize,
	},
	Ori {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Jump {
		location: usize,
	},
	JumpTo {
		register: u8,
	},
	JumpIfLess {
		a: u8,
		b: u8,
		location: usize,
	},
	JumpIfEqual {
		a: u8,
		b: u8,
		location: usize,
	},
	JumpIfNotEqual {
		a: u8,
		b: u8,
		location: usize,
	},
}

pub struct Jit {
	inner: x86_64::Jit,
	pc: u32,
}

impl Jit {
	pub fn new(pc: u32) -> Self {
		Self { inner: x86_64::Jit::new(), pc }
	}

	pub fn push(&mut self, instr: u32) {
		let ops = match Op::try_from(instr).unwrap() {
			Op::Function => {
				let r = R::decode(instr);
				match Function::try_from(instr).unwrap() {
					Function::Add => [IrOp::Add { dst: r.d.try_into().unwrap(), a: r.s.try_into().unwrap(), b: r.t.try_into().unwrap() }],
					Function::Jr => [IrOp::JumpTo { register: r.s.try_into().unwrap() }],
					f => todo!("{:?}", f),
				}
			}
			// TODO: check for overflow
			Op::Addi => {
				let i = I::decode(instr);
				[IrOp::Addi { dst: i.t.try_into().unwrap(), a: i.s.try_into().unwrap(), imm: (i.imm as i16 as u32 as isize) }]
			}
			Op::Ori => {
				let i = I::decode(instr);
				[IrOp::Ori { dst: i.t.try_into().unwrap(), a: i.s.try_into().unwrap(), imm: i.imm.into() }]
			}
			Op::Beq => {
				let i = I::decode(instr);
				let location = (self.pc.wrapping_add(i.imm as i16 as u32 + 1) << 2).try_into().unwrap();
				[IrOp::JumpIfEqual { a: i.s.try_into().unwrap(), b: i.t.try_into().unwrap(), location }]
			}
			Op::Bne => {
				let i = I::decode(instr);
				let location = (self.pc.wrapping_add(i.imm as i16 as u32 + 1) << 2).try_into().unwrap();
				[IrOp::JumpIfNotEqual { a: i.s.try_into().unwrap(), b: i.t.try_into().unwrap(), location }]
			}
			Op::Lui => {
				let i = I::decode(instr);
				[IrOp::Addi { dst: i.t.try_into().unwrap(), a: 0, imm: isize::try_from(i.imm).unwrap() << 16 }]
			}
			op => todo!("{:?}", op),
		};
		dbg!(ops[0]);
		self.inner.push(ops[0]);
		self.pc += 1;
	}

	fn finish(self) -> x86_64::Executable {
		self.inner.finish()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn ret() {
		let asm = "
			jr		$ra
		";
		let mut code = [0; 1];
		crate::Assembler::assemble(asm, &mut code).unwrap();
		let mut jit = Jit::new(0);
		code.iter().for_each(|c| jit.push(*c));
		let exec = jit.finish();
		let mut regs = [0; 32];
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(&regs[..], &[0; 32]);
	}

	#[test]
	fn loop_1000() {
		let asm = "
			li		$2, 1000
			li		$3, -1
		loop:
			add		$2, $2, $3
			bne		$2, $0, loop
			jr		$ra
		";
		let mut code = [0; 6];
		crate::Assembler::assemble(asm, &mut code).unwrap();
		let mut jit = Jit::new(0);
		code.iter().for_each(|c| jit.push(*c));
		let exec = jit.finish();
		let mut regs = [0; 32];
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(&regs[0..2], [0; 2]);
		assert_eq!(regs[2], 0);
		assert_eq!(regs[3], u32::MAX);
		assert_eq!(&regs[4..], &[0; 28]);
	}
}
