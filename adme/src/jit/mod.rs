//! # JIT compilers

mod x86_64;

use crate::interpreter::{Function, Op, I, J, R};

/// Pseudo instructions to ease translation
#[derive(Clone, Copy, Debug)]
enum IrOp {
	Add {
		dst: u8,
		a: u8,
		b: u8,
	},
	Sub {
		dst: u8,
		a: u8,
		b: u8,
	},
	And {
		dst: u8,
		a: u8,
		b: u8,
	},
	Or {
		dst: u8,
		a: u8,
		b: u8,
	},
	// 'not' can be modelled with 'nor' by using 0 register
	Nor {
		dst: u8,
		a: u8,
		b: u8,
	},
	Xor {
		dst: u8,
		a: u8,
		b: u8,
	},
	Mulu {
		dst: u8,
		a: u8,
		b: u8,
	},
	Muls {
		dst: u8,
		a: u8,
		b: u8,
	},
	Divu {
		dst: u8,
		a: u8,
		b: u8,
	},
	Divs {
		dst: u8,
		a: u8,
		b: u8,
	},
	Sl {
		dst: u8,
		a: u8,
		b: u8,
	},
	Srl {
		dst: u8,
		a: u8,
		b: u8,
	},
	Sra {
		dst: u8,
		a: u8,
		b: u8,
	},
	Slts {
		dst: u8,
		a: u8,
		b: u8,
	},
	Sltu {
		dst: u8,
		a: u8,
		b: u8,
	},
	Sltis {
		dst: u8,
		a: u8,
		imm: isize,
	},
	Sltiu {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Sli {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Srli {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Srai {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Addi {
		dst: u8,
		a: u8,
		imm: isize,
	},
	Andi {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Ori {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Xori {
		dst: u8,
		a: u8,
		imm: usize,
	},
	Jump {
		location: usize,
	},
	JumpAndLink {
		link: u8,
		location: usize,
	},
	JumpRegister {
		register: u8,
	},
	JumpAndLinkRegister {
		link: u8,
		register: u8,
	},
	JumpIfLessOrEqual {
		a: u8,
		b: u8,
		location: usize,
	},
	JumpIfGreater {
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
	Lu8 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	Li8 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	Lu16 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	Li16 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	Lu32 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	S8 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	S16 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	S32 {
		reg: u8,
		mem: u8,
		offset: isize,
	},
	InvalidOp,
	Nop,
}

pub struct Jit {
	inner: x86_64::Jit,
	pc: usize,
}

impl Jit {
	pub fn new(pc: usize) -> Self {
		Self { inner: x86_64::Jit::new(), pc }
	}

	pub fn push(&mut self, instr: u32) {
		if let Ok(op) = Op::try_from(instr) {
			dbg!(op);
			let r = R::decode(instr);
			let i = I::decode(instr);
			let j = J::decode(instr);
			let ops = match op {
				Op::Function => match dbg!(Function::try_from(instr).unwrap()) {
					// TODO check for overflow
					Function::Add => [IrOp::Add { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Addu => [IrOp::Add { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					// TODO check for overflow
					Function::Sub => [IrOp::Sub { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Subu => [IrOp::Sub { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Mult => [IrOp::Muls { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Multu => [IrOp::Mulu { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Div => [IrOp::Divs { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Divu => [IrOp::Divu { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::And => [IrOp::And { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Or => [IrOp::Or { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Xor => [IrOp::Xor { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Nor => [IrOp::Nor { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Sll => [IrOp::Sli { dst: r.d, a: r.t, imm: r.s.into() }, IrOp::Nop],
					Function::Srl => [IrOp::Srli { dst: r.d, a: r.t, imm: r.s.into() }, IrOp::Nop],
					Function::Sra => [IrOp::Srai { dst: r.d, a: r.t, imm: r.s.into() }, IrOp::Nop],
					Function::Sllv => [IrOp::Sl { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Srlv => [IrOp::Srl { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Srav => [IrOp::Sra { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Jr => [IrOp::JumpRegister { register: r.s }, IrOp::Nop],
					Function::Jalr => [IrOp::JumpAndLinkRegister { link: r.t, register: r.s }, IrOp::Nop],
					Function::Mfhi => [IrOp::Or { dst: r.d, a: 0, b: 32 }, IrOp::Nop],
					Function::Mflo => [IrOp::Or { dst: r.d, a: 0, b: 33 }, IrOp::Nop],
					Function::Mthi => [IrOp::Or { dst: 32, a: 0, b: r.d }, IrOp::Nop],
					Function::Mtlo => [IrOp::Or { dst: 33, a: 0, b: r.d }, IrOp::Nop],
					Function::Slt => [IrOp::Slts { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
					Function::Sltu => [IrOp::Sltu { dst: r.d, a: r.s, b: r.t }, IrOp::Nop],
				}
				// TODO: check for overflow
				Op::Addi => {
					[IrOp::Addi { dst: i.t, a: i.s, imm: (i.imm_i16() as u32 as isize) }, IrOp::Nop]
				}
				Op::Addiu => {
					[IrOp::Addi { dst: i.t, a: i.s, imm: (i.imm_i16() as u32 as isize) }, IrOp::Nop]
				}
				Op::Andi => {
					[IrOp::Andi { dst: i.t, a: i.s, imm: i.imm.into() }, IrOp::Nop]
				}
				Op::Ori => {
					[IrOp::Ori { dst: i.t, a: i.s, imm: i.imm.into() }, IrOp::Nop]
				}
				Op::Xori => {
					[IrOp::Xori { dst: i.t, a: i.s, imm: i.imm.into() }, IrOp::Nop]
				}
				Op::Slti => {
					[IrOp::Sltis { dst: i.t, a: i.s, imm: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Sltiu => {
					[IrOp::Sltiu { dst: i.t, a: i.s, imm: i.imm.into() }, IrOp::Nop]
				}
				Op::Beq => {
					let location = self.pc.wrapping_add(i.imm as i16 as usize + 1) << 2;
					[IrOp::JumpIfEqual { a: i.s, b: i.t, location }, IrOp::Nop]
				}
				Op::Bne => {
					let location = self.pc.wrapping_add(i.imm as i16 as usize + 1) << 2;
					[IrOp::JumpIfNotEqual { a: i.s, b: i.t, location }, IrOp::Nop]
				}
				Op::Lui => [IrOp::Ori { dst: i.t, a: 0, imm: usize::from(i.imm) << 16 }, IrOp::Nop],
				Op::Lhi => [
					IrOp::Andi { dst: i.t, a: i.t, imm: 0x0000_ffff },
					IrOp::Ori { dst: i.t, a: i.t, imm: usize::from(i.imm) << 16 },
				],
				Op::Llo => [
					IrOp::Andi { dst: i.t, a: i.t, imm: 0xffff_0000 },
					IrOp::Ori { dst: i.t, a: i.t, imm: usize::from(i.imm) },
				],
				Op::Lb => {
					[IrOp::Li8 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Lbu => {
					[IrOp::Lu8 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Lh => {
					[IrOp::Li16 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Lhu => {
					[IrOp::Lu16 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Lw => {
					[IrOp::Lu32 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Sb => {
					[IrOp::S8 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Sh => {
					[IrOp::S16 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::Sw => {
					[IrOp::S32 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, IrOp::Nop]
				}
				Op::J => {
					let location = self.pc.wrapping_add(j.imm_i32() as usize) << 2;
					dbg!(self.pc, j.imm_i32(), location);
					[IrOp::Jump { location }, IrOp::Nop]
				}
				Op::Jal => {
					let location = self.pc.wrapping_add(i.imm_i16() as usize + 1) << 2;
					[IrOp::JumpAndLink { link: i.t, location }, IrOp::Nop]
				}
				Op::Blez => {
					let location = self.pc.wrapping_add(i.imm_i16() as usize + 1) << 2;
					[IrOp::JumpIfLessOrEqual { a: i.s, b: 0, location }, IrOp::Nop]
				}
				Op::Bgtz => {
					let location = self.pc.wrapping_add(i.imm_i16() as usize + 1) << 2;
					[IrOp::JumpIfGreater { a: i.s, b: 0, location }, IrOp::Nop]
				}
			};
			dbg!(ops[0]);
			self.inner.push(ops[0]);
		} else {
			dbg!(IrOp::InvalidOp);
			self.inner.push(IrOp::InvalidOp);
		}
		self.pc += 1;
	}

	pub fn finish(self) -> x86_64::Executable {
		self.inner.finish()
	}
}

#[repr(C)]
pub struct Registers {
	gp: [u32; 32],
	hi: u32,
	lo: u32,
	pc: u32,
}

impl Registers {
	const OFFSET_HI: u8 = 32;
	const OFFSET_LO: u8 = 33;
	const OFFSET_PC: u8 = 34;

	pub fn new() -> Self {
		Self {
			gp: [0; 32],
			hi: 0,
			lo: 0,
			pc: 0,
		}
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
		let mut regs = Registers::new();
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(&regs.gp[..], &[0; 32]);
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
		let mut regs = Registers::new();
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(&regs.gp[0..2], [0; 2]);
		assert_eq!(regs.gp[2], 0);
		assert_eq!(regs.gp[3], u32::MAX);
		assert_eq!(&regs.gp[4..], &[0; 28]);
	}
}
