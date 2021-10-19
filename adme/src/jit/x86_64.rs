//! # JIT compiler for MIPS32 -> x86_64
//!
//! The current implementation is very naive: it simply takes a MIPS
//! instruction and outputs corresponding x86_64 assembly. It does
//! not handle self-modifying code either.

/// List of opcodes and functions to generate them.
mod op {
	#[derive(Clone, Copy, Debug)]
	pub enum Register {
		AX = 0b0_000,
		BX = 0b0_011,
		CX = 0b0_001,
		DX = 0b0_010,
		DI = 0b0_111,
		SI = 0b0_110,
		BP = 0b0_101,
		SP = 0b0_100,
		R8 = 0b1_000,
		R9 = 0b1_001,
		R10 = 0b1_010,
		R11 = 0b1_011,
		R12 = 0b1_100,
		R13 = 0b1_101,
		R14 = 0b1_110,
		R15 = 0b1_111,
	}

	impl Register {
		fn extended(self) -> bool {
			(self as u8) & 0b1_000 > 0
		}

		/// The 3-bit opcode for this register
		fn num3(self) -> u8 {
			(self as u8) & 0b0_111
		}
	}

	impl super::Block {
		fn op32_r2(&mut self, op: u8, a: Register, b: Register) {
			self.push_u8(op);
			self.push_u8(3 << 6 | b.num3() << 3 | a.num3()); // MOD = 3 | from | to
		}

		fn op32_m_r(&mut self, op: u8, a: Register, b: Register) {
			self.push_u8(op);
			self.push_u8(b.num3() << 3 | a.num3()); // MOD = 0 | from | to
		}

		fn op32_m_o8_r(&mut self, op: u8, a: Register, offset: i8, b: Register) {
			self.push_u8(op);
			self.push_u8(1 << 6 | b.num3() << 3 | a.num3()); // MOD = 1 | from | to
			self.push_u8(offset as u8);
		}

		pub(super) fn add32_r2(&mut self, a: Register, b: Register) {
			self.op32_r2(0x01, a, b);
		}

		pub(super) fn add_r64_r64(&mut self, a: Register, b: Register) {
			self.push_u8(0x48); // REX.W
			self.op32_r2(0x01, a, b);
		}

		pub(super) fn add_m64_offset_r32(&mut self, a: Register, offset: isize, b: Register) {
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			self.op32_m_o8_r(0x01, a, offset, b);
		}

		pub(super) fn sub32_r2(&mut self, a: Register, b: Register) {
			self.op32_r2(0x29, a, b);
		}

		pub(super) fn or_m64_offset_imm(&mut self, a: Register, offset: isize, imm: usize) {
			assert!(offset < 128, "todo: dword offset");
			if imm <= usize::from(u16::MAX) {
				self.push_u8(0x66); //  Prefix
			} else if imm > usize::try_from(u32::MAX).unwrap() {
				todo!("64 bit immediates");
			}
			self.push_u8(0x81); // OR
			self.push_u8(0x48 | a.num3()); // MOD = 1 | dst
			self.push_u8(offset as i8 as u8);
			if imm <= usize::from(u16::MAX) {
				(imm as u16).to_le_bytes().iter().for_each(|b| self.push_u8(*b));
			} else if imm <= usize::try_from(u32::MAX).unwrap() {
				(imm as u32).to_le_bytes().iter().for_each(|b| self.push_u8(*b));
			} else {
				todo!();
			}
		}

		pub(super) fn cmp32_r2(&mut self, a: Register, b: Register) {
			self.op32_r2(0x39, a, b);
		}

		pub(super) fn cmp_r32_m64_offset(&mut self, a: Register, b: Register, offset: isize) {
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			// src/dst are swapped for mov r, [m] (bit 2 in opcode set)
			self.op32_m_o8_r(0x3b, b, offset, a);
		}

		pub(super) fn mov_r32_imm32(&mut self, dst: Register, num: u32) {
			assert!(!dst.extended(), "todo: extended registers");
			self.push_u8(0xb8 | dst.num3());
			num.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
		}

		pub(super) fn mov_m64_r32(&mut self, dst: Register, src: Register) {
			assert!(!dst.extended(), "todo: extended registers");
			self.op32_m_r(0x89, dst, src)
		}

		pub(super) fn mov_m64_offset_imm(&mut self, dst: Register, offset: isize, imm: usize) {
			assert!(!dst.extended(), "todo: extended registers");
			assert!(offset < 128, "todo: dword offset");
			if imm <= usize::from(u16::MAX) {
				self.push_u8(0x66); //  Prefix
			} else if imm > usize::try_from(u32::MAX).unwrap() {
				todo!("64 bit immediates");
			}
			self.push_u8(0xc7); // MOV
			self.push_u8(0x40 | dst.num3()); // MOD = 1 | dst
			self.push_u8(offset as i8 as u8);
			if imm <= usize::from(u16::MAX) {
				(imm as u16).to_le_bytes().iter().for_each(|b| self.push_u8(*b));
			} else if imm <= usize::try_from(u32::MAX).unwrap() {
				(imm as u32).to_le_bytes().iter().for_each(|b| self.push_u8(*b));
			} else {
				todo!();
			}
		}

		pub(super) fn mov_r32_m64_offset(&mut self, a: Register, b: Register, offset: isize) {
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			// src/dst are swapped for mov r, [m] (bit 2 in opcode set)
			self.op32_m_o8_r(0x8b, b, offset, a);
		}

		pub(super) fn jnz(&mut self, offset: isize) {
			if let Ok(offset) = i8::try_from(offset - 2) {
				self.push_u8(0x75);
				self.push_u8(offset as i8 as u8);
			} else {
				todo!("16/32/64 bit jumps");
			}
		}

		pub(super) fn jmp_r64(&mut self, to: Register) {
			to.extended().then(|| self.push_u8(0x41)); // ???
			self.push_u8(0xff); // JMP
			self.push_u8(0xe0 | to.num3()); // ??? | to
		}

		pub(super) fn ret(&mut self) {
			self.push_u8(0xc3);
		}
	}
}

use super::IrOp;
use memmap::{Mmap, MmapMut, MmapOptions};

struct Block {
	code: Vec<u8>,
}

impl Block {
	fn new() -> Self {
		Self { code: Vec::new() }
	}

	fn push_u8(&mut self, value: u8) {
		self.code.push(value);
	}
}

pub struct Jit {
	blocks: Vec<Block>,
	address_map: Vec<(u32, u32)>,
	ip: usize,
}

impl Jit {
	pub fn new() -> Self {
		let mut blk = Block::new();
		blk.ret();
		Self {
			blocks: Vec::from([blk]),
			address_map: Vec::new(),
			ip: 0,
		}
	}

	/// Translate a single instruction.
	pub(super) fn push(&mut self, op: IrOp) {
		let blk = self.blocks.last_mut().unwrap();
		let blk_len = blk.code.len();
		self.address_map.push((0, blk_len.try_into().unwrap()));
		match op {
			IrOp::Add { dst, a, b } => {
				if dst == a || dst == b {
					blk.mov_r32_m64_offset(
						op::Register::BX,
						op::Register::SI,
						isize::from(a) * 4,
					);
					blk.add_m64_offset_r32(
						op::Register::SI,
						isize::from(dst) * 4,
						op::Register::BX,
					);
				} else {
					todo!("3 operand add");
				}
			}
			IrOp::Addi { dst, a, imm } => {
				assert_eq!(a, 0, "todo: non-move addi");
				blk.mov_m64_offset_imm(
					op::Register::SI,
					isize::from(dst) * 4,
					imm as usize,
				)
			}
			IrOp::Ori { dst, a, imm } => {
				if a == 0 {
					blk.mov_m64_offset_imm(
						op::Register::SI,
						isize::from(dst) * 4,
						imm as usize,
					)
				} else if dst == a {
					blk.or_m64_offset_imm(
						op::Register::SI,
						isize::from(dst) * 4,
						imm as usize,
					)
				} else {
					todo!("todo: non-move ori");
				}
			}
			IrOp::JumpIfNotEqual { a, b, location } => {
				blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(a) * 4);
				blk.cmp_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(b) * 4);
				assert_eq!(location & 0x3, 0, "bad alignment");
				let (loc_blk, loc_ip) = self.address_map[location / 4];
				assert_eq!(loc_blk, 0, "todo");
				let offset = usize::try_from(loc_ip).unwrap().wrapping_sub(blk.code.len()) as isize;
				blk.jnz(offset);
			}
			IrOp::JumpTo { register } => {
				blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(register) * 4);
				blk.add_r64_r64(op::Register::BX, op::Register::DI);
				blk.jmp_r64(op::Register::BX);
			}
			_ => todo!("{:?}", op),
		}
		self.ip += blk.code.len() - blk_len;
	}

	/// Generate a single block from the given instructions.
	pub(super) fn finish(mut self) -> Executable {
		let len = self.blocks.iter().map(|b| b.code.len()).sum();
		let mut mmap = MmapOptions::new().len(len).map_anon().unwrap();
		let mut pos = 0;
		for b in self.blocks {
			mmap[pos..pos + b.code.len()].copy_from_slice(&b.code[..]);
		}
		Executable {
			mmap: mmap.make_exec().unwrap(),
		}
	}
}

pub struct Executable {
	mmap: Mmap,
}

impl Executable {
	pub unsafe fn run(&self, registers: &mut [u32; 32]) {
		registers[0] = 0; // Set to ret at start
		asm!(
			"
				push	rbx
				push	rbp

				mov		rbx, rdi
				inc		rbx			# Skip ret at start
				call	rbx

				pop		rbp
				pop		rbx
			",
			in("rdi") self.mmap.as_ptr(),
			in("rsi") registers,
			lateout("rax") _,
			lateout("rcx") _,
			lateout("rdx") _,
			lateout("rdi") _,
			lateout("rsi") _,
			lateout("r8") _,
			lateout("r9") _,
			lateout("r10") _,
			lateout("r11") _,
			lateout("r12") _,
			lateout("r13") _,
			lateout("r14") _,
			lateout("r15") _,
		);
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn jmp_r64() {
		let mut b = Block::new();
		b.jmp_r64(op::Register::BX);
		assert_eq!(&b.code[..], &[
			0xff, 0xe3,
		]);
	}

	#[test]
	fn add32_r2() {
		let mut b = Block::new();
		b.add32_r2(op::Register::AX, op::Register::BX);
		b.add32_r2(op::Register::AX, op::Register::SI);
		assert_eq!(&b.code[..], &[
			0x01, 0xd8,
			0x01, 0xf0,
		]);
	}

	#[test]
	fn run_mov_eax() {
		let mut b = Block::new();
		b.ret();
		b.mov_r32_imm32(op::Register::AX, 0x1234);
		b.mov_m64_r32(op::Register::SI, op::Register::AX);
		b.ret();
		assert_eq!(&b.code[..], &[
			0xc3,
			0xb8, 0x34, 0x12, 0x00, 0x00,
			0x89, 0x06,
			0xc3,
		]);
		let jit = Jit {
			blocks: Vec::from([b]),
			address_map: Vec::new(),
			ip: 6,
		};
		let exec = jit.finish();
		let mut regs = [0; 32];
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(regs[0], 0x1234);
		assert_eq!(&regs[1..], &[0; 31]);
	}
}
