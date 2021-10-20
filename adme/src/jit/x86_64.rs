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

		fn op32_r_m_o8(&mut self, op: u8, a: Register, b: Register, offset: i8) {
			self.push_u8(op | 0b10); // D = 1
			self.push_u8(1 << 6 | a.num3() << 3 | b.num3()); // MOD = 1 | to | from
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

		pub(super) fn add_r32_m64_offset(&mut self, a: Register, b: Register, offset: isize) {
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			self.op32_r_m_o8(0x01, a, b, offset);
		}

		pub(super) fn add_m64_32_offset_imm(&mut self, to: Register, offset: isize, imm: isize) {
			assert!(!to.extended(), "todo");
			let imm = i8::try_from(imm).expect("todo: 16/32/64 bit immediates");
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			self.push_u8(0x83); // ADD
			self.push_u8(1 << 6 | to.num3()); // MOD = 1 | to
			self.push_u8(offset as u8);
			self.push_u8(imm as u8);
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
			self.op32_r_m_o8(0x89, a, b, offset);
		}

		pub(super) fn mov_m64_offset_r32(&mut self, a: Register, offset: isize, b: Register) {
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			self.op32_m_o8_r(0x89, a, offset, b);
		}

		pub(super) fn movzx_r8_m64_sib_offset(&mut self, dst: Register, base: Register, index: Register, scale: u8, offset: isize) {
			assert!(scale < 4, "todo");
			assert!(!dst.extended(), "todo");
			assert!(!base.extended(), "todo");
			assert!(!index.extended(), "todo");
			let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
			self.push_u8(0x0f); // Expansion prefix
			self.push_u8(0xb6); // MOVZX
			self.push_u8(1 << 6 | dst.num3() << 3 | 0b100); // MOD = off8 | dst | mode = SIB
			self.push_u8(scale << 6 | index.num3() << 3 | base.num3()); // scale | index | base
			self.push_u8(offset as u8);
		}

		pub(super) fn jz(&mut self, offset: isize) {
			if let Ok(offset) = i8::try_from(offset - 2) {
				self.push_u8(0x74);
				self.push_u8(offset as i8 as u8);
			} else {
				todo!("16/32/64 bit jumps");
			}
		}

		pub(super) fn jnz(&mut self, offset: isize) {
			if let Ok(offset) = i8::try_from(offset - 2) {
				self.push_u8(0x75);
				self.push_u8(offset as i8 as u8);
			} else {
				todo!("16/32/64 bit jumps");
			}
		}

		pub(super) fn jmp(&mut self, offset: isize) {
			if let Ok(offset) = i8::try_from(offset - 2) {
				self.push_u8(0xeb); // JMP
				self.push_u8(offset as u8); // offset
			} else {
				todo!("16/32 bit offsets")
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

		pub(super) fn ud2(&mut self) {
			self.push_u8(0x0f);
			self.push_u8(0x0b);
		}
	}
}

use super::IrOp;
use memmap::{Mmap, MmapMut, MmapOptions};

enum BlockJumpCondition {
	Always,
	Equal,
	NotEqual,
	LessOrEqual,
	Greater,
}

struct BlockJ {
	location: usize,
	condition: BlockJumpCondition,
}

struct Block {
	code: Vec<u8>,
	jump: Option<BlockJ>,
}

impl Block {
	fn new() -> Self {
		Self { code: Vec::new(), jump: None }
	}

	fn push_u8(&mut self, value: u8) {
		self.code.push(value);
	}

	fn len(&self) -> usize {
		self.code.len() + self.jump.is_some().then(|| 2 + 4).unwrap_or(0)
	}
}

pub struct Jit {
	blocks: Vec<Block>,
	address_map: Vec<(usize, usize, usize)>,
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
		let blk_i = self.blocks.len() - 1;
		let blk = self.blocks.last().unwrap();
		let blk_len = blk.code.len();
		self.address_map.push((blk_i, blk_len, self.ip));
		let mut blk = self.blocks.last_mut().unwrap();
		match op {
			IrOp::Add { dst, a, b } => {
				if dst == a || dst == b {
					blk.mov_r32_m64_offset(
						op::Register::BX,
						op::Register::SI,
						isize::from((dst == a).then(|| a).unwrap_or(b)) * 4,
					);
					blk.add_m64_offset_r32(
						op::Register::SI,
						isize::from(dst) * 4,
						op::Register::BX,
					);
				} else {
					blk.mov_r32_m64_offset(
						op::Register::BX,
						op::Register::SI,
						isize::from(a) * 4,
					);
					blk.add_r32_m64_offset(
						op::Register::BX,
						op::Register::SI,
						isize::from(b) * 4,
					);
					blk.mov_m64_offset_r32(
						op::Register::SI,
						isize::from(b) * 4,
						op::Register::BX,
					);
				}
			}
			IrOp::Addi { dst, a, imm } => {
				if a == 0 {
					blk.mov_m64_offset_imm(
						op::Register::SI,
						isize::from(dst) * 4,
						imm as usize,
					)
				} else if dst == a {
					blk.add_m64_32_offset_imm(
						op::Register::SI,
						isize::from(dst) * 4,
						imm,
					);
				} else {
					todo!("3 operand addi");
				}
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
			IrOp::Lu8 { reg, mem, offset } => {
				blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(mem) * 4);
				blk.movzx_r8_m64_sib_offset(op::Register::BX, op::Register::DX, op::Register::BX, 0, offset);
				blk.mov_m64_offset_r32(op::Register::SI, isize::from(reg) * 4, op::Register::BX);
			}
			IrOp::S8 { reg, mem, offset } => {
				blk.push_u8(0x90); // NOP
				//todo!();
			}
			IrOp::J { location } => {
				dbg!(&self.address_map, location / 4);
				if let Some(&(loc_blk, loc_ip, _)) = self.address_map.get(location / 4) {
					dbg!(loc_ip);
					let offset = if loc_blk == 0 { // FIXME fucking terrible hack
						let mut len = blk.len();
						drop(blk);
						len += self.blocks[0].len();
						let o = usize::try_from(loc_ip).unwrap().wrapping_sub(dbg!(len)) as isize;
						blk = &mut self.blocks[blk_i];
						o
					} else {
						assert_eq!(loc_blk, blk_i, "todo");
						usize::try_from(loc_ip).unwrap().wrapping_sub(blk.code.len()) as isize
					};
					dbg!(offset);
					blk.jmp(offset);
				} else {
					todo!();
					blk.jump = Some(BlockJ {
						location,
						condition: BlockJumpCondition::Always,
					});
					drop(blk);
					self.blocks.push(Block::new());
					blk = &mut self.blocks[blk_i];
				}
			}
			IrOp::Beq { a, b, location } => {
				assert_eq!(location & 0x3, 0, "bad alignment");
				blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(a) * 4);
				blk.cmp_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(b) * 4);
				if let Some(&(loc_blk, loc_ip, _)) = self.address_map.get(location / 4) {
					assert_eq!(loc_blk, blk_i, "todo");
					let offset = usize::try_from(loc_ip).unwrap().wrapping_sub(blk.len()) as isize;
					blk.jnz(offset);
				} else {
					blk.jump = Some(BlockJ {
						location,
						condition: BlockJumpCondition::Equal,
					});
					drop(blk);
					self.blocks.push(Block::new());
					let i = self.blocks.len() - 2;
					blk = &mut self.blocks[i];
				}
			}
			IrOp::Bne { a, b, location } => {
				blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(a) * 4);
				blk.cmp_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(b) * 4);
				assert_eq!(location & 0x3, 0, "bad alignment");
				let (loc_blk, loc_ip, _) = self.address_map[location / 4];
				assert_eq!(loc_blk, 0, "todo");
				let offset = usize::try_from(loc_ip).unwrap().wrapping_sub(blk.code.len()) as isize;
				blk.jnz(offset);
			}
			IrOp::Jr { register } => {
				blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(register) * 4);
				blk.add_r64_r64(op::Register::BX, op::Register::DI);
				blk.jmp_r64(op::Register::BX);
			}
			IrOp::InvalidOp => {
				blk.ud2();
			}
			_ => todo!("{:?}", op),
		}
		self.ip += blk.len() - blk_len;
	}

	/// Generate a single block from the given instructions.
	pub(super) fn finish(mut self) -> Executable {
		let len: usize = self.blocks.iter().map(|b| b.code.len()).sum();
		// TODO don't do just *2 dumbass
		let mut mmap = MmapOptions::new().len(len * 2).map_anon().unwrap();
		let mut pos = 0;

		let mut blk_offts = Vec::new();
		// SAFETY: zeroed usize is valid.
		let mut blk_fill_jmps = unsafe {
			Box::<[(usize, usize)]>::new_zeroed_slice(self.blocks.len()).assume_init()
		};

		for (i, b) in self.blocks.iter().enumerate() {
			let og_pos = pos;

			mmap[pos..pos + b.code.len()].copy_from_slice(&b.code[..]);
			pos += b.code.len();
			if let Some(j) = &b.jump {
				let mut push = |b| {
					mmap[pos] = b;
					pos += 1;
				};
				match j.condition {
					BlockJumpCondition::Equal => {
						// TODO use helper function
						push(0x0f); // Expansion prefix
						push(0x84); // JE/JZ rel32
						dbg!(j.location);
						blk_fill_jmps[i] = (pos, j.location);
						pos += 4; // Reserve space
					}
					_ => todo!(),
				}
			}

			blk_offts.push(pos);
		}

		for (b, fj) in self.blocks.iter().zip(blk_fill_jmps.iter()) {
			let (fill, loc) = *fj;
			if b.jump.is_some() {
				let pos = self.address_map[loc / 4].2;
				dbg!(pos, fill + 4);
				let rel_loc = pos.wrapping_sub(fill + 4 /* acc for instr len */);
				// FIXME idk why I have an off-by-one error but I no longer care
				// This one-pass approach is complete shit anyways.
				let rel_loc = rel_loc + 1;
				dbg!(rel_loc);
				mmap[fill..fill + 4].copy_from_slice(&(rel_loc as u32).to_le_bytes());
			}
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
	pub unsafe fn run(&self, registers: &mut super::Registers, memory: &mut [u32; 4096]) {
		registers.gp[31] = 0; // Set to ret at start
		asm!(
			"
				push	rbx
				push	rbp

				mov		rbx, rdi
				inc		rbx			# Skip ret at start
			break_on_me:
				call	rbx

				pop		rbp
				pop		rbx
			",
			in("rdi") self.mmap.as_ptr(),
			in("rsi") registers,
			in("rdx") memory,
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
		let mut regs = crate::Registers::new();
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(regs.gp[0], 0x1234);
		assert_eq!(&regs.gp[1..], &[0; 31]);
	}
}
