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

		pub(super) fn mov_r64_r64(&mut self, dst: Register, src: Register) {
			assert!(!dst.extended(), "todo: extended registers");
			self.push_u8(0x48); // REX.W
			self.push_u8(0x89); // MOV
			self.push_u8(3 << 6 | src.num3() << 3 | dst.num3()); // MOD = 3
		}

		pub(super) fn mov_r32_imm32(&mut self, dst: Register, num: u32) {
			assert!(!dst.extended(), "todo: extended registers");
			self.push_u8(0xb8 | dst.num3());
			num.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
		}

		pub(super) fn mov_r64_immu(&mut self, dst: Register, num: usize) {
			assert!(!dst.extended(), "todo: extended registers");
			if let Ok(num) = u32::try_from(num) {
				self.push_u8(0xb8 | dst.num3());
				num.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
			} else {
				self.push_u8(0x48); // REX.W
				self.push_u8(0xb8 | dst.num3());
				num.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
			}
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

		pub(super) fn call_r64(&mut self, to: Register) {
			to.extended().then(|| self.push_u8(0x41)); // ???
			self.push_u8(0xff); // ???
			self.push_u8(0xd0 | to.num3()); // CALL | fn
		}

		pub(super) fn ret(&mut self) {
			self.push_u8(0xc3);
		}

		pub(super) fn ud2(&mut self) {
			self.push_u8(0x0f);
			self.push_u8(0x0b);
		}

		pub(super) fn push_r64(&mut self, reg: Register) {
			assert!(!reg.extended(), "todo");
			self.push_u8(0x50 | reg.num3());
		}

		pub(super) fn pop_r64(&mut self, reg: Register) {
			assert!(!reg.extended(), "todo");
			self.push_u8(0x58 | reg.num3());
		}
	}
}

use super::{IrOp, BlockLink};
use memmap::{Mmap, MmapMut, MmapOptions};

#[derive(Clone, Copy, Debug)]
enum BlockJump {
	Equal,
	NotEqual,
	LessOrEqual,
	Greater,
}

struct Block {
	code: Vec<u8>,
	jump_condition: Option<BlockJump>,
}

impl Block {
	fn new() -> Self {
		Self { code: Vec::new(), jump_condition: None }
	}

	fn push_u8(&mut self, value: u8) {
		self.code.push(value);
	}
}

pub struct Jit {
	blocks: Vec<Block>,
	splits: Vec<BlockJump>,
}

impl Jit {
	pub fn new() -> Self {
		Self {
			blocks: Vec::new(),
			splits: Vec::new(),
		}
	}

	/// Translate a single block
	pub(super) fn compile(&mut self, ir: &[IrOp], syscall_handler: extern "C" fn(&mut crate::Registers)) {
		dbg!(ir);
		let mut blk = Block::new();
		for op in ir {
			match *op {
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
				IrOp::J { .. } => (), // Nothing to do until link time
				IrOp::Beq { a, b, .. } | IrOp::Bne { a, b, .. } => {
					// Insert comparison instruction, wait with jump op until link time
					blk.mov_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(a) * 4);
					blk.cmp_r32_m64_offset(op::Register::BX, op::Register::SI, isize::from(b) * 4);
					blk.jump_condition = Some(match *op {
						IrOp::Beq { .. } => BlockJump::Equal,
						IrOp::Bne { .. } => BlockJump::NotEqual,
						_ => unreachable!(),
					});
				}
				IrOp::Jr { register } => {
					// Return to caller to let it handle an arbitrary jump
					blk.mov_r32_m64_offset(op::Register::AX, op::Register::SI, isize::from(register) * 4);
					blk.ret();
				}
				IrOp::Syscall => {
					blk.push_r64(op::Register::SI);
					blk.push_r64(op::Register::DI);
					blk.push_r64(op::Register::DX);
					blk.mov_r64_r64(op::Register::DI, op::Register::SI);
					blk.mov_r64_immu(op::Register::BX, syscall_handler as usize);
					blk.call_r64(op::Register::BX);
					blk.pop_r64(op::Register::DX);
					blk.pop_r64(op::Register::DI);
					blk.pop_r64(op::Register::SI);
				}
				IrOp::InvalidOp => {
					blk.ud2();
				}
				_ => todo!("{:?}", op),
			}
		}
		self.blocks.push(blk);
	}

	/// Generate an executable from the blocks.
	pub(super) fn link(mut self, block_links: &[Option<BlockLink>]) -> Executable {
		let len = self.blocks.iter().map(|b| b.code.len()).sum::<usize>() + 1;
		// TODO don't do just *2 dumbass
		let mut mmap = MmapOptions::new().len(len * 2).map_anon().unwrap();

		let mut pos = 0;
		let mut extend = |sl: &[_], pos: &mut _| {
			mmap[*pos..*pos + sl.len()].copy_from_slice(sl);
			*pos += sl.len();
		};

		extend(&[0xc3], &mut pos);

		dbg!(block_links);
		dbg!(self.blocks.iter().for_each(|b| { dbg!(b.jump_condition); }));

		let mut block_locations = Vec::new();
		let mut fill_jumps = Vec::new();

		for (i, (block, links)) in self.blocks.iter().zip(block_links.iter()).enumerate() {
			block_locations.push(pos);
			// Copy code
			extend(&block.code, &mut pos);
			match *links {
				Some(BlockLink::Next) => assert!(block.jump_condition.is_none()), // Nothing to do
				Some(BlockLink::Jump(loc)) => {
					assert!(block.jump_condition.is_none());
					// Insert a jump
					// TODO try to use short jumps. We can determine the maximum distance we need
					// to jump by summing [block length + 6 bytes for a 32 bit relative jump].
					extend(&[0xe9, 0, 0, 0, 0], &mut pos);
					fill_jumps.push((pos - 4, loc));
				},
				Some(BlockLink::Branch(loc)) => {
					assert!(block.jump_condition.is_some());
					// Insert a conditional jump
					let op = match block.jump_condition.unwrap() {
						BlockJump::Equal => 0x84,
						BlockJump::NotEqual => 0x85,
						BlockJump::LessOrEqual => 0x8e,
						BlockJump::Greater => 0x8f,
					};
					// TODO ditto
					extend(&[0x0f, op, 0, 0, 0, 0], &mut pos);
					fill_jumps.push((pos - 4, loc));
				},
				None => extend(&[0x0f, 0x0b], &mut pos),
			}
		}

		// Fill out jumps
		dbg!(&block_locations, &fill_jumps);
		for (pos, loc) in fill_jumps {
			let loc = block_locations[loc] as isize;
			let rel_loc = i32::try_from(loc - pos as isize - 4).unwrap();
			dbg!((loc, pos, rel_loc));
			mmap[pos..pos + 4].copy_from_slice(&rel_loc.to_le_bytes());
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
