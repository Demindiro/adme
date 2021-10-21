//! # JIT compiler for MIPS32 -> x86_64
//!
//! The current implementation is very naive: it simply takes a MIPS
//! instruction and outputs corresponding x86_64 assembly. It does
//! not handle self-modifying code either.

#[macro_use]
mod op;

use super::{BlockLink, IrOp};
use memmap::{Mmap, MmapMut, MmapOptions};
use op::*;

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
		Self { blocks: Vec::new(), splits: Vec::new() }
	}

	/// Translate a single block
	pub(super) fn compile(
		&mut self,
		ir: &[IrOp],
		syscall_handler: extern "C" fn(&mut crate::Registers, &mut [u8; 0x4000]),
	) {
		dbg!(ir);
		let mut blk = Block::new();
		for op in ir {
			match *op {
				IrOp::Add { dst, a, b } => {
					if dst == a || dst == b {
						let disp = regi((dst == a).then(|| b).unwrap_or(a));
						blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, disp)));
						blk.add(Size::DW, ModRegMR::r32r((Reg::DI, regi(dst)), Reg::DX));
					} else {
						blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(a))));
						blk.add(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(b))));
						blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(dst)), Reg::DX));
					}
				}
				IrOp::Or { dst, a, b } => {
					if dst == a || dst == b {
						let disp = regi((dst == a).then(|| b).unwrap_or(a));
						blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, disp)));
						blk.or(Size::DW, ModRegMR::r32r((Reg::DI, regi(dst)), Reg::DX));
					} else {
						blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(a))));
						blk.or(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(b))));
						blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(dst)), Reg::DX));
					}
				}
				IrOp::Addi { dst, a, imm: im } => {
					let imm = Immediate::try_from(im).unwrap().optimize();
					if a == 0 {
						blk.mov_m64_offset_imm32(Reg::DI, isize::from(dst) * 4, im as usize)
					} else if dst == a {
						blk.addi(Size::DW, ModRegMI::r32((Reg::DI, regi(dst))), imm);
					} else {
						blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(a))));
						blk.addi(Size::DW, ModRegMI::r(Reg::DX), imm);
						blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(dst)), Reg::DX));
					}
				}
				IrOp::Ori { dst, a, imm: im } => {
					let imm = Immediate::try_from(im).unwrap().optimize();
					if a == 0 {
						blk.mov_m64_offset_imm32(Reg::DI, isize::from(dst) * 4, im as usize)
					} else if dst == a {
						blk.ori(Size::DW, ModRegMI::r32((Reg::DI, regi(dst))), imm);
					} else {
						todo!("todo: non-move ori");
					}
				}
				IrOp::Divu { quot, rem, a, b } => {
					blk.mov(Size::DW, ModRegMR::rr32(Reg::AX, (Reg::DI, regi(a))));
					blk.xor(Size::DW, ModRegMR::rr(Reg::DX, Reg::DX));
					blk.div_m64_offset(Reg::DI, isize::from(b) * 4);
					blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(quot)), Reg::AX));
					blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(rem)), Reg::DX));
				}
				IrOp::Lu8 { reg, mem, offset } => {
					blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(mem))));
					let src = (Reg::BX, Reg::DX, Scale::_1, offset.try_into().unwrap());
					blk.movzx(Size::DW, ModRegMR::ri32(Reg::DX, src));
					blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(reg)), Reg::DX));
				}
				IrOp::S8 { reg, mem, offset } => {
					blk.push_u8(0x90); // NOP
					 //todo!();
				}
				IrOp::J { .. } => (), // Nothing to do until link time
				IrOp::Beq { a, b, .. } | IrOp::Bne { a, b, .. } => {
					// Insert comparison instruction, wait with jump op until link time
					blk.mov(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(a))));
					blk.cmp(Size::DW, ModRegMR::rr32(Reg::DX, (Reg::DI, regi(b))));
					blk.jump_condition = Some(match *op {
						IrOp::Beq { .. } => BlockJump::Equal,
						IrOp::Bne { .. } => BlockJump::NotEqual,
						_ => unreachable!(),
					});
				}
				IrOp::Jr { register } => {
					// Return to caller to let it handle an arbitrary jump
					blk.mov(Size::DW, ModRegMR::rr32(Reg::AX, (Reg::DI, regi(register))));
					blk.ret();
				}
				IrOp::Syscall => {
					blk.push_r64(Reg::DI);
					blk.mov_r64_r64(Reg::SI, op::Reg::BX);
					blk.mov_r64_immu(Reg::DX, syscall_handler as usize);
					blk.call_r64(Reg::DX);
					blk.pop_r64(Reg::DI);
				}
				IrOp::InvalidOp => {
					blk.ud2();
				}
				IrOp::Sli { .. } => {
					blk.ud2();
					dbg!("todo: sli");
				}
				IrOp::Slts { dst, a, b } => {
					blk.mov(Size::DW, ModRegMR::rr32(Reg::AX, (Reg::DI, regi(a))));
					blk.xor(Size::DW, ModRegMR::rr(Reg::DX, Reg::DX));
					blk.cmp(Size::DW, ModRegMR::rr32(Reg::AX, (Reg::DI, regi(b))));
					blk.setl(Reg::DX, false);
					blk.mov(Size::DW, ModRegMR::r32r((Reg::DI, regi(dst)), Reg::DX));
				}
				_ => todo!("{:?}", op),
			}
			blk.push_u8(0x90);
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
		dbg!(self.blocks.iter().for_each(|b| {
			dbg!(b.jump_condition);
		}));

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
				}
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
				}
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

		Executable { mmap: mmap.make_exec().unwrap() }
	}
}

fn regi(gp: u8) -> i32 {
	i32::from(gp) * 4
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
				push	rbx		# FIXME it seems Rust doesn't properly align the stack?

				mov		rbx, rax
				inc		rsi			# Skip ret at start
			break_on_me:
				call	rsi

				pop		rbx
				pop		rbx
			",
			in("rsi") self.mmap.as_ptr(),
			in("rdi") registers,
			in("rax") memory,
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

	pub fn dump(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
		out.write_all(&self.mmap)?;
		out.flush()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn jmp_r64() {
		let mut b = Block::new();
		b.jmp_r64(op::Reg::BX);
		assert_eq!(&b.code[..], &[0xff, 0xe3,]);
	}

	#[test]
	fn add32_r2() {
		let mut b = Block::new();
		b.add32_r2(op::Reg::AX, op::Reg::BX);
		b.add32_r2(op::Reg::AX, op::Reg::SI);
		assert_eq!(&b.code[..], &[0x01, 0xd8, 0x01, 0xf0,]);
	}

	#[test]
	fn run_mov_eax() {
		let mut b = Block::new();
		b.ret();
		b.mov_r32_imm32(op::Reg::AX, 0x1234);
		b.mov_m64_r32(op::Reg::SI, op::Reg::AX);
		b.ret();
		assert_eq!(
			&b.code[..],
			&[0xc3, 0xb8, 0x34, 0x12, 0x00, 0x00, 0x89, 0x06, 0xc3,]
		);
		let jit = Jit { blocks: Vec::from([b]), address_map: Vec::new(), ip: 6 };
		let exec = jit.finish();
		let mut regs = crate::Registers::new();
		unsafe {
			exec.run(&mut regs);
		}
		assert_eq!(regs.gp[0], 0x1234);
		assert_eq!(&regs.gp[1..], &[0; 31]);
	}
}
