//! # JIT compilers
//!
//! ## Operation
//!
//! - First, IR is generated. Alongside the IR, a source to IR PC mapping
//!   is created.
//! - Then, the IR is split up in basic blocks based on jumps in the IR. Each block is linked to
//!   other. Each block may link to zero, one or two blocks.
//! - Each block is passed to the host compiler, which converts it to host machine code.
//! - Finally, all blocks are combined and an executable region is produced.

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
	J {
		location: usize,
	},
	Jal {
		link: u8,
		location: usize,
	},
	Jr {
		register: u8,
	},
	Jalr {
		link: u8,
		register: u8,
	},
	Ble {
		a: u8,
		b: u8,
		location: usize,
	},
	Bgt {
		a: u8,
		b: u8,
		location: usize,
	},
	Beq {
		a: u8,
		b: u8,
		location: usize,
	},
	Bne {
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
	Syscall,
	InvalidOp,
}

#[derive(Clone, Copy, Debug)]
enum BlockLink {
	Next,
	Jump(usize),
	Branch(usize),
}

pub struct Jit {
	ir: Vec<IrOp>,
	pc: usize,
	address_map: Vec<usize>,
	syscall_handler: extern "C" fn(&mut Registers, &mut [u8; 0x4000]),
}

impl Jit {
	pub fn new(pc: usize, syscall_handler: extern "C" fn(&mut Registers, &mut [u8; 0x4000])) -> Self {
		Self { ir: Vec::new(), pc, address_map: Vec::new(), syscall_handler }
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
					Function::Add => (IrOp::Add { dst: r.d, a: r.s, b: r.t }, None),
					Function::Addu => (IrOp::Add { dst: r.d, a: r.s, b: r.t }, None),
					// TODO check for overflow
					Function::Sub => (IrOp::Sub { dst: r.d, a: r.s, b: r.t }, None),
					Function::Subu => (IrOp::Sub { dst: r.d, a: r.s, b: r.t }, None),
					Function::Mult => (IrOp::Muls { dst: r.d, a: r.s, b: r.t }, None),
					Function::Multu => (IrOp::Mulu { dst: r.d, a: r.s, b: r.t }, None),
					Function::Div => (IrOp::Divs { dst: r.d, a: r.s, b: r.t }, None),
					Function::Divu => (IrOp::Divu { dst: r.d, a: r.s, b: r.t }, None),
					Function::And => (IrOp::And { dst: r.d, a: r.s, b: r.t }, None),
					Function::Or => (IrOp::Or { dst: r.d, a: r.s, b: r.t }, None),
					Function::Xor => (IrOp::Xor { dst: r.d, a: r.s, b: r.t }, None),
					Function::Nor => (IrOp::Nor { dst: r.d, a: r.s, b: r.t }, None),
					Function::Sll => (IrOp::Sli { dst: r.d, a: r.t, imm: r.s.into() }, None),
					Function::Srl => (IrOp::Srli { dst: r.d, a: r.t, imm: r.s.into() }, None),
					Function::Sra => (IrOp::Srai { dst: r.d, a: r.t, imm: r.s.into() }, None),
					Function::Sllv => (IrOp::Sl { dst: r.d, a: r.s, b: r.t }, None),
					Function::Srlv => (IrOp::Srl { dst: r.d, a: r.s, b: r.t }, None),
					Function::Srav => (IrOp::Sra { dst: r.d, a: r.s, b: r.t }, None),
					Function::Jr => (IrOp::Jr { register: r.s }, None),
					Function::Jalr => (IrOp::Jalr { link: r.t, register: r.s }, None),
					Function::Mfhi => (IrOp::Or { dst: r.d, a: 0, b: 32 }, None),
					Function::Mflo => (IrOp::Or { dst: r.d, a: 0, b: 33 }, None),
					Function::Mthi => (IrOp::Or { dst: 32, a: 0, b: r.d }, None),
					Function::Mtlo => (IrOp::Or { dst: 33, a: 0, b: r.d }, None),
					Function::Slt => (IrOp::Slts { dst: r.d, a: r.s, b: r.t }, None),
					Function::Sltu => (IrOp::Sltu { dst: r.d, a: r.s, b: r.t }, None),
					Function::Syscall => (IrOp::Syscall, None),
				}
				// TODO: check for overflow
				Op::Addi => {
					(IrOp::Addi { dst: i.t, a: i.s, imm: (i.imm_i16() as isize) }, None)
				}
				Op::Addiu => {
					(IrOp::Addi { dst: i.t, a: i.s, imm: (i.imm_i16() as isize) }, None)
				}
				Op::Andi => {
					(IrOp::Andi { dst: i.t, a: i.s, imm: i.imm.into() }, None)
				}
				Op::Ori => {
					(IrOp::Ori { dst: i.t, a: i.s, imm: i.imm.into() }, None)
				}
				Op::Xori => {
					(IrOp::Xori { dst: i.t, a: i.s, imm: i.imm.into() }, None)
				}
				Op::Slti => {
					(IrOp::Sltis { dst: i.t, a: i.s, imm: i.imm_i16().into() }, None)
				}
				Op::Sltiu => {
					(IrOp::Sltiu { dst: i.t, a: i.s, imm: i.imm.into() }, None)
				}
				Op::Beq => {
					let location = self.pc.wrapping_add(i.imm as i16 as usize).wrapping_add(1);
					(IrOp::Beq { a: i.s, b: i.t, location }, None)
				}
				Op::Bne => {
					let location = self.pc.wrapping_add(i.imm as i16 as usize).wrapping_add(1);
					(IrOp::Bne { a: i.s, b: i.t, location }, None)
				}
				Op::Lui => (IrOp::Ori { dst: i.t, a: 0, imm: usize::from(i.imm) << 16 }, None),
				Op::Lhi => (
					IrOp::Andi { dst: i.t, a: i.t, imm: 0x0000_ffff },
					Some(IrOp::Ori { dst: i.t, a: i.t, imm: usize::from(i.imm) << 16 }),
				),
				Op::Llo => (
					IrOp::Andi { dst: i.t, a: i.t, imm: 0xffff_0000 },
					Some(IrOp::Ori { dst: i.t, a: i.t, imm: usize::from(i.imm) }),
				),
				Op::Lb => {
					(IrOp::Li8 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Lbu => {
					(IrOp::Lu8 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Lh => {
					(IrOp::Li16 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Lhu => {
					(IrOp::Lu16 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Lw => {
					(IrOp::Lu32 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Sb => {
					(IrOp::S8 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Sh => {
					(IrOp::S16 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::Sw => {
					(IrOp::S32 { reg: i.t, mem: i.s, offset: i.imm_i16().into() }, None)
				}
				Op::J => {
					let location = self.pc.wrapping_add(j.imm_i32() as usize);
					dbg!(self.pc, j.imm_i32(), location);
					(IrOp::J { location }, None)
				}
				Op::Jal => {
					let location = self.pc.wrapping_add(i.imm_i16() as usize + 1);
					(IrOp::Jal { link: i.t, location }, None)
				}
				Op::Blez => {
					let location = self.pc.wrapping_add(i.imm_i16() as usize + 1);
					(IrOp::Ble { a: i.s, b: 0, location }, None)
				}
				Op::Bgtz => {
					let location = self.pc.wrapping_add(i.imm_i16() as usize + 1);
					(IrOp::Bgt { a: i.s, b: 0, location }, None)
				}
			};
			self.address_map.push(self.ir.len());
			self.ir.push(ops.0);
			ops.1.map(|op| self.ir.push(op));
		} else {
			dbg!(format_args!("{:3} {:x}", self.pc, instr));
			self.ir.push(IrOp::InvalidOp);
		}
		self.pc += 1;
	}

	pub fn finish(self) -> x86_64::Executable {
		// Find all jumps locations in IR space
		let mut jump_locations = Vec::new();
		for (i, op) in self.ir.iter().enumerate() {
			match op {
				IrOp::J { location }
				| IrOp::Jal { location, .. }
				| IrOp::Bgt { location, .. }
				| IrOp::Ble { location, .. }
				| IrOp::Beq { location, .. }
				| IrOp::Bne { location, .. }
				=> {
					let loc = self.address_map[*location];
					jump_locations.push(i + 1);
					jump_locations.push(loc);
				}
				_ => (),
			}
		}
		jump_locations.sort_unstable();
		jump_locations.dedup();
		dbg!(&jump_locations);

		// Link each block to other blocks
		let mut block_links = Vec::new();
		for (i, loc) in jump_locations.iter().chain(&[self.ir.len() - 1]).enumerate() {
			dbg!(self.ir[*loc - 1]);
			match &self.ir[*loc - 1] {
				IrOp::J { location }
				| IrOp::Jal { location, .. }
				=> {
					// Link to one block
					let location = self.address_map[*location];
					let block = jump_locations.binary_search(&location).unwrap();
					// + 1 because we don't include the start of block 0
					block_links.push(Some(BlockLink::Jump(block + 1)))
				}
				IrOp::Bgt { location, .. }
				| IrOp::Ble { location, .. }
				| IrOp::Beq { location, .. }
				| IrOp::Bne { location, .. }
				=> {
					// Link to the next and another block
					let location = self.address_map[*location];
					let block = jump_locations.binary_search(&location).unwrap();
					// + 1 because we don't include the start of block 0
					block_links.push(Some(BlockLink::Branch(block + 1)))
				}
				IrOp::Jr { .. }
				| IrOp::Jalr { .. }
				=> {
					// No definite links
					block_links.push(None);
				}
				_ => {
					// Link to the next block
					block_links.push(Some(BlockLink::Next))
				}
			}
		}

		// Compile each block
		let mut host_jit = x86_64::Jit::new();
		let mut prev_split = 0;
		for split in jump_locations {
			let block = &self.ir[prev_split..split];
			host_jit.compile(block, self.syscall_handler);
			prev_split = split;
		}
		let block = &self.ir[prev_split..];
		host_jit.compile(block, self.syscall_handler);

		// Link all blocks together
		host_jit.link(&block_links)
	}
}

#[repr(C)]
pub struct Registers {
	pub gp: [u32; 32],
	hi: u32,
	lo: u32,
	pc: u32,
}

impl Registers {
	const OFFSET_HI: u8 = 32;
	const OFFSET_LO: u8 = 33;
	const OFFSET_PC: u8 = 34;

	pub const V0: usize = 2;
	pub const V1: usize = 3;

	pub const A0: usize = 4;
	pub const A1: usize = 5;
	pub const A2: usize = 6;
	pub const A3: usize = 7;

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
