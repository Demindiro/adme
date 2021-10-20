//! List of opcodes and functions to generate them.

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

	/// The 3-bit opcode for this 8-bit register
	fn num3b(self, high: bool) -> u8 {
		let n = self as u8;
		assert!(n < 4, "todo");
		n | high.then(|| 0b100).unwrap_or(0)
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

	fn op32_m_o32_r(&mut self, op: u8, a: Register, offset: i32, b: Register) {
		self.push_u8(op);
		self.push_u8(2 << 6 | b.num3() << 3 | a.num3()); // MOD = 2 | from | to
		offset.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
	}

	fn op32_r_m_o8(&mut self, op: u8, a: Register, b: Register, offset: i8) {
		self.push_u8(op | 0b10); // D = 1
		self.push_u8(1 << 6 | a.num3() << 3 | b.num3()); // MOD = 1 | to | from
		self.push_u8(offset as u8);
	}

	fn op32_r_m_o32(&mut self, op: u8, a: Register, b: Register, offset: i32) {
		self.push_u8(op | 0b10); // D = 1
		self.push_u8(2 << 6 | a.num3() << 3 | b.num3()); // MOD = 2 | to | from
		offset.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
	}

	pub(super) fn add32_r2(&mut self, a: Register, b: Register) {
		self.op32_r2(0x01, a, b);
	}

	pub(super) fn add_r64_r64(&mut self, a: Register, b: Register) {
		self.push_u8(0x48); // REX.W
		self.op32_r2(0x01, a, b);
	}

	pub(super) fn add_r32_imm(&mut self, dst: Register, imm: isize) {
		dst.extended().then(|| self.push_u8(0x41));
		if let Ok(imm) = i8::try_from(imm) {
			self.push_u8(0x83);
			self.push_u8(0xc0 | dst.num3());
			imm.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
		} else if let Ok(imm) = i32::try_from(imm) {
			self.push_u8(0x81);
			self.push_u8(0xb0 | dst.num3());
			imm.to_le_bytes().iter().for_each(|b| self.push_u8(*b));
		} else {
			panic!("immediate too large");
		}
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

	pub(super) fn or_m64_offset_r32(&mut self, a: Register, offset: isize, b: Register) {
		let offset = i8::try_from(offset).expect("todo: 16/32 bit offsets");
		self.op32_m_o8_r(0x09, a, offset, b);
	}

	pub(super) fn or_r32_m64_offset(&mut self, a: Register, b: Register, offset: isize) {
		if let Ok(offset) = i8::try_from(offset) {
			self.op32_r_m_o8(0x09, a, b, offset);
		} else if let Ok(offset) = i32::try_from(offset) {
			self.op32_r_m_o32(0x09, a, b, offset);
		} else {
			panic!("offset out of range");
		}
	}

	pub(super) fn div_m64_offset(&mut self, base: Register, offset: isize) {
		let offset = i8::try_from(offset).unwrap();
		base.extended().then(|| self.push_u8(0x41)); // REX.R
		self.push_u8(0xf7); // DIV
		self.push_u8(0x70 | base.num3()); // MOD = 0
		self.push_u8(offset as u8);
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

	pub(super) fn mov_m64_offset_imm32(&mut self, dst: Register, offset: isize, imm: usize) {
		assert!(!dst.extended(), "todo: extended registers");
		assert!(offset < 128, "todo: dword offset");
		if imm > usize::try_from(u32::MAX).unwrap() {
			todo!("64 bit immediates");
		}
		self.push_u8(0xc7); // MOV
		self.push_u8(0x40 | dst.num3()); // MOD = 1 | dst
		self.push_u8(offset as i8 as u8);
		if imm <= usize::try_from(u32::MAX).unwrap() {
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
		if let Ok(offset) = i8::try_from(offset) {
			self.op32_m_o8_r(0x89, a, offset, b);
		} else if let Ok(offset) = i32::try_from(offset) {
			self.op32_m_o32_r(0x89, a, offset, b);
		} else {
			panic!("offset out of range");
		}
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

	pub(super) fn xor_r32_r32(&mut self, dst: Register, src: Register) {
		self.op32_r2(0x31, dst, src)
	}

	pub(super) fn setl(&mut self, dst: Register, high: bool) {
		self.push_u8(0x0f); // Expansion prefix
		self.push_u8(0x9c); // SETL
		self.push_u8(0xc0 | dst.num3b(high));
	}

	pub(super) fn setg(&mut self, dst: Register, high: bool) {
		self.push_u8(0x0f); // Expansion prefix
		self.push_u8(0x9f); // SETG
		self.push_u8(0xc0 | dst.num3b(high));
	}
}
