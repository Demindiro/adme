//! List of opcodes and functions to generate them.

#[derive(Clone, Copy, Debug, PartialEq)]
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

/// mod-reg-r/m encoding for instructions that support it.
///
/// Based on http://www.c-jump.com/CIS77/CPU/x86/lecture.html
#[derive(Clone, Copy, Debug)]
pub enum ModRegMR {
	/// SIB without base (d = 1, m = 0)
	RegScale { dst: Register, src: Register, scale: Scale, disp: i32 },
	/// SIB without base (d = 0, m = 0)
	ScaleReg { dst: Register, src: Register, scale: Scale, disp: i32 },

	/// SIB without base (d = 1, m = 0)
	RegIndex { dst: Register, src: Register, index: Register, scale: Scale },
	/// SIB without base (d = 0, m = 0)
	IndexReg { dst: Register, src: Register, index: Register, scale: Scale },

	/// SIB with base (d = 1, m = 0)
	RegIndex8 { dst: Register, src: Register, index: Register, scale: Scale, disp: i8 },
	/// SIB with base (d = 0, m = 0)
	Index8Reg { dst: Register, src: Register, index: Register, scale: Scale, disp: i8 },

	/// SIB with base (d = 1, m = 0)
	RegIndex32 { dst: Register, src: Register, index: Register, scale: Scale, disp: i32 },
	/// SIB with base (d = 0, m = 0)
	Index32Reg { dst: Register, src: Register, index: Register, scale: Scale, disp: i32 },

	/// i8 offset (d = 1, m = 0)
	RegRel8 { dst: Register, src: Register, disp: i8 },
	/// i8 offset (d = 0, m = 0)
	Rel8Reg { dst: Register, disp: i8, src: Register },

	/// Direct memory addressing (d = 1, m = 0)
	RegMem { dst: Register, src: Register },
	/// Direct memory addressing (d = 0, m = 0)
	MemReg { dst: Register, src: Register },

	/// i32 offset (d = 1, m = 0)
	RegRel32 { dst: Register, src: Register, disp: i32 },
	/// i32 offset (d = 0, m = 0)
	Rel32Reg { dst: Register, disp: i32, src: Register },

	/// u8 displacement (d = 1, m = 1)
	RegDisp8 { dst: Register, src: Register, disp: u8 },
	/// u8 displacement (d = 0, m = 1)
	Disp8Reg { dst: Register, src: Register, disp: u8 },

	/// u32 displacement (d = 1, m = 2)
	RegDisp32 { dst: Register, src: Register, disp: u32 },
	/// u32 displacement (d = 0, m = 2)
	Disp32Reg { dst: Register, index: Register, disp: u32 },

	/// Register addressing mode (d = 1, m = 3)
	RegReg { src: Register, dst: Register },
}

impl ModRegMR {
	fn encode(self, mut op: &mut [u8], size: Size, mut push: impl FnMut(u8)) {
		// Encode op
		let op_l = op.last_mut().expect("op cannot be empty");
		assert_eq!(*op_l & 3, 0, "op[0:1] are used for s and d");
		match size {
			Size::B => todo!("account for overlap between [abcd]h and si/di/ps/bs"),
			Size::W => push(0x66), // Operand size prefix
			Size::DW => *op_l |= 1, // s = 1
			Size::QW => {
				push(0x48); // REX.W
				*op_l |= 1; // s = 1
			}
		}
		*op_l |= match self {
			Self::ScaleReg { .. }
			| Self::Index8Reg { .. }
			| Self::Index32Reg { .. }
			| Self::Disp8Reg { .. }
			| Self::Disp32Reg { .. }
			| Self::MemReg { .. }
			| Self::Rel8Reg { .. }
			| Self::Rel32Reg { .. } => 0, // d = 0
			_ => 2, // d = 1
		};
		op.iter().copied().for_each(&mut push);

		// Encode args
		match self {
			Self::RegScale { dst: r, src: m, scale, disp }
			| Self::ScaleReg { dst: m, src: r, scale, disp } => {
				push(0 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | m.num3() << 3 | 0b101);
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegIndex { dst: r, src: m, index, scale }
			| Self::IndexReg { dst: m, src: r, index, scale } => {
				assert_ne!(m, Register::BP, "todo: handle SIB for BP");
				push(0 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | m.num3());
			}
			Self::RegIndex8 { dst: r, src: m, index, scale, disp }
			| Self::Index8Reg { dst: m, src: r, index, scale, disp } => {
				assert_ne!(m, Register::BP, "todo: handle SIB for BP");
				push(0 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegIndex32 { dst: r, src: m, index, scale, disp }
			| Self::Index32Reg { dst: m, src: r, index, scale, disp } => {
				assert_ne!(m, Register::BP, "todo: handle SIB for BP");
				push(0 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegMem { dst: r, src: m } | Self::MemReg { dst: m, src: r} => {
				assert_ne!(m, Register::SP, "todo: handle SIB for SP");
				push(0 << 6 | r.num3() << 3 | m.num3());
			}
			Self::RegRel8 { dst: r, src: m, disp } | Self::Rel8Reg { dst: m, src: r, disp } => {
				assert_ne!(m, Register::SP, "todo: handle SIB for SP");
				push(1 << 6 | r.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegRel32 { dst: r, src: m, disp } | Self::Rel32Reg { dst: m, src: r, disp } => {
				assert_ne!(m, Register::SP, "todo: handle SIB for SP");
				push(2 << 6 | r.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegReg { dst, src } => {
				push(3 << 6 | dst.num3() << 3 | src.num3());
			}
			Self::RegDisp8 { .. }
			| Self::Disp8Reg { .. }
			| Self::RegDisp32 { .. }
			| Self::Disp32Reg { .. } => todo!(),
		}
	}

	pub fn optimize(self) -> Self {
		match self {
			Self::RegRel32 { dst, src, disp } => if let Ok(disp) = i8::try_from(disp) {
				return Self::RegRel8 { dst, src, disp }.optimize();
			}
			Self::Rel32Reg { dst, src, disp } => if let Ok(disp) = i8::try_from(disp) {
				return Self::Rel8Reg { dst, src, disp }.optimize();
			}
			_ => (),
		}
		self
	}
}

/// mod-reg-r/m encoding with immediate for instructions that support it.
///
/// Based on http://www.c-jump.com/CIS77/CPU/x86/lecture.html
#[derive(Clone, Copy, Debug)]
pub enum ModRegMI {
	/// SIB without base (d = 0, m = 0)
	Scale { dst: Register, scale: Scale, disp: i32 },

	/// SIB without base (d = 0, m = 0)
	Index { dst: Register, index: Register, scale: Scale },

	/// SIB with base (d = 0, m = 0)
	Index8 { dst: Register, index: Register, scale: Scale, disp: i8 },

	/// SIB with base (d = 0, m = 0)
	Index32 { dst: Register, index: Register, scale: Scale, disp: i32 },

	/// i7 offset (d = 1, m = 0)
	Rel8 { dst: Register, disp: i8 },

	/// Direct memory addressing (d = 0, m = 0)
	Mem { dst: Register },

	/// i32 offset (d = 0, m = 0)
	Rel32 { dst: Register, disp: i32 },

	/// u8 displacement (d = 0, m = 1)
	Disp8 { dst: Register, disp: u8 },

	/// u32 displacement (d = 0, m = 2)
	Disp32 { dst: Register, disp: u32 },

	/// Register addressing mode (d = 0, m = 3)
	Reg { dst: Register },
}

impl ModRegMI {
	fn encode(self, op: &mut [u8], size: Size, imm: Immediate, mut push: impl FnMut(u8)) {
		// Encode op
		let op_l = op.last_mut().expect("op cannot be empty");
		assert_eq!(*op_l & 3, 0, "op[0:1] are used for s and x");
		match size {
			Size::B => todo!("account for overlap between [abcd]h and si/di/ps/bs"),
			Size::W => push(0x66), // Operand size prefix
			Size::DW => *op_l |= 1, // s = 1
			Size::QW => {
				push(0x48); // REX.W
				*op_l |= 1; // s = 1
			}
		}
		let imm = match imm {
			Immediate::B(imm) => {
				*op_l |= 2; // x = 1
				Immediate::B(imm)
			}
			Immediate::W(imm) => {
				assert!(size >= Size::W, "immediate too large");
				(size == Size::DW).then(|| Immediate::DW(imm.into())).unwrap_or(Immediate::W(imm))
			}
			Immediate::DW(imm) => {
				assert!(size >= Size::DW, "immediate too large");
				Immediate::DW(imm)
			}
			Immediate::QW(_) => panic!("64 bit immediates are unsupported"),
		};
		op.iter().copied().for_each(&mut push);

		// Encode args
		match self {
			Self::Scale { dst, scale, disp } => {
				push(0 << 6 | 0b100);
				push((scale as u8) << 6 | dst.num3() << 3 | 0b101);
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Index { dst, index, scale } => {
				assert_ne!(dst, Register::BP, "todo: handle SIB for BP");
				push(0 << 6 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | dst.num3());
			}
			Self::Index8 { dst, index, scale, disp } => {
				assert_ne!(dst, Register::BP, "todo: handle SIB for BP");
				push(0 << 6 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Index32 { dst, index, scale, disp } => {
				assert_ne!(dst, Register::BP, "todo: handle SIB for BP");
				push(0 << 6 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Mem { dst } => {
				assert_ne!(dst, Register::SP, "todo: handle SIB for SP");
				push(0 << 6 | dst.num3());
			}
			Self::Rel8 { dst, disp } => {
				assert_ne!(dst, Register::SP, "todo: handle SIB for SP");
				push(1 << 6 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Rel32 { dst, disp } => {
				assert_ne!(dst, Register::SP, "todo: handle SIB for SP");
				push(2 << 6 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Reg { dst } => {
				push(3 << 6 | dst.num3());
			}
			Self::Disp8 { .. } | Self::Disp32 { .. } => todo!(),
		}

		dbg!(imm);
		imm.to_le_bytes(&mut [0; 8]).iter().copied().for_each(push);
	}

	pub fn optimize(self) -> Self {
		match self {
			Self::Rel32 { dst, disp } => if let Ok(disp) = i8::try_from(disp) {
				return Self::Rel8 { dst, disp }.optimize();
			}
			Self::Rel8 { dst, disp } => if disp == 0 {
				return Self::Mem { dst }.optimize();
			}
			_ => (),
		}
		self
	}
}


#[derive(Clone, Copy, Debug)]
pub enum Scale {
	_1 = 0,
	_2 = 1,
	_4 = 2,
	_8 = 3,
}

/// The size of the operands an instruction operates on.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Size {
	B = 0,
	W = 1,
	DW = 2,
	QW = 3,
}

#[derive(Clone, Copy, Debug)]
pub enum Immediate {
	B(i8),
	W(i16),
	DW(i32),
	QW(i64),
}

impl Immediate {
	fn to_le_bytes<'a>(&self, out: &'a mut [u8; 8]) -> &'a [u8] {
		match self {
			Self::B(i) => {
				out[..1].copy_from_slice(&i.to_le_bytes());
				&out[..1]
			}
			Self::W(i) => {
				out[..2].copy_from_slice(&i.to_le_bytes());
				&out[..2]
			}
			Self::DW(i) => {
				out[..4].copy_from_slice(&i.to_le_bytes());
				&out[..4]
			}
			Self::QW(i) => {
				out[..8].copy_from_slice(&i.to_le_bytes());
				&out[..8]
			}
		}
	}

	pub fn optimize(self) -> Self {
		match self {
			Self::QW(i) => if let Ok(i) = i32::try_from(i) {
				return Self::DW(i).optimize();
			}
			Self::DW(i) => if let Ok(i) = i16::try_from(i) {
				return Self::W(i).optimize();
			}
			Self::W(i) => if let Ok(i) = i8::try_from(i) {
				return Self::B(i).optimize();
			}
			Self::B(_) => (),
		}
		self
	}
}

macro_rules! imm_from {
	($ty:ty, $v:ident) => {
		impl From<$ty> for Immediate {
			fn from(imm: $ty) -> Self {
				Self::$v(imm.into())
			}
		}
	};
	(try $from:ty, $ty:ty, $v:ident) => {
		impl TryFrom<$ty> for Immediate {
			type Error = <$ty as TryFrom<$from>>::Error;

			fn try_from(imm: $ty) -> Result<Self, Self::Error> {
				imm.try_into().map(Self::$v)
			}
		}
	};
}

imm_from!(i8, B);
imm_from!(u8, W);
imm_from!(i16, W);
imm_from!(u16, DW);
imm_from!(i32, DW);
imm_from!(u32, QW);
imm_from!(i64, QW);
imm_from!(try i64, isize, QW);
imm_from!(try i64, usize, QW);

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

	pub(super) fn add(&mut self, size: Size, args: ModRegMR) {
		args.encode(&mut [0x00], size, |b| self.push_u8(b));
	}

	pub(super) fn addi(&mut self, size: Size, args: ModRegMI, imm: Immediate) {
		args.encode(&mut [0x80], size, imm, |b| self.push_u8(b));
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
