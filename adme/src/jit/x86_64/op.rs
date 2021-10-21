//! List of opcodes and functions to generate them.

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg {
	AX  = 0b0_000,
	BX  = 0b0_011,
	CX  = 0b0_001,
	DX  = 0b0_010,
	DI  = 0b0_111,
	SI  = 0b0_110,
	BP  = 0b0_101,
	SP  = 0b0_100,
	R8  = 0b1_000,
	R9  = 0b1_001,
	R10 = 0b1_010,
	R11 = 0b1_011,
	R12 = 0b1_100,
	R13 = 0b1_101,
	R14 = 0b1_110,
	R15 = 0b1_111,
}

impl Reg {
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
	RegScale { dst: Reg, src: Reg, scale: Scale, disp: i32 },
	/// SIB without base (d = 0, m = 0)
	ScaleReg { dst: Reg, src: Reg, scale: Scale, disp: i32 },

	/// SIB without base (d = 1, m = 0)
	RegIndex { dst: Reg, src: Reg, index: Reg, scale: Scale },
	/// SIB without base (d = 0, m = 0)
	IndexReg { dst: Reg, src: Reg, index: Reg, scale: Scale },

	/// SIB with base (d = 1, m = 0)
	RegIndex8 { dst: Reg, src: Reg, index: Reg, scale: Scale, disp: i8 },
	/// SIB with base (d = 0, m = 0)
	Index8Reg { dst: Reg, src: Reg, index: Reg, scale: Scale, disp: i8 },

	/// SIB with base (d = 1, m = 0)
	RegIndex32 { dst: Reg, src: Reg, index: Reg, scale: Scale, disp: i32 },
	/// SIB with base (d = 0, m = 0)
	Index32Reg { dst: Reg, src: Reg, index: Reg, scale: Scale, disp: i32 },

	/// i8 offset (d = 1, m = 0)
	RegRel8 { dst: Reg, src: Reg, disp: i8 },
	/// i8 offset (d = 0, m = 0)
	Rel8Reg { dst: Reg, disp: i8, src: Reg },

	/// Direct memory addressing (d = 1, m = 0)
	RegMem { dst: Reg, src: Reg },
	/// Direct memory addressing (d = 0, m = 0)
	MemReg { dst: Reg, src: Reg },

	/// i32 offset (d = 1, m = 0)
	RegRel32 { dst: Reg, src: Reg, disp: i32 },
	/// i32 offset (d = 0, m = 0)
	Rel32Reg { dst: Reg, disp: i32, src: Reg },

	/// u8 displacement (d = 1, m = 1)
	RegDisp8 { dst: Reg, src: Reg, disp: u8 },
	/// u8 displacement (d = 0, m = 1)
	Disp8Reg { dst: Reg, src: Reg, disp: u8 },

	/// u32 displacement (d = 1, m = 2)
	RegDisp32 { dst: Reg, src: Reg, disp: u32 },
	/// u32 displacement (d = 0, m = 2)
	Disp32Reg { dst: Reg, index: Reg, disp: u32 },

	/// Reg addressing mode (d = 1, m = 3)
	RegReg { src: Reg, dst: Reg },
}

impl ModRegMR {
	fn encode(self, mut op: &mut [u8], size: Size, mut push: impl FnMut(u8)) {
		// Encode op
		let op_l = op.last_mut().expect("op cannot be empty");
		assert_eq!(*op_l & 3, 0, "op[0:1] are used for s and d");
		match size {
			Size::B => todo!("account for overlap between [abcd]h and si/di/ps/bs"),
			Size::W => push(0x66),  // Operand size prefix
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
				assert_ne!(m, Reg::BP, "todo: handle SIB for BP");
				push(0 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | m.num3());
			}
			Self::RegIndex8 { dst: r, src: m, index, scale, disp }
			| Self::Index8Reg { dst: m, src: r, index, scale, disp } => {
				assert_ne!(m, Reg::BP, "todo: handle SIB for BP");
				push(1 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegIndex32 { dst: r, src: m, index, scale, disp }
			| Self::Index32Reg { dst: m, src: r, index, scale, disp } => {
				assert_ne!(m, Reg::BP, "todo: handle SIB for BP");
				push(2 << 6 | r.num3() << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegMem { dst: r, src: m } | Self::MemReg { dst: m, src: r } => {
				assert_ne!(m, Reg::SP, "todo: handle SIB for SP");
				push(0 << 6 | r.num3() << 3 | m.num3());
			}
			Self::RegRel8 { dst: r, src: m, disp } | Self::Rel8Reg { dst: m, src: r, disp } => {
				assert_ne!(m, Reg::SP, "todo: handle SIB for SP");
				push(1 << 6 | r.num3() << 3 | m.num3());
				disp.to_le_bytes().iter().copied().for_each(push);
			}
			Self::RegRel32 { dst: r, src: m, disp } | Self::Rel32Reg { dst: m, src: r, disp } => {
				assert_ne!(m, Reg::SP, "todo: handle SIB for SP");
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
			Self::RegIndex32 { dst, src, index, scale, disp } => {
				if let Ok(disp) = i8::try_from(disp) {
					return Self::RegIndex8 { dst, src, index, scale, disp }.optimize();
				}
			}
			Self::RegIndex8 { dst, src, index, scale, disp } => {
				if disp == 0 {
					return Self::RegIndex { dst, src, index, scale }.optimize();
				}
			}
			Self::RegRel32 { dst, src, disp } => {
				if let Ok(disp) = i8::try_from(disp) {
					return Self::RegRel8 { dst, src, disp }.optimize();
				}
			}
			Self::RegRel8 { dst, src, disp } => {
				if disp == 0 {
					return Self::RegMem { dst, src }.optimize();
				}
			}
			Self::Index32Reg { dst, src, index, scale, disp } => {
				if let Ok(disp) = i8::try_from(disp) {
					return Self::Index8Reg { dst, src, index, scale, disp }.optimize();
				}
			}
			Self::Index8Reg { dst, src, index, scale, disp } => {
				if disp == 0 {
					return Self::IndexReg { dst, src, index, scale }.optimize();
				}
			}
			Self::Rel32Reg { dst, src, disp } => {
				if let Ok(disp) = i8::try_from(disp) {
					return Self::Rel8Reg { dst, src, disp }.optimize();
				}
			}
			Self::Rel8Reg { dst, src, disp } => {
				if disp == 0 {
					return Self::MemReg { dst, src }.optimize();
				}
			}
			_ => (),
		}
		self
	}

	pub fn rr(dst: Reg, src: Reg) -> Self {
		Self::RegReg { dst, src }.optimize()
	}

	pub fn rr32(dst: Reg, src: (Reg, i32)) -> Self {
		Self::RegRel32 { dst, src: src.0, disp: src.1 }.optimize()
	}

	pub fn r32r(dst: (Reg, i32), src: Reg) -> Self {
		Self::Rel32Reg { dst: dst.0, disp: dst.1, src }.optimize()
	}

	pub fn ri32(dst: Reg, src: (Reg, Reg, Scale, i32)) -> Self {
		Self::RegIndex32 { dst, src: src.0, index: src.1, scale: src.2, disp: src.3 }.optimize()
	}
}

/// mod-reg-r/m encoding with immediate for instructions that support it.
///
/// Based on http://www.c-jump.com/CIS77/CPU/x86/lecture.html
#[derive(Clone, Copy, Debug)]
pub enum ModRegMI {
	/// SIB without base (d = 0, m = 0)
	Scale { dst: Reg, scale: Scale, disp: i32 },

	/// SIB without base (d = 0, m = 0)
	Index { dst: Reg, index: Reg, scale: Scale },

	/// SIB with base (d = 0, m = 0)
	Index8 { dst: Reg, index: Reg, scale: Scale, disp: i8 },

	/// SIB with base (d = 0, m = 0)
	Index32 { dst: Reg, index: Reg, scale: Scale, disp: i32 },

	/// i7 offset (d = 1, m = 0)
	Rel8 { dst: Reg, disp: i8 },

	/// Direct memory addressing (d = 0, m = 0)
	Mem { dst: Reg },

	/// i32 offset (d = 0, m = 0)
	Rel32 { dst: Reg, disp: i32 },

	/// u8 displacement (d = 0, m = 1)
	Disp8 { dst: Reg, disp: u8 },

	/// u32 displacement (d = 0, m = 2)
	Disp32 { dst: Reg, disp: u32 },

	/// Reg addressing mode (d = 0, m = 3)
	Reg { dst: Reg },
}

impl ModRegMI {
	fn encode(
		self,
		op: &mut [u8],
		op_ext: u8,
		size: Size,
		imm: Immediate,
		mut push: impl FnMut(u8),
	) {
		// Encode op
		let op_l = op.last_mut().expect("op cannot be empty");
		assert_eq!(*op_l & 3, 0, "op[0:1] are used for s and x");
		assert!(op_ext < 8, "op_ext must be in range [0; 7]");
		match size {
			Size::B => todo!("account for overlap between [abcd]h and si/di/ps/bs"),
			Size::W => push(0x66),  // Operand size prefix
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
				(size == Size::DW)
					.then(|| Immediate::DW(imm.into()))
					.unwrap_or(Immediate::W(imm))
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
				push(0 << 6 | op_ext << 3 | 0b100);
				push((scale as u8) << 6 | dst.num3() << 3 | 0b101);
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Index { dst, index, scale } => {
				assert_ne!(dst, Reg::BP, "todo: handle SIB for BP");
				push(0 << 6 | op_ext << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | dst.num3());
			}
			Self::Index8 { dst, index, scale, disp } => {
				assert_ne!(dst, Reg::BP, "todo: handle SIB for BP");
				push(0 << 6 | op_ext << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Index32 { dst, index, scale, disp } => {
				assert_ne!(dst, Reg::BP, "todo: handle SIB for BP");
				push(0 << 6 | op_ext << 3 | 0b100);
				push((scale as u8) << 6 | index.num3() << 3 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Mem { dst } => {
				assert_ne!(dst, Reg::SP, "todo: handle SIB for SP");
				push(0 << 6 | op_ext << 3 | dst.num3());
			}
			Self::Rel8 { dst, disp } => {
				assert_ne!(dst, Reg::SP, "todo: handle SIB for SP");
				push(1 << 6 | op_ext << 3 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Rel32 { dst, disp } => {
				assert_ne!(dst, Reg::SP, "todo: handle SIB for SP");
				push(2 << 6 | op_ext << 3 | dst.num3());
				disp.to_le_bytes().iter().copied().for_each(&mut push);
			}
			Self::Reg { dst } => {
				push(3 << 6 | op_ext << 3 | dst.num3());
			}
			Self::Disp8 { .. } | Self::Disp32 { .. } => todo!(),
		}

		dbg!(imm);
		imm.to_le_bytes(&mut [0; 8]).iter().copied().for_each(push);
	}

	pub fn optimize(self) -> Self {
		match self {
			Self::Rel32 { dst, disp } => {
				if let Ok(disp) = i8::try_from(disp) {
					return Self::Rel8 { dst, disp }.optimize();
				}
			}
			Self::Rel8 { dst, disp } => {
				if disp == 0 {
					return Self::Mem { dst }.optimize();
				}
			}
			_ => (),
		}
		self
	}

	pub fn r(dst: Reg) -> Self {
		Self::Reg { dst }
	}

	pub fn r32(dst: (Reg, i32)) -> Self {
		Self::Rel32 { dst: dst.0, disp: dst.1 }
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
	B  = 0,
	W  = 1,
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
			Self::QW(i) => {
				if let Ok(i) = i32::try_from(i) {
					return Self::DW(i).optimize();
				}
			}
			Self::DW(i) => {
				if let Ok(i) = i16::try_from(i) {
					return Self::W(i).optimize();
				}
			}
			Self::W(i) => {
				if let Ok(i) = i8::try_from(i) {
					return Self::B(i).optimize();
				}
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

#[derive(Clone, Copy, Debug)]
pub enum IOpExt {
	Add = 0b000,
	Or  = 0b001,
	Adc = 0b010,
	Sbb = 0b011,
	And = 0b100,
	Sub = 0b101,
	Xor = 0b110,
	Cmp = 0b111,
}

impl super::Block {
	pub(super) fn add(&mut self, size: Size, args: ModRegMR) {
		args.encode(&mut [0x00], size, |b| self.push_u8(b));
	}

	pub(super) fn addi(&mut self, size: Size, args: ModRegMI, imm: Immediate) {
		args.encode(&mut [0x80], IOpExt::Add as u8, size, imm, |b| {
			self.push_u8(b)
		});
	}

	pub(super) fn or(&mut self, size: Size, args: ModRegMR) {
		args.encode(&mut [0x08], size, |b| self.push_u8(b));
	}

	pub(super) fn ori(&mut self, size: Size, args: ModRegMI, imm: Immediate) {
		args.encode(&mut [0x80], IOpExt::Or as u8, size, imm, |b| {
			self.push_u8(b)
		});
	}

	pub(super) fn xor(&mut self, size: Size, args: ModRegMR) {
		args.encode(&mut [0x30], size, |b| self.push_u8(b));
	}

	pub(super) fn cmp(&mut self, size: Size, args: ModRegMR) {
		args.encode(&mut [0x38], size, |b| self.push_u8(b));
	}

	pub(super) fn div_m64_offset(&mut self, base: Reg, offset: isize) {
		let offset = i8::try_from(offset).unwrap();
		base.extended().then(|| self.push_u8(0x41)); // REX.R
		self.push_u8(0xf7); // DIV
		self.push_u8(0x70 | base.num3()); // MOD = 0
		self.push_u8(offset as u8);
	}

	pub(super) fn mov(&mut self, size: Size, args: ModRegMR) {
		args.encode(&mut [0x88], size, |b| self.push_u8(b));
	}

	pub(super) fn movzx(&mut self, size: Size, args: ModRegMR) {
		assert_ne!(size, Size::B, "no variant for 8 byte operands");
		args.encode(&mut [0x0f, 0xb4], size, |b| self.push_u8(b));
	}

	pub(super) fn mov_r64_r64(&mut self, dst: Reg, src: Reg) {
		assert!(!dst.extended(), "todo: extended registers");
		self.push_u8(0x48); // REX.W
		self.push_u8(0x89); // MOV
		self.push_u8(3 << 6 | src.num3() << 3 | dst.num3()); // MOD = 3
	}

	pub(super) fn mov_r64_immu(&mut self, dst: Reg, num: usize) {
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

	pub(super) fn mov_m64_offset_imm32(&mut self, dst: Reg, offset: isize, imm: usize) {
		assert!(!dst.extended(), "todo: extended registers");
		assert!(offset < 128, "todo: dword offset");
		if imm > usize::try_from(u32::MAX).unwrap() {
			todo!("64 bit immediates");
		}
		self.push_u8(0xc7); // MOV
		self.push_u8(0x40 | dst.num3()); // MOD = 1 | dst
		self.push_u8(offset as i8 as u8);
		if imm <= usize::try_from(u32::MAX).unwrap() {
			(imm as u32)
				.to_le_bytes()
				.iter()
				.for_each(|b| self.push_u8(*b));
		} else {
			todo!();
		}
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

	pub(super) fn jmp_r64(&mut self, to: Reg) {
		to.extended().then(|| self.push_u8(0x41)); // ???
		self.push_u8(0xff); // JMP
		self.push_u8(0xe0 | to.num3()); // ??? | to
	}

	pub(super) fn call_r64(&mut self, to: Reg) {
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

	pub(super) fn push_r64(&mut self, reg: Reg) {
		assert!(!reg.extended(), "todo");
		self.push_u8(0x50 | reg.num3());
	}

	pub(super) fn pop_r64(&mut self, reg: Reg) {
		assert!(!reg.extended(), "todo");
		self.push_u8(0x58 | reg.num3());
	}

	pub(super) fn setl(&mut self, dst: Reg, high: bool) {
		self.push_u8(0x0f); // Expansion prefix
		self.push_u8(0x9c); // SETL
		self.push_u8(0xc0 | dst.num3b(high));
	}

	pub(super) fn setg(&mut self, dst: Reg, high: bool) {
		self.push_u8(0x0f); // Expansion prefix
		self.push_u8(0x9f); // SETG
		self.push_u8(0xc0 | dst.num3b(high));
	}
}
