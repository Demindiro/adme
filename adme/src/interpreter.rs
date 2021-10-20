use crate::*;
use core::fmt;
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

// Sources:
// * https://web.cse.ohio-state.edu/~crawfis.3/cse675-02/Slides/MIPS%20Instruction%20Set.pdf

macro_rules! op {
	{ [$ty:ident ($lb:literal .. $rb:literal)] $($name:ident = $n:literal,)* } => {
		const _: usize = $lb - $rb - 1;

		#[derive(Clone, Copy, Debug)]
		#[repr(u8)]
		pub(crate) enum $ty {
			$($name = $n,)*
		}

		impl TryFrom<u32> for $ty {
			type Error = StepError;

			fn try_from(n: u32) -> Result<Self, Self::Error> {
				match (n >> $rb) & ((1 << ($lb - $rb)) - 1) {
					$($n => Ok(Self::$name),)*
					_ => Err(StepError::InvalidOp),
				}
			}
		}
	};
}

op! {
	[Op (32..26)]

	Function = 0,

	J = 2,
	Jal = 3,
	Beq = 4,
	Bne = 5,
	Blez = 6,
	Bgtz = 7,

	Addi = 8,
	Addiu = 9,

	Slti = 10,
	Sltiu = 11,

	Andi = 12,
	Ori = 13,
	Xori = 14,

	Lui = 15,

	Llo = 20,
	Lhi = 21,

	Lb  = 32,
	Lh  = 33,
	Lw = 35,
	Lbu = 36,
	Lhu = 37,
	Sb = 40,
	Sh = 41,
	Sw = 43,
}

op! {
	[Function (6..0)]

	Jr = 8,
	Jalr = 9,

	Syscall = 12,

	Mfhi = 16,
	Mthi = 17,
	Mflo = 18,
	Mtlo = 19,

	Add  = 32,
	Addu = 33,
	Sub  = 34,
	Subu = 35,

	Mult  = 24,
	Multu = 25,
	Div  = 26,
	Divu = 27,

	Sll  = 0b000,
	Srl  = 0b010,
	Sra  = 0b011,
	Sllv = 0b100,
	Srlv = 0b110,
	Srav = 0b111,

	Slt  = 42,
	Sltu = 43,

	And = 36,
	Or  = 37,
	Nor = 39,
	Xor = 40,
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub struct Cpu {
	gp: [u32; 32],
	fp: [f32; 32],
	ip: u32,
	steps: u32,
	hi: u32,
	lo: u32,
}

impl Cpu {
	pub fn step(&mut self, memory: &mut impl Memory) -> Result<(), StepError> {
		self.gp[0] = 0;

		let instr = memory.load_u32(self.ip)?;
		self.ip = self.ip.wrapping_add(4);

		let r = R::decode(instr);
		let i = I::decode(instr);
		let j = J::decode(instr);
		let (r_s, r_t, r_d) = (usize::from(r.s), usize::from(r.t), usize::from(r.d));
		let (i_s, i_t) = (usize::from(i.s), usize::from(i.t));

		match Op::try_from(instr)? {
			Op::Function => {
				match Function::try_from(instr)? {
					Function::Add => {
						self.gp[r_d] = (self.gp[r_s] as i32)
							.checked_add(self.gp[r_t] as i32)
							.ok_or(StepError::Trap)? as u32
					}
					Function::Addu => self.apply_r(instr, u32::wrapping_add),
					Function::And => self.apply_r(instr, |a, b| a & b),
					Function::Nor => self.apply_r(instr, |a, b| !(a | b)),
					Function::Or => self.apply_r(instr, |a, b| a | b),
					Function::Div => {
						if self.gp[r_t] == 0 {
							return Err(StepError::Trap);
						}
						self.hi = (self.gp[r_s] as i32 % self.gp[r_t] as i32) as u32;
						self.lo = (self.gp[r_s] as i32 / self.gp[r_t] as i32) as u32;
					}
					Function::Divu => {
						if self.gp[r_t] == 0 {
							return Err(StepError::Trap);
						}
						self.hi = self.gp[r_s] % self.gp[r_t];
						self.lo = self.gp[r_s] / self.gp[r_t];
					}
					Function::Mult => {
						let r = i64::from(self.gp[r_s] as i32) * i64::from(self.gp[r_t] as i32);
						self.hi = (r as u64 >> 32) as u32;
						self.lo = r as u64 as u32;
					}
					Function::Multu => {
						let r = u64::from(self.gp[r_s]) * u64::from(self.gp[r_t]);
						self.hi = (r >> 32) as u32;
						self.lo = r as u32;
					}
					Function::Sll => self.gp[r_d] = self.gp[r_t].wrapping_shl(r_s as u32),
					Function::Srl => self.gp[r_d] = self.gp[r_t].wrapping_shr(r_s as u32),
					Function::Sra => {
						self.gp[r_d] = (self.gp[r_t] as i32).wrapping_shr(r_s as u32) as u32
					}
					Function::Sllv => self.apply_r(instr, u32::wrapping_shl),
					Function::Srlv => self.apply_r(instr, u32::wrapping_shl),
					Function::Srav => self.apply_r(instr, |a, b| (a as i32).wrapping_shr(b) as u32),
					Function::Sub => self.apply_r_checked(instr, u32::checked_sub)?,
					Function::Subu => self.apply_r(instr, u32::wrapping_sub),
					Function::Xor => self.apply_r(instr, |a, b| a ^ b),

					Function::Slt => self.apply_r(instr, |a, b| u32::from((a as i32) < b as i32)),
					Function::Sltu => self.apply_r(instr, |a, b| u32::from(a < b)),

					Function::Jr => self.ip = self.gp[r_s],
					Function::Jalr => {
						self.gp[31] = self.ip;
						self.ip = self.gp[r_s]
					}

					Function::Mfhi => self.gp[r_d] = self.hi,
					Function::Mflo => self.gp[r_d] = self.lo,
					Function::Mthi => self.hi = self.gp[r_s],
					Function::Mtlo => self.lo = self.gp[r_s],
					
					Function::Syscall => todo!(),
				}
			}
			Op::Addi => self.apply_i_checked(instr, |a, b| a.checked_add(b as i16 as u32))?,
			Op::Addiu => self.apply_i(instr, |a, b| a.wrapping_add(b as i16 as u32)),
			Op::Andi => self.apply_i(instr, |a, b| a & u32::from(b)),
			Op::Ori => self.apply_i(instr, |a, b| a | u32::from(b)),
			Op::Xori => self.apply_i(instr, |a, b| a ^ u32::from(b)),

			Op::Slti => self.apply_i(instr, |a, b| u32::from((a as i32) < i32::from(b as i16))),
			Op::Sltiu => self.apply_i(instr, |a, b| u32::from(a < u32::from(b))),

			Op::Beq => self.branch_i(instr, |a, b| a == b)?,
			Op::Bgtz => self.branch_i(instr, |a, _| a as i32 > 0)?,
			Op::Blez => self.branch_i(instr, |a, _| a as i32 <= 0)?,
			Op::Bne => self.branch_i(instr, |a, b| a != b)?,
			Op::J => {
				let offset = (j.imm_i32() << 2) as u32;
				self.ip = self.ip.wrapping_add(offset).wrapping_sub(4);
			}
			Op::Jal => {
				let offset = (j.imm_i32() << 2) as u32;
				self.gp[31] = self.ip;
				self.ip = self.ip.wrapping_add(offset).wrapping_sub(4);
			}

			Op::Lb => {
				let m = memory.load_u8(self.gp[i_s].wrapping_add(i.imm as i16 as u32))?;
				self.gp[i_t] = m as i8 as i32 as u32;
			}
			Op::Lbu => {
				let m = memory.load_u8(self.gp[i_s].wrapping_add(i.imm as i16 as u32))?;
				self.gp[i_t] = m.into();
			}
			Op::Lh => {
				let m = memory.load_u16(self.gp[i_s].wrapping_add(i.imm as i16 as u32))?;
				self.gp[i_t] = m as i16 as i32 as u32;
			}
			Op::Lhu => {
				let m = memory.load_u16(self.gp[i_s].wrapping_add(i.imm as i16 as u32))?;
				self.gp[i_t] = m.into();
			}
			Op::Lw => {
				let m = memory.load_u32(self.gp[i_s].wrapping_add(i.imm as i16 as u32))?;
				self.gp[i_t] = m;
			}
			Op::Lui => {
				self.gp[i_t] = u32::from(i.imm) << 16;
			}
			Op::Lhi => {
				self.gp[i_t] &= 0x0000_ffff;
				self.gp[i_t] |= u32::from(i.imm) << 16;
			}
			Op::Llo => {
				self.gp[i_t] &= 0xffff_0000;
				self.gp[i_t] |= u32::from(i.imm);
			}

			Op::Sb => {
				memory.store_u8(
					self.gp[i_s].wrapping_add(i.imm as i16 as u32),
					self.gp[i_t] as u8,
				)?;
			}
			Op::Sh => {
				memory.store_u16(
					self.gp[i_s].wrapping_add(i.imm as i16 as u32),
					self.gp[i_t] as u16,
				)?;
			}
			Op::Sw => {
				memory.store_u32(self.gp[i_s].wrapping_add(i.imm as i16 as u32), self.gp[i_t])?;
			}
		}

		self.steps = self.steps.wrapping_add(1);

		Ok(())
	}

	fn apply_r(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> u32) {
		let r = R::decode(instr);
		let (r_s, r_t, r_d) = (usize::from(r.s), usize::from(r.t), usize::from(r.d));
		self.gp[r_d] = f(self.gp[r_s], self.gp[r_t]);
	}

	fn apply_r_checked(
		&mut self,
		instr: u32,
		f: impl FnOnce(u32, u32) -> Option<u32>,
	) -> Result<(), StepError> {
		let r = R::decode(instr);
		let (r_s, r_t, r_d) = (usize::from(r.s), usize::from(r.t), usize::from(r.d));
		self.gp[r_d] = f(self.gp[r_s], self.gp[r_t]).ok_or(StepError::Trap)?;
		Ok(())
	}

	fn apply_i(&mut self, instr: u32, f: impl FnOnce(u32, u16) -> u32) {
		let i = I::decode(instr);
		let (i_s, i_t) = (usize::from(i.s), usize::from(i.t));
		self.gp[i_t] = f(self.gp[i_s], i.imm);
	}

	fn apply_i_checked(
		&mut self,
		instr: u32,
		f: impl FnOnce(u32, u16) -> Option<u32>,
	) -> Result<(), StepError> {
		let i = I::decode(instr);
		let (i_s, i_t) = (usize::from(i.s), usize::from(i.t));
		self.gp[i_t] = f(self.gp[i_s], i.imm).ok_or(StepError::Trap)?;
		Ok(())
	}

	fn branch_i(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> bool) -> Result<(), StepError> {
		let i = I::decode(instr);
		let (i_s, i_t) = (usize::from(i.s), usize::from(i.t));
		if f(self.gp[i_s], self.gp[i_t]) {
			self.ip = self.ip.wrapping_add((i.imm as i16 as u32) << 2);
		}
		Ok(())
	}

	fn store_i(
		&mut self,
		instr: u32,
		f: impl FnOnce(u32, u32, u16) -> Option<()>,
	) -> Result<(), StepError> {
		let i = I::decode(instr);
		let (i_s, i_t) = (usize::from(i.s), usize::from(i.t));
		f(self.gp[i_s], self.gp[i_t], i.imm).ok_or(StepError::Trap)?;
		Ok(())
	}
}

#[cfg_attr(feature = "wasm", wasm_bindgen)]
impl Cpu {
	#[cfg_attr(feature = "wasm", wasm_bindgen(constructor))]
	pub fn new() -> Self {
		Self {
			gp: [0; 32],
			fp: [0.0; 32],
			ip: 0,
			steps: 0,
			hi: 0,
			lo: 0,
		}
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(js_name = "step"))]
	pub fn step_js(&mut self, memory: &mut crate::wasm::Mem) -> Result<(), JsValue> {
		self.step(memory).map_err(|e| format!("{:?}", e).into())
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(method, getter))]
	pub fn steps(&self) -> u32 {
		self.steps
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(method, getter))]
	pub fn ip(&self) -> u32 {
		self.ip
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(js_name = "gp"))]
	pub fn js_gp(&self, reg: u8) -> Option<u32> {
		(1 <= reg && reg < 32).then(|| self.gp[usize::from(reg)])
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(method, getter))]
	pub fn hi(&self) -> u32 {
		self.hi
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(method, getter))]
	pub fn lo(&self) -> u32 {
		self.lo
	}
}

impl fmt::Debug for Cpu {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.debug_struct(stringify!(Cpu))
			.field("ip", &format_args!("{:}", self.ip))
			.field("gp", &format_args!("{:?}", &self.gp[1..32]))
			.field("fp", &format_args!("{:?}", &self.fp))
			.finish()
	}
}

#[derive(Debug)]
pub enum StepError {
	IpOutOfBounds,
	InvalidOp,
	Trap,
	LoadError(LoadError),
	StoreError(StoreError),
}

impl From<LoadError> for StepError {
	fn from(e: LoadError) -> Self {
		Self::LoadError(e)
	}
}

impl From<StoreError> for StepError {
	fn from(e: StoreError) -> Self {
		Self::StoreError(e)
	}
}

pub(crate) struct R {
	pub(crate) s: u8,
	pub(crate) t: u8,
	pub(crate) d: u8,
}

impl R {
	pub(crate) fn decode(instr: u32) -> R {
		R {
			s: ((instr >> 21) & 0x1f).try_into().unwrap(),
			t: ((instr >> 16) & 0x1f).try_into().unwrap(),
			d: ((instr >> 11) & 0x1f).try_into().unwrap(),
		}
	}
}

pub(crate) struct I {
	pub(crate) s: u8,
	pub(crate) t: u8,
	pub(crate) imm: u16,
}

impl I {
	pub(crate) fn decode(instr: u32) -> I {
		I {
			s: ((instr >> 21) & 0x1f).try_into().unwrap(),
			t: ((instr >> 16) & 0x1f).try_into().unwrap(),
			imm: instr as u16,
		}
	}

	pub(crate) fn imm_i16(&self) -> i16 {
		self.imm as i16
	}
}

pub(crate) struct J {
	pub(crate) imm: u32,
}

impl J {
	pub(crate) fn decode(instr: u32) -> J {
		J {
			imm: instr & 0x3ff_ffff,
		}
	}

	pub(crate) fn imm_i32(&self) -> i32 {
		// Make 26 bit unsigned int into 26 bit signed int.
		((self.imm << 6) as i32) >> 6
	}
}
