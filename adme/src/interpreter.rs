use crate::*;
use core::fmt;
use core::ops;
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

// Sources:
// * https://web.cse.ohio-state.edu/~crawfis.3/cse675-02/Slides/MIPS%20Instruction%20Set.pdf

macro_rules! op {
	{ [$ty:ident ($lb:literal .. $rb:literal)] $($name:ident = $n:literal,)* } => {
		const _: usize = $lb - $rb - 1;

		#[derive(Debug)]
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

	Bltz = 1,
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

	Exception = 16,

	Fp = 17,

	Lw = 35,
	Sw = 43,

	Lbu = 36,
	Lb  = 32,
	Sb  = 40,

	Lwcl = 49,
	Swcl = 57,
}

op! {
	[Function (6..0)]

	Nop = 0,
	Jr = 8,
	Jalr = 9,

	Mfhi = 16,
	Mflo = 18,

	Add  = 32,
	Addu = 33,
	Sub  = 34,
	Subu = 35,

	Mul  = 24,
	Mulu = 25,
	Div  = 26,
	Divu = 27,

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
}

impl Cpu {
	pub fn step(&mut self, memory: &mut impl Memory) -> Result<(), StepError> {
		self.gp[0] = 0;

		let instr = memory.load_u32(self.ip)?;
		match Op::try_from(instr)? {
			Op::Function => match Function::try_from(instr)? {
				Function::Nop => (),
				Function::Add => self.apply_r_checked(instr, u32::checked_add)?,
				Function::Addu => self.apply_r(instr, u32::wrapping_add),
				Function::Sub => self.apply_r_checked(instr, u32::checked_sub)?,
				Function::Subu => self.apply_r(instr, u32::wrapping_sub),
				f => todo!("{:?}", f),
			},
			Op::Addi => self.apply_i_checked(instr, u32::checked_add)?,
			Op::Addiu => self.apply_i(instr, u32::wrapping_add),
			Op::Ori => self.apply_i(instr, ops::BitOr::bitor),

			Op::Beq => self.branch_i(instr, |a, b| a == b)?,
			Op::Bne => self.branch_i(instr, |a, b| a != b)?,
			Op::J => {
				let j = Self::decode_j(instr);
				self.ip = self.ip & 0xffc0_0000 | j.imm & 0x3ff_ffff;
				self.ip = self.ip.wrapping_sub(4);
			}

			Op::Lbu => {
				let i = Self::decode_i(instr);
				self.gp[i.t] = memory.load_u8(self.gp[i.s].wrapping_add(i.imm))?.into();
			}
			Op::Sb => {
				let i = Self::decode_i(instr);
				memory.store_u8(self.gp[i.s].wrapping_add(i.imm), self.gp[i.t] as u8)?;
			}
			Op::Lui => {
				let i = Self::decode_i(instr);
				self.gp[i.t] = i.imm << 16;
			}
			o => todo!("{:?}", o),
		}

		self.ip = self.ip.wrapping_add(4);
		self.steps = self.steps.wrapping_add(1);

		Ok(())
	}

	fn apply_r(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> u32) {
		let r = Self::decode_r(instr);
		self.gp[r.d] = f(self.gp[r.s], self.gp[r.t]);
	}

	fn apply_r_checked(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> Option<u32>) -> Result<(), StepError> {
		let r = Self::decode_r(instr);
		self.gp[r.d] = f(self.gp[r.s], self.gp[r.t]).ok_or(StepError::Trap)?;
		Ok(())
	}

	fn decode_r(instr: u32) -> R {
		R {
			s: ((instr >> 21) & 0x1f).try_into().unwrap(),
			t: ((instr >> 16) & 0x1f).try_into().unwrap(),
			d: ((instr >> 11) & 0x1f).try_into().unwrap(),
		}
	}

	fn apply_i(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> u32) {
		let i = Self::decode_i(instr);
		self.gp[i.t] = f(self.gp[i.s], i.imm);
	}

	fn apply_i_checked(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> Option<u32>) -> Result<(), StepError> {
		let i = Self::decode_i(instr);
		self.gp[i.t] = f(self.gp[i.s], i.imm).ok_or(StepError::Trap)?;
		Ok(())
	}

	fn branch_i(&mut self, instr: u32, f: impl FnOnce(u32, u32) -> bool) -> Result<(), StepError> {
		let i = Self::decode_i(instr);
		if f(self.gp[i.s], self.gp[i.t]) {
			self.ip = self.ip.wrapping_add(i.imm as i16 as u32);
		}
		Ok(())
	}

	fn store_i(&mut self, instr: u32, f: impl FnOnce(u32, u32, u32) -> Option<()>) -> Result<(), StepError> {
		let i = Self::decode_i(instr);
		f(self.gp[i.s], self.gp[i.t], i.imm).ok_or(StepError::Trap)?;
		Ok(())
	}

	fn decode_i(instr: u32) -> I {
		I {
			s: ((instr >> 21) & 0x1f).try_into().unwrap(),
			t: ((instr >> 16) & 0x1f).try_into().unwrap(),
			imm: instr & 0xffff,
		}
	}

	fn decode_j(instr: u32) -> J {
		J {
			imm: instr & 0x3ff_ffff,
		}
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
		}
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(js_name = "step"))]
	pub fn step_js(&mut self, memory: &mut crate::wasm::Mem) -> Result<(), JsValue> {
		self.step(memory)
			.map_err(|e| format!("{:?}", e).into())
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(method, getter))]
	pub fn steps(&self) -> u32 {
		self.steps
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen(method, getter))]
	pub fn ip(&self) -> u32 {
		self.ip
	}

	#[cfg_attr(feature = "wasm", wasm_bindgen)]
	pub fn gp(&self, reg: u8) -> Option<u32> {
		(1 <= reg && reg < 32).then(|| self.gp[usize::from(reg)])
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

struct R {
	s: usize,
	t: usize,
	d: usize,
}

struct I {
	s: usize,
	t: usize,
	imm: u32,
}

struct J {
	imm: u32,
}
