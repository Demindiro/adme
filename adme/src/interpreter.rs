use core::fmt;
use core::mem;
use core::slice;

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

pub struct Cpu {
	gp: [u32; 32],
	fp: [f32; 32],
	ip: u32,
}

impl Cpu {
	pub fn new() -> Self {
		Self {
			gp: [0; 32],
			fp: [0.0; 32],
			ip: 0,
		}
	}

	pub fn step(&mut self, memory: &mut [u32]) -> Result<(), StepError> {
		// SAFETY:
		// * A &[u8] slice from any &[T] slice will be properly aligned.
		// * The length is trivially determined and cannot overflow.
		let memory_u8 = unsafe {
			slice::from_raw_parts_mut(
				memory.as_mut_ptr().cast::<u8>(),
				memory.len() * mem::size_of::<u32>(),
			)
		};

		self.gp[0] = 0;

		let ip = usize::try_from(self.ip).unwrap() / 4;
		let instr = *memory.get(ip).ok_or(StepError::IpOutOfBounds)?;
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
			o => todo!("{:?}", o),
		}

		self.ip = self.ip.wrapping_add(4);

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

	fn decode_i(instr: u32) -> I {
		I {
			s: ((instr >> 21) & 0x1f).try_into().unwrap(),
			t: ((instr >> 16) & 0x1f).try_into().unwrap(),
			imm: instr & 0xffff,
		}
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
