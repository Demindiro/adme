mod source_map;

pub use source_map::SourceMap;

use crate::interpreter::{Function, Op};
use crate::util::*;
use core::ops::Range;
use std::collections::HashMap;

type Result<'a, T = ()> = core::result::Result<T, AssembleError<'a>>;

struct PendingLabel {
	right_shift: u8,
	bits: u8,
	location: u32,
	flags: u8,
	offset: i8,
}

impl PendingLabel {
	const RELATIVE: u8 = 1;
	const MUST_FIT: u8 = 2;
}

pub struct Assembler<'a, 'b> {
	known_labels: HashMap<&'a str, u32>,
	pending_labels: HashMap<&'a str, Vec<PendingLabel>>,
	memory: &'b mut [u32],
	ip: u32,
	source_map: SourceMap,
	last_line: u32,
}

impl<'a, 'b> Assembler<'a, 'b> {
	fn push_byte(&mut self, byte: u8) -> Result<'a> {
		let mem = as_u8_mut(self.memory);
		*mem.get_mut(usize::try_from(self.ip).unwrap())
			.ok_or(AssembleError::OutOfMemory)? = byte;
		self.ip = self.ip.wrapping_add(1);
		Ok(())
	}

	fn push_half(&mut self, half: u16) -> Result<'a> {
		let mem = as_u16_mut(self.memory);
		*mem.get_mut(usize::try_from(self.ip / 2).unwrap())
			.ok_or(AssembleError::OutOfMemory)? = half;
		self.ip = self.ip.wrapping_add(2);
		Ok(())
	}

	fn push_word(&mut self, word: u32) -> Result<'a> {
		self.source_map.insert(self.ip, self.last_line);
		*self
			.memory
			.get_mut(usize::try_from(self.ip / 4).unwrap())
			.ok_or(AssembleError::OutOfMemory)? = word;
		self.ip = self.ip.wrapping_add(4);
		Ok(())
	}

	fn align_ip(&mut self) {
		self.ip = self.ip.wrapping_add(3) & !0x3;
	}

	fn push_r(&mut self, s: u32, t: u32, d: u32, sa: u32, function: Function) -> Result<'a> {
		self.align_ip();
		// op is always 0
		let i = s << 21 | t << 16 | d << 11 | sa << 6 | function as u32;
		self.push_word(i)
	}

	fn push_i(&mut self, op: Op, s: u32, t: u32, imm: u32) -> Result<'a> {
		self.align_ip();
		self.push_word((op as u32) << 26 | s << 21 | t << 16 | imm)
	}

	fn push_i_label(
		&mut self,
		op: Op,
		s: u32,
		t: u32,
		label: &'a str,
		bits: Range<u8>,
		offset: i8,
		relative: bool,
	) -> Result<'a> {
		self.align_ip();
		self.push_word((op as u32) << 26 | s << 21 | t << 16)?;
		self.resolve_label(
			label,
			bits,
			offset,
			u8::from(relative) * PendingLabel::RELATIVE,
		);
		Ok(())
	}

	fn push_j(&mut self, op: Op, imm: u32) -> Result<'a> {
		self.align_ip();
		self.push_word((op as u32) << 26 | imm)
	}

	fn push_j_label(
		&mut self,
		op: Op,
		label: &'a str,
		bits: Range<u8>,
		relative: bool,
	) -> Result<'a> {
		self.align_ip();
		self.push_word((op as u32) << 26)?;
		self.resolve_label(
			label,
			bits,
			0,
			PendingLabel::MUST_FIT | u8::from(relative) * PendingLabel::RELATIVE,
		);
		Ok(())
	}

	fn resolve_label(&mut self, label: &'a str, bits: Range<u8>, offset: i8, flags: u8) {
		let p = PendingLabel {
			right_shift: bits.start,
			bits: bits.end - bits.start,
			location: self.ip - 4,
			flags,
			offset,
		};
		if let Some(loc) = self.known_labels.get(label).copied() {
			self.set_pending(p, loc);
		} else {
			self.pending_labels.entry(label).or_default().push(p);
		}
	}

	fn insert_label(&mut self, label: &'a str) {
		let prev = self.known_labels.insert(label, self.ip);
		assert!(prev.is_none());
		for p in self.pending_labels.remove(label).unwrap_or_default() {
			self.set_pending(p, self.ip);
		}
	}

	fn set_pending(&mut self, pending: PendingLabel, value: u32) {
		let value = match pending.flags & PendingLabel::RELATIVE != 0 {
			true => value.wrapping_sub(pending.location),
			false => value,
		};
		let mask = (1 << pending.bits) - 1;
		let value = (value >> pending.right_shift) & mask;
		let value = value.wrapping_add(pending.offset as u32);
		self.memory[usize::try_from(pending.location / 4).unwrap()] |= value;
	}

	fn translate_register(reg: &str) -> Result<u32> {
		let reg = reg.trim();
		if reg.bytes().next() != Some(b'$') {
			return Err(AssembleError::ExpectedRegister);
		}
		// Stupid? Yes.
		// Easiest? Also yes.
		// I bet it's the fastest too ;)
		Ok(match &reg.as_bytes()[1..] {
			b"zero" | b"0" => 0,
			b"at" | b"1" => 1,
			b"v0" | b"2" => 2,
			b"v1" | b"3" => 3,
			b"a0" | b"4" => 4,
			b"a1" | b"5" => 5,
			b"a2" | b"6" => 6,
			b"a3" | b"7" => 7,
			b"t0" | b"8" => 8,
			b"t1" | b"9" => 9,
			b"t2" | b"10" => 10,
			b"t3" | b"11" => 11,
			b"t4" | b"12" => 12,
			b"t5" | b"13" => 13,
			b"t6" | b"14" => 14,
			b"t7" | b"15" => 15,
			b"s0" | b"16" => 16,
			b"s1" | b"17" => 17,
			b"s2" | b"18" => 18,
			b"s3" | b"19" => 19,
			b"s4" | b"20" => 20,
			b"s5" | b"21" => 21,
			b"s6" | b"22" => 22,
			b"s7" | b"23" => 23,
			b"t8" | b"24" => 24,
			b"t9" | b"25" => 25,
			b"k0" | b"26" => 26,
			b"k1" | b"27" => 27,
			b"gp" | b"28" => 28,
			b"sp" | b"29" => 29,
			b"s8" | b"30" => 30,
			b"ra" | b"31" => 31,
			_ => return Err(AssembleError::ExpectedRegister),
		})
	}

	fn ensure_no_args<R>(mut args: impl Iterator<Item = &'a str>, ret: R) -> Result<'a, R> {
		args.next()
			.is_none()
			.then(|| ret)
			.ok_or(AssembleError::UnexpectedArgument)
	}

	fn decode_3_regs(args: &'a str) -> Result<'a, [u32; 3]> {
		let mut a = args.split(',');
		let ok = [
			Self::translate_register(a.next().unwrap_or(""))?,
			Self::translate_register(a.next().unwrap_or(""))?,
			Self::translate_register(a.next().unwrap_or(""))?,
		];
		Self::ensure_no_args(a, ok)
	}

	fn decode_2_regs(args: &'a str) -> Result<'a, [u32; 2]> {
		let mut a = args.split(',');
		let ok = [
			Self::translate_register(a.next().unwrap_or(""))?,
			Self::translate_register(a.next().unwrap_or(""))?,
		];
		Self::ensure_no_args(a, ok)
	}

	fn decode_1_reg(args: &'a str) -> Result<'a, u32> {
		let mut a = args.split(',');
		let ok = Self::translate_register(a.next().unwrap_or(""))?;
		Self::ensure_no_args(a, ok)
	}

	fn decode_2_regs_1_imm(args: &'a str) -> Result<'a, (u32, u32, &'a str)> {
		let mut a = args.split(',');
		let ok = (
			Self::translate_register(a.next().unwrap_or(""))?,
			Self::translate_register(a.next().unwrap_or(""))?,
			a.next()
				.ok_or(AssembleError::ExpectedImmediate)?
				.trim_start(),
		);
		Self::ensure_no_args(a, ok)
	}

	fn decode_2_regs_1_offset(args: &'a str) -> Result<'a, [u32; 3]> {
		let mut a = args.split(',');
		let d = Self::translate_register(a.next().unwrap_or(""))?;
		let (reg, offt) = Self::decode_reg_offset(a.next().unwrap_or(""))?;
		Self::ensure_no_args(a, [d, reg, offt])
	}

	fn decode_1_reg_1_imm(args: &'a str) -> Result<'a, (u32, &'a str)> {
		let mut a = args.split(',');
		let ok = (
			Self::translate_register(a.next().unwrap_or(""))?,
			a.next()
				.ok_or(AssembleError::ExpectedImmediate)?
				.trim_start(),
		);
		Self::ensure_no_args(a, ok)
	}

	fn decode_reg_offset(arg: &'a str) -> Result<'a, (u32, u32)> {
		let (offset, reg) = arg.split_once('(').ok_or(AssembleError::ExpectedOffset)?;
		assert_eq!(reg.chars().last(), Some(')'));
		Ok((
			Self::translate_register(&reg[..reg.len() - 1])?,
			parse_int::parse(offset).map_err(|_| AssembleError::ExpectedImmediate)?,
		))
	}

	fn parse_arithlog(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let [d, t, s] = Self::decode_3_regs(args)?;
		self.push_r(s, t, d, 0, function)
	}

	fn parse_divmult(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let [s, t] = Self::decode_2_regs(args)?;
		self.push_r(s, t, 0, 0, function)
	}

	fn parse_shift(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let (d, t, imm) = Self::decode_2_regs_1_imm(args)?;
		let imm = parse_int::parse(imm).map_err(|_| AssembleError::ExpectedImmediate)?;
		self.push_r(imm, t, d, 0, function)
	}

	fn parse_shiftv(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let [d, t, s] = Self::decode_3_regs(args)?;
		self.push_r(s, t, d, 0, function)
	}

	fn parse_arithlogi(&mut self, op: Op, args: &'a str) -> Result<'a> {
		let (t, s, imm) = Self::decode_2_regs_1_imm(args)?;
		let imm = parse_int::parse(imm).map_err(|_| AssembleError::ExpectedImmediate)?;
		assert!(imm <= u32::from(u16::MAX));
		self.push_i(op, s, t, imm)
	}

	fn parse_branch(&mut self, op: Op, args: &'a str) -> Result<'a> {
		let (t, s, imm) = Self::decode_2_regs_1_imm(args)?;
		self.push_i_label(op, s, t, imm, 2..18, -1, true)
	}

	fn parse_branchz(&mut self, op: Op, args: &'a str) -> Result<'a> {
		let (s, imm) = Self::decode_1_reg_1_imm(args)?;
		self.push_i_label(op, s, 0, imm, 2..18, -1, true)
	}

	fn parse_loadstore(&mut self, op: Op, args: &'a str) -> Result<'a> {
		let [t, s, offset] = Self::decode_2_regs_1_offset(args)?;
		assert!(offset <= u32::from(u16::MAX));
		self.push_i(op, s, t, offset)
	}

	fn parse_loadi(&mut self, op: Op, args: &'a str) -> Result<'a> {
		let (t, imm) = Self::decode_1_reg_1_imm(args)?;
		let imm = parse_int::parse(imm).map_err(|_| AssembleError::ExpectedImmediate)?;
		assert!(imm <= u32::from(u16::MAX));
		self.push_i(op, 0, t, imm)
	}

	fn parse_movefrom(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let d = Self::decode_1_reg(args)?;
		self.push_r(0, 0, d, 0, function)
	}

	fn parse_moveto(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let s = Self::decode_1_reg(args)?;
		self.push_r(s, 0, 0, 0, function)
	}

	fn parse_j(&mut self, op: Op, args: &str) -> Result<'a> {
		let offset = parse_int::parse(args.trim()).map_err(|_| AssembleError::ExpectedImmediate)?;
		assert!(offset <= 1 << 26);
		self.push_j(op, offset)
	}

	fn parse_j_label(&mut self, op: Op, args: &'a str) -> Result<'a> {
		self.push_j_label(op, args.trim(), 0..26, false)
	}

	fn parse_jump(&mut self, op: Op, args: &'a str) -> Result<'a> {
		self.push_j_label(op, args.trim(), 2..28, true)
	}

	fn parse_jumpr(&mut self, function: Function, args: &'a str) -> Result<'a> {
		let s = Self::decode_1_reg(args)?;
		self.push_r(s, 0, 0, 0, function)
	}

	fn parse_pseudo_li(&mut self, args: &'a str) -> Result<'a> {
		let (t, imm) = Self::decode_1_reg_1_imm(args)?;
		let imm = parse_int::parse::<i64>(imm).map_err(|_| AssembleError::ExpectedImmediate)?;
		if imm < i64::from(i32::MIN) || i64::from(u32::MAX) < imm {
			return Err(AssembleError::ImmediateTooLarge);
		}
		let imm = imm as u32;
		if imm >> 16 != 0 {
			self.push_i(Op::Lui, 0, t, imm >> 16)?;
			if imm & 0xffff != 0 {
				self.push_i(Op::Ori, t, t, imm & 0xffff)?;
			}
		} else {
			self.push_i(Op::Ori, 0, t, imm & 0xffff)?;
		}
		Ok(())
	}

	fn parse_pseudo_la(&mut self, args: &'a str) -> Result<'a> {
		let (t, imm) = Self::decode_1_reg_1_imm(args)?;
		self.push_i_label(Op::Lui, 0, t, imm, 16..32, 0, false)?;
		self.push_i_label(Op::Ori, t, t, imm, 0..16, 0, false)
	}

	fn parse_pseudo_move(&mut self, args: &'a str) -> Result<'a> {
		let [t, s] = Self::decode_2_regs(args)?;
		self.push_r(s, t, 0, 0, Function::Add)
	}

	fn parse_pseudo_branch(&mut self, branch: PseudoBranch, unsigned: bool, args: &'a str) -> Result<'a> {
		let (a, b, label) = Self::decode_2_regs_1_imm(args)?;
		let cmp_op = unsigned.then(|| Function::Sltu).unwrap_or(Function::Slt);
		let (a, b) = match branch {
			PseudoBranch::Blt | PseudoBranch::Bge => (a, b),
			PseudoBranch::Bgt | PseudoBranch::Ble => (b, a),
		};
		let eq_op = match branch {
			PseudoBranch::Blt | PseudoBranch::Bgt => Op::Bne,
			PseudoBranch::Ble | PseudoBranch::Bge => Op::Beq,
		};
		self.push_r(1, a, b, 0, cmp_op)?;
		self.push_i_label(eq_op, 1, 0, label, 2..18, -1, true)
	}

	fn parse_pseudo_abs(&mut self, args: &'a str) -> Result<'a> {
		let [t, s] = Self::decode_2_regs(args)?;
		self.push_i(Op::Lui, 0, 1, 0x7fff)?;
		self.push_i(Op::Ori, 1, 1, 0xffff)?;
		self.push_r(s, 1, t, 0, Function::And)
	}

	fn parse_ascii(&mut self, args: &'a str, zero_terminate: bool) -> Result<'a> {
		for b in snailquote::unescape(args)
			.map_err(|_| AssembleError::InvalidString)?
			.bytes()
		{
			self.push_byte(b)?;
		}
		if zero_terminate {
			self.push_byte(0)?;
		}
		Ok(())
	}

	fn parse_integer(&mut self, args: &'a str, length: u8) -> Result<'a> {
		let e = AssembleError::ExpectedImmediate;
		match length {
			1 => self.push_byte(parse_int::parse(args).map_err(|_| e)?)?,
			2 => self.push_half(parse_int::parse(args).map_err(|_| e)?)?,
			4 => self.push_word(parse_int::parse(args).map_err(|_| e)?)?,
			_ => unreachable!(),
		};
		Ok(())
	}

	pub fn assemble(source: &'a str, destination: &'b mut [u32]) -> Result<'a, SourceMap> {
		let mut slf = Self {
			known_labels: HashMap::default(),
			pending_labels: HashMap::default(),
			memory: destination,
			ip: 0,
			source_map: SourceMap::new(),
			last_line: 0,
		};

		for (i, line) in source.lines().enumerate() {
			slf.last_line = i as u32;

			// Remove any comments
			let line = line.split(';').next().unwrap();
			let line = line.split('#').next().unwrap();
			let line = line.split("//").next().unwrap();
			let line = line.trim();

			// Get the mnemonic
			// If this fails, the line is either empty or a label (or invalid)
			if let Some((mnem, args)) = line.trim().split_once(char::is_whitespace) {
				match mnem {
					"abs" => slf.parse_pseudo_abs(args),
					"add" => slf.parse_arithlog(Function::Add, args),
					"addu" => slf.parse_arithlog(Function::Addu, args),
					"addi" => slf.parse_arithlogi(Op::Addi, args),
					"addiu" => slf.parse_arithlogi(Op::Addiu, args),
					"and" => slf.parse_arithlog(Function::And, args),
					"andi" => slf.parse_arithlogi(Op::Andi, args),
					"div" => slf.parse_divmult(Function::Div, args),
					"divu" => slf.parse_divmult(Function::Divu, args),
					"move" => slf.parse_pseudo_move(args),
					"mult" => slf.parse_divmult(Function::Mult, args),
					"multu" => slf.parse_divmult(Function::Multu, args),
					"nor" => slf.parse_arithlog(Function::Nor, args),
					"or" => slf.parse_arithlog(Function::Or, args),
					"ori" => slf.parse_arithlogi(Op::Ori, args),
					"sll" => slf.parse_shift(Function::Sll, args),
					"sllv" => slf.parse_shiftv(Function::Sllv, args),
					"sra" => slf.parse_shift(Function::Sra, args),
					"srav" => slf.parse_shiftv(Function::Srav, args),
					"srl" => slf.parse_shift(Function::Srl, args),
					"srlv" => slf.parse_shiftv(Function::Srlv, args),
					"sub" => slf.parse_arithlog(Function::Sub, args),
					"subu" => slf.parse_arithlog(Function::Subu, args),
					"xor" => slf.parse_arithlog(Function::Xor, args),
					"xori" => slf.parse_arithlogi(Op::Xori, args),
					"lhi" => slf.parse_loadi(Op::Lhi, args),
					"llo" => slf.parse_loadi(Op::Llo, args),
					"slt" => slf.parse_arithlog(Function::Slt, args),
					"sltu" => slf.parse_arithlog(Function::Sltu, args),
					"slti" => slf.parse_arithlogi(Op::Slti, args),
					"sltiu" => slf.parse_arithlogi(Op::Sltiu, args),
					"beq" => slf.parse_branch(Op::Beq, args),
					"bgtz" => slf.parse_branchz(Op::Bgtz, args),
					"blez" => slf.parse_branchz(Op::Blez, args),
					"bne" => slf.parse_branch(Op::Bne, args),
					"blt" => slf.parse_pseudo_branch(PseudoBranch::Blt, false, args),
					"bgt" => slf.parse_pseudo_branch(PseudoBranch::Bgt, false, args),
					"ble" => slf.parse_pseudo_branch(PseudoBranch::Ble, false, args),
					"bge" => slf.parse_pseudo_branch(PseudoBranch::Bge, false, args),
					"bltu" => slf.parse_pseudo_branch(PseudoBranch::Blt, true, args),
					"bgtu" => slf.parse_pseudo_branch(PseudoBranch::Bgt, true, args),
					"bleu" => slf.parse_pseudo_branch(PseudoBranch::Ble, true, args),
					"bgeu" => slf.parse_pseudo_branch(PseudoBranch::Bge, true, args),
					"j" => slf.parse_jump(Op::J, args),
					"jr" => slf.parse_jumpr(Function::Jr, args),
					"lb" => slf.parse_loadstore(Op::Lb, args),
					"lbu" => slf.parse_loadstore(Op::Lbu, args),
					"lh" => slf.parse_loadstore(Op::Lh, args),
					"lhu" => slf.parse_loadstore(Op::Lhu, args),
					"lw" => slf.parse_loadstore(Op::Lw, args),
					"lui" => slf.parse_loadi(Op::Lui, args),
					"li" => slf.parse_pseudo_li(args),
					"la" => slf.parse_pseudo_la(args),
					"sb" => slf.parse_loadstore(Op::Sb, args),
					"sh" => slf.parse_loadstore(Op::Sh, args),
					"sw" => slf.parse_loadstore(Op::Sw, args),
					"mfhi" => slf.parse_movefrom(Function::Mfhi, args),
					"mflo" => slf.parse_movefrom(Function::Mflo, args),
					"mthi" => slf.parse_moveto(Function::Mthi, args),
					"mtlo" => slf.parse_moveto(Function::Mtlo, args),
					".ascii" => slf.parse_ascii(args, false),
					".asciiz" => slf.parse_ascii(args, true),
					".byte" => slf.parse_integer(args, 1),
					".half" => slf.parse_integer(args, 2),
					".word" => slf.parse_integer(args, 4),
					mnem => Err(AssembleError::UnknownOp(mnem)),
				}?;
			} else if line.chars().last() == Some(':') {
				let l = core::str::from_utf8(&line.as_bytes()[..line.len() - 1]).unwrap();
				slf.insert_label(l);
			} else if !line.is_empty() {
				return Err(AssembleError::InvalidLine(line));
			}
		}

		Ok(slf.source_map)
	}
}

#[derive(Debug)]
pub enum AssembleError<'a> {
	UnknownOp(&'a str),
	InvalidLine(&'a str),
	OutOfMemory,
	ExpectedRegister,
	ExpectedImmediate,
	ExpectedOffset,
	InvalidString,
	UnexpectedArgument,
	ImmediateTooLarge,
}

enum PseudoBranch {
	Blt,
	Ble,
	Bge,
	Bgt,
}
