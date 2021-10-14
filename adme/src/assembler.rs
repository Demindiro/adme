use crate::util::as_u8_mut;
use crate::interpreter::{Op, Function};
use std::collections::HashMap;
use core::ops::Range;

struct PendingLabel {
	left_shift: u8,
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
}

impl<'a, 'b> Assembler<'a, 'b> {
	fn push_byte(&mut self, byte: u8) {
		let mem = as_u8_mut(self.memory);
		mem[usize::try_from(self.ip).unwrap()] = byte;
		self.ip = self.ip.wrapping_add(1);
	}

	fn push_word(&mut self, word: u32) {
		self.memory[usize::try_from(self.ip / 4).unwrap()] = word;
		self.ip = self.ip.wrapping_add(4);
	}

	fn align_ip(&mut self) {
		self.ip = self.ip.wrapping_add(3) & !0x3;
	}

	fn push_r(&mut self, op: Op, s: u32, t: u32, d: u32, sa: u32, function: Function) {
		self.align_ip();
		let i = (op as u32) << 26 | s << 21 | t << 16 | d << 11 | sa << 6 | function as u32;
		self.push_word(i);
	}

	fn push_i(&mut self, op: Op, s: u32, t: u32, imm: u32) {
		self.align_ip();
		self.push_word((op as u32) << 26 | s << 21 | t << 16 | imm)
	}

	fn push_i_label(&mut self, op: Op, s: u32, t: u32, label: &'a str, bits: Range<u8>, offset: i8, relative: bool) {
		self.align_ip();
		self.push_word((op as u32) << 26 | s << 21 | t << 16);
		self.resolve_label(label, bits, offset, u8::from(relative) * PendingLabel::RELATIVE);
	}

	fn push_j(&mut self, op: Op, imm: u32) {
		self.align_ip();
		self.push_word((op as u32) << 26 | imm);
	}

	fn push_j_label(&mut self, op: Op, label: &'a str, offset: i8) {
		self.align_ip();
		self.push_word((op as u32) << 26);
		self.resolve_label(label, 0..26, offset, PendingLabel::MUST_FIT);
	}

	fn resolve_label(&mut self, label: &'a str, bits: Range<u8>, offset: i8, flags: u8) {
		let p = PendingLabel {
			left_shift: bits.start,
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
		let value = if pending.flags & PendingLabel::RELATIVE == 0 {
			value
		} else {
			value.wrapping_sub(pending.location)
		};
		let mask = (1 << pending.bits) - 1;
		let value = (value & mask) << pending.left_shift;
		let value = value.wrapping_add(pending.offset as u32);
		self.memory[usize::try_from(pending.location / 4).unwrap()] |= value;
	}

	fn translate_register(reg: &str) -> Option<u8> {
		// Stupid? Yes.
		// Easiest? Also yes.
		// I bet it's the fastest too ;)
		Some(match reg {
			"zero" | "$0" => 0,
			"AT" | "$at" | "$1" => 1,
			"v0" | "$2" => 2,
			"v1" | "$3" => 3,
			"a0" | "$4" => 4,
			"a1" | "$5" => 5,
			"a2" | "$6" => 6,
			"a3" | "$7" => 7,
			"t0" | "$8" => 8,
			"t1" | "$9" => 9,
			"t2" | "$10" => 10,
			"t3" | "$11" => 11,
			"t4" | "$12" => 12,
			"t5" | "$13" => 13,
			"t6" | "$14" => 14,
			"t7" | "$15" => 15,
			"s0" | "$16" => 16,
			"s1" | "$17" => 17,
			"s2" | "$18" => 18,
			"s3" | "$19" => 19,
			"s4" | "$20" => 20,
			"s5" | "$21" => 21,
			"s6" | "$22" => 22,
			"s7" | "$23" => 23,
			"t8" | "$24" => 24,
			"t9" | "$25" => 25,
			"kt0" | "$26" => 26,
			"kt1" | "$27" => 27,
			"gp" | "$gp" | "$28" => 28,
			"sp" | "$sp" | "$29" => 29,
			"s8" | "$30" => 30,
			"ra" | "$31" => 31,
			_ => return None,
		})
	}

	fn decode_3_regs(args: &str) -> [u32; 3] {
		let mut a = args.split(',');
		let s = [
			a.next().unwrap().trim_start(),
			a.next().unwrap().trim_start(),
			a.next().unwrap().trim_start(),
		];
		[
			Self::translate_register(s[0]).unwrap().into(),
			Self::translate_register(s[1]).unwrap().into(),
			Self::translate_register(s[2]).unwrap().into(),
		]
	}

	fn decode_2_regs_1_imm(args: &str) -> (u32, u32, &str) {
		let mut a = args.split(',');
		let s = [
			a.next().unwrap().trim_start(),
			a.next().unwrap().trim_start(),
			a.next().unwrap().trim_start(),
		];
		(
			Self::translate_register(s[0]).unwrap().into(),
			Self::translate_register(s[1]).unwrap().into(),
			s[2],
		)
	}

	fn decode_2_regs_1_offset(args: &str) -> [u32; 3] {
		let mut a = args.split(',');
		let s = [
			a.next().unwrap().trim_start(),
			a.next().unwrap().trim_start(),
		];
		let (reg, offt) = Self::decode_reg_offset(s[1]);
		[
			Self::translate_register(s[0]).unwrap().into(),
			reg,
			offt,
		]
	}

	fn decode_1_reg_1_imm(args: &str) -> (u32, &str) {
		let mut a = args.split(',');
		let s = [
			a.next().unwrap().trim_start(),
			a.next().unwrap().trim_start(),
		];
		(
			Self::translate_register(s[0]).unwrap().into(),
			s[1],
		)
	}

	fn decode_reg_offset(arg: &str) -> (u32, u32) {
		let (offset, reg) = arg.split_once('(').unwrap();
		assert_eq!(reg.chars().last(), Some(')'));
		(
			Self::translate_register(&reg[..reg.len() - 1]).unwrap().into(),
			offset.parse().unwrap(),
		)
	}

	fn parse_r(&mut self, op: Op, function: Function, args: &str) {
		let [d, t, s] = Self::decode_3_regs(args);
		self.push_r(op, s, t, d, 0, function)
	}

	fn parse_i_2r(&mut self, op: Op, args: &str) {
		let (t, s, imm) = Self::decode_2_regs_1_imm(args);
		let imm = imm.parse().unwrap();
		assert!(imm <= u32::from(u16::MAX));
		self.push_i(op, s, t, imm)
	}

	fn parse_i_2r_label(&mut self, op: Op, args: &'a str, offset: i8) {
		let (t, s, imm) = Self::decode_2_regs_1_imm(args);
		self.push_i_label(op, s, t, imm, 0..16, offset, true)
	}

	fn parse_i_2r_offset(&mut self, op: Op, args: &str) {
		let [t, s, offset] = Self::decode_2_regs_1_offset(args);
		assert!(offset <= u32::from(u16::MAX));
		self.push_i(op, s, t, offset)
	}

	fn parse_i_1r(&mut self, op: Op, args: &str) {
		let (t, imm) = Self::decode_1_reg_1_imm(args);
		let imm = imm.parse().unwrap();
		assert!(imm <= u32::from(u16::MAX));
		self.push_i(op, 0, t, imm)
	}

	fn parse_j(&mut self, op: Op, args: &str) {
		let offset = args.trim().parse().unwrap();
		assert!(offset <= 1 << 26);
		self.push_j(op, offset)
	}

	fn parse_j_label(&mut self, op: Op, args: &'a str, offset: i8) {
		self.push_j_label(op, args.trim(), offset)
	}

	fn parse_pseudo_li(&mut self, args: &'a str) {
		let (t, imm) = Self::decode_1_reg_1_imm(args);
		if let Ok(imm) = imm.parse::<u32>() {
			self.push_i(Op::Lui, 0, t, imm >> 16);
			self.push_i(Op::Ori, t, t, imm & 0xffff);
		} else {
			self.push_i_label(Op::Lui, 0, t, imm, 16..32, 0, false);
			self.push_i_label(Op::Ori, t, t, imm,  0..16, 0, false);
		}
	}

	fn parse_ascii(&mut self, args: &'a str, zero_terminate: bool) {
		snailquote::unescape(args).unwrap().bytes().for_each(|b| self.push_byte(b));
		zero_terminate.then(|| self.push_byte(0));
	}

	pub fn assemble(source: &'a str, destination: &'b mut [u32]) {
		let mut slf = Self {
			known_labels: HashMap::default(),
			pending_labels: HashMap::default(),
			memory: destination,
			ip: 0,
		};

		for line in source.lines() {
			// Remove any comments
			let line = line.split(';').next().unwrap();
			let line = line.split('#').next().unwrap();
			let line = line.split("//").next().unwrap();
			let line = line.trim();

			// Get the mnemonic
			// If this fails, the line is either empty or a label (or invalid)
			if let Some((mnem, args)) = line.trim().split_once(char::is_whitespace) {
				match mnem {
					"add" => slf.parse_r(Op::Function, Function::Add, args),
					"addi" => slf.parse_i_2r(Op::Addi, args),
					"beq" => slf.parse_i_2r_label(Op::Beq, args, -4),
					"bne" => slf.parse_i_2r_label(Op::Bne, args, -4),
					"j" => slf.parse_j_label(Op::J, args, 0),
					"lb" => slf.parse_i_2r_offset(Op::Lb, args),
					"lbu" => slf.parse_i_2r_offset(Op::Lbu, args),
					"lw" => slf.parse_i_2r_offset(Op::Lw, args),
					"lui" => slf.parse_i_1r(Op::Lui, args),
					"li" => slf.parse_pseudo_li(args),
					"sb" => slf.parse_i_2r_offset(Op::Sb, args),
					".ascii" => slf.parse_ascii(args, false),
					".asciz" => slf.parse_ascii(args, true),
					op => todo!("{}", op),
				};
			} else if line.chars().last() == Some(':') {
				let l = core::str::from_utf8(&line.as_bytes()[..line.len() - 1]).unwrap();
				slf.insert_label(l);
			} else if !line.is_empty() {
				todo!("handle invalid line");
			}
		}
	}
}
