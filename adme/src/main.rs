fn main() {
	let mut cpu = adme::Cpu::new();

	let mut mem = [0; 128];

	adme::assemble("
		# Calculates 8 in a convoluted way
		// hm yes
		addi	r1, r0, 1 ; Immediate shit
		add		r1, r1, r1
		add		r1, r1, r1
		add		r1, r1, r1
		add		r1, r1, r1
	", &mut mem);

	for _ in 0..6 {
		cpu.step(&mut mem).unwrap();
		dbg!(&cpu);
	}
}
