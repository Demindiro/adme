import init, { Cpu, Mem, assemble } from "./js/adme.js";

(async () => {
	await init();

	cpu = new Cpu();
	mem = new Mem();
	assemble_asm = assemble;
	new_cpu = () => new Cpu();

	init_html();
})();
