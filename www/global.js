let cpu;
let mem;
let assemble_asm;
let new_cpu;
let source_map;

let cpu_steps;
let cpu_ip;
let cpu_gp = [undefined];
let cpu_hi;
let cpu_lo;
let cpu_hz;

let asm_input;
let asm_input_highlight;
let asm_input_backdrop;

let memory;

let run_interval;
let run_hz = 8;

// setInterval delay cannot be arbitrarily low - assume 64 Hz is reasonable.
// We're also using powers of 2 for herts, so dividing by this value makes things easy.
const MIN_HZ = 64;

const MEM_WIDTH = 64;
const MEM_HEIGHT = 32;

function hex(n, pad) {
	return n.toString(16).padStart(pad, '0');
}

function serial_out(v) {
	v = String.fromCharCode(v);
	document.getElementById('serial').innerHTML += v;
};

function serial_clear() {
	document.getElementById('serial').innerHTML = '';
}

function error(err) {
	document.getElementById('error').innerHTML = err;
}

function step(manual = false) {
	try {
		if (manual) {
			cpu.step(mem);
		} else {
			for (let i = 0; i < Math.max(1, run_hz / MIN_HZ); i++)
				cpu.step(mem);
		}
	} catch (e) {
		error(e);
	}
	update_stats();
}

function run(e) {
	e.innerHTML = 'Stop';
	e.onclick = () => stop(e);
	run_interval = setInterval(step, 1000 / Math.min(run_hz, MIN_HZ));
}

function stop(e) {
	e.innerHTML = 'Run';
	e.onclick = () => run(e);
	clearInterval(run_interval);
	run_interval = undefined;
}

function set_run_hz(e) {
	run_hz = 1 << e.value;
	if (run_interval !== undefined) {
		clearInterval(run_interval);
		run_interval = setInterval(step, 1000 / Math.min(run_hz, MIN_HZ));
	}
	if (run_hz >= 1000 * 1000) {
		cpu_hz.innerHTML = (run_hz >> 20) + ' MiHz';
	} else if (run_hz >= 1000) {
		cpu_hz.innerHTML = (run_hz >> 10) + ' KiHz';
	} else {
		cpu_hz.innerHTML = run_hz + ' Hz';
	}
}

function update_stats() {

	// CPU
	document.getElementById('cpu_steps').innerHTML = cpu.steps;
	cpu_ip.innerHTML = '0x' + hex(cpu.ip, 8);

	for (let i = 1; i < 32; i++) {
		cpu_gp[i].innerHTML = '0x' + hex(cpu.gp(i), 8);
	}
	cpu_hi.innerHTML = '0x' + hex(cpu.hi, 8);
	cpu_lo.innerHTML = '0x' + hex(cpu.lo, 8);

	// Memory
	const D = 4;

	let t = '     <b>'
	for (let i = MEM_WIDTH - D; i >= 0; i -= D) {
		t += ' ' + hex(i, 2) + '      ';
	}
	t += '</b>\n';

	let n = '<b>0000</b>';
	for (let i = 0; i < MEM_HEIGHT * MEM_WIDTH; i += D) {
		let v = hex(mem.get_u32(i), 2 * D);
		if (i == cpu.ip) {
			v = '<mark>' + v + '</mark>';
		}
		n = v + ' ' + n;
		if (i % MEM_WIDTH === MEM_WIDTH - D) {
			t += n + '\n';
			n = '<b>' + hex(i + D, 4) + '</b>';
		}
	}
	memory.innerHTML = t;

	// Source map
	let line = source_map && source_map.get_line(cpu.ip);
	let total_lines = asm_input.value.split('\n').length - 1;
	asm_input_highlight.innerHTML = line !== undefined
		// Add a couple extra lines to compensate for horizontal scroll bar
		? '\n'.repeat(line) + '<mark> </mark>' + '\n'.repeat(total_lines - line + 3)
		: '';
}

function assemble() {
	source_map = assemble_asm(asm_input.value, mem);
	console.log(source_map);
	cpu = new_cpu();
	update_stats();
	serial_clear();
	error('');
}

function init_html() {
	let stats = document.getElementById('cpu_stats');

	for (let i = 0; i < 32; i += 2) {
		let tr = document.createElement('tr');
		for (let k = i; k < i + 2; k++) {
			let th = document.createElement('th');
			let td = document.createElement('td');
			if (k > 0) {
				th.innerHTML = '$' + k;
				cpu_gp.push(td);
			} else {
				th.innerHTML = 'IP';
				cpu_ip = td;
			}
			tr.appendChild(th);
			tr.appendChild(td);
		}
		stats.appendChild(tr);
	}

	let tr = document.createElement('tr');

	let th = document.createElement('th');
	cpu_hi = document.createElement('td');
	th.innerHTML = 'HI';
	tr.appendChild(th);
	tr.appendChild(cpu_hi);
	stats.appendChild(tr);

	th = document.createElement('th');
	cpu_lo = document.createElement('td');
	th.innerHTML = 'LO';
	tr.appendChild(th);
	tr.appendChild(cpu_lo);
	stats.appendChild(tr);

	cpu_hz = document.getElementById('cpu_hz');
	cpu_hz.innerHTML = (1 << 3) + ' Hz';

	memory = document.getElementById('memory');

	asm_input = document.getElementById('code_input');
	asm_input_highlight = document.getElementById('code_highlight');
	asm_input_backdrop = document.getElementById('code_backdrop');

	// Stuff for highlighting assembly lines from source map
	asm_input.onscroll = () => {
		asm_input_backdrop.scrollTop = asm_input.scrollTop;
		asm_input_backdrop.scrollLeft = asm_input.scrollLeft;
	};

	assemble();
}
