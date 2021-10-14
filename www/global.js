let cpu;
let mem;
let assemble_asm;
let new_cpu;

let cpu_steps;
let cpu_ip;
let cpu_gp = [undefined];
let cpu_hz;

let asm_input;

let memory;

let run_interval;
let run_hz = 1;

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

function step() {
	try {
		for (let i = 0; i < Math.max(1, run_hz / MIN_HZ); i++)
			cpu.step(mem);
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
	document.getElementById('cpu_steps').innerHTML = cpu.steps;
	cpu_ip.innerHTML = '0x' + hex(cpu.ip, 8);

	for (let i = 1; i < 32; i++) {
		cpu_gp[i].innerHTML = '0x' + hex(cpu.gp(i), 8);
	}

	const D = 4;

	let t = '     <b>'
	for (let i = MEM_WIDTH - D; i >= 0; i -= D) {
		t += ' ' + hex(i, 2) + '      ';
	}
	t += '</b>\n';

	let n = '<b>0000</b>';
	for (let i = 0; i < MEM_HEIGHT * MEM_WIDTH; i += D) {
		n = hex(mem.get_u32(i), 2 * D) + ' ' + n;
		if (i % MEM_WIDTH === MEM_WIDTH - D) {
			t += n + '\n';
			n = '<b>' + hex(i + D, 4) + '</b>';
		}
	}
	memory.innerHTML = t;
}

function assemble() {
	assemble_asm(asm_input.innerHTML, mem);
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

	cpu_hz = document.getElementById('cpu_hz');
	cpu_hz.innerHTML = (1 << 3) + ' Hz';

	memory = document.getElementById('memory');
	let t = '';
	for (let i = 0; i < MEM_HEIGHT * MEM_WIDTH; i++) {
		t += '00 ';
		if (i % MEM_WIDTH === MEM_WIDTH - 1) {
			t += '\n';
		}
	}
	memory.innerHTML = t;

	asm_input = document.getElementById('code');

	update_stats();
}
