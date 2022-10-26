const output: number[] = [];
const display: any = new Uint8Array(10000);
const pixels: number[][] = [];

const bytecode = Uint8Array.from(Deno.args[0].split(' ').map(b => parseInt(b, 16)));
const memory = new WebAssembly.Memory({ initial: 1 });
const result = await WebAssembly.instantiate(bytecode, {
	env: {
		print: (d: number) => output.push(d),
		print2: (d: number) => output.push(d),
		printf: (d: number) => output.push(d),
		display,
		memory,
	},
});

(result.instance.exports.run as () => void)();
display.set(new Uint8Array(memory.buffer, 0, 10000));

// find any pixels that have been written to
display.forEach((value: number, index: number) => {
	if (value !== 0) {
		pixels.push([index, value]);
	}
});

console.log(output)
console.log(pixels)