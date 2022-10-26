const output: number[] = [];

const bytecode = Uint8Array.from(Deno.args[0].split(' ').map(b => parseInt(b, 16)));
const memory = new WebAssembly.Memory({ initial: 1 });
const result = await WebAssembly.instantiate(bytecode, {
	env: {
		print: (d: number) => output.push(d),
		print2: (d: number) => output.push(d),
		printf: (d: number) => output.push(d),
		memory,
	},
});

(result.instance.exports.run as () => void)();

console.log(output)