import {assertEquals} from "https://deno.land/std@0.160.0/testing/asserts.ts";

Deno.test("an empty program", async () => {
	const output = await executeCode();

	assertEquals(output, [])
})

Deno.test("a print statement", async () => {
	const output = await executeCode();

	assertEquals(output, [8])
})

export async function executeCode(): Promise<number[]> {
	const output: number[] = [];

	const bytecode = Uint8Array.from(Deno.args[0].split(' ').map(b => parseInt(b, 16)));
	const result = await WebAssembly.instantiate(bytecode, {
		env: {
			print: (d: number) => output.push(d),
			print2: (d: number) => output.push(d),
			printf: (d: number) => output.push(d),
		}
	});

	(result.instance.exports.run as any)();

	return output;
}