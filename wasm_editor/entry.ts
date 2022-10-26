import {basicSetup, EditorView} from "codemirror";
import {moly} from "./lezerGrammar"
import init, {eval_wasm} from "./pkg";

let inputView: EditorView;
let outputView: EditorView;
let canvas: HTMLCanvasElement;

window.addEventListener('load', () => {
	inputView = new EditorView({
		doc: "print(1 + 2)\n",
		extensions: [basicSetup, moly()],
		parent: document.getElementById('editor'),
	});

	outputView = new EditorView({
		doc: "output\n",
		extensions: [basicSetup],
		parent: document.getElementById('output'),
	});

	document.getElementById('run').onclick = () => runCode();
	canvas = document.getElementById('canvas') as HTMLCanvasElement;
});

async function runCode() {
	await init('./pkg/moly_bg.wasm');

	const {bytecode, wat} = eval_wasm(inputView.state.doc.toString());
	const bytecode_buffer = Uint8Array.from(bytecode)

	outputView.destroy();
	outputView = new EditorView({
		doc: wat,
		extensions: [basicSetup],
		parent: document.getElementById('output'),
	});

	const display = new Uint8Array(10000);
	const output = [];

	const memory = new WebAssembly.Memory({initial: 1});
	const result = await WebAssembly.instantiate(bytecode_buffer, {
		env: {
			print: (d) => output.push(d),
			print2: (d) => output.push(d),
			printf: (d) => output.push(d),
			memory,
			display: (display as any),
		},
	});

	(result.instance.exports.run as () => void)();

	display.set(new Uint8Array(memory.buffer, 0, 10000));

	updateCanvas(display);

	console.log(output)
	document.getElementById('result').textContent = `Output: ${output}`
}

function updateCanvas(display: Uint8Array) {
	const context = canvas.getContext("2d");
	const imgData = context.createImageData(100, 100);
	for (let i = 0; i < 100 * 100; i++) {
		imgData.data[i * 4] = display[i];
		imgData.data[i * 4 + 1] = display[i];
		imgData.data[i * 4 + 2] = display[i];
		imgData.data[i * 4 + 3] = 255;
	}
	const data = scaleImageData(imgData, 3, context);
	context.putImageData(data, 0, 0);
}

const scaleImageData = (
	imageData: ImageData,
	scale: number,
	ctx: CanvasRenderingContext2D
) => {
	const scaled = ctx.createImageData(
		imageData.width * scale,
		imageData.height * scale
	);
	const subLine = ctx.createImageData(scale, 1).data;
	for (let row = 0; row < imageData.height; row++) {
		for (let col = 0; col < imageData.width; col++) {
			const sourcePixel = imageData.data.subarray(
				(row * imageData.width + col) * 4,
				(row * imageData.width + col) * 4 + 4
			);
			for (let x = 0; x < scale; x++) subLine.set(sourcePixel, x * 4);
			for (let y = 0; y < scale; y++) {
				const destRow = row * scale + y;
				const destCol = col * scale;
				scaled.data.set(subLine, (destRow * scaled.width + destCol) * 4);
			}
		}
	}
	return scaled;
};