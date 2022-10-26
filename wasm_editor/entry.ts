import {basicSetup, EditorView} from "codemirror";
import {moly} from "./lezerGrammar"
import init, {eval_wasm} from "./pkg";

let inputView: EditorView;
let outputView: EditorView;

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

	const memory = new WebAssembly.Memory({initial: 1});
	const output = [];
	const result = await WebAssembly.instantiate(bytecode_buffer, {
		env: {
			print: (d) => output.push(d),
			print2: (d) => output.push(d),
			printf: (d) => output.push(d),
			memory,
		},
	});

	(result.instance.exports.run)();

	console.log(output)
	document.getElementById('result').textContent = `Output: ${output}`
}

