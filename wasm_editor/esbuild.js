const fs = require('fs');

console.log('Building entry.ts');
require("esbuild").build({
	entryPoints: ['entry.ts'],
	bundle: true,
	outdir: 'dist',
	sourcemap: true,
})
	.then(() => {
		console.log('Copying html and wasm');
		fs.copyFileSync('./index.html', './dist/index.html')
		fs.copyFileSync('./pkg/moly_bg.wasm', './dist/moly_bg.wasm')
	})
	.catch(() => process.exit(1))