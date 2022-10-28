const fs = require('fs');

require("esbuild").build({
	entryPoints: ['entry.ts'],
	bundle: true,
	outdir: 'dist',
	sourcemap: true,
})
	.then(() => {
		fs.copyFileSync('./index.html', './dist/index.html');
		fs.copyFileSync('./pkg/moly_bg.wasm', './dist/moly_bg.wasm');
	})
	.catch(err => {
		console.error(err);
		process.exit(1)
	})