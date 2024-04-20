import { defineConfig } from "vite";
import wasmPack from "vite-plugin-wasm-pack";

export default defineConfig({
	build: {
		minify: false,
		assetsDir: "immutable",
	},
	plugins: [wasmPack("../../rink-js")],
});
