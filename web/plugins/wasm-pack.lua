local out_dir = "../web/build/rink-wasm"

if not Sys.is_dir(Sys.join_path(build_dir, "rink-wasm")) then
	Log.info("Building wasm (this is slow)")
	Sys.run_program("wasm-pack build ../rink-js --out-dir " .. out_dir)
end
