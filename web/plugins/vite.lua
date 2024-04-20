Log.info("Running vite build")
Sys.run_program("cd repl; npm run build; cd ..")

local immutable = Sys.join_path(build_dir, "immutable")
Sys.mkdir(immutable)
Sys.run_program("cp repl/dist/immutable/*.js " .. immutable)

-- use hashed filenames so that extremely aggressive caching can be set.
-- no idea why wasm-pack doesn't do this by default.
local wasm_path = "repl/dist/assets/rink_js_bg.wasm"
local wasm = Sys.read_file(wasm_path)
local long_hash = Digest.sha256(wasm)
local hash = strsub(long_hash, 1, 8)
local wasm_out = format("immutable/rink.%s.wasm", hash)
Sys.write_file(Sys.join_path(build_dir, wasm_out), wasm)

local files = Sys.list_dir("repl/dist/immutable")
local i = 1
while files[i] do
	local path = files[i]
	local file = Sys.basename(path)
	if String.starts_with(file, "index") then
		local script = HTML.create_element("script")
		HTML.set_attribute(script, "src", "/immutable/" .. file)
		HTML.set_attribute(script, "data-wasm", "/" .. wasm_out)
		HTML.set_attribute(script, "async", "")
		local head = HTML.select_one(page, "head")
		HTML.append_child(head, script)
	end
	i = i + 1
end
