Log.info("Running vite build")
Sys.run_program("cd repl; npm run build; cd ..")
Sys.run_program("cp repl/dist/assets -r " .. build_dir)

local files = Sys.list_dir("repl/dist/assets")
local i = 1
while files[i] do
	local path = files[i]
	local file = Sys.basename(path)
	if String.starts_with(file, "index") then
		local script = HTML.create_element("script")
		HTML.set_attribute(script, "src", "/assets/" .. file)
		HTML.set_attribute(script, "async", "")
		local head = HTML.select_one(page, "head")
		HTML.append_child(head, script)
	end
	i = i + 1
end
