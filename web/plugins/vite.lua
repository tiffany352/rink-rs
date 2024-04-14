Log.info("Running vite build")
Sys.run_program("cd repl; npm run build; cd ..")
Sys.run_program("cp repl/dist/assets -r " .. build_dir)

local files = Sys.list_dir("repl/dist/assets")
local i = 1
while files[i] do
	local path = files[i]
	if Sys.has_extension(path, "js") then
		local file = Sys.basename(path)
		local script = HTML.create_element("script")
		HTML.set_attribute(script, "src", "/assets/" .. file)
		HTML.append_child(page, script)
	end
	i = i + 1
end
