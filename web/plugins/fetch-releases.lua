local feed_file = Sys.join_path(build_dir, "releases.json")
local feed_url = "https://api.github.com/repos/tiffany352/rink-rs/releases"
local command = "cmark-gfm --smart --unsafe"

if not Sys.file_exists(feed_file) then
	Sys.run_program("curl " .. feed_url .. " -o " .. feed_file)
end

local json = Sys.read_file(feed_file)
local releases = JSON.from_string(json)

local h_feed = HTML.select_one(page, ".h-feed")

local i = 1
while releases[i] do
	local release = releases[i]
	local h2 = HTML.create_element("h2", release.name)

	local nice_published = Date.reformat(release.published_at, { "%Y-%m-%dT%H:%M:%S" }, "%B %d, %Y")
	local published = HTML.create_element("time", nice_published)
	HTML.set_attribute(published, "datetime", release.published_at)
	HTML.set_attribute(published, "class", "dt-published")

	local meta = HTML.create_element("span")
	HTML.set_attribute(meta, "class", "meta")
	HTML.append_child(meta, published)

	local body_content = Sys.get_program_output(command, release.body)
	local body = HTML.parse(body_content)

	local headers = HTML.select(body, "h2")
	local j = 1
	while headers[j] do
		HTML.set_tag_name(headers[j], "h3")
		j = j + 1
	end

	local content = HTML.create_element("section")
	HTML.append_child(content, body)
	HTML.set_attribute(content, "class", "e-content")

	local article = HTML.create_element("article")
	HTML.set_attribute(article, "class", "h-entry")
	HTML.append_child(article, h2)
	HTML.append_child(article, meta)
	HTML.append_child(article, content)

	HTML.append_child(h_feed, article)
	HTML.append_child(h_feed, HTML.create_element("hr"))

	i = i + 1
end
