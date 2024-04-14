local feed_file = Sys.join_path(build_dir, "releases.json")
local feed_url = "https://api.github.com/repos/tiffany352/rink-rs/releases"
command = "cmark-gfm --smart -e autolink"

if not Sys.file_exists(feed_file) then
	Sys.run_program("curl " .. feed_url .. " -o " .. feed_file)
end

local json = Sys.read_file(feed_file)
local releases = JSON.from_string(json)

h_feed = HTML.select_one(page, ".h-feed")

function humanize_bytes(bytes)
	local value = bytes
	local suffix = "bytes"
	if bytes > 500000 then
		value = value / 1000000
		suffix = "MB"
	elseif bytes > 1500 then
		value = value / 1000
		suffix = "KB"
	end
	return format("%.2f %s", value, suffix)
end

function build_downloads(assets)
	if not assets or not assets[1] then
		return nil
	end

	local downloads = HTML.create_element("section")

	HTML.append_child(downloads, HTML.create_element("h3", "Downloads"))
	local dl_ul = HTML.create_element("ul")
	j = 1
	while assets[j] do
		local asset = assets[j]
		local li = HTML.create_element("li")
		local a = HTML.create_element("a", asset.name)
		HTML.set_attribute(a, "href", asset.browser_download_url)
		HTML.set_attribute(a, "download", asset.name)
		HTML.append_child(li, a)
		local text = format(" (%s)", humanize_bytes(asset.size))
		HTML.append_child(li, HTML.create_text(text))
		HTML.append_child(dl_ul, li)
		j = j + 1
	end
	HTML.append_child(downloads, dl_ul)
	return downloads
end

function build_description(markdown_text)
	-- run markdown processor
	local body_content = Sys.get_program_output(command, markdown_text)

	-- match issue names like #123
	body_content = Sys.get_program_output(
		"sed -r 's,#([0-9]+),<a href=\"https://github.com/tiffany352/rink-rs/issues/\\1\">#\\1</a>,g'",
		body_content
	)

	-- match username handles like @tiffany352
	body_content = Sys.get_program_output(
		"sed -r 's,@([a-zA-Z0-9_]+),<a href=\"https://github.com/\\1\">@\\1</a>,g'",
		body_content
	)

	-- parse html & shrink headings
	local body = HTML.parse(body_content)
	local headers = HTML.select(body, "h2")
	local j = 1
	while headers[j] do
		HTML.set_tag_name(headers[j], "h3")
		j = j + 1
	end

	-- shorten github links
	local links = HTML.select(body, "a")
	j = 1
	while links[j] do
		local repo = "https://github.com/tiffany352/rink-rs/"
		local text = HTML.inner_text(links[j])
		local issues = repo .. "issues"
		local pulls = repo .. "pull"
		if String.starts_with(text, issues) then
			text = "#" .. strsub(text, strlen(issues) + 2)
			local node = HTML.create_text(text)
			HTML.replace_content(links[j], node)
		elseif String.starts_with(text, repo .. "pull") then
			text = "#" .. strsub(text, strlen(pulls) + 2)
			local node = HTML.create_text(text)
			HTML.replace_content(links[j], node)
		end
		j = j + 1
	end

	return body
end

function add_release(release)
	local nice_published = Date.reformat(release.published_at, { "%Y-%m-%dT%H:%M:%S" }, "%B %d, %Y")
	local published = HTML.create_element("time", nice_published)
	HTML.set_attribute(published, "datetime", release.published_at)
	HTML.set_attribute(published, "class", "dt-published")

	local meta = HTML.create_element("span")
	HTML.set_attribute(meta, "class", "meta")
	HTML.append_child(meta, published)

	local content = HTML.create_element("section")
	local body = build_description(release.body)
	HTML.append_child(content, body)
	HTML.set_attribute(content, "class", "e-content")

	local article = HTML.create_element("article")

	local downloads = build_downloads(release.assets)

	HTML.set_attribute(article, "class", "h-entry")
	HTML.append_child(article, HTML.create_element("h2", release.name))
	HTML.append_child(article, meta)
	HTML.append_child(article, content)
	if downloads ~= nil then
		HTML.append_child(article, downloads)
	end

	HTML.append_child(h_feed, article)
	HTML.append_child(h_feed, HTML.create_element("hr"))
end

local i = 1
while releases[i] do
	local release = releases[i]

	if not release.draft and not release.prerelease then
		add_release(release)
	end

	i = i + 1
end
