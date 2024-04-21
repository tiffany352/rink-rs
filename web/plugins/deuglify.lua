-- AsciiDoctor HTML fixing plugin for Soupault,
-- written by Tiffany Bennett <https://tiffnix.com>
--
-- This work is licensed under CC BY-SA 4.0
-- <https://creativecommons.org/licenses/by-sa/4.0/>
--
-- The idea with this plugin is to attempt to transform the HTML4-style
-- output from AsciiDoctor into modern HTML5 using semantic tags like
-- `<figure>`.

-- Inline all the section divs, they aren't really useful.
local sects = HTML.select(page, "section.e-content>div")
local index = 1
while sects[index] do
	local sect = sects[index]
	local body = HTML.select_one(sect, ".sectionbody")
	HTML.unwrap(body)
	HTML.unwrap(sect)
	index = index + 1
end

-- Adjust the <pre> tags to be more friendly to syntax highlighting.
local pres = HTML.select(page, "pre.highlight")
index = 1
while pres[index] do
	local pre = pres[index]
	HTML.delete_attribute(pre, "class")
	local code = HTML.select_one(pre, "code")
	if code then
		HTML.set_attribute(code, "class", "hljs")
	end
	index = index + 1
end

-- This unwraps some useless `<div><div class="content"></div></div>`.
-- But sometimes there is also a `<div class="title">` in there, which
-- should be transformed into a proper `<figure>` element.
local divs = HTML.select(page, "section.e-content>div")
index = 1
while divs[index] do
	local div = divs[index]
	local title = HTML.select_one(div, ".title")
	local content = HTML.select_one(div, ".content")
	local class = HTML.get_attribute(div, "class")
	if title and content then
		HTML.set_tag_name(div, "figure")
		HTML.delete_attribute(div, "class")
		HTML.set_tag_name(title, "figcaption")
		HTML.set_attribute(content, "class", class)	
		div = content
	end

	if index == 1 and class == "paragraph" then
		local p = HTML.select_one(div, "p")
		HTML.set_attribute(p, "class", "p-summary")
	end

	if class == "paragraph" or class == "imageblock" or class == "ulist" then
		HTML.unwrap(div)
	elseif class == "stemblock" then
		local content = HTML.select_one(div, ".content")
		if content then
			HTML.unwrap(content)
		end
		HTML.set_tag_name(div, "p")
		-- Add a class that allows stem blocks to be processed by katex.
		HTML.set_attribute(div, "class", "katex-block")
	elseif class == "listingblock" then
		local content = HTML.select_one(div, ".content")
		if content then
			HTML.unwrap(content)
		end
		HTML.unwrap(div)
	end
	index = index + 1
end

-- There should never be a `<li><p></p></li>`, so unwrap them.
local bad_ps = HTML.select(page, "li>p")
index = 1
while bad_ps[index] do
	HTML.unwrap(bad_ps[index])
	index = index + 1
end

-- Same with `<td><p></p></td>`.
bad_ps = HTML.select(page, "td>p")
index = 1
while bad_ps[index] do
	HTML.unwrap(bad_ps[index])
	index = index + 1
end
