-- Atom feed generator

Plugin.require_version("4.0.0")

-- If you have it installed, the image transcoding plugin will
-- be used to process images inside your Atom feeds.
local plugins_dir = soupault_config["plugins_dir"] or "plugins"
local images_plugin_path = Sys.join_path(plugins_dir, "images.lua")
if Sys.file_exists(images_plugin_path) then
	have_images_plugin = 1
	dofile(images_plugin_path)
	Log.info("Found images plugin")
else
	Log.warning("Images plugin not found")
end

data = {}

date_input_formats = soupault_config["index"]["date_formats"]

feed_file = config["feed_file"]

custom_options = soupault_config["custom_options"]

if not Table.has_key(custom_options, "site_url") then
	Plugin.exit(
		[[Atom feed generation is not enabled in the config. If you want to enable it, set custom_options.atom_feeds = true]]
	)
end

if not Table.has_key(custom_options, "site_url") then
	Plugin.fail(
		[[custom_options["site_url"] option is required when feed generation is enabled]]
	)
end

local site_url = custom_options["site_url"]
data["site_url"] = site_url
data["feed_id"] = Sys.join_path(site_url, feed_file)

data["soupault_version"] = Plugin.soupault_version()

data["feed_author"] = custom_options["site_author"]
data["feed_author_email"] = custom_options["site_author_email"]
data["feed_title"] = custom_options["site_title"]
data["feed_subtitle"] = custom_options["site_subtitle"]
if custom_options["site_logo"] then
	data["feed_logo"] = Sys.join_path(site_url, custom_options["site_logo"])
end
if custom_options["site_icon"] then
	data["feed_icon"] = Sys.join_path(site_url, custom_options["site_icon"])
end

function in_section(entry)
	return (entry["nav_path"][1] == config["use_section"])
		and entry["published"]
end

function tags_match(entry)
	if config["use_tag"] then
		return Regex.match(entry["tags"], format("\\b%s\\b", config["use_tag"]))
	else
		return 1
	end
end

entries = {}

-- Original, unfiltered entries inded
local n = 1

-- Index of the new array of entries we are building
local m = 1

local count = size(site_index)
while n <= count do
	entry = site_index[n]
	if in_section(entry) and tags_match(entry) then
		if entry["published"] then
			entry["published"] = Date.reformat(
				entry["published"],
				date_input_formats,
				"%Y-%m-%dT%H:%M:%S%:z"
			)
		end
		if entry["updated"] then
			entry["updated"] = Date.reformat(
				entry["updated"],
				date_input_formats,
				"%Y-%m-%dT%H:%M:%S%:z"
			)
		end
		if entry["excerpt"] then
			local html = HTML.parse(entry["excerpt"])
			entry["excerpt"] = HTML.inner_text(html)
		end
		if entry["content"] then
			-- HTML.unwrap() errors out if elements have
			-- no parent, so wrap everything.
			local html = HTML.parse(
				'<article class="e-content">'
					.. entry["content"]
					.. "</article>"
			)
			-- process images
			if have_images_plugin then
				process_page(html)
			end
			-- in this context, no styling exists, so strip out
			-- all the katex HTML and only include MathML.
			local katexes = HTML.select(html, "span.katex")
			local i = 1
			while katexes[i] do
				local katex = katexes[i]
				local katex_html = HTML.select_one(katex, "span.katex-html")
				HTML.delete(katex_html)
				local katex_mathml = HTML.select_one(katex, "span.katex-mathml")
				HTML.unwrap(katex_mathml)
				HTML.unwrap(katex)
				i = i + 1
			end
			-- strip out stuff that was explicitly asked to be stripped out.
			local strips = HTML.select(html, ".atom-feed-strip")
			i = 1
			while strips[i] do
				HTML.delete(strips[i])
				i = i + 1
			end
			-- remove all `class` attributes, since they're useless here.
			local styles = HTML.select(html, "[class]")
			i = 1
			while styles[i] do
				HTML.delete_attribute(styles[i], "class")
				i = i + 1
			end
			-- remove useless divs
			local divs = HTML.select(html, "div")
			i = 1
			while divs[i] do
				local div = divs[i]
				if
					next(HTML.list_attributes(div)) == nil
					and HTML.parent(div) ~= nil
				then
					HTML.unwrap(div)
				end
				i = i + 1
			end
			-- remove useless spans
			local spans = HTML.select(html, "span")
			i = 1
			while spans[i] do
				local span = spans[i]
				if
					next(HTML.list_attributes(span)) == nil
					and HTML.parent(span) ~= nil
				then
					HTML.unwrap(span)
				end
				i = i + 1
			end

			entry["content"] = HTML.pretty_print(html)
		end

		entries[m] = entry
		m = m + 1
	end
	n = n + 1
end

if
	soupault_config["index"]["sort_descending"]
	or (not Table.has_key(soupault_config["index"], "sort_descending"))
then
	data["feed_last_updated"] = entries[1]["updated"]
else
	data["feed_last_updated"] = entries[size(entries)]["published"]
end

data["entries"] = entries

feed_template = [[
<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en">
  <id>{{feed_id}}</id>
  <link href="{{feed_id}}" rel="self" />
  <updated>{{feed_last_updated}}</updated>
  <title>{{feed_title}}</title>
  {%- if feed_subtitle -%} <subtitle>{{feed_subtitle}}</subtitle> {%- endif -%}
  {%- if feed_logo -%} <logo>{{feed_logo}}</logo> {%- endif -%}
  {%- if feed_icon -%} <icon>{{feed_icon}}</icon> {%- endif -%}
  <author>
    <name>{{feed_author}}</name>
    {%- if feed_author_email -%}<email>{{feed_author_email}}</email> {%- endif -%}
  </author>
  {%- for e in entries %}
  <entry>
	{%- if e.uid -%}
	<id>{{e.uid}}</id>
	{%- else -%}
    <id>{{site_url}}{{e.url}}</id>
	{%- endif -%}
	{%- if e.section -%}<category term="{{e.section}}" />{%- endif -%}
    <title>{{e.title}}</title>
	<published>{{e.published}}</published>
    <updated>{{e.updated}}</updated>
    <summary>{{e.excerpt}}</summary>
    <content type="html">
    {{e.content | escape}}
    </content>
    <link href="{{site_url}}{{e.url}}" rel="alternate"/>
  </entry>
  {% endfor %}
</feed>
]]

feed = String.render_template(feed_template, data)

local path = Sys.join_path(target_dir, feed_file)
Sys.write_file(path, String.trim(feed))
