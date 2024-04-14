-- Element translocation plugin for Soupault,
-- written by Tiffany Bennett <https://tiffnix.com>
--
-- This work is licensed under CC BY-SA 4.0
-- <https://creativecommons.org/licenses/by-sa/4.0/>
--
-- This allows you to pluck an element from somewhere in the page and
-- move it to another part of the page.

-- The selector to use to find the element to be moved.
selector = config["selector"]
-- The selector of where the element should be moved to.
new_parent = config["new_parent"]
-- https://soupault.app/reference-manual/#glossary-action
action = config["action"]

if not selector or not new_parent or not action then
	Log.error("selector, new_parent, and action configurations are required")
end

element = HTML.select_one(page, selector)
new_parent_elt = HTML.select_one(page, new_parent)

if not element then
	Log.info("Selector "..selector.." didn't match anything")
	return
end
if not new_parent_elt then
	Log.info("Selector "..new_parent.." didn't match anything")
	return
end

if action == "prepend_child" then
	HTML.prepend_child(new_parent_elt, element)
elseif action == "append_child" then
	HTML.append_child(new_parent_elt, element)
elseif action == "insert_before" then
	HTML.insert_before(new_parent_elt, element)
elseif action == "insert_after" then
	HTML.insert_after(new_parent_elt, element)
elseif action == "replace_content" then
	HTML.replace_content(new_parent_elt, element)
elseif action == "replace_element" then
	HTML.insert_after(new_parent_elt, element)
else
	Log.error("Unknown action "..action)
end
