import hljs from "highlight.js";
import hljsSvelte from "highlightjs-svelte";
import fs from "fs";

hljsSvelte(hljs);

let input = fs.readFileSync(0);

let lang = process.env["ATTR_DATA-LANG"];
let lang2 = process.env["CLASS"];
lang2 = lang2 && lang2.replace("^language-", "");
let html;
if (lang && lang != "undefined") {
	html = hljs.highlight(input.toString(), {
		language: process.env["ATTR_DATA-LANG"],
	});
} else if (lang2 && lang2 != "undefined") {
	html = hljs.highlight(input.toString(), {
		language: lang2,
	});
} else {
	html = hljs.highlightAuto(input.toString());
}

console.log(html.value.toString());
