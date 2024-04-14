// AsciiDoctor stem block to katex widget for Soupault,
// written by Tiffany Bennett <https://tiffnix.com>
//
// This work is licensed under CC BY-SA 4.0
// <https://creativecommons.org/licenses/by-sa/4.0/>

import katex from "katex";
import fs from "fs";

let input = fs.readFileSync(0);

let strip_html = /^\s*<div[^>]*>\s*(.*)\s*<\/div>\s*$/;
let result = strip_html.exec(input);
if (result) {
	input = result[1];
}

let strip_brackets = /^\s*\\\[\s*(.*)\s*\\\]\s*$/;
result = strip_brackets.exec(input);
if (result) {
	input = result[1];
}

let html = katex.renderToString(String.raw`${input}`, {
	throwOnError: false,
	displayMode: true,
});
console.log(html);
