// AsciiDoctor preprocessor for Soupault,
// written by Tiffany Bennett <https://tiffnix.com>
//
// This work is licensed under CC BY-SA 4.0
// <https://creativecommons.org/licenses/by-sa/4.0/>
//
// Adds a `katex:[]` inline macro for math.
//
// Inserts extra metadata into the page using mf2 metadata. This is much
// more information than is included when using `asciidoctor
// --embedded`.

import Asciidoctor from "asciidoctor";

const authors = {
	Tiffany: {
		url: "https://tiffnix.com",
		pfp: "/images/profile-pic.png",
	},
};

let asciidoctor = Asciidoctor();
let registry = asciidoctor.Extensions.create();
let options = {
	extension_registry: registry,
	// Turn off section IDs, because I use soupault to generate them
	// instead.
	attributes: "sectids!",
	safe: "unsafe",
};

let path = process.argv[2];
let doc = asciidoctor.loadFile(path, options);

let output = [];

// .convert() doesn't include h1, so it's added here, with the mf2
// p-name tag.
if (doc.getTitle()) {
	output.push(`<h1 class="p-name">${doc.getTitle()}</h1>`);
}

let meta = [];

// Generate an mf2 h-card for the author
if (doc.getAuthor() != "") {
	let author = authors[doc.getAuthor()];
	if (author) {
		meta.push(
			`<span class="h-card">
				<img class="u-photo" src="${author.pfp}" alt="" />
				<a class="p-name u-url p-author" rel="me" href="${author.url}">${doc.getAuthor()}</a>
			</span>`,
		);
	}
}

// I do a nerd thing of rendering dates like `april 03, 2024` (in
// lowercase specifically). You might want to adjust it to your own
// tastes.
let format = new Intl.DateTimeFormat("en-US", {
	year: "numeric",
	month: "long",
	day: "2-digit",
});

function timeTag(date, klass) {
	let fmt = format.format(new Date(date)).toLowerCase();
	return `<time class="${klass}" datetime="${date}">${fmt}</time>`;
}

let published = doc.getAttribute("published");
if (published) {
	meta.push(timeTag(published, "dt-published"));
}

let revision = doc.getRevisionDate();
if (revision && published != revision) {
	let revHtml = timeTag(revision, "dt-updated");
	meta.push(`updated ${revHtml}`);
}

if (doc.hasAttribute("section")) {
	let sect = doc.getAttribute("section");
	meta.push(`<span class="p-category" id="post-section">${sect}</span>`);
}

// Meta elements are strung together with dots.
// Aesthetic choice, you might want to change it.
if (meta.length > 0 || doc.hasAttribute("uid")) {
	let uidHtml = "";
	if (doc.hasAttribute("uid")) {
		let uid = doc.getAttribute("uid");
		uidHtml = `<span class="u-uid">${uid}</span>`;
	}

	output.push(`<span class="meta">${meta.join(" • ")}${uidHtml}</span>`);
}

let html = doc.convert(options);
output.push(html);
console.log(output.join(""));
