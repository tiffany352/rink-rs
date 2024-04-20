import type { ExecuteRes, HelloReq, HelloRes, RinkResponse, SpanOrList } from "./proto";

// Taken from https://stackoverflow.com/a/3809435
const urlRegex =
	/https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)/g;
const powRegex = /\^(\-?\d+)/g;

function buildInline(parent: HTMLElement, text: string) {
	// escape any html tags
	parent.innerText = text;
	text = parent.innerHTML;
	// apply the regexes
	text = text.replace(
		urlRegex,
		(match) => `<a href="${match}" rel="nofollow">${match}</a>`,
	);
	text = text.replace(powRegex, (_match, rest) => `<sup>${rest}</sup>`);
	parent.innerHTML = text;
}

const dateFmt = new Intl.DateTimeFormat(undefined, {
	dateStyle: "long",
	timeStyle: "long",
});

function buildHtml(tokens: [SpanOrList], parent: HTMLElement) {
	let ul: HTMLUListElement | null = null;
	let cur: HTMLElement = parent;
	for (const token of tokens) {
		if (token.type == "list") {
			buildHtml(token.children, cur);
		} else if (token.fmt == "pow") {
			let span = document.createElement("span");
			span.classList.add(`hl-hidden`);
			span.innerText = "^";

			let sup = document.createElement("sup");
			let text = token.text.replace(/^\^/, "");
			sup.append(span, text);
			sup.prepend(span);

			cur.appendChild(sup);
		} else if (token.fmt == "list_begin") {
			ul = document.createElement("ul");
			parent.appendChild(ul);
			let li = document.createElement("li");
			cur = li;
			ul.appendChild(li);
		} else if (token.fmt == "list_sep" && ul) {
			let li = document.createElement("li");
			cur = li;
			ul.appendChild(li);
		} else if (token.fmt == "date_time") {
			let time = document.createElement("time");
			let date = new Date(token.text);
			time.setAttribute("datetime", date.toISOString());
			time.innerText = dateFmt.format(date);
			cur.appendChild(time);
		} else {
			let span = document.createElement("span");
			span.classList.add(`hl-${token.fmt.replace("_", "-")}`);
			buildInline(span, token.text);
			cur.appendChild(span);
		}
	}
}

let rinkDiv: HTMLElement = document.querySelector("#rink-outputs")!;
let form: HTMLFormElement = document.querySelector("#rink-query")!;
let textEntry: HTMLInputElement = document.querySelector("#query")!;

function pushError(error: Error | string) {
	let message: string;
	if (error instanceof Error) {
		message = error.message;
	} else {
		message = error;
	}

	const element = document.createElement("p");
	element.classList.add("hl-error");
	element.innerText = message;
	rinkDiv.appendChild(element);
}

// The only way to fetch with progress is to use the old fashioned XMLHttpRequest API.
const wasmBlob = new Promise<ArrayBuffer>((resolve, reject) => {
	const label = document.createElement("label");
	label.htmlFor = "rink-dl";
	label.innerText = "Downloading rink.wasm...";
	const progress = document.createElement("progress");
	progress.id = "rink-dl";
	progress.value = 0;
	const cancel = document.createElement("button");
	cancel.innerText = "Cancel";

	rinkDiv.appendChild(label);
	rinkDiv.appendChild(progress);
	rinkDiv.appendChild(cancel);

	const req = new XMLHttpRequest();
	req.open("GET", "/assets/rink_js_bg.wasm");
	req.responseType = "arraybuffer";

	const onFinish = () => {
		label.remove();
		progress.remove();
		cancel.remove();
	};

	req.onprogress = (event) => {
		let total = event.total;
		// ugly hack because for some reason browsers don't report correct totals
		const length = req.getResponseHeader("Content-Length");
		if (!event.lengthComputable && length) {
			total = parseInt(length);
		}
		progress.value = event.loaded;
		progress.max = total;
		const totalKb = Math.floor(total / 1000).toString();
		const loadedKb = Math.floor(event.loaded / 1000)
			.toString()
			.padStart(totalKb.length, "0");
		label.innerHTML = `Downloading rink.wasm... <code>${loadedKb} / ${totalKb}</code> kB`;
	};
	req.onload = (_event) => {
		onFinish();
		const buffer: ArrayBuffer = req.response;
		if (buffer) {
			resolve(buffer);
		} else {
			reject("Unable to get binary data from request");
		}
	};
	req.onerror = (_event) => {
		onFinish();
		reject("Download failed");
	};
	req.onabort = (_event) => {
		onFinish();
		reject("Download aborted");
	};

	cancel.onclick = () => {
		req.abort();
	};

	req.send();
});

let wasmBuffer: ArrayBuffer | null = null;
let worker: Worker | null = null;
function startWorker() {
	worker = new Worker(new URL('./worker.ts', import.meta.url));
	worker.postMessage({
		type: "hello",
		buffer: wasmBuffer,
	} as HelloReq);
}

wasmBlob.then((buffer) => {
	wasmBuffer = buffer;
	startWorker();

	function expectMessage(type: string, id: number | null): Promise<RinkResponse> {
		return new Promise((resolve, _reject) => {
			const handler = (event: MessageEvent<RinkResponse>) => {
				const msg = event.data;
				if (msg.type == type && (!id || (msg as any).id == id)) {
					worker!.removeEventListener("message", handler);
					resolve(msg);
				}
			};
			worker!.addEventListener("message", handler);
		})
	}

	let queryCounter = 1;
	function requestExecute(query: string): Promise<[SpanOrList]> {
		const id = queryCounter++;
		worker!.postMessage({
			type: "execute",
			id,
			query,
		})
		return new Promise((resolve, reject) => {
			const timeoutId = setTimeout(() => {
				worker!.terminate();
				startWorker();
				reject("Terminated after 5 seconds");
			}, 5000);
			expectMessage("execute", id).then((msg) => {
				clearTimeout(timeoutId);
				msg = msg as ExecuteRes;
				if (msg.status == "success")
					resolve(msg.tokens);
				else
					reject(msg.message);
			});
		})
	}

	let history = JSON.parse(
		window.localStorage.getItem("rink-history") || "[]",
	);
	let historyIndex = history.length;

	async function execute(queryString: string) {
		let quote = document.createElement("blockquote");
		quote.innerText = queryString;
		let permalink = document.createElement("a");
		permalink.href = `${location.origin}/?q=${queryString}`;
		permalink.text = "#";
		quote.appendChild(permalink);
		rinkDiv.appendChild(quote);

		let p = document.createElement("p");
		p.innerHTML = "<progress></progress>";
		rinkDiv.appendChild(p);

		try {
			const tokens = await requestExecute(queryString);
			console.log("formatting tokens: ", tokens);

			p.innerHTML = "";
			buildHtml(tokens, p);

			rinkDiv.appendChild(p);
			textEntry.value = "";
			window.scrollTo(0, document.body.scrollHeight);

			// prevent duplicates in history
			history = history.filter(
				(query: string) => query != queryString,
			);
			history.push(queryString);
			// keep history from becoming too long
			if (history.length > 200) history.shift();
			historyIndex = history.length;
			window.localStorage.setItem(
				"rink-history",
				JSON.stringify(history),
			);
		} catch (error) {
			pushError(error);
			p.remove();
		}
	}

	async function on_hello(msg: HelloRes) {
		let h1 = document.querySelector("h1");
		if (h1) {
			h1.innerText = `Rink ${msg.version}`;
		}
		// clear the loading message
		textEntry.placeholder = "Enter a query, like `3 feet to meters`";

		const urlParams = new URLSearchParams(window.location.search);
		const queries = urlParams.getAll("q");
		for (const q of queries) {
			await execute(q);
		}
	}

	expectMessage("hello", null).then((res) => on_hello(res as HelloRes)).catch((error) => pushError(error));

	form.addEventListener("submit", (event) => {
		event.preventDefault();
		execute(textEntry.value);
	});

	textEntry.addEventListener("keydown", (event) => {
		if (event instanceof KeyboardEvent && event.key == "ArrowUp") {
			event.preventDefault();
			historyIndex--;
			if (historyIndex < 0) historyIndex = 0;
			textEntry.value = history[historyIndex];
		} else if (
			event instanceof KeyboardEvent &&
			event.key == "ArrowDown"
		) {
			event.preventDefault();
			historyIndex++;
			if (historyIndex > history.length)
				historyIndex = history.length;
			if (history[historyIndex])
				textEntry.value = history[historyIndex];
			else textEntry.value = "";
		}
	});
}).catch((error) => {
	pushError(error);
	textEntry.placeholder = "Loading failed.";
	textEntry.disabled = true;
});

// unregister any service workers left over from previous versions of the site
navigator.serviceWorker.getRegistrations().then((registrations) => {
	for (const registration of registrations) {
		console.log("unregistering service worker", registration);
		registration.unregister();
	}
});
