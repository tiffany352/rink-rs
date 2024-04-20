export type RinkRequest = HelloReq | ExecuteReq;

export type HelloReq = {
	type: "hello",
	buffer: ArrayBuffer,
}

export type ExecuteReq = {
	type: "execute",
	id: number,
	query: string,
}

export type RinkResponse = HelloRes | ExecuteRes;

export type HelloRes = {
	type: "hello",
	version: string,
};

export type TokenType =
	| "plain"
	| "error"
	| "unit"
	| "quantity"
	| "number"
	| "user_input"
	| "list_begin"
	| "list_sep"
	| "doc_string"
	| "pow"
	| "prop_name"
	| "date_time";

export type Token = {
	type: "span";
	text: string;
	fmt: TokenType;
};
export type TokenList = {
	type: "list";
	children: [SpanOrList];
};
export type SpanOrList = Token | TokenList;

export type ExecuteRes = {
	type: "execute",
	id: number,
	status: "success",
	tokens: [SpanOrList],
} | {
	type: "execute",
	id: number,
	status: "error",
	message: string,
};
