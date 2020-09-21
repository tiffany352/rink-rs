export interface DefBase {
  name: string;
  doc: string | null;
  category: string | null;
}

export interface Dimension extends DefBase {
  type: "dimension";
}

export interface Canonicalization extends DefBase {
  type: "canonicalization";
  of: string;
}

export interface Prefix extends DefBase {
  type: "prefix";
  expr: string;
}

export interface SPrefix extends DefBase {
  type: "sprefix";
  expr: string;
}

export interface Unit extends DefBase {
  type: "unit";
  expr: string;
}

export interface Quantity extends DefBase {
  type: "quantity";
  expr: string;
}

export interface Property extends DefBase {
  name: string;
  doc: string | null;
  input: string;
  inputName: string;
  output: string;
  outputName: string;
}

export interface Substance extends DefBase {
  type: "substance";
  symbol: string | null;
  properties: Property[];
}

export interface Category extends DefBase {
  type: "category";
  displayName: string;
}

export interface Error extends DefBase {
  type: "error";
  message: string;
}

export type Def =
  | Dimension
  | Canonicalization
  | Prefix
  | SPrefix
  | Unit
  | Quantity
  | Substance
  | Category
  | Error;
