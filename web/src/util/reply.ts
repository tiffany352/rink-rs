export interface NumberParts {
  exact_value: string | null;
  approx_value: string | null;
  factor: string | null;
  divfactor: string | null;
  rawUnit: Quantity | null;
  unit: string | null;
  quantity: string | null;
  dimensions: string | null;
}

export interface Quantity {
  [dimension: string]: number;
}

export type NumberReply = NumberParts & { type: "number" };

export interface DateReply {
  type: "date";
  year: number;
  month: number;
  day: number;
  hour: number;
  minute: number;
  second: number;
  nanosecond: number;
  string: string;
}

export interface PropertyReply {
  name: string;
  value: NumberParts;
  doc: string | null;
}

export interface SubstanceReply {
  type: "substance";
  name: String;
  doc: string | null;
  amount: NumberParts;
  properties: PropertyReply[];
}

export interface DurationReply {
  type: "duration";
  raw: NumberParts;
  years: NumberParts;
  months: NumberParts;
  weeks: NumberParts;
  days: NumberParts;
  hours: NumberParts;
  minutes: NumberParts;
  seconds: NumberParts;
}

export interface ExprLiteral {
  type: "literal";
  text: string;
}

export interface ExprUnit {
  type: "unit";
  name: string;
}

export interface ExprProperty {
  type: "property";
  property: string;
  subject: ExprParts[];
}

export interface ExprError {
  type: "error";
  message: string;
}

export type ExprParts = ExprLiteral | ExprUnit | ExprProperty | ExprError;

export interface ExprReply {
  exprs: ExprParts[];
}

export interface DefReply {
  type: "def";
  canonName: string;
  def: string | null;
  defExpr: ExprReply | null;
  value: NumberParts | null;
  doc: string | null;
}

export interface ConversionReply {
  type: "conversion";
  value: NumberParts;
}

export interface FactorizeReply {
  type: "factorize";
  factorizations: Quantity[];
}

export interface UnitsInCategory {
  category: string | null;
  units: string[];
}

export interface UnitsForReply {
  type: "unitsFor";
  units: UnitsInCategory[];
  of: NumberParts;
}

export interface UnitListReply {
  type: "unitList";
  rest: NumberParts;
  list: NumberParts[];
}

export interface SearchReply {
  type: "search";
  results: NumberParts[];
}

export type QueryReply =
  | NumberReply
  | DateReply
  | SubstanceReply
  | DurationReply
  | DefReply
  | ConversionReply
  | FactorizeReply
  | UnitsForReply
  | UnitListReply
  | SearchReply;

export interface ConformanceError {
  type: "conformance";
  left: NumberParts;
  right: NumberParts;
  suggestions: string[];
}

export interface NotFoundError {
  type: "notFound";
  got: string;
  suggestion: string | null;
}

export interface GenericError {
  type: "generic";
  message: string;
}

export type QueryError = ConformanceError | NotFoundError | GenericError;

export type Result<Ok, Err> =
  | ({
      success: "ok";
    } & Ok)
  | ({ success: "err" } & Err);

export type QueryResult = Result<QueryReply, QueryError>;
