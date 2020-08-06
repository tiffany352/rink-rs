import { BinOpType, ConstExpr, Expr } from "./expr";

export enum Precedence {
  Term = 0,
  Plus = 1,
  Pow = 2,
  Mul = 3,
  Div = 4,
  Add = 5,
  Equals = 6,
}

export function symbol(op: BinOpType): string {
  switch (op) {
    case BinOpType.Add:
      return " + ";
    case BinOpType.Sub:
      return " - ";
    case BinOpType.Frac:
      return " / ";
    case BinOpType.Pow:
      return "^";
    case BinOpType.Equals:
      return " = ";
  }
}

export function expectedPrecedence(op: BinOpType): Precedence {
  switch (op) {
    case BinOpType.Add:
      return Precedence.Add;
    case BinOpType.Sub:
      return Precedence.Add;
    case BinOpType.Frac:
      return Precedence.Div;
    case BinOpType.Pow:
      return Precedence.Pow;
    case BinOpType.Equals:
      return Precedence.Equals;
  }
}

export interface Fraction {
  type: "binop";
  op: BinOpType.Frac;
  left: ConstExpr;
  right: ConstExpr;
}

export function isFraction(value: Expr): value is Fraction {
  return (
    value.type == "binop" &&
    value.op == BinOpType.Frac &&
    value.left.type == "const" &&
    value.right.type == "const"
  );
}

export interface PowExpr {
  type: "binop";
  op: BinOpType.Pow;
  left: Expr;
  right: Expr;
}

export function isPow(value: Expr): value is PowExpr {
  return value.type == "binop" && value.op == BinOpType.Pow;
}
