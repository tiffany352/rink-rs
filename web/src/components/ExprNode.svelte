<script lang="typescript">
  import * as expr from "../util/expr";
  import PrecedenceWrapper from "./PrecedenceWrapper.svelte";
  import Numeric from "./Numeric.svelte";

  export let value: expr.Expr;
  export let precedence: expr.Precedence = expr.Precedence.Equals;

  function symbol(op: expr.BinOpType): string {
    switch (op) {
      case expr.BinOpType.Add:
        return " + ";
      case expr.BinOpType.Sub:
        return " - ";
      case expr.BinOpType.Frac:
        return " / ";
      case expr.BinOpType.Pow:
        return "^";
      case expr.BinOpType.Equals:
        return " = ";
    }
  }

  function expectedPrecedence(op: expr.BinOpType): expr.Precedence {
    switch (op) {
      case expr.BinOpType.Add:
        return expr.Precedence.Add;
      case expr.BinOpType.Sub:
        return expr.Precedence.Add;
      case expr.BinOpType.Frac:
        return expr.Precedence.Div;
      case expr.BinOpType.Pow:
        return expr.Precedence.Pow;
      case expr.BinOpType.Equals:
        return expr.Precedence.Equals;
    }
  }

  interface Fraction {
    type: "binop";
    op: expr.BinOpType.Frac;
    left: expr.ConstExpr;
    right: expr.ConstExpr;
  }

  function isFraction(value: expr.Expr): value is Fraction {
    return (
      value.type == "binop" &&
      value.op == expr.BinOpType.Frac &&
      value.left.type == "const" &&
      value.right.type == "const"
    );
  }
</script>

<style>
  .highlight {
    font-weight: 500;
  }
  span {
    white-space: pre;
  }

  sup,
  sub {
    vertical-align: baseline;
    position: relative;
    top: -0.4em;
  }
  sub {
    top: 0.4em;
  }
</style>

{#if value.type == 'unit'}
  <span class="highlight">{value.name}</span>
{:else if value.type == 'quote'}
  <span>"{value.string}"</span>
{:else if value.type == 'const'}
  <span>
    <Numeric number={value.value} />
  </span>
{:else if value.type == 'date'}
  <pre>{(JSON.stringify(value), null, 2)}</pre>
{:else if value.type == 'binop' && value.op == 'pow'}
  <PrecedenceWrapper {precedence} expected={expr.Precedence.Pow}>
    <svelte:self value={value.left} precedence={expr.Precedence.Term} />
    <sup>
      <svelte:self value={value.right} precedence={expr.Precedence.Term} />
    </sup>
  </PrecedenceWrapper>
{:else if isFraction(value)}
  <sup>
    <svelte:self
      value={value.left}
      precedence={expectedPrecedence(value.op) - 1} />
  </sup>
  <!-- ∕ U+2215 division slash-->
  <span>&#x2215;</span>
  <sub>
    <svelte:self
      value={value.right}
      precedence={expectedPrecedence(value.op) - 1} />
  </sub>
{:else if value.type == 'binop'}
  <PrecedenceWrapper {precedence} expected={expectedPrecedence(value.op)}>
    <svelte:self
      value={value.left}
      precedence={expectedPrecedence(value.op) - 1} />
    <span>{symbol(value.op)}</span>
    <svelte:self
      value={value.right}
      precedence={expectedPrecedence(value.op) - 1} />
  </PrecedenceWrapper>
{:else if value.type == 'unaryop'}
  <span>{value.op}</span>
  <svelte:self value={value.expr} />
{:else if value.type == 'mul'}
  <PrecedenceWrapper {precedence} expected={expr.Precedence.Mul}>
    {#each value.exprs as item, i}
      {#if i != 0}
        <span>&ThinSpace;</span>
      {/if}
      <svelte:self value={item} precedence={expr.Precedence.Div} />
    {/each}
  </PrecedenceWrapper>
{:else if value.type == 'of'}
  <PrecedenceWrapper {precedence} expected={expr.Precedence.Add}>
    <span>{value.property}</span>
    <span>of</span>
    <svelte:self value={value.expr} />
  </PrecedenceWrapper>
{:else if value.type == 'call'}
  <span>{value.func}</span>
  <span>(</span>
  {#each value.args as arg}
    <svelte:self value={arg} />
  {/each}
  <span>)</span>
{:else if value.type == 'error'}
  <span style="color: red">{value.message}</span>
{/if}
