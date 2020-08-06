<script lang="typescript">
  import * as expr from "../../util/expr";
  import * as prec from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";

  export let value: expr.BinOpExpr;
  export let precedence: prec.Precedence;
</script>

<PrecedenceWrapper {precedence} expected={prec.expectedPrecedence(value.op)}>
  <svelte:self
    value={value.left}
    precedence={prec.expectedPrecedence(value.op) - 1} />
  <span>{prec.symbol(value.op)}</span>
  <svelte:self
    value={value.right}
    precedence={prec.expectedPrecedence(value.op) - 1} />
</PrecedenceWrapper>
