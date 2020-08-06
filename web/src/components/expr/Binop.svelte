<script lang="typescript">
  import * as expr from "../../util/expr";
  import * as prec from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: expr.BinOpExpr;
  export let precedence: prec.Precedence;
</script>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={prec.expectedPrecedence(value.op)}
  ><Node value={value.left} precedence={prec.expectedPrecedence(value.op) - 1}
  /><span
    >{prec.symbol(value.op)}</span
  ><Node
    value={value.right}
    precedence={prec.expectedPrecedence(value.op) - 1}
/></PrecedenceWrapper>
