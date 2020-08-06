<script lang="typescript">
  import type { BinOpExpr } from "../../util/expr";
  import type { Precedence } from "../../util/precedence";
  import { expectedPrecedence, symbol } from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: BinOpExpr;
  export let precedence: Precedence;
</script>

<!-- prettier-ignore -->
<PrecedenceWrapper {precedence} expected={expectedPrecedence(value.op)}
  ><Node value={value.left} precedence={expectedPrecedence(value.op) - 1}
  /><span
    >{symbol(value.op)}</span
  ><Node
    value={value.right}
    precedence={expectedPrecedence(value.op) - 1}
/></PrecedenceWrapper>
