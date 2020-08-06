<script lang="typescript">
  import * as expr from "../../util/expr";
  import * as prec from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: expr.OfExpr;
  export let precedence: prec.Precedence;
</script>

<PrecedenceWrapper {precedence} expected={prec.Precedence.Term}>
  <span>{value.property}</span>
  <span>of</span>
  <Node value={value.expr} />
</PrecedenceWrapper>
