<script lang="typescript">
  import type { OfExpr } from "../../util/expr";
  import { Precedence } from "../../util/precedence";
  import PrecedenceWrapper from "../PrecedenceWrapper.svelte";
  import Node from "./Node.svelte";

  export let value: OfExpr;
  export let precedence: Precedence;
</script>

<PrecedenceWrapper {precedence} expected={Precedence.Term}>
  <span>{value.property}</span>
  <span>of</span>
  <Node value={value.expr} />
</PrecedenceWrapper>
