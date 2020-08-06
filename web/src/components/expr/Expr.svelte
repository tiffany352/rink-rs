<script lang="typescript">
  import Node from "./Node.svelte";
  import * as expr from "../../util/expr";

  export let value: expr.Expr;
</script>

<span>
  <Node {value} />
</span>
