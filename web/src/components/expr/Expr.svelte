<script lang="typescript">
  import Node from "./Node.svelte";
  import type { Expr } from "../../util/expr";

  export let value: Expr;
</script>

<span>
  <Node {value} />
</span>
