<script lang="typescript">
  import type { UnaryOpExpr } from "../../util/expr";
  import Node from "./Node.svelte";

  export let value: UnaryOpExpr;
</script>

<span>{value.op}</span>
<Node value={value.expr} />
