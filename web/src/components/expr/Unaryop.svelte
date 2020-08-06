<script lang="typescript">
  import * as expr from "../../util/expr";
  import Node from "./Node.svelte";

  export let value: expr.UnaryOpExpr;
</script>

<span>{value.op}</span>
<Node value={value.expr} />
