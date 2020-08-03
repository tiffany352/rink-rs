<script lang="typescript">
  import ExprNode from "./ExprNode.svelte";
  import * as expr from "../util/expr";

  export let value: expr.Expr;
</script>

<style>
  div {
    display: inline-flex;
    flex-direction: row;
    align-items: baseline;
  }
</style>

<div>
  <ExprNode {value} />
</div>
