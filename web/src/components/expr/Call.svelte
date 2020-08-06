<script lang="typescript">
  import * as expr from "../../util/expr";

  export let value: expr.CallExpr;
</script>

<span>{value.func}</span>
<span>(</span>
{#each value.args as arg}
  <svelte:self value={arg} />
{/each}
<span>)</span>
