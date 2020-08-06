<script lang="typescript">
  import type { CallExpr } from "../../util/expr";

  export let value: CallExpr;
</script>

<span>{value.func}</span>
<span>(</span>
{#each value.args as arg}
  <svelte:self value={arg} />
{/each}
<span>)</span>
