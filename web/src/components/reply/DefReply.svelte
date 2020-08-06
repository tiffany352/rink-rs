<script lang="typescript">
  import type { DefReply } from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";
  import Expr from "../expr/Expr.svelte";

  export let value: DefReply;
</script>

<h3>{value.canonName}</h3>

{#if value.defExpr}
  <p>
    Definition:
    <Expr value={value.defExpr.ast} />
  </p>
{:else if value.type == 'def'}
  <p>Definition: {value.def}</p>
{/if}

{#if value.defExpr && value.value}
  <p>
    Value:
    <NumberParts number={value.value} />
  </p>
{:else if value.defExpr}
  <p>Value: {value.defExpr}</p>
{/if}

<p>{value.doc}</p>
