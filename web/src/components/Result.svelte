<script lang="typescript">
  import * as reply from "../util/reply";
  import NumberParts from "./NumberParts.svelte";
  import ExprParts from "./ExprParts.svelte";

  export let value: reply.QueryResult;
</script>

<style>
  .capitalize::first-letter {
    text-transform: capitalize;
  }
</style>

{#if value.type == 'number'}
  <p>
    <NumberParts number={value} />
  </p>
{:else if value.type == 'def'}
  <h3>{value.canonName}</h3>
  <p>Definition: {value.def}</p>
  {#if value.type == 'def' && value.defExpr && value.value}
    <p>
      Value:
      <ExprParts expr={value.defExpr.exprs} />
      =
      <NumberParts number={value.value} />
    </p>
  {:else if value.type == 'def' && value.defExpr}
    <p>Value: {value.defExpr}</p>
  {/if}
  {#if value.type == 'def' && value.doc}
    <p>{value.doc}</p>
  {/if}
  <!-- Errors -->
{:else if value.type == 'notFound' && value.suggestion != null}
  <p>No such unit {value.got}, did you mean {value.suggestion}?</p>
{:else if value.type == 'notFound'}
  <p>No such unit {value.got}.</p>
{:else if value.type == 'conformance'}
  <h3>Conformance Error</h3>
  <p>
    {value.left.quantity} ({value.left.dimensions}) != {value.right.quantity} ({value.right.dimensions})
  </p>
  <h4>Suggestions</h4>
  <ul>
    {#each value.suggestions as suggestion}
      <li class="capitalize">{suggestion}</li>
    {/each}
  </ul>
{:else if value.type == 'generic'}
  <p>{value.message}</p>
{:else}
  <h3>Unknown result type</h3>
  <pre>{JSON.stringify(value, null, 2)}</pre>
{/if}
