<script lang="typescript">
  import type { NumberParts } from "../util/reply";
  import Dimensionality from "./Dimensionality.svelte";

  export let number: NumberParts;
</script>

{#if number.exactValue && number.exactValue.indexOf('/') != -1}
  {number.exactValue}, approx. {number.approxValue}
{:else if number.exactValue}
  {number.exactValue}
{:else if number.approxValue}approx. {number.approxValue}{/if}
{#if number.rawUnit}
  <Dimensionality quantity={number.rawUnit} />
{/if}
<!-- prettier-ignore -->
{#if number.quantity}
    (<a href={`/quantity/${number.quantity}`}>{number.quantity}</a>)
{/if}
