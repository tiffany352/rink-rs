<script lang="typescript">
  import type { NumericParts } from "../util/expr";

  export let number: NumericParts;
</script>

{#if number.exactValue}
  {number.exactValue}
{:else if number.approxValue}
  approx. {number.approxValue}
{:else}{number.numer}/{number.denom}{/if}
