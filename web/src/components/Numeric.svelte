<script lang="typescript">
  import * as expr from "../util/expr";

  export let number: expr.NumericParts;
</script>

{#if number.exactValue}
  {number.exactValue}
{:else if number.approxValue}
  approx. {number.approxValue}
{:else}{number.numer}/{number.denom}{/if}
