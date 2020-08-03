<script lang="typescript">
  import * as reply from "../util/reply";

  export let number: reply.NumberParts;
</script>

{#if number.exactValue}
  {number.exactValue}
{:else}approx. {number.approxValue}{/if}
{#if number.unit}{number.unit}{/if}
{#if number.quantity}({number.quantity}){/if}
