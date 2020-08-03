<script lang="typescript">
  import * as reply from "../../util/reply";
  import NumberParts from "../NumberParts.svelte";

  export let value: reply.UnitListReply;
</script>

{#each value.list as unit, i}
  {#if i != 0}
    <span>,</span>
  {/if}
  <NumberParts number={unit} />
{/each}

<NumberParts number={value.rest} />
